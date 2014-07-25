#lang racket/base
;
; Twilight <-> Sparkle Communication Component
;

(require racket/contract
         racket/pretty
         racket/string
         racket/match
         racket/class
         racket/list
         racket/set
         srfi/43
         srfi/26
         tasks
         json
         zmq)

(require libuuid)

(provide (all-defined-out))


;; Function that will be called for every sent and received message
;; with ('in/'out payload) arguments to facilitate debugging.
(define current-communicator-logger (make-parameter void))


;; Communicator logger implementation that writes to stdout.
(define (pretty-communicator-logger direction payload)
  (printf "~a ~a\n" (if (eq? 'in direction) "<-" "->")
                    (string-replace (pretty-format payload 76) "\n" "\n   ")))


;; Retrieve dependency score for given change.
;; Some change types should be incorporated before others,
;; this is a simple dependency management using type for ordering.
(define (score-change c)
  (define order
    #("nic" "bond" "net_role" "disk" "storage_pool" "image" "extent"
      "volume" "volume"))

  (match c
    ((list "volume" _ _ (hash-table ('base_image (not #f)) _ ...))
     (add1 (vector-index-right (cut string=? "volume" <>) order)))

    ((list name _ _ _)
     ((if (cadddr c) + -)
      (add1 (or (vector-index (cut string=? (car c) <>) order) 999))))))


(define communicator%
  (class object%
    ;; Twilight instance that we work for.
    (init-field twilight)

    ;; Copy uuid from Twilight as a convenience.
    (field (uuid (get-field uuid twilight)))

    ;; Address of Sparkle to connect to.
    (field (connect-to (get-field connect-to twilight)))

    ;; Connection to the Sparkle cloud controller.
    (field (router (socket 'router
                           #:identity (uuid-generate)
                           #:connect (list connect-to))))

    ;; Incarnations of the local and remote changes streams.
    ;; We start with completely bogus IDs, which will lead to an early reset.
    (field (local-incarnation  (uuid-generate))
           (remote-incarnation (uuid-generate)))

    ;; Next change number of local and remote streams.
    ;; Starting unknown incarnation with 1 is invalid and remote
    ;; change number does not matter with wrong incarnation above.
    (field (local-sequence  1)
           (remote-sequence 0))

    ;; Flag indicating that our outgoing stream have been synchronized
    ;; and we can send incremental changes.  Happens after we send an
    ;; initial message.
    (field (synchronized? #f))

    ;; Collection of current states of local entities.
    ;; Gets updated by the public method (publish changes).
    (field (local-state (make-hash)))

    ;; Collection of desired states of remote entities.
    ;; Gets updated by the private method (receive-changes changes).
    (field (remote-state (make-hash)))


    (define/private (keep-alive)
      ;; Send empty list of changes to appear up.
      (send-changes null))


    (define/private (send-changes changes)
      ;; Send a state exchange message.
      (send-message (hasheq 'uuid uuid
                            'incarnation local-incarnation
                            'seq local-sequence
                            'event "update"
                            'changes changes))

      ;; Bump local sequence number.
      (set! local-sequence (add1 local-sequence)))


    (define/private (send-message payload)
      ;; Log outbound message.
      ((current-communicator-logger) 'out payload)

      ;; Send payload to sparkle with current time.
      (socket-send router "sparkle"
                          (jsexpr->bytes payload)
                          (number->string (current-seconds))))


    (define/private (send-resync-request)
      ;; Ask Sparkle for complete dump of our configuration since the
      ;; incremental stream of changes have been disrupted or we are
      ;; just starting.
      (send-message (hasheq 'uuid uuid
                            'event "resync")))


    (define/private (receive message-bytes time-bytes)
      ;; Get time as a number.
      (define time (string->number (bytes->string/utf-8 time-bytes)))

      ;; Deserialize the payload.
      (define message (bytes->jsexpr message-bytes #:null #f))

      ;; Log inbound message.
      ((current-communicator-logger) 'in message)

      (cond
        ;; Verify that the message is fresh enough.
        ((> (current-seconds) (+ time 15))
         (void))

        ;; Handle resync requests.
        ;; We can safely ignore them if we have just complied.
        ((string=? "resync" (hash-ref message 'event))
         (unless (= local-sequence 1)
           (set! local-sequence 0)
           (set! synchronized? #t)
           (send-changes (for/list (((k v) local-state))
                           (mangle (first k) (second k) v)))))

        ;; From here on, it's only desired state update.
        ((not (string=? "update" (hash-ref message 'event)))
         (void))

        ;; Verify that we are talking to the same Sparkle instance.
        ((not (string=? remote-incarnation (hash-ref message 'incarnation)))
         (set! remote-incarnation (hash-ref message 'incarnation))
         (set! remote-sequence 0)
         (send-resync-request))

        ;; Treat full resync specially.
        ((= remote-sequence 0)
         (receive-changes/full (hash-ref message 'changes))
         (set! remote-sequence (add1 remote-sequence)))

        ;; Verify that we did not miss any messages.
        ((not (= remote-sequence (hash-ref message 'seq)))
         (set! remote-sequence 0)
         (send-resync-request))

        ;; Finally, handle incremental desired state changes.
        (else
         (receive-changes (hash-ref message 'changes))
         (set! remote-sequence (add1 remote-sequence)))))


    ;; Special case of (receive-changes) for initial full resync.
    ;; This variant replaces desired state instead of augmenting it.
    (define/private (receive-changes/full changes)
      ;; Determine what entities we have right now and what entities
      ;; have we received just now so that we can drop leftovers.
      (let ((old-set (list->set (hash-keys remote-state)))
            (new-set (for/set ((change changes))
                       (list (first change) (second change)))))

        ;; Receive the changes as usual.
        (receive-changes changes)

        ;; Now generate fake deletion changes for whatever we have left.
        (receive-changes
          (for/list ((to-delete (set-subtract old-set new-set)))
            (flatten (list to-delete (hash-ref remote-state to-delete) #f))))))


    ;; Augment desired state with partial changes.
    (define/private (receive-changes changes)
      (for ((change (sort changes < #:key score-change)))
        (let-values (((entity id part new-data) (apply values change)))
          (let* ((key     (list entity id))
                 (current (hash-ref remote-state key #f)))
            (unless (equal? current new-data)
              (if new-data
                (begin
                  (hash-set! remote-state key new-data)
                  (task (send twilight setup-entity entity id new-data)))

                (begin
                  (hash-remove! remote-state key)
                  (task (send twilight remove-entity entity id current)))))))))


    (define/public (publish/one entity id value)
      ;; Publish just one change.
      (publish (list (list entity id value))))


    (define/public (publish changes)
      ;; Collect changes to send out while consulting
      ;; and updating the local state cache.
      (define processed-changes
        (for/list ((change changes))
          (process-change change)))

      ;; After we replicate whole current state we can start sending
      ;; differences as they are published.
      (when synchronized?
        (send-changes processed-changes)))


    ;; Cache the change and potentially mangle the output,
    ;; producing a valid change to be sent over the network.
    (define/private (process-change change)
      (match change
        ((list entity id new-data)
         (let* ((key (list entity id))
                (current (hash-ref local-state key #f)))
           ;; Update the cache.
           (if new-data
               (hash-set! local-state key new-data)
               (hash-remove! local-state key))

           ;; Convert to form required by the network protocol.
           (mangle entity id new-data)))))


    ;; Produce list-form of the current state change with properly
    ;; mangled disks, storage pools, volumes, and instances to
    ;; host_disks, host_storage_pools et cetera.
    (define/private (mangle entity id data)
      (define (modify old-key new-key)
        (and data
             (let ((key-value (hash-ref data old-key)))
               (hash-set (hash-remove data old-key)
                         new-key key-value))))

      (define (make-id . keys)
        (map (λ (k) (hash-ref data k)) keys))

      (match entity
        ("disk"
         (list "host_disk"
               (make-id 'host 'id)
               "current"
               (modify 'id 'disk)))

        ((or "storage_pool" "extent" "image" "volume")
         (list (string-append "host_" entity)
               (make-id 'host 'uuid)
               "current"
               (modify 'uuid (string->symbol entity))))

        (else
         (list entity id "current" data))))


    (begin
      ;; Yeah, we are alive.
      (task
        (publish/one "host" uuid (hasheq 'uuid uuid 'status "present")))

      ;; Send keep-alive every 15 seconds (plus one right away).
      (recurring-task 15
        (keep-alive))

      ;; Receive messages from Sparkle.
      (recurring-event-task (router message)
        (receive (cadr message) (caddr message))))


    ;; Construct parent object.
    (super-new)))


; vim:set ts=2 sw=2 et:
