#lang racket/base
;
;
;

(require racket/contract
         racket/class
         racket/list
         racket/set
         tasks
         json
         zmq)

(require (planet williams/uuid/uuid))

(provide (all-defined-out))


(define communicator%
  (class object%
    ;; Twilight instance that we work for.
    (init-field twilight)

    ;; Address of Sparkle to connect to.
    (field (connect-to (get-field connect-to twilight)))

    ;; Connection to the Sparkle cloud controller.
    (field (socket (make-socket 'router
                                #:identity (get-field uuid twilight)
                                #:connect (list connect-to))))

    ;; Incarnations of the local and remote changes streams.
    ;; We start with completely bogus IDs, which will lead to an early reset.
    (field (local-incarnation  (uuid->string (make-uuid-4)))
           (remote-incarnation (uuid->string (make-uuid-4))))

    ;; Next change number of local and remote streams.
    ;; Starting unknown incarnation with 1 is invalid and remote
    ;; change number does not matter with wrong incarnation above.
    (field (local-sequence  1)
           (remote-sequence 0))

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
      (send-message (hasheq 'uuid (get-field uuid twilight)
                            'incarnation local-incarnation
                            'seq local-sequence
                            'event "twilight-state-update"
                            'changes changes))

      ;; Bump local sequence number.
      (set! local-sequence (add1 local-sequence)))


    (define/private (send-message payload)
      ;; Send payload to sparkle with current time.
      (socket-send socket "sparkle"
                          (jsexpr->bytes payload)
                          (number->string (current-seconds))))


    (define/private (send-resync-request)
      ;; Ask Sparkle for complete dump of our configuration since the
      ;; incremental stream of changes have been disrupted or we are
      ;; just starting.
      (send-message (hasheq 'uuid (get-field uuid twilight)
                            'event "twilight-resync")))


    (define/private (receive message time)
      ;; Get time as a number.
      (set! time (string->number (bytes->string/utf-8 time)))

      ;; Deserialize the payload.
      (set! message (bytes->jsexpr message))

      (cond
        ;; Verify that the message is fresh enough.
        ((> (current-seconds) (+ time 15))
         (void))

        ;; Handle resync requests.
        ((string=? "sparkle-resync" (hash-ref message 'event))
         (set! local-sequence 0)
         (send-changes (for/list ((info (in-hash-pairs local-state)))
                         (flatten (list (car info) "current" (cdr info))))))

        ;; From here on, it's only desired state update.
        ((not (string=? "sparkle-state-update" (hash-ref message 'event)))
         (void))

        ;; Verify that we are talking to the same Sparkle instance.
        ((string=? remote-incarnation (hash-ref message 'incarnation))
         (set! remote-incarnation (hash-ref message 'incarnation))
         (set! remote-sequence 0)
         (send-resync-request))

        ;; Verify that we did not miss any messages.
        ((= remote-sequence (hash-ref message 'seq))
         (set! remote-sequence 0)
         (send-resync-request))

        ;; Treat full resync specially.
        ((= remote-sequence 0)
         (receive-changes/full (hash-ref message 'changes))
         (set! remote-sequence (add1 remote-sequence)))

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
            (new-set (for/set ((change (in-list changes)))
                       (cons (car change) (cadr change)))))

        ;; Receive the changes as usual.
        (receive-changes changes)

        ;; Now generate fake deletion changes for whatever we have left.
        (receive-changes
          (for/list ((to-delete (in-set (set-subtract old-set new-set))))
            (flatten (list to-delete (hash-ref remote-state to-delete) #f))))))


    ;; Augment desired state with partial changes.
    (define/private (receive-changes changes)
      (for ((change (in-list changes)))
        (let-values (((entity id part new-data) (apply values change)))
          (let* ((key     (cons entity id))
                 (current (hash-ref remote-state key #f)))
            (unless (equal? current new-data)
              (if new-data
                (begin
                  (hash-set! remote-state key new-data)
                  (task (send twilight setup-entity entity id new-data)))

                (begin
                  (hash-remove! remote-state key)
                  (task (send twilight remove-entity entity id current)))))))))


    (define/public (publish changes)
      ;; Collect changes to send out while consulting
      ;; and updating the local state cache.
      (define processed-changes
        (for/list ((change (in-list changes)))
          (let-values (((entity id new-data) (apply values change)))
            (let* ((key     (cons entity id))
                   (current (hash-ref local-state key #f)))

              ;; Update the cache.
              (if new-data
                (hash-set! local-state key new-data)
                (hash-remove! local-state key))

              ;; Prepare the form required by the protocol.
              (list entity id current new-data)))))

      ;; Transmit the incremental changes.
      (send-changes processed-changes))


    (begin
      ;; Yeah, we are alive.
      (hash-set! local-state (cons "host" (get-field uuid twilight))
                 (hasheq 'uuid (get-field uuid twilight)
                         'state "present"))

      ;; Send keep-alive every 15 seconds (plus one right away).
      (recurring-task 15
        (keep-alive))

      ;; Receive messages from Sparkle.
      (recurring-event-task (message (socket-receive-evt socket))
        (let-values (((sender payload time) (apply values message)))
          (receive payload time))))


    ;; Construct parent object.
    (super-new)))


; vim:set ts=2 sw=2 et:
