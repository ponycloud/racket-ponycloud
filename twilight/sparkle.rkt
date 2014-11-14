#lang racket/base
;
; Twilight <-> Sparkle Communication Component
;

(require racket/contract
         racket/generator
         racket/sequence
         racket/pretty
         racket/string
         racket/match
         racket/class
         racket/list
         racket/set
         json)

(require misc1/fast-channel
         misc1/evt
         libuuid
         zmq)

(require twilight/config)

(provide
  (contract-out
    (sparkle% sparkle-class/c)))


(define-logger sparkle)


(define (indent str)
  (string-replace str "\n" "\n   "))

(define (direction->string direction)
  (if (eq? 'in direction) "<-" "->"))

(define (log-message direction payload)
  (log-sparkle-debug "~a ~a"
                     (direction->string direction)
                     (indent (pretty-format payload 70))))


(define-syntax-rule (get/++ name)
  (begin0 name (set! name (add1 name))))


(define sparkle-class/c
  (class/c
    (init-field (uuid uuid?)
                (connect-to string?))
    (publish/one (->m change? void?))
    (publish (->m (listof change?) void?))
    (get-evt (->m (evt/c (listof change?))))))


(define sparkle%
  (class object%
    (init-field uuid connect-to)

    (field (router (socket 'router
                           #:identity (uuid-generate)
                           #:connect (list connect-to))))

    (field (local-incarnation (uuid-generate))
           (remote-incarnation (uuid-generate)))

    (field (local-sequence 1)
           (remote-sequence 0))

    (field (synchronized? #f))

    (field (local-state (make-hash))
           (remote-state (make-hash)))

    (field (changes-channel (make-fast-channel)))


    (define/private (send-changes changes)
      (send-message
        (hasheq 'uuid uuid
                'incarnation local-incarnation
                'seq (get/++ local-sequence)
                'event "update"
                'changes changes)))


    (define/private (send-message payload)
      (let ((bstr (jsexpr->bytes payload))
            (time (number->string (current-seconds))))
        (log-message 'out payload)
        (socket-send router "sparkle" bstr time)))


    (define/private (request-resync)
      (set! remote-sequence 0)
      (send-message (hasheq 'uuid uuid
                            'event "resync")))


    (define/private (process-message message-bytes time-bytes)
      (define time
        (string->number
          (bytes->string/utf-8 time-bytes)))

      (define message
        (bytes->jsexpr message-bytes #:null #f))

      ;; Log the inbound message before we start digging into it.
      ;; The code below is not ready to cope with malformed messages
      ;; and this might help us debug potential issues.
      (log-message 'in message)

      (define event
        (hash-ref message 'event))

      (define incarnation
        (hash-ref message 'incarnation #f))

      (cond
        ;; Drop messages that are older than 15 seconds.
        ((> (current-seconds)
            (+ time 15))
         (void))

        ;; With all published rows cached, we can resync cheaply.
        ((string=? "resync" event)
         ;; Only resync when we have not just done that.
         (unless (= local-sequence 1)
           (log-sparkle-info "peer requested resync")
           (set! local-sequence 0)
           (set! synchronized? #t)
           (send-changes
             (for/list (((key data) (in-hash local-state)))
               (append key (list "current" data))))))

        ;; We are not ready to cope with any other message types.
        ((not (string=? "update" event))
         (log-sparkle-warning "unknown event ~s" event))

        ;; Treat full resync specially.
        ((= remote-sequence 0)
         (log-sparkle-info "peer sent everything")
         (set! remote-sequence 1)
         (set! remote-incarnation incarnation)
         (receive/full (hash-ref message 'changes)))

        ;; Verify that we are talking to the same Sparkle instance.
        ((not (string=? remote-incarnation incarnation))
         (log-sparkle-warning "peer incarnation changed, requesting resync")
         (set! remote-incarnation incarnation)
         (request-resync))

        ;; Verify that we did not miss any messages.
        ((not (= (get/++ remote-sequence)
                 (hash-ref message 'seq)))
         (log-sparkle-warning "missing message detected, requesting resync")
         (request-resync))

        ;; Finally, handle incremental desired state changes.
        (else
         (receive (hash-ref message 'changes)))))


    (define/private (receive/full changes)
      ;; In order to match the full configuration we have just received,
      ;; any rows that are not part of it need to be deleted.
      (define deletes
        (let ((old-keys (list->set (hash-keys remote-state)))
              (new-keys (for/set ((a-change changes))
                          (take a-change 2))))
          (for/list ((key (set-subtract old-keys new-keys)))
            (append key '("desired" #f)))))

      ;; Receive both lists in a single transaction.
      (receive (append changes deletes)))


    (define/private (receive changes)
      (define struct-changes
        (in-generator
          (for ((a-change changes))
            (match-let (((list table pkey "desired" next) a-change))
              (let ((table (string->table table)))
                (let* ((key (list table pkey))
                       (prev (hash-ref remote-state key #f)))
                  (cond
                    ((and next prev)
                     (hash-set! remote-state key next)
                     (yield (change table pkey prev next)))

                    (next
                      (hash-set! remote-state key next)
                      (yield (change table pkey prev next)))

                    (prev
                      (hash-remove! remote-state key)
                      (yield (change table pkey prev #f))))))))))

      (let ((transaction (sequence->list struct-changes)))
        (fast-channel-put changes-channel transaction)))


    (define/public (publish/one a-change)
      (publish (list a-change)))


    (define/public (publish changes)
      (define list-changes
        (for/list ((a-change changes))
          (match a-change
            ((change table pkey _ #f)
             (let* ((table (table->string table))
                    (key (list table pkey)))
               (hash-remove! local-state key)
               (list table pkey "current" #f)))

             ((change table pkey _ next)
              (let* ((table (table->string table))
                     (key (list table pkey)))
                (hash-set! local-state key next)
                (list table pkey "current" next))))))

      ;; Delay sending incremental changes until after we have
      ;; synchronized communication with peer. Prevents lots of
      ;; unnecessary resyncs on startup.
      (when synchronized?
        (send-changes list-changes)))


    ;; Create a complex event that is responsible for the communication.
    (define/public (get-evt)
      (choice-evt changes-channel
                  (timer-evt 15000 (λ _ (send-changes null)))
                  (recurring-evt router
                                 (λ (message)
                                   (process-message (second message)
                                                    (third message))))))


    (begin
      ;; Automatically claim that the host exists.
      (publish/one (change 'host uuid #f (hasheq 'uuid uuid
                                                 'status "present")))

      ;; Ask for our configuration as soon as possible.
      (request-resync))


    ;; Construct parent object.
    (super-new)))


(define (table->string table)
  (string-replace (symbol->string table) "-" "_"))

(define (string->table str)
  (string->symbol (string-replace str "_" "-")))


; vim:set ts=2 sw=2 et:
