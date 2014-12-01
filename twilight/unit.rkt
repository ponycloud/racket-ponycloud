#lang racket/base
;
; Unit-Based Work Framework
;

(require racket/contract
         racket/function
         racket/match)

(require misc1/syntax
         misc1/locking)

(provide spawn-unit
         with-unit-result)

(provide
  (contract-out
    (unit? predicate/c)
    (make-unit (-> (-> any/c) unit?))
    (call-with-unit-result (-> unit? (-> any/c any) any))
    (unit-start! (-> unit? void?))
    (unit-cancel! (-> unit? void?))))


(struct unit
  (thread evt wlock rwlock)
  #:property prop:evt (struct-field-index evt))


(define (call/wrap a-proc)
  (with-handlers ((void values))
    (call-with-values a-proc list)))


(define (make-unit long-running-proc)
  (let* ((wlock (make-semaphore 0))
         (rwlock (make-rwlock wlock)))
    (let ((result null))
      (let ((thread (parameterize-break #f
                      (spawn-thread
                        (parameterize-break #t
                          (thread-receive))
                        (set! result (call/wrap long-running-proc))
                        (semaphore-post wlock)))))
        (let ((event (wrap-evt thread
                               (λ (thread)
                                 (apply values result)))))
          (unit thread event wlock rwlock))))))

(define (call-with-unit-result unit proc)
  (with-read-lock (unit-rwlock unit)
    (let ((result (sync unit)))
      (when (exn? result)
        (raise result))
      (proc result))))


(define (unit-start! an-unit)
  (let ((thread (unit-thread an-unit)))
    (thread-send thread 'start)))

(define (unit-cancel! an-unit)
  (match-let (((unit thread _ wlock _) an-unit))
    (break-thread thread)
    (semaphore-wait wlock)))


(define-syntax with-unit-result
  (syntax-rules ()
    ((_ ((name unit) rest ...) body ...)
     (call-with-unit-result unit
                            (λ (name)
                              (with-unit-result (rest ...) body ...))))
    ((_ () body ...)
     (begin body ...))))

(define-syntax-rule (spawn-unit body ...)
  (make-unit (thunk body ...)))


; vim:set ts=2 sw=2 et:
