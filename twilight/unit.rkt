#lang racket/base
;
; Unit-Based Work Framework
;

(require racket/contract)

(require misc1/syntax
         misc1/locking)

(provide spawn-unit
         with-unit-result)

(provide
  (contract-out
    (unit? predicate/c)
    (make-unit (-> (-> any/c) unit?))
    (call-with-unit-result (-> unit? (-> any/c any) any))
    (unit-close! (-> unit? void?))))


(struct unit
  (thread evt wlock rwlock)
  #:property prop:evt (struct-field-index evt))


(define (make-raiser exn)
  (λ ()
    (raise exn)))

(define (make-producer . results)
  (λ ()
    (apply values results)))

(define (call/wrap a-proc)
  (with-handlers ((void make-raiser))
    (call-with-values a-proc make-producer)))


(define (make-unit long-running-proc)
  (let* ((wlock (make-semaphore 0))
         (rwlock (make-rwlock wlock)))
    (let ((result void))
      (let ((thread (spawn-thread
                      (set! result (call/wrap long-running-proc))
                      (semaphore-post wlock))))
        (let ((event (wrap-evt thread
                               (λ (thread)
                                 (result)))))
          (unit thread event wlock rwlock))))))

(define (call-with-unit-result unit proc)
  (with-read-lock (unit-rwlock unit)
    (proc (sync unit))))

(define (unit-close! unit)
  (semaphore-wait
    (unit-rwlock unit)))

(define-syntax with-unit-result
  (syntax-rules ()
    ((_ ((name unit) rest ...) body ...)
     (call-with-unit-result unit
                            (λ (name)
                              (with-unit-result (rest ...) body ...))))
    ((_ () body ...)
     (begin body ...))))

(define-syntax-rule (spawn-unit body ...)
  (make-unit (λ () body ...)))


; vim:set ts=2 sw=2 et:
