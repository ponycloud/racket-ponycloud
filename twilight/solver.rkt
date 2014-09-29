#lang racket/base
;
; Reference-Counting Dependency Solver
;
; The actual dependency-solving and target handling is taken care of
; by dds solver, but we add the reference counting in order to simplify
; our network / storage / instance managers.
;

(require racket/contract
         racket/match)

(require misc1/syntax
         dds)

(provide
  (contract-out
    (rc-solver? predicate/c)
    (make-rc-solver (-> solver? rc-solver?))
    (rc-solver-create! (-> rc-solver? target? void?))
    (rc-solver-destroy! (-> rc-solver? target? void?))))


(struct rc-solver
  (real targets)
  #:property prop:evt (struct-field-index real))

(struct rc-target
  (real (refcnt #:mutable)))


(define (make-rc-solver real)
  (rc-solver real (make-hash)))


(define (incref! target)
  (let ((refcnt (rc-target-refcnt target)))
    (set-rc-target-refcnt! target (add1 refcnt))
    (= 0 refcnt)))

(define (decref! target)
  (let ((refcnt (rc-target-refcnt target)))
    (set-rc-target-refcnt! target (sub1 refcnt))
    (= 1 refcnt)))


(define (rc-solver-create! solver real-target)
  (define (make-rc-target)
    (rc-target real-target 0))

  (match-let (((rc-solver real-solver targets) solver))
    (let* ((r-t-id (target-id real-target))
           (rc-t (hash-ref! targets r-t-id make-rc-target)))
      (unless (equal? real-target (rc-target-real rc-t))
        (error 'rc-solver-create! "different targets with matching ids"))
      (when (incref! rc-t)
        (solver-create! real-solver real-target)))))


(define (rc-solver-destroy! solver r-t-id)
  (match-let (((rc-solver real-solver targets) solver))
    (when* ((rc-t (hash-ref targets r-t-id #f)))
      (when (decref! rc-t)
        (solver-destroy! real-solver (rc-target-real rc-t))))))


; vim:set ts=2 sw=2 et:
