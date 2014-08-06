#lang racket/base
;
; Component: IP Address
;

(require racket/contract
         racket/match)

(require misc1/async
         kernel/address
         kernel/link
         dds)

(provide
  (contract-out
    (addr? predicate/c)
    (addr (-> string? string? addr?))
    (addr-link-name (-> addr? string?))
    (addr-local (-> addr? string?))))


(struct addr
  (link-name local)
  #:transparent
  #:methods gen:target
  ((define (target-id self)
     (match-let (((addr link-name local) self))
       `(addr ,link-name ,local)))

   (define (target-create/async self)
     (match-let (((addr link-name local) self))
       (async
         (let* ((index (link-name->index link-name))
                (a (address index local)))
           (unless (member a (addresses))
             (create-address index local))))))

   (define (target-destroy/async self)
     (match-let (((addr link-name local) self))
       (async
         (let* ((index (link-name->index link-name))
                (a (address index local)))
           (when (member a (addresses))
             (address-delete! a))))))

   (define (target-needs self)
     (match-let (((addr link-name _) self))
       `((link ,link-name))))))


; vim:set ts=2 sw=2 et:
