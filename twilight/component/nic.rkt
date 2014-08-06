#lang racket/base
;
; Component: Network Interface
;

(require racket/contract
         racket/match)

(require misc1/async
         kernel/link
         dds)

(provide
  (contract-out
    (nic? predicate/c)
    (nic (-> string? string? nic?))
    (nic-name (-> nic? string?))
    (nic-bond-name (-> nic? string?))))


(struct nic
  (name bond-name)
  #:transparent
  #:methods gen:target
  ((define (target-id self)
     (match-let (((nic name _) self))
       `(link ,name)))

   (define (target-create/async self)
     (match-let (((nic name bond-name) self))
       (async
         (let ((index (link-name->index name))
               (bond-index (link-name->index bond-name)))
           (set-link-bond! index bond-index)
           (set-link-flags! index 'up)))))

   (define (target-destroy/async self)
     (match-let (((nic name _) self))
       (async
         (let ((index (link-name->index name)))
           (unset-link-flags! index 'up)
           (set-link-bond! index #f)))))

   (define (target-needs self)
     (match-let (((nic _ bond-name) self))
       (if bond-name `((link ,bond-name)) null)))))


; vim:set ts=2 sw=2 et:
