#lang racket/base
;
; Component: VLAN Interface
;

(require racket/contract
         racket/match)

(require
  (rename-in kernel/link (vlan? k:vlan?)))

(require misc1/async
         misc1/syntax
         dds)

(provide
  (contract-out
    (vlan? predicate/c)
    (vlan (-> string? string? (integer-in 1 4094) vlan?))
    (vlan-name (-> vlan? string?))
    (vlan-uplink-name (-> vlan? string?))
    (vlan-tag (-> vlan? (integer-in 1 4094)))))


(struct vlan
  (name uplink-name tag)
  #:transparent
  #:methods gen:target
  ((define (target-id self)
     (match-let (((vlan name _ _) self))
       `(link ,name)))

   (define (target-create/async self)
     (match-let (((vlan name uplink-name tag) self))
       (async
         (unless (link-name->index name)
           (let ((uplink-index (link-name->index uplink-name)))
             (create-vlan name uplink-index tag))))))

   (define (target-destroy/async self)
     (match-let (((vlan name _ _) self))
       (async
         (when* ((index (link-name->index name)))
           (link-delete! index)))))

   (define (target-needs self)
     (match-let (((vlan _ uplink-name _) self))
       `((link ,uplink-name))))))


; vim:set ts=2 sw=2 et:
