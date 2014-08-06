#lang racket/base
;
; Component: Bridge Interface
;

(require racket/contract
         racket/match)

(require
  (rename-in kernel/link (bridge? k:bridge?)))

(require misc1/async
         misc1/syntax
         dds)

(provide
  (contract-out
    (bridge? predicate/c)
    (bridge (-> string? string? bridge?))
    (bridge-name (-> bridge? string?))
    (bridge-uplink-name (-> bridge? string?))))


(struct bridge
  (name uplink-name)
  #:transparent
  #:methods gen:target
  ((define (target-id self)
     (match-let (((bridge name _) self))
       `(link ,name)))

   (define (target-create/async self)
     (match-let (((bridge name uplink-name) self))
       (async
         (unless (link-name->index name)
           (create-bridge name))

         (let ((uplink-index (link-name->index uplink-name))
               (index (link-name->index name)))
           (set-link-bridge! uplink-index index)
           (set-link-flags! index 'up)))))

   (define (target-destroy/async self)
     (match-let (((bridge name _) self))
       (async
         (when* ((index (link-name->index name)))
           (link-delete! index)))))

   (define (target-needs self)
     (match-let (((bridge _ uplink-name) self))
       `((link ,uplink-name))))))


; vim:set ts=2 sw=2 et:
