#lang racket/base
;
; Component: Bonding Interface
;

(require racket/contract
         racket/match)

(require
  (rename-in kernel/link
             (bond? k:bond?)
             (bond-mode get-bond-mode)
             (bond-lacp-rate get-bond-lacp-rate)
             (bond-xmit-hash-policy get-bond-xmit-hash-policy)))

(require misc1/async
         misc1/syntax
         dds)

(provide
  (contract-out
    (bond? predicate/c)
    (bond (-> string? mode/c lacp-rate/c xmit-hash-policy/c bond?))
    (bond-mode (-> bond? mode/c))
    (bond-lacp-rate (-> bond? lacp-rate/c))
    (bond-xmit-hash-policy (-> bond? xmit-hash-policy/c))))


(define mode/c
  (symbols 'active-backup '802.3ad))

(define lacp-rate/c
  (or/c #f 'fast 'slow))

(define xmit-hash-policy/c
  (or/c #f 'layer2 'layer2+3 'layer3+4))


(struct bond
  (name mode lacp-rate xmit-hash-policy)
  #:transparent
  #:methods gen:target
  ((define (target-id self)
     (match-let (((bond name _ _ _) self))
       `(link ,name)))

   (define (target-create/async self)
     (match-let (((bond name mode lacp-rate xmit-hash-policy) self))
       (async
         ;; Bond with slaves cannot be reconfigured. Drop it completely when
         ;; we need to change it's parameters. The solver will refresh all
         ;; dependent NICs and they will re-enslave themselves.
         (when* ((index (link-name->index name)))
           (unless (equal? (list mode lacp-rate xmit-hash-policy)
                           (list (get-bond-mode index)
                                 (get-bond-lacp-rate index)
                                 (get-bond-xmit-hash-policy index)))
             (link-delete! index)))

         (unless (link-name->index name)
           (create-bond name))

         (let ((index (link-name->index name)))
           (set-bond-mode! index mode)
           (set-bond-lacp-rate! index lacp-rate)
           (set-bond-xmit-hash-policy! index xmit-hash-policy)))))

   (define (target-destroy/async self)
     (match-let (((bond name _ _ _) self))
       (async
         (when* ((index (link-name->index name)))
           (link-delete! index)))))))


; vim:set ts=2 sw=2 et:
