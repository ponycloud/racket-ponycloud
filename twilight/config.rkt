#lang racket/base
;
; Changes And Configurable Objects
;

(require racket/contract
         racket/generic
         racket/set)

(require misc1/syntax)

(require twilight/config/types
         twilight/config/empty
         twilight/config/nic
         twilight/config/bond
         twilight/config/role
         twilight/config/vlan)

(provide
  (all-from-out twilight/config/types))

(provide
  (contract-out
    (no-config config?)
    (apply-change (->* (change?) ((set/c config?)) (set/c config?)))))


(define no-config
  (empty-config))


(define (apply-change a-change (configs (set no-config)))
  (for/set ((a-config configs))
    (config-change a-config a-change)))


; vim:set ts=2 sw=2 et:
