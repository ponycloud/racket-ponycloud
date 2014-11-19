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
    (apply-change (->* (change?) ((set/c config?)) (set/c config?)))))


(define no-config
  (empty-config))


(define (apply-change a-change (configs (set)))
  (if (change-prev a-change)
      (update-configs configs a-change)
      (when (config-can-change? no-config a-change)
        (let ((new-config (config-change no-config a-change)))
          (set-add configs new-config)))))


(define (update-configs configs a-change)
  (define new-configs
    (for/set ((a-config configs))
      (if (config-can-change? a-config a-change)
          (config-change a-config a-change)
          a-config)))

  (filter values new-configs))


; vim:set ts=2 sw=2 et:
