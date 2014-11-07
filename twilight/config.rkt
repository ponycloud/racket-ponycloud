#lang racket/base
;
; Changes And Configurable Objects
;

(require racket/contract
         racket/generic
         racket/match
         racket/set
         json)

(require twilight/unit)

(provide
  (contract-out
    (struct change ((table string?)
                    (pkey pkey/c)
                    (data data/c)))

    (config? predicate/c)
    (config-type (-> config? symbol?))
    (config-pkey (-> config? any/c))
    (config-delete? (-> config? boolean?))
    (config-spawn-unit (-> config? units/c unit?))
    (config-can-apply? (-> config? change? boolean?))
    (config-apply-change (-> config? change? config?))

    (apply-change (->* (change?) ((set/c config?)) (set/c config?)))))

(define pkey/c
  (or/c string? integer? (listof (recursive-contract pkey/c))))

(define data/c
  (hash/c symbol? jsexpr?))

(define units/c
  (hash/c (list/c symbol? any/c) unit?))


(struct change
  (table pkey data)
  #:transparent)


(define-generics config
  (config-type config)
  (config-pkey config)
  (config-delete? config)
  (config-can-apply? config change)
  (config-apply-change config change)
  (config-spawn-unit config other-units))


(define (apply-change a-change (configs (set)))
  (for/set ((a-config configs))
    (if (config-can-apply? a-config a-change)
        (config-apply-change a-config a-change)
        (values a-config))))


; vim:set ts=2 sw=2 et:
