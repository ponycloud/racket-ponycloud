#lang racket/base
;
; Types Of Changes And Generic Configuration Objects
;

(require racket/contract
         racket/generic
         json)

(require twilight/unit)

(provide gen:config)

(provide
  (contract-out
    (struct change
      ((table symbol?)
       (pkey pkey/c)
       (prev data/c)
       (next data/c)))

    (change-ref/next (->* (change? symbol?) (any/c) any/c))
    (change-ref/prev (->* (change? symbol?) (any/c) any/c))

    (config? predicate/c)
    (config-type (-> config? symbol?))
    (config-pkey (-> config? any/c))
    (config-delete? (-> config? boolean?))
    (config-spawn-unit (-> config? units/c unit?))
    (config-can-change? (-> config? change? boolean?))
    (config-change (-> config? change? config?))))

(define pkey/c
  (or/c string? integer? (listof (recursive-contract pkey/c))))

(define data/c
  (or/c #f (hash/c symbol? jsexpr?)))

(define units/c
  (hash/c (list/c symbol? any/c) unit?))


(define-generics config
  (config-type config)
  (config-pkey config)
  (config-delete? config)
  (config-can-change? config change)
  (config-change config change)
  (config-spawn-unit config other-units))

(struct change
  (table pkey prev next)
  #:transparent)


(define (change-ref/next a-change key (dfl #f))
  (let ((next (change-next a-change)))
    (if next (hash-ref next key dfl) dfl)))

(define (change-ref/prev a-change key (dfl #f))
  (let ((prev (change-prev a-change)))
    (if prev (hash-ref prev key dfl) dfl)))


; vim:set ts=2 sw=2 et:
