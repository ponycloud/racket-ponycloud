#lang racket/base
;
; Configurable Objects
;

(require racket/contract
         racket/generic
         racket/class)

(require twilight/unit)

(provide
  (contract-out
    (config-type (-> config? symbol?))
    (config-pkey (-> config? any/c))
    (config-delete? (-> config? boolean?))
    (config-spawn-unit (-> config? units/c unit?))))

(define units/c
  (hash/c (list/c symbol? any/c) unit?))


(define-generics config
  (config-type config)
  (config-pkey config)
  (config-delete? config)
  (config-spawn-unit config other-units))


; vim:set ts=2 sw=2 et:
