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
    (config-unit (-> config? manager/c unit?))))

(define manager/c
  (object/c
    (get-unit (->m symbol? any/c unit?))))


(define-generics config
  (config-type config)
  (config-pkey config)
  (config-unit config manager))


; vim:set ts=2 sw=2 et:
