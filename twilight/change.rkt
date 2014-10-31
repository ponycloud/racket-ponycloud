#lang racket/base
;
; Change Description
;

(require racket/contract)

(require json)

(provide
  (contract-out
    (struct change ((table string?)
                    (pkey pkey/c)
                    (data data/c)))
    (struct create ((table string?)
                    (pkey pkey/c)
                    (data data/c)))
    (struct update ((table string?)
                    (pkey pkey/c)
                    (data data/c)))
    (struct delete ((table string?)
                    (pkey pkey/c)
                    (data data/c)))))


(define pkey/c
  (or/c string? integer? (listof (recursive-contract pkey/c))))

(define data/c
  (hash/c symbol? jsexpr?))


(struct change
  (table pkey data)
  #:transparent)

(struct create change () #:transparent)
(struct update change () #:transparent)
(struct delete change () #:transparent)


; vim:set ts=2 sw=2 et:
