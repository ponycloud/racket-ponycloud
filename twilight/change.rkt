#lang racket/base
;
; Change Description
;

(require racket/contract)

(require json)

(provide struct:change
         struct:create
         struct:update
         struct:delete)

(provide
  (contract-out
    (change? predicate/c)

    (change-table (-> change? string?))
    (change-key (-> change? pkey/c))
    (change-data (-> change? data/c))

    (create? predicate/c)
    (create (-> string? pkey/c data/c create?))

    (update? predicate/c)
    (update (-> string? pkey/c data/c update?))

    (delete? predicate/c)
    (delete (-> string? pkey/c data/c delete?))))


(define pkey/c
  (or/c string? integer? (listof (recursive-contract pkey/c))))

(define data/c
  (hash/c symbol? jsexpr?))


(struct change
  (table key data)
  #:transparent)

(struct create change () #:transparent)
(struct update change () #:transparent)
(struct delete change () #:transparent)


; vim:set ts=2 sw=2 et:
