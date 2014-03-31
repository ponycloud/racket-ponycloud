#lang racket/base
;
; Common Tools
;

(require racket/contract)

(provide (all-defined-out))


(define pkey/c
  (or/c (listof string?) string?))


(define/contract current-notify
                 (parameter/c (-> string? pkey/c (or/c hash? #f) void?))
  (make-parameter void))


(define/contract (notify-sparkle entity id info)
                 (-> string? pkey/c (or/c hash? #f) void?)
  ((current-notify) entity id info))


; vim:set ts=2 sw=2 et:
