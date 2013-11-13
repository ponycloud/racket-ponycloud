#lang racket/base
;
; Common Tools
;

(require racket/contract)

(provide (all-defined-out))


(define/contract current-notify
                 (parameter/c (-> string? string? (or/c hash? #f) void?))
  (make-parameter void))


; vim:set ts=2 sw=2 et:
