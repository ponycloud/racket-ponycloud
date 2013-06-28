#lang racket/base
;
; Miscelaneous Utilities
;

(require racket/contract
         racket/function
         racket/generator
         racket/string
         racket/format)

(provide (except-out (all-defined-out) make-allocator))


(define (make-allocator prefix num-bytes)
  (generator ()
    (for ((i (in-cycle (in-range (expt 16 num-bytes)))))
      (let ((value (format "~a.~a" prefix (number->string i 16))))
        (yield value)))))


(define allocate-bond-name   (make-allocator "bond" 5))
(define allocate-bridge-name (make-allocator "br" 7))
(define allocate-vxlan-name  (make-allocator "vx" 7))


(define (generate-hwaddr)
  (string-append
    "42:"
    (string-join (for/list ((i (in-range 5))
                            (octet (in-producer random #f 256)))
                   (~a (number->string octet 16)
                       #:width 2 #:left-pad-string "0" #:align 'right))
                 ":")))


; vim:set ts=2 sw=2 et:
