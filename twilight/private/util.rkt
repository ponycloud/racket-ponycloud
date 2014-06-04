#lang racket/base
;
; Miscelaneous Utilities
;

(require racket/contract
         racket/function
         racket/generator
         racket/string
         racket/format
         racket/dict)

(require (for-syntax racket/base))

(provide allocate-bond-name
         allocate-bridge-name
         allocate-vxlan-name
         generate-hwaddr)


(define (make-allocator prefix num-bytes)
  (generator ()
    (for ((i (in-cycle (expt 16 num-bytes))))
      (let ((value (format "~a.~a" prefix (number->string i 16))))
        (yield value)))))

(define allocate-bond-name   (make-allocator "bond" 5))
(define allocate-bridge-name (make-allocator "br" 7))
(define allocate-vxlan-name  (make-allocator "vx" 7))

(define (random-octets n)
  (for/list ((i n))
    (~a (number->string (random 256))
        #:width 2 #:left-pad-string "0" #:align 'right)))

(define (append-octet-string octet hwaddr)
  (string-append hwaddr ":" octet))

(define (generate-hwaddr)
  (foldl append-octet-string "42" (random-octets 5)))


; vim:set ts=2 sw=2 et:
