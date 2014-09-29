#lang racket/base
;
; Miscelaneous Utilities
;

(require racket/contract
         racket/generator
         racket/format)

(provide
  (contract-out
    (hwaddr? predicate/c)
    (generate-hwaddr (-> hwaddr?))

    (allocate-bond-name (-> string?))
    (allocate-vlan-name (-> string?))
    (allocate-bridge-name (-> string?))
    (allocate-vxlan-name (-> string?))))


(define (make-allocator prefix num-bytes)
  (generator ()
    (for ((i (in-cycle (expt 16 num-bytes))))
      (let ((value (format "~a.~a" prefix (number->string i 16))))
        (yield value)))))

(define allocate-bond-name   (make-allocator "bn" 7))
(define allocate-vlan-name   (make-allocator "vl" 7))
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

(define (hwaddr? v)
  (and (string? v)
       (regexp-match? #px"^[0-9a-fA-F]{2}(:[0-9a-fA-F]{2})*$" v)))


; vim:set ts=2 sw=2 et:
