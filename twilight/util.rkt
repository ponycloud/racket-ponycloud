#lang racket/base
;
; Miscelaneous Utilities
;

(require racket/contract
         racket/generator
         racket/format
         racket/string
         racket/list)

(provide
  (contract-out
    (hwaddr? predicate/c)
    (generate-hwaddr (-> hwaddr?))

    (allocate-bond-name (-> string?))
    (allocate-vlan-name (-> string?))
    (allocate-bridge-name (-> string?))
    (allocate-vxlan-name (-> string?))

    (props-hwaddr (-> (hash/c symbol? string?) (or/c hwaddr? #f)))))


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


(define (props-hwaddr props)
  (id-net-name-mac->hwaddr
    (hash-ref props 'id-net-name-mac #f)))

(define (id-net-name-mac->hwaddr id-net-name-mac)
  (and id-net-name-mac
       (let ((hex-mac (last (regexp-split "x" id-net-name-mac))))
         (string-join (regexp-match* ".." hex-mac) ":"))))


; vim:set ts=2 sw=2 et:
