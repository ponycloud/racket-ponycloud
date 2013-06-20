#lang racket/base
;
; Miscelaneous Utilities
;

(require racket/function
         racket/string
         racket/format)

(provide bond-name-allocator
         bridge-name-allocator
         vxlan-name-allocator
         generate-hwaddr)


(define-syntax-rule (define-allocator name pfx num-bytes)
  (begin
    (void
      (thread
        (thunk
          (for ((i (in-cycle (in-range (expt 16 num-bytes)))))
            (let ((value (format "~a.~a" pfx (number->string i 16))))
              (channel-put name value))))))
    (define name (make-channel))))


(define-allocator bond-name-allocator "bond" 5)
(define-allocator bridge-name-allocator "br" 7)
(define-allocator vxlan-name-allocator "vx" 7)


(define (generate-hwaddr)
  (string-append
    "42:"
    (string-join (for/list ((i (in-range 5))
                            (octet (in-producer random #f 256)))
                   (~a (number->string octet 16)
                       #:width 2 #:left-pad-string "0" #:align 'right))
                 ":")))


; vim:set ts=2 sw=2 et:
