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
         generate-hwaddr
         dict-merge
         let-from-dict
         let-from-list
         set!-many
         producing
         using
         when*
         pipe*)


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

(define (dict-extend ext base)
  (define (update-key key dict)
    (dict-set dict key (dict-ref ext key)))
  (foldl update-key base (dict-keys ext)))

(define (dict-merge dict . other-dicts)
  (foldl dict-extend dict other-dicts))


;; Extract values from a dictionary to local bindings.
(define-syntax-rule (let-from-dict ((key ...) dict) body ...)
  (let ((key (dict-ref dict 'key)) ...)
    body ...))


;; Extract values from a list to local bindings.
(define-syntax-rule (let-from-list ((key ...) lst) body ...)
  (let-values (((key ...) (apply values lst)))
    body ...))


;; Set multiple variables to the same value at once.
(define-syntax-rule (set!-many (var ...) value)
  (let ((bound-value value))
    (set! var bound-value) ...))


;; Bind value to `<it>` and perform a few operations,
;; returning the original value.
(define-syntax (producing stx)
  (syntax-case stx ()
    ((_ value body ...)
     #`(let* ((bound-value value)
              (#,(datum->syntax stx '<it>) bound-value))
         (begin body ...)
         bound-value))))


;; Bind value to `<it>` and perform a few operations.
;; Returns `#<void>`.
(define-syntax (using stx)
  (syntax-case stx ()
    ((_ value body ...)
     #`(let* ((#,(datum->syntax stx '<it>) value))
         body ...))))


;; Bind value to `<it>` and perform a few operations,
;; provided the value is true.  Returns `#<void>`.
(define-syntax (when* stx)
  (syntax-case stx ()
    ((_ value body ...)
     #`(let* ((bound-value value)
              (#,(datum->syntax stx '<it>) bound-value))
         (when bound-value
           body ...)))))


;; Pipe initial list of arguments throught specified procedures.
(define (pipe* initial . procs)
  (apply (apply compose (reverse procs)) initial))


; vim:set ts=2 sw=2 et:
