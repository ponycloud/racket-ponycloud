#lang racket/base
;
; Storage Configuration Component
;

(require racket/contract
         racket/function
         racket/class
         tasks)

(require "util.rkt"
         "common.rkt")

(provide (all-defined-out))


(define storage-manager%
  (class object%
    ;; Known disk devices - either present, configured or both.
    (field (disks (make-hash)))

    (define/public (assign-disk-device info)
      (void))

    (define/public (unassign-disk-device info)
      (void))

    ;; Construct parent object.
    (super-new)))


; vim:set ts=2 sw=2 et:
