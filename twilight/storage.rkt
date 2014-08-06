#lang racket/base
;
; Storage Manager
;

(require racket/contract
         racket/class
         json)

(require libuuid)

(provide
  (contract-out
    (storage-manager% storage-manager/c)))

(define create-update-delete/c
  (case->m (-> "disk" string? jsexpr? void?)
           (-> "volume" uuid? jsexpr? void?)
           (-> "extent" uuid? jsexpr? void?)
           (-> "storage_pool" uuid? jsexpr? void?)))

(define storage-manager/c
  (class/c
    (init-field (twilight (object/c)))

    (create create-update-delete/c)
    (update create-update-delete/c)
    (delete create-update-delete/c)))


(define storage-manager%
  (class object%
    (init-field twilight)

    (define/public (create table pkey data)
      (void))

    (define/public (update table pkey data)
      (void))

    (define/public (delete table pkey data)
      (void))

    (define/public (get-evt)
      never-evt)

    ;; Construct parent object.
    (super-new)))


; vim:set ts=2 sw=2 et:
