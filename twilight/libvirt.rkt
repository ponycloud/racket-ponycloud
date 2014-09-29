#lang racket/base
;
; Libvirt Manager
;

(require racket/contract
         racket/class
         json)

(require libuuid
         dds)

(provide
  (contract-out
    (libvirt-manager% libvirt-manager/c)))


(define libvirt-table/c
  (or/c "instance" "vdisk" "vnic"))

(define create-update-delete/c
  (->m libvirt-table/c string? jsexpr? void?))

(define libvirt-manager/c
  (class/c
    (init-field (twilight (object/c)))

    (create create-update-delete/c)
    (update create-update-delete/c)
    (delete create-update-delete/c)

    (on-solver-event (->m symbol? target? any/c void?))

    (get-evt (->m (evt/c string? jsexpr? jsexpr?)))))


(define libvirt-manager%
  (class object%
    (init-field twilight)

    (define/public (create table pkey data)
      (void))

    (define/public (update table pkey data)
      (void))

    (define/public (delete table pkey data)
      (void))

    (define/public (on-solver-event action target result)
      (void))

    (define/public (get-evt)
      never-evt)

    ;; Construct parent object.
    (super-new)))


; vim:set ts=2 sw=2 et:
