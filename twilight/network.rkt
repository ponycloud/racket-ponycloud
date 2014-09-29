#lang racket/base
;
; Network Manager
;

(require racket/contract
         racket/class
         json)

(require libuuid)

(require "util.rkt"
         "component/nic.rkt"
         "component/bond.rkt"
         "component/bridge.rkt"
         "component/vlan.rkt"
         "component/addr.rkt")

(provide
  (contract-out
    (network-manager% network-manager/c)))


(define network-table/c
  (or/c "nic" "bond" "net_role"))

(define create-update-delete/c
  (->m network-table/c string? jsexpr? void?))

(define network-manager/c
  (class/c
    (init-field (twilight (object/c)))

    (create create-update-delete/c)
    (update create-update-delete/c)
    (delete create-update-delete/c)))


(define network-manager%
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
