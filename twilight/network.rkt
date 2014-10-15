#lang racket/base
;
; Network Manager
;

(require racket/contract
         racket/string
         racket/class
         racket/match
         racket/list
         json)

(require misc1/fast-channel
         libuuid
         udev
         dds)

(require "util.rkt"
         "change.rkt"
         "component/nic.rkt"
         "component/bond.rkt"
         "component/bridge.rkt"
         "component/vlan.rkt"
         "component/addr.rkt")

(provide
  (contract-out
    (network-manager% network-manager/c)))


(define network-manager/c
  (class/c
    (init-field (twilight (object/c)))

    (apply-change (->m change? void?))
    (cork (->m void?))

    (appear (->m device? void?))
    (disappear (->m device? void?))

    (get-evt (->m (evt/c change?)))))


(define network-manager%
  (class object%
    (init-field twilight)

    (define events
      (make-fast-channel))

    (define/public (apply-change a-change)
      (void))

    (define/public (cork)
      (void))

    (define/public (appear device)
      (void))

    (define/public (disappear device)
      (void))

    (define/public (get-evt)
      (values events))

    (super-new)))


; vim:set ts=2 sw=2 et:
