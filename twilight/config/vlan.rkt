#lang racket/base
;
; VLAN Configuration
;

(require racket/contract)

(require libuuid)

(require twilight/config/types)

(provide
  (contract-out
    (struct vlan-config
      ((bond-uuid uuid?)
       (vlan-tag (integer-in 1 4096))))))


(struct vlan-config
  (bond-uuid vlan-tag)
  #:transparent
  #:methods gen:config
  ())


; vim:set ts=2 sw=2 et:
