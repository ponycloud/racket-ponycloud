#lang racket/base
;
; Network Role Configuration
;

(require racket/contract
         unstable/contract)

(require libuuid)

(require twilight/config/types)

(provide
  (contract-out
    (struct role-config
      ((uuid uuid?)
       (bond-uuid (maybe/c uuid?))
       (vlan-tag (maybe/c (integer-in 1 4096)))
       (role (or/c 'management 'core 'virtual 'storage 'public))
       (address string?)))))


(struct role-config
  (uuid bond-uuid vlan-tag role address)
  #:transparent
  #:methods gen:config
  ())


; vim:set ts=2 sw=2 et:
