#lang racket/base
;
; Bonding Interface Configuration
;

(require racket/contract)

(require libuuid)

(require twilight/config/types)

(provide
  (contract-out
    (struct bond-config
      ((uuid uuid?)
       (bond-name string?)
       (mode (or/c 'active-backup '802.3ad))
       (lacp-rate (or/c 'fast 'slow))
       (xmit-hash-policy (or/c 'layer2 'layer2+3 'layer3+4))))))


(struct bond-config
  (uuid bond-name mode lacp-rate xmit-hash-policy)
  #:transparent
  #:methods gen:config
  ())


; vim:set ts=2 sw=2 et:
