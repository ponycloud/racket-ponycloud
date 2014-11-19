#lang racket/base
;
; Network Interface Configuration
;

(require racket/contract
         unstable/contract)

(require libuuid)

(require twilight/util
         twilight/config/types)

(provide
  (contract-out
    (struct nic-config
      ((hwaddr hwaddr?)
       (link-name string?)
       (bond-uuid (maybe/c uuid?))))))


(struct nic-config
  (hwaddr link-name bond-uuid)
  #:transparent
  #:methods gen:config
  ())


; vim:set ts=2 sw=2 et:
