#lang racket/base
;
; Not-Yet-Configured Object
; =========================
;
; This is a special configuration type that implements initial
; creation of other configuration types from appropriate changes.
;

(require racket/contract
         racket/match)

(require twilight/util
         twilight/config/types
         twilight/config/nic
         twilight/config/bond
         twilight/config/role
         twilight/config/vlan)

(provide
  (contract-out
    (struct empty-config ())))


(struct empty-config
  ()
  #:transparent
  #:methods gen:config
  ((define (config-can-change? a-config a-change)
     (and (not (change-prev a-change))
          (memq (change-table) '(nic bond net-role vlan))))

   (define (config-change a-config a-change)
     (match a-change
       ((change 'nic hwaddr _ next)
        (nic-config hwaddr #f
                    (hash-ref next 'bond)))

       ((change 'bond uuid _ next)
        (bond-config uuid
                     (allocate-bond-name)
                     (string->symbol (hash-ref next 'mode))
                     (string->symbol (hash-ref next 'lacp_rate))
                     (string->symbol (hash-ref next 'xmit_hash_policy))))

       ((change 'net-role uuid _ next)
        (role-config uuid
                     (allocate-bridge-name)
                     (hash-ref next 'bond)
                     (hash-ref next 'vlan_id)
                     (string->symbol (hash-ref next 'role))
                     (hash-ref next 'address)))

       ((change 'vlan (list bond-uuid vlan-tag) _ next)
        (vlan-config bond-uuid vlan-tag
                     (allocate-vlan-name)))))))

; vim:set ts=2 sw=2 et:
