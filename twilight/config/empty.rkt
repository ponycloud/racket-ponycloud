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
         twilight/unit
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
  ((define (config-change self a-change)
     (match a-change
       ((change _ _ _ #f)
        (values self))

       ((change 'nic hwaddr _ next)
        (nic-config hwaddr #t #f (hash-ref next 'bond)))

       ((change 'dev/net _ _ next)
        (let ((hwaddr (props-hwaddr next))
              (iface  (hash-ref next 'interface)))
          (if hwaddr (nic-config hwaddr #f iface #f) self)))

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
                     (allocate-vlan-name)))

       (else self)))))


; vim:set ts=2 sw=2 et:
