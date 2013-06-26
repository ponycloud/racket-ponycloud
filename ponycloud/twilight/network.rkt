#lang racket/base
;
; Network Configuration
;

(require racket/contract
         racket/function
         racket/class
         sysfs/net
         rtnl/simple)

(require "util.rkt")

(provide (all-defined-out))


;; Network entity changes notification function.
(define network-notify (make-parameter void))


;; Configured bond (with it's private bridge).
(define bond%
  (class object%
    (field (bond-name (allocate-bond-name))
           (bridge-name (allocate-bridge-name))
           (slaves null)
           (roles null))

    (init-field uuid
                (mode "active-backup")
                (lacp-rate #f)
                (xmit-hash-policy #f))


    (define/private (reset new-slaves)
      ;; Remember our slaves.
      (define old-slaves slaves)

      ;; Update list of slaves.
      (set! slaves new-slaves)

      ;; Delete bond if it already exists.
      (when (interface-bond? bond-name)
        (remove-bond bond-name))

      ;; Create the bridge interface unless it already exists.
      (unless (interface-bridge? bridge-name)
        (add-bridge bridge-name))

      ;; We don't want bond unless we have slaves for it.
      (unless (null? slaves)
        ;; We have slaves, create the bond interface.
        (add-bond bond-name)

        ;; Set desired bonding options.
        (when mode
          (set-bond-mode! bond-name mode))
        (when lacp-rate
          (set-bond-lacp-rate! bond-name lacp-rate))
        (when xmit-hash-policy
          (set-bond-xmit-hash-policy! bond-name xmit-hash-policy))

        ;; Bring the interface up, now that it has been set.
        (set-interface-up! bond-name #t)

        ;; Re-enslave our slaves.
        (for ((slave (in-list slaves)))
          (bond-slave-add bond-name (get-field device slave)))

        ;; Add the bond to our bridge.
        (bridge-port-add bridge-name bond-name))

      ;; Notify user about the changes.
      (notify))


    (define/public (add-slave nic)
      ;; Enslave only if not already a slave.
      (when (get-field master nic)
        (error 'add-slave "nic is already enslaved"))

      ;; Fail when we do not know name of the NIC device yet.
      (unless (get-field device nic)
        (error 'add-slave "nic is not present yet"))

      ;; Update it's master field.
      (set-field! master nic this)

      ;; Change the configuration.
      (reset (cons nic slaves))

      ;; Notify user about change in the NIC.
      (send nic notify)

      ;; Notify user about the changes.
      (notify))


    (define/public (remove-slave nic)
      ;; Remove only if we are it's master.
      (unless (eq? this (get-field master nic))
        (error 'remove-slave "the nic is not enslaved to this bond"))

      ;; Update the bond configuration.
      (reset (remove nic slaves))

      ;; Update nic's master field.
      (set-field! master nic #f)

      ;; Notify user about change in the NIC.
      (send nic notify)

      ;; Notify user about the changes.
      (notify))


    (define/public (add-role role)
      ;; Make sure we are adding a role without master bond.
      (when (get-field master role)
        (error 'add-role "role already has a master bond"))

      ;; Find existing role with the same VLAN tag.
      (let ((similar (filter (curry dynamic-send role 'same-vlan?) roles)))
        (if (null? similar)
          (begin
            ;; Create role interfaces.
            (send role create-interfaces this)

            ;; Assign the role address.
            (send role assign-address))

          (begin
            ;; There is one, borrow it's interfaces but set the address.
            (send role copy-interfaces (car similar))
            (send role assign-address))))

      ;; Add role to others.
      (set! roles (cons role roles))

      ;; Notify user about the changes.
      (notify))


    (define/public (remove-role role)
      ;; Make sure we are removing our role.
      (unless (eq? this (get-field master role))
        (error 'remove-role "role is not defined on this bond"))

      ;; Remove the role from the list.
      (set! roles (remove role roles))

      ;; Find other roles with the same IP address.
      (let ((similar (filter (curry dynamic-send role 'same-address?) roles)))
        (when (null? similar)
          ;; No other interface needs the address.
          (send role remove-address)))

      ;; Find other roles with the same VLAN tag.
      (let ((similar (filter (curry dynamic-send role 'same-vlan?) roles)))
        (if (null? similar)
          ;; No other roles need the VLAN and bridge, clean up.
          (send role destroy-interfaces)

          ;; Others need to interfaces, just forget about them.
          (send role release-interfaces)))

      ;; Notify user about the changes.
      (notify))


    (define/public (destroy)
      ;; Free all the slaves.
      (for ((slave (in-list slaves)))
        (set-field! master slave #f))

      ;; Physically drop all slaves, thus removing the bond interface.
      (reset null)

      ;; Remove all the roles.
      (for ((role (in-list roles)))
        (remove-role role))

      ;; Drop bridge if it exists.
      (when (interface-bridge? bridge-name)
        (set-interface-up! bridge-name #f)
        (remove-bridge bridge-name))

      ;; Notify user about the changes.
      (notify))


    (define/public (notify)
      ;; Notify about our current state.
      ((network-notify)
       'bond (hasheq 'uuid uuid
                     'mode mode
                     'lacp-rate lacp-rate
                     'xmit-hash-policy xmit-hash-policy
                     'slaves (map (curry dynamic-get-field 'uuid) slaves)
                     'roles (map (curry dynamic-get-field 'uuid) roles))))


    (begin
      ;; Update takes care of projecting the configuration to reality.
      (reset null))


    ;; Construct parent object.
    (super-new)))


;; Configured physical interface.
(define nic%
  (class object%
    (init-field uuid         ; Unique identificator of the entity.
                hwaddr)      ; Hardware (MAC) address of the interface.

    (init-field (device #f)) ; Name of the network interface.

    (field (master #f))      ; Bonding master.


    ;; Called when interface disappears or Twilight terminates.
    (define/public (destroy)
      ;; Disappear from master's slaves.
      (when master
        (send master remove-slave this))

      ;; Notify user about the changes.
      (notify))


    (define/public (notify)
      ;; Notify about our current state.
      ((network-notify)
       'nic (hasheq 'uuid uuid
                    'hwaddr hwaddr
                    'device device
                    'bond (and master (get-field uuid master)))))


    ;; Construct parent object.
    (super-new)))


;; Configured network role, with it's VLAN and Bridge devices.
(define role%
  (class object%
    (init-field uuid          ; Unique identificator of the entity.
                name          ; Role name, something like 'core or 'virtual.
                (vlan-id #f)  ; VLAN of the underlying bond to use.
                (address #f)) ; IP address associated with the role.

    (field (device-name #f)  ; The primary device of the role.
           (bridge-name #f)  ; Name of the bridge to use when vlan-id defined.
           (vlan-name   #f)  ; Name of VLAN interface to use with vlan-id.
           (master      #f)) ; Master bond configuration.


    (define/public (destroy)
      ;; Tell bond to get rid of this role's interfaces.
      (send master remove-role this)

      ;; Notify user about the changes.
      (notify))


    (define/public (copy-interfaces role)
      ;; Copy runtime fields of the other role.
      (set! device-name (get-field device-name role))
      (set! bridge-name (get-field bridge-name role))
      (set! vlan-name   (get-field vlan-name role))
      (set! master      (get-field master role))

      ;; Notify user about the changes.
      (notify))


    (define/public (release-interfaces)
      ;; Drop runtime fields of the role without killing interfaces.
      (set! device-name #f)
      (set! bridge-name #f)
      (set! vlan-name   #f)
      (set! master      #f)

      ;; Notify user about the changes.
      (notify))


    (define/public (create-interfaces bond)
      ;; Set our master.
      (set! master bond)

      ;; Determine whether we need a VLAN tagged interface or not.
      (if vlan-id
        (begin
          ;; Generate some names and place the address on the new bridge.
          (set! bridge-name (allocate-bridge-name))
          (set! vlan-name   (format "~a.~a" (get-field bridge-name bond)
                                            vlan-id))
          (set! device-name bridge-name))

        ;; Our device is bond's bridge directly.
        (set! device-name (get-field bridge-name bond)))

      ;; Create vlan if we need it.
      (when (and vlan-name (not (interface-vlan? vlan-name)))
        (add-vlan vlan-name (get-field bridge-name bond) vlan-id))

      ;; Create bridge if we need it.
      (when (and bridge-name (not (interface-bridge? bridge-name)))
        (add-bridge bridge-name)
        (bridge-port-add bridge-name vlan-name))

      ;; Notify user about the changes.
      (notify))


    (define/public (destroy-interfaces)
      ;; Destroy the bridge if it exists.
      (when (and bridge-name (interface-bridge? bridge-name))
        (set-interface-up! bridge-name #f)
        (remove-bridge bridge-name))

      ;; Destroy the VLAN interface if it exists.
      (when (and vlan-name (interface-vlan? vlan-name))
        (remove-vlan vlan-name))

      ;; Forget things...
      (release-interfaces))


    (define/public (assign-address)
      (when (and device-name address
                 (not (member address (get-interface-ipaddrs device-name))))
        (add-interface-ipaddr device-name address)))


    (define/public (remove-address)
      (when (and device-name address)
        (remove-interface-ipaddr/safe device-name address)))


    (define/public (same-vlan? other)
      (eq? vlan-id (get-field vlan-id other)))


    (define/public (same-address? other)
      (equal? address (get-field address other)))


    (define/public (notify)
      ;; Notify about our current state.
      ((network-notify)
       'nic-role (hasheq 'uuid uuid
                         'vlan-id vlan-id
                         'address address
                         'bond (and master (get-field uuid master))
                         'device-name device-name
                         'bridge-name bridge-name
                         'vlan-name vlan-name)))


    ;; Construct parent object.
    (super-new)))


; vim:set ts=2 sw=2 et:
