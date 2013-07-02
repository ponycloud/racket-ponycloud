#lang racket/base
;
; Network Configuration Components
;

(require racket/contract
         racket/function
         racket/class
         racket/set
         sysfs/net
         rtnl/simple
         tasks)

(require "util.rkt")

(provide (all-defined-out))


;; Network entity changes notification function.
(define network-notify (make-parameter void))


;; Configured bond (with it's private bridge).
(define bond%
  (class object%
    (field (bond-name (allocate-bond-name))
           (bridge-name (allocate-bridge-name))
           (hwaddr (generate-hwaddr))
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

      (unless (interface-bridge? bridge-name)
        ;; Create the bridge interface unless it already exists.
        (add-bridge bridge-name)

        ;; Assign the persistent, generated address.
        (set-interface-addr! bridge-name hwaddr))

      (unless (null? slaves)
        ;; We have slaves, create the bond interface.
        (add-bond bond-name)

        ;; Assign the persistent, generated address.
        (set-interface-addr! bond-name hwaddr)

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
          (bond-slave-add bond-name (get-field device-name slave)))

        ;; Add the bond to our bridge.
        (bridge-port-add bridge-name bond-name))

      ;; Notify user about the changes.
      (notify))


    ;; Update bond parameters.
    (define/public (update)
      (reset slaves))


    (define/public (add-slave nic)
      ;; Enslave only if not already a slave.
      (when (get-field master nic)
        (error 'add-slave "nic is already enslaved"))

      ;; Fail when we do not know name of the NIC device yet.
      (unless (get-field device-name nic)
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
      ((network-notify) "bond" uuid
        (hasheq 'uuid uuid
                'mode mode
                'hwaddr hwaddr
                'lacp_rate (or lacp-rate 'null)
                'xmit_hash_policy (or xmit-hash-policy 'null)
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
    ;; Hardware (MAC) address of the interface.
    (init-field hwaddr)

    ;; Name of the network interface.
    (init-field (device-name #f))

    ;; Bonding master.
    (field (master #f))


    ;; Called when interface disappears or Twilight terminates.
    (define/public (destroy)
      ;; Disappear from master's slaves.
      (when master
        (send master remove-slave this))

      ;; Notify user about the changes.
      (notify))


    (define/public (notify)
      ;; Notify about our current state.
      ((network-notify) "nic" hwaddr
        (hasheq 'hwaddr hwaddr
                'bond (or (and master (get-field uuid master)) 'null))))


    (begin
      ;; Notify about new nics.
      (notify))


    ;; Construct parent object.
    (super-new)))


;; Configured network role, with it's VLAN and Bridge devices.
(define role%
  (class object%
    (init-field uuid          ; Unique identificator of the entity.
                (name #f)     ; Role name, something like 'core or 'virtual.
                (vlan-id #f)  ; VLAN of the underlying bond to use.
                (address #f)) ; IP address associated with the role.

    (field (device-name #f)  ; The primary device of the role.
           (bridge-name #f)  ; Name of the bridge to use when vlan-id defined.
           (vlan-name   #f)  ; Name of VLAN interface to use with vlan-id.
           (master      #f)) ; Master bond configuration.

    (field (hwaddr (generate-hwaddr))) ; Generate MAC for potential bridge.


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

      (when (and bridge-name (not (interface-bridge? bridge-name)))
        ;; Create the required bridge.
        (add-bridge bridge-name)

        ;; Assign the persistent MAC address.
        (set-interface-addr! bridge-name hwaddr)

        ;; Plug in the upstream interface.
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
      ((network-notify) "nic-role" uuid
        (hasheq 'uuid uuid
                'name name
                'vlan_id (or vlan-id 'null)
                'address (or address 'null)
                'bond (or (and master (get-field uuid master)) 'null)
                'hwaddr hwaddr)))


    (begin
      ;; Notify about new role.
      (notify))


    ;; Construct parent object.
    (super-new)))


;; Component that interprets desired state changes as instances of
;; the classes defined above.  In other words, it manages networking.
(define network-manager%
  (class object%
    ;; Known bonds, roles and network interfaces.
    (field (bonds (make-hash))
           (roles (make-hash))
           (nics  (make-hash)))

    ;; Network interfaces and roles that are to be assigned to bonds.
    ;; Mapped by bond uuid, consulted when a new bond is added.
    (field (bond-slaves (make-hash))
           (bond-roles  (make-hash)))


    ;; Create or update specified bond.
    (define/public (setup-bond uuid config)
      (let ((bond (hash-ref! bonds uuid (thunk (new bond% (uuid uuid))))))

        ;; Set it's parameters.
        (set-field! mode             bond (hash-ref config 'mode))
        (set-field! lacp-rate        bond (hash-ref config 'lacp_rate))
        (set-field! xmit-hash-policy bond (hash-ref config 'xmit_hash_policy))

        ;; Update the configuration.
        ;; If it has any slaves, it will be re-created with the parameters
        ;; above.  Otherwise nothing will happen until first slave is added.
        (send bond update)

        ;; Assign any unassigned slaves destined for this bond.
        (for ((slave (in-set (hash-ref bond-slaves uuid set))))
          (unless (get-field master slave)
            (when (get-field device-name slave)
              (send bond add-slave slave))))

        ;; Assign any unassigned roles destined for this bond.
        (for ((role (in-set (hash-ref bond-roles uuid set))))
          (unless (get-field master role)
            (send bond add-role role)))))


    ;; Delete specified bond.
    ;; A no-op if the bond does not exist.
    (define/public (remove-bond uuid config)
      (let ((bond (hash-ref bonds uuid #f)))
        (when bond
          (hash-remove! bonds uuid)
          (send bond destroy))))


    ;; Create or update specified network interface.
    (define/public (setup-nic hwaddr config)
      (let* ((nic (hash-ref! nics hwaddr (thunk (new nic% (hwaddr hwaddr)))))
             (master-uuid (hash-ref config 'bond))
             (old-bond    (get-field master nic))
             (new-bond    (hash-ref bonds master-uuid #f)))

        ;; Update the collection of possible slaves.
        (let ((slaves (hash-ref bond-slaves master-uuid set)))
          (hash-set! bond-slaves master-uuid (set-add slaves nic)))

        ;; If we know the device name, we need to consider enslavement.
        (when (get-field device-name nic)
          ;; Remove the nic from old bonding master, if needed.
          (when old-bond
            (unless (eq? old-bond new-bond)
              (send old-bond remove-slave nic)))

          ;; Add the NIC to the new bonding master, if configured.
          (when new-bond
            (send new-bond add-slave nic)))))


    ;; Assign device name to a network interface.
    ;; A new NIC object can be created just for this purpose.
    (define/public (assign-nic-device hwaddr device-name)
      (let ((nic (hash-ref! nics hwaddr (thunk (new nic% (hwaddr hwaddr))))))
        ;; If the device is a member of a bond, remove it.
        (let ((master (get-field master nic)))
          (when master
            (send master remove-slave nic)))

        ;; Update the device name.
        (set-field! device-name nic device-name)

        ;; Assign the device to it's destined bond, if any.
        (unless (get-field master nic)
          (for (((master-uuid other-nic) (in-hash-pairs bond-slaves)))
            (when (eq? other-nic nic)
              (let ((bond (hash-ref bonds master-uuid #f)))
                (when bond
                  (send bond add-slave nic))))))))


    ;; Remove device name assignment from specified network interface object.
    (define/public (unassign-nic-device hwaddr device-name)
      (let ((nic (hash-ref nics hwaddr #f)))
        (when nic
          ;; Kill the NIC's membership in the bond.
          (send nic destroy)

          ;; Unassign the device name.
          (set-field! device-name nic #f)

          ;; Check if destined for any bond.
          (unless (set-member?
                    (foldl set-union (set) (hash-values bond-slaves)) nic)

            ;; Remove it completely, it's not.
            (hash-remove! nics hwaddr)))))


    ;; Delete specified network interface.
    (define/public (remove-nic hwaddr config)
      (let ((nic (hash-ref nics hwaddr #f)))
        (when nic
          ;; Remove the nic from bond, etc...
          (send nic destroy)

          ;; Remove the nic from potential slaves.
          (let* ((master-uuid (hash-ref config 'bond #f))
                 (slaves      (hash-ref bond-slaves master-uuid set))
                 (slaves      (set-remove slaves nic)))
            (if (> (set-count slaves) 0)
              (hash-set! bond-slaves master-uuid slaves)
              (hash-remove! bond-slaves master-uuid)))

          ;; If we don't have device name then we can forget the nic
          ;; completely - it is not physically present.
          (unless (get-field device-name nic)
            (hash-remove! nics hwaddr)))))


    ;; Create or update specified network role.
    (define/public (setup-role uuid config)
      (let* ((role (hash-ref! roles uuid (thunk (new role% (uuid uuid)))))
             (master-uuid (hash-ref config 'bond))
             (old-bond    (get-field master role))
             (new-bond    (hash-ref bonds master-uuid #f)))

        ;; Update the collection of possible roles.
        (let ((roles (hash-ref bond-roles master-uuid set)))
          (hash-set! bond-roles master-uuid (set-add roles role)))

        ;; Remove the role from old bonding master.
        (when old-bond
          (unless (eq? old-bond new-bond)
            (send old-bond remove-role role)))

        ;; Add the role to the new bonding master, if configured.
        (when new-bond
          (send new-bond add-role role))))


    ;; Delete specified network role.
    (define/public (remove-role uuid config)
      (let ((role (hash-ref roles uuid #f)))
        (when role
          ;; Remove the role from bond, etc...
          (send role destroy)

          ;; Remove the role from potential roles.
          (let* ((master-uuid (hash-ref config 'bond #f))
                 (roles       (hash-ref bond-roles master-uuid set))
                 (roles       (set-remove roles role)))
            (if (> (set-count roles) 0)
              (hash-set! bond-roles master-uuid roles)
              (hash-remove! bond-roles master-uuid)))

          ;; Forget the role completely.
          (hash-remove! roles uuid))))


    ;; Construct parent object.
    (super-new)))


; vim:set ts=2 sw=2 et:
