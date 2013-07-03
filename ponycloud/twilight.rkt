#lang racket/base
;
; The Twilight Agent
;

(require racket/contract
         racket/function
         racket/class
         racket/match
         xml/path
         tasks
         virt)

(require "twilight/util.rkt"
         "twilight/network.rkt"
         "twilight/udev.rkt"
         "twilight/communicator.rkt")

(provide (all-defined-out))


;; Main object that takes care of platform management.
(define twilight%
  (class object%
    ;; 0MQ URL of controller to connect to.
    (init-field connect-to)

    ;; Libvirt / QEMU connection.
    (field (qemu (virt-connect)))

    ;; UUID of this host.
    (field (uuid (se-path* '(host uuid) (virt-capabilities qemu))))

    ;; Component that communicates with Sparkle.
    (field (communicator (new communicator% (twilight this))))

    ;; Network manager, takes care of our interfaces.
    (field (net-manager (new network-manager%)))


    ;; Serves as network changes notification callback.
    ;; Enhances the information and forwards it to the communicator.
    (define (network-notify entity id value)
      (let ((value-with-host (and value (hash-set value 'host uuid))))
        (send communicator publish/one entity id value-with-host)))


    ;; Called by net-monitor above.
    (define (network-event action sysname hwaddr)
      (parameterize ((current-network-notify network-notify))
        (cond
          ((eq? action 'add)
           (send net-manager assign-nic-device hwaddr sysname))

          ((eq? action 'remove)
           (send net-manager unassign-nic-device hwaddr sysname)))))


    ;; Udev network device monitor.
    (field (net-monitor (new udev-monitor%
                             (sink network-event)
                             (subsystem "net"))))


    (define/public (setup-entity entity id value)
      (parameterize ((current-network-notify network-notify))
        (cond
          ((equal? entity "nic")
           (send net-manager setup-nic id value))

          ((equal? entity "bond")
           (send net-manager setup-bond id value))

          ((equal? entity "nic_role")
           (send net-manager setup-role id value)))))


    (define/public (remove-entity entity id value)
      (parameterize ((current-network-notify network-notify))
        (cond
          ((equal? entity "nic")
           (send net-manager remove-nic id value))

          ((equal? entity "bond")
           (send net-manager remove-bond id value))

          ((equal? entity "nic_role")
           (send net-manager remove-role id value)))))


    ;; Construct parent object.
    (super-new)))


; vim:set ts=2 sw=2 et:
