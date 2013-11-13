#lang racket/base
;
; The Twilight Agent
;

(require racket/contract
         racket/function
         racket/string
         racket/class
         racket/match
         racket/list
         unstable/socket)

(require libvirt
         tasks
         udev)

(require "private/util.rkt"
         "private/common.rkt"
         "private/network.rkt"
         "private/libvirt.rkt"
         "private/communicator.rkt")

(provide twilight%)


;; Main object that takes care of platform management.
(define twilight%
  (class object%
    ;; 0MQ URL of controller to connect to.
    (init-field connect-to)

    ;; Libvirt connection.
    (field (libvirt (libvirt-unix-client)))

    ;; UUID of this host.
    (field (uuid (libvirt-uuid libvirt)))

    ;; Component that communicates with Sparkle.
    (field (communicator (new communicator% (twilight this))))

    ;; Network manager, takes care of our interfaces.
    (field (net-manager (new network-manager%)))


    ;; Serves as network changes notification callback.
    ;; Enhances the information and forwards it to the communicator.
    (define (network-notify entity id value)
      (let ((value-with-host (and value (hash-set value 'host uuid))))
        (send communicator publish/one entity id value-with-host)))


    ;; Called by net-monitor below.
    (define (network-event action name hwaddr)
      (parameterize ((current-notify network-notify))
        (cond
          ((eq? action 'add)
           (send net-manager assign-nic-device hwaddr name))

          ((eq? action 'remove)
           (send net-manager unassign-nic-device hwaddr name)))))


    (begin
      (monitor-network-devices network-event))


    (define/public (setup-entity entity id value)
      (parameterize ((current-notify network-notify))
        (cond
          ((equal? entity "nic")
           (send net-manager setup-nic id value))

          ((equal? entity "bond")
           (send net-manager setup-bond id value))

          ((equal? entity "nic_role")
           (send net-manager setup-role id value)))))


    (define/public (remove-entity entity id value)
      (parameterize ((current-notify network-notify))
        (cond
          ((equal? entity "nic")
           (send net-manager remove-nic id value))

          ((equal? entity "bond")
           (send net-manager remove-bond id value))

          ((equal? entity "nic_role")
           (send net-manager remove-role id value)))))


    ;; Construct parent object.
    (super-new)))


(define (info-with-hwaddr? info)
  (and (hash? info)
       (hash-has-key? info 'ID_NET_NAME_MAC)))


(define (hwaddr? addr)
  (and (string? addr)
       (regexp-match? #rx"^[a-f0-9][a-f0-9](:[a-f0-9][a-f0-9])*$" addr)))


(define/contract (get-device-hwaddr info)
                 (-> info-with-hwaddr? hwaddr?)
  (let ((id (hash-ref info 'ID_NET_NAME_MAC)))
    (string-join (regexp-match* ".." (substring id 3)) ":")))


(define/contract (monitor-network-devices sink)
                 (-> (-> (one-of/c 'add 'remove) string? hwaddr? void?) void?)
  (task
    (for (((syspath info) (list-devices #:subsystem "net")))
      (when (info-with-hwaddr? info)
        (let ((name   (hash-ref info 'INTERFACE))
              (hwaddr (get-device-hwaddr info)))
          (sink 'add name hwaddr)))))

  (recurring-event-task (info (device-changed-evt #:subsystem "net"))
    (when (info-with-hwaddr? info)
      (let ((action (string->symbol (hash-ref info 'ACTION)))
            (name   (hash-ref info 'INTERFACE))
            (hwaddr (get-device-hwaddr info)))
        (sink action name hwaddr)))))


; vim:set ts=2 sw=2 et:
