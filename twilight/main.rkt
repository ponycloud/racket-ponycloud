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
         "private/storage.rkt"
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

    ;; Component that takes care of network interfaces.
    (field (network-manager (new network-manager% (twilight this))))

    ;; Component that takes care of block devices.
    (field (storage-manager (new storage-manager% (twilight this))))


    ;; Serves as network changes notification callback.
    ;; Enhances the information and forwards it to the communicator.
    (define (communicator-notify entity id value)
      (let ((value-with-host (and value (hash-set value 'host uuid))))
        (send communicator publish/one entity id (or value-with-host 'null))))


    (define/public (setup-entity entity id value)
      (parameterize ((current-notify communicator-notify))
        (match entity
          ("nic"          (send network-manager setup-nic id value))
          ("bond"         (send network-manager setup-bond id value))
          ("net_role"     (send network-manager setup-role id value))
          ("storage_pool" (send storage-manager pool-configure value))
          ("disk"         (send storage-manager disk-configure value))
          (else
            (printf "setup-entity ~s not implemented\n" entity)))))


    (define/public (remove-entity entity id value)
      (parameterize ((current-notify communicator-notify))
        (match entity
          ("nic"          (send network-manager remove-nic id value))
          ("bond"         (send network-manager remove-bond id value))
          ("net_role"     (send network-manager remove-role id value))
          ("storage_pool" (send storage-manager pool-deconfigure value))
          ("disk"         (send storage-manager disk-deconfigure value))
          (else
            (printf "remove-entity ~s not implemented\n" entity)))))


    (begin
      (monitor-network-devices
        (λ (action name hwaddr)
          (parameterize ((current-notify communicator-notify))
            (match action
              ('remove
               (send network-manager unassign-nic-device hwaddr name))

              (else
               (send network-manager assign-nic-device hwaddr name))))))

      (monitor-storage-devices
        (λ (action info)
          (parameterize ((current-notify communicator-notify))
            (match action
              ('remove
               (send storage-manager disk-blkdev-removed info))

              (else
               (send storage-manager disk-blkdev-changed info)))))))


    ;; Construct parent object.
    (super-new)))


(define (info-with-hwaddr? info)
  (and (hash? info)
       (hash-has-key? info 'ID_NET_NAME_MAC)))


(define (info-mpath? info)
  (and (hash? info)
       (regexp-match? #rx"^mpath-" (hash-ref info 'DM_UUID ""))))


(define (hwaddr? addr)
  (and (string? addr)
       (regexp-match? #rx"^[a-f0-9][a-f0-9](:[a-f0-9][a-f0-9])*$" addr)))


(define/contract (get-device-hwaddr info)
                 (-> info-with-hwaddr? hwaddr?)
  (let ((id (hash-ref info 'ID_NET_NAME_MAC)))
    (string-join (regexp-match* ".." (substring id 3)) ":")))


(define/contract (monitor-network-devices sink)
                 (-> (-> symbol? string? hwaddr? void?) void?)
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


(define/contract (monitor-storage-devices sink)
                 (-> (-> symbol? hash? void?) void?)
  (task
    (for (((syspath info) (list-devices #:subsystem "block")))
      (when (info-mpath? info)
        (sink 'add info))))

  (recurring-event-task (info (device-changed-evt #:subsystem "block"))
    (when (info-mpath? info)
      (let ((action (string->symbol (hash-ref info 'ACTION))))
        (sink action info)))))


; vim:set ts=2 sw=2 et:
