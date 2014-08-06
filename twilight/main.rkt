#lang racket/base
;
; The Twilight Agent
;

(require racket/contract
         racket/class
         racket/match
         racket/list
         json)

(require misc1/evt
         libuuid
         libvirt
         udev)

(require "sparkle.rkt"
         "network.rkt"
         "storage.rkt"
         "libvirt.rkt")

(provide
  (contract-out
    (twilight% twilight-class/c)))


;; Custom logger for the class below.
(define-logger twilight)


(define c-u-d/c
  (symbols 'create 'update 'delete))


(define twilight-class/c
  (class/c
    (init-field (connect-to string?))

    (field (uuid uuid?)
           (sparkle (is-a?/c sparkle%))
           (blkdev-evt (evt/c symbol? device?))
           (netdev-evt (evt/c symbol? device?))
           (network-manager (is-a?/c network-manager%))
           (storage-manager (is-a?/c storage-manager%))
           (libvirt-manager (is-a?/c libvirt-manager%)))

    (get-evt (->m evt?))))


(define twilight%
  (class object%
    ;; 0MQ URL of controller to connect to.
    (init-field connect-to)

    ;; UUID of this host.
    (field (uuid (libvirt-uuid)))

    ;; Component that communicates with Sparkle.
    (field (sparkle (new sparkle%
                         (uuid uuid)
                         (connect-to connect-to))))

    ;; Udev device monitoring.
    (field (blkdev-evt (device-changed-evt #:subsystems '(block)))
           (netdev-evt (device-changed-evt #:subsystems '(net))))

    ;; High-level subsystem managers.
    (field (network-manager (new network-manager% (twilight this)))
           (storage-manager (new storage-manager% (twilight this)))
           (libvirt-manager (new libvirt-manager% (twilight this))))


    ;; Complex event that runs Twilight without producing any values.
    (define/public (get-evt)
      (define/contract (publish/one table pkey data)
                       (-> string? jsexpr? jsexpr? void?)
        (send sparkle publish/one table pkey data))

      (define/contract (forward action table pkey data)
                       (-> c-u-d/c string? jsexpr? jsexpr? void?)
        (case table
          (("nic" "bond" "net_role")
           (dynamic-send network-manager action table pkey data))

          (("disk" "volume" "extent" "storage_pool")
           (dynamic-send storage-manager action table pkey data))

          (("instance" "vdisk" "vnic")
           (dynamic-send libvirt-manager action table pkey data))))

      (recurring-evt
        (choice-evt (wrap-evt (send sparkle get-evt) forward)

                    (wrap-evt (send network-manager get-evt) publish/one)
                    (wrap-evt (send storage-manager get-evt) publish/one)
                    (wrap-evt (send libvirt-manager get-evt) publish/one)

                    (wrap-evt blkdev-evt
                              (λ (action device)
                                (send storage-manager
                                      on-device-event action device)))

                    (wrap-evt netdev-evt
                              (λ (action device)
                                (send network-manager
                                      on-device-event action device))))))


    ;; Construct parent object.
    (super-new)))


; vim:set ts=2 sw=2 et:
