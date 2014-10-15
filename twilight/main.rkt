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
         udev
         dds)

(require "sparkle.rkt"
         "network.rkt"
         "storage.rkt"
         "libvirt.rkt"
         "change.rkt")

(provide
  (contract-out
    (twilight% twilight-class/c)))


;; Custom logger for the class below.
(define-logger twilight)


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

    ;; Intra-component dependency solver and scheduler.
    (field (solver (make-solver)))

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

      (define/contract (forward changes)
                       (-> (listof (or/c create? update? delete?)) void?)
        (for ((a-change changes))
          (case (change-table a-change)
            (("nic" "bond" "vlan" "net_role")
             (send network-manager apply-change a-change))

            (("disk" "volume" "extent" "storage_pool")
             (send storage-manager apply-change a-change))

            (("instance" "vdisk" "vnic")
             (send libvirt-manager apply-change a-change))))

        (send network-manager cork)
        (send storage-manager cork)
        (send libvirt-manager cork))

      (recurring-evt
        (choice-evt (wrap-evt (send sparkle get-evt) forward)

                    (wrap-evt (send network-manager get-evt) publish/one)
                    (wrap-evt (send storage-manager get-evt) publish/one)
                    (wrap-evt (send libvirt-manager get-evt) publish/one)

                    (wrap-evt blkdev-evt
                              (λ (action device)
                                (case action
                                  ((add change)
                                   (send storage-manager appear device))

                                  ((remove)
                                   (send storage-manager disappear device)))))

                    (wrap-evt netdev-evt
                              (λ (action device)
                                (case action
                                  ((add change)
                                   (send network-manager appear device))

                                  ((remove)
                                   (send network-manager disappear device))))))))

    ;; Shortcut for waiting on the event above.
    (define/public (run)
      (sync (get-evt)))

    ;; Read the initial device events.
    (begin
      (for ((sys-path (list-devices #:subsystems '(net))))
        (send network-manager appear (device sys-path)))

      (for ((sys-path (list-devices #:subsystems '(block))))
        (send storage-manager appear (device sys-path))))

    ;; Construct parent object.
    (super-new)))


; vim:set ts=2 sw=2 et:
