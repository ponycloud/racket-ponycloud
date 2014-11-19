#lang racket/base
;
; The Twilight Agent
;

(require racket/contract
         racket/class
         racket/match
         racket/list
         racket/set
         json)

(require misc1/evt)

(require libuuid
         libvirt
         udev)

(require twilight/sparkle
         twilight/config
         twilight/unit)

(provide
  (contract-out
    (twilight% twilight-class/c)))


(define twilight-class/c
  (class/c
    (init-field (connect-to string?))

    (field (uuid uuid?)
           (sparkle (is-a?/c sparkle%))
           (blkdev-evt (evt/c symbol? device?))
           (netdev-evt (evt/c symbol? device?)))

    (field (configs (set/c config?))
           (units (hash/c (list/c symbol? any/c) unit?)))

    (get-evt (->m evt?))))


;; Custom logger for the class below.
(define-logger twilight)

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

    ;; Hash tables with configurations and running/finished units.
    (field (configs (set))
           (units (make-hash)))

    ;; Complex event that runs Twilight without producing any values.
    (define/public (get-evt)
      (recurring-evt
        (choice-evt
          (wrap-evt (send sparkle get-evt)
                    (λ (changes)
                      (update (foldl apply-change configs changes))))

          (wrap-evt (get-units-evt)
                    (λ (changes)
                      (send sparkle publish changes)))

          (wrap-evt blkdev-evt
                    (λ (action device)
                      (void)))

          (wrap-evt netdev-evt
                    (λ (action device)
                      (void))))))

    (define (get-units-evt)
      never-evt)

    (define (update new-configs)
      (let ((old-configs configs)
            (new-units (hash-copy units))
            (mod-units (mutable-seteq)))
        ;; Save updated configuration objects.
        (set! configs new-configs)

        ;; Spawn units for modified configurations.
        (for ((a-config (set-subtract configs old-configs)))
          (let ((new-unit (config-spawn-unit a-config new-units))
                (key      (list (config-type a-config)
                                (config-pkey a-config))))
            (hash-set! new-units key new-unit)
            (set-add!  mod-units new-unit)))

        ;; Save updated unit collection.
        (set! units new-units)

        ;; Start modified units.
        (for ((unit-to-start mod-units))
          (unit-start! unit-to-start))))

    ;; Read the initial device events.
    (begin
      (for ((sys-path (list-devices #:subsystems '(net))))
        (void (device sys-path)))

      (for ((sys-path (list-devices #:subsystems '(block))))
        (void (device sys-path))))

    ;; Construct parent object.
    (super-new)))


; vim:set ts=2 sw=2 et:
