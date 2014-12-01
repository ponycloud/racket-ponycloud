#lang racket/base
;
; The Twilight Agent
;

(require racket/contract
         racket/function
         racket/class
         racket/match
         racket/list
         racket/set
         json)

(require misc1/syntax
         misc1/evt)

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
           (device-evt (evt/c symbol? device?)))

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
    (field (device-evt (device-changed-evt #:subsystems '(block net))))

    ;; Hash tables with configurations and running/finished units.
    (field (configs (set))
           (units (make-hash)))

    ;; Units whose result have not yet been reported.
    (define pending-units (mutable-set))

    ;; Complex event that runs Twilight without producing any values.
    (define/public (get-evt)
      (recurring-evt
        (choice-evt
          (wrap-evt (send sparkle get-evt)
                    (λ (changes)
                      (apply-changes! changes)))

          (wrap-evt (get-units-evt)
                    (λ (changes)
                      (let ((changes (map inject-host changes)))
                        (send sparkle publish changes))))

          (wrap-evt device-evt
                    (λ (action device)
                      (apply-changes!
                        (list (device-change action device))))))))

    (define (get-units-evt)
      (guard-evt
        (thunk (apply choice-evt (set->list pending-units)))))

    (define (apply-changes! changes)
      (update!
        (foldl apply-change configs changes)))

    (define (inject-host a-change)
      (match-let (((change table pkey prev next) a-change))
        (let ((prev (if prev (hash-set prev 'host uuid) prev))
              (next (if next (hash-set next 'host uuid) next)))
          (change table pkey prev next))))

    (define (update! new-configs)
      (let ((old-configs configs)
            (new-units (hash-copy units))
            (mod-units (mutable-seteq)))
        ;; Save updated configuration objects.
        (set! configs new-configs)

        ;; Spawn units for modified configurations.
        (for ((a-config (set-subtract configs old-configs)))
          (let ((new-unit (config-spawn-unit a-config new-units))
                (key      (cons (config-type a-config)
                                (config-pkey a-config))))
            (hash-set! new-units key new-unit)
            (set-add!  mod-units new-unit)

            (set-add! pending-units
                      (recursive (new-evt)
                        (wrap-evt new-unit
                                  (λ (result)
                                    (set-remove! pending-units new-evt)
                                    (when (config-delete? a-config)
                                      (set-remove! configs a-config))
                                    (config-report a-config result)))))))

        ;; Save updated unit collection.
        (set! units new-units)

        ;; Start modified units.
        (for ((unit-to-start mod-units))
          (unit-start! unit-to-start))))

    ;; Read the initial device events.
    (apply-changes!
      (for/list ((sys-path (list-devices #:subsystems '(net block))))
        (device-change 'add (device sys-path))))

    ;; Construct parent object.
    (super-new)))


(define (device-change action a-device)
  (let ((data (device-properties a-device))
        (subs (device-subsystem a-device)))
    (change (string->symbol
              (format "dev/~a" subs))
            (device-sys-name a-device)
            (if (eq? action 'remove) data #f)
            (if (eq? action 'remove) #f data))))


; vim:set ts=2 sw=2 et:
