#lang racket/base
;
; Udev Monitor
;

(require racket/contract
         racket/class
         tasks
         udev)

(provide (all-defined-out))


(define udev-monitor%
  (class object%
    ;; Subsystem to monitor.
    (init-field subsystem)

    ;; Procedure to sink device events to.
    ;; It will receive (event sysname) arguments.
    (init-field (sink void))

    ;; An udev context.
    (field (udev (udev-new)))

    ;; Monitor object.
    (field (monitor (udev-monitor-new-from-netlink udev)))


    (define/public (enumerate)
      ;; Create device enumerator.
      (let ((enumerator (udev-enumerate-new udev)))
        ;; Match our designated subsystem.
        (udev-enumerate-add-match-subsystem! enumerator subsystem)

        ;; Scan devices and match them against the subsystem.
        (udev-enumerate-scan-devices! enumerator)

        ;; Generate list of sysnames for all matches devices.
        (for/list ((syspath (in-list (udev-enumerate-get enumerator))))
          (let ((device (udev-device-new-from-syspath udev syspath)))
            (udev-device-get-sysname device)))))


    (begin
      ;; Setup our monitor to watch given subsystem.
      (udev-monitor-filter-add-match-subsystem-devtype
        monitor subsystem #f)

      ;; Start receiving events.
      (udev-monitor-enable-receiving! monitor)

      (task
        ;; Replay existing devices once we start.
        (for ((sysname (in-list (enumerate))))
          (sink 'add sysname)))

      ;; Dispatch device events.
      (recurring-event-task (device (udev-monitor-evt monitor))
        (sink (string->symbol (udev-device-get-action device))
              (udev-device-get-sysname device))))

    ;; Construct parent object.
    (super-new)))


; vim:set ts=2 sw=2 et:
