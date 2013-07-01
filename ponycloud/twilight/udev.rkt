#lang racket/base
;
; Udev Monitor
;

(require racket/contract
         racket/string
         racket/class
         tasks
         udev)

(provide (except-out (all-defined-out) get-device-mac))


(define (get-device-mac device)
  (let ((id (udev-device-get-property-value device 'ID_NET_NAME_MAC)))
    (if id (string-join (regexp-match* ".." (substring id 3)) ":") #f)))


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
        (filter (lambda (info)
                  (andmap values info))
                (for/list ((syspath (in-list (udev-enumerate-get enumerator))))
                  (let* ((device  (udev-device-new-from-syspath udev syspath))
                         (sysname (udev-device-get-sysname device))
                         (hwaddr  (get-device-mac device)))
                    (list sysname hwaddr))))))


    (begin
      ;; Setup our monitor to watch given subsystem.
      (udev-monitor-filter-add-match-subsystem-devtype
        monitor subsystem #f)

      ;; Start receiving events.
      (udev-monitor-enable-receiving! monitor)

      (task
        ;; Replay existing devices once we start.
        (for ((info (in-list (enumerate))))
          (apply sink 'add info)))

      ;; Dispatch device events.
      (recurring-event-task (device (udev-monitor-evt monitor))
        (let ((action  (udev-device-get-action device))
              (sysname (udev-device-get-sysname device))
              (hwaddr  (get-device-mac device)))
          (when (and action sysname hwaddr)
            (sink (string->symbol action) sysname hwaddr)))))


    ;; Construct parent object.
    (super-new)))


; vim:set ts=2 sw=2 et:
