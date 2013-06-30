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

    ;; Udev network device monitor.
    (field (net-monitor (new udev-monitor%
                             (sink (curry dynamic-send this 'net-event))
                             (subsystem "net"))))

    ;; Network manager, takes care of our interfaces.
    (field (net-manager (new network-manager%)))


    ;; Called by net-monitor above.
    (define/public (net-event action sysname hwaddr)
      (printf "net ~a ~s ~s\n" action sysname hwaddr))


    (define/public (setup-entity entity id value)
      (printf "setup-entity ~s ~s ~s\n" entity id value))


    (define/public (remove-entity entity id value)
      (printf "remove-entity ~s ~s\n" entity id value))


    ;; Construct parent object.
    (super-new)))


; vim:set ts=2 sw=2 et:
