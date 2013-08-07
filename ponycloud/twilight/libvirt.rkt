#lang racket/base
;
; Libvirt Utilities
;

(require racket/class
         racket/contract
         unstable/socket)

(require xml/xexpr-path
         libvirt)

(provide (all-defined-out))


;; Connect to libvirt using the UNIX domain socket.
(define (libvirt-unix-client (socket "/var/run/libvirt/libvirt-sock")
                             (connection "qemu:///system"))
  (let-values (((in out) (unix-socket-connect socket)))
    (let ((libvirt (new libvirt% (in in) (out out))))
      (send libvirt open connection)
      libvirt)))



(define (libvirt-uuid libvirt)
  (string-downcase
    (xexpr-path-text '(system entry (name "uuid") *)
                     (send libvirt system-info))))


; vim:set ts=2 sw=2 et:
