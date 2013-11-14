#lang racket/base
;
; Storage Configuration Component
;

(require racket/contract
         racket/function
         racket/class
         racket/path
         sysfs/block
         tasks)

(require "util.rkt"
         "common.rkt")

(provide storage-manager%)


(define disk%
  (class object%
    ;; Unique identificator of the disk.
    ;; Expect something like wwid or a serial number.
    (init-field id)

    ;; Detailed information about the block device backing the disk.
    (field (name #f)
           (major #f)
           (minor #f)
           (size #f))

    ;; Storage pool uuid the disk have been configured to be a member of.
    (field (pool #f))


    (define/public (notify)
      ((current-notify) "disk" id
        (hasheq 'id id
                'storage_pool (or pool 'null)
                'size size
                'present (and name #t))))


    ;; Construct parent object.
    (super-new)))


(define storage-manager%
  (class object%
    ;; Known disk devices - either present, configured or both.
    (field (disks (make-hash)))


    ;; Add information relevant for volume allocation to correct disk object.
    ;; Might cause new disk% instance allocation in case this have not yet
    ;; been configured.
    (define/public (assign-disk-device info)
      (let* ((id   (substring (hash-ref info 'DM_UUID) 6))
             (disk (hash-ref! disks id (thunk (new disk% (id id)))))
             (name (basename (hash-ref info 'DEVNAME))))
        (set-field! name  disk name)
        (set-field! size  disk (get-block-device-size name))
        (set-field! major disk (string->number (hash-ref info 'MAJOR)))
        (set-field! minor disk (string->number (hash-ref info 'MINOR)))
        (send disk notify)))


    ;; Remove runtime information from disk% instance.
    (define/public (unassign-disk-device info)
      (let* ((id   (substring (hash-ref info 'DM_UUID) 6))
             (disk (hash-ref disks id #f)))
        (when disk
          (set-field! name  disk #f)
          (set-field! size  disk #f)
          (set-field! major disk #f)
          (set-field! minor disk #f)
          (send disk notify)
          (unless (get-field pool disk)
            (hash-remove! disks id)
            ((current-notify) "disk" id #f)))))


    ;; Construct parent object.
    (super-new)))


(define (basename path)
  (path->string (file-name-from-path path)))


; vim:set ts=2 sw=2 et:
