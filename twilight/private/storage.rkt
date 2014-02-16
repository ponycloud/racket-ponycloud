#lang racket/base
;
; Storage Configuration Component
;

(require racket/contract
         racket/class
         racket/match
         racket/path
         racket/list
         racket/set
         srfi/26)

(require sysfs/block
         tasks)

(require "util.rkt"
         "common.rkt"
         "entity.rkt")

(provide storage-manager%
         storage-manager/c)


(define/contract host-disk%
                 entity/c
  (class entity%
    ;; Inherit the primary key, we are going to need it for `(get-key)`.
    (inherit-field pkey)

    ;; Inherit presence information, required for `(status)`.
    (inherit-field present?)

    ;; Detailed information about the block device.
    ;; These get filled when the disk block device appears.
    (field (blkdev-name #f)
           (blkdev-major #f)
           (blkdev-minor #f)
           (blkdev-size #f))

    ;; Filled when the disk is configured.
    ;; The `pool` field contains UUID of the assigned storage pool
    ;; (if any) and `order` can be used to sort disks within the pool.
    (field (storage-pool #f)
           (minimal-size +inf.0))


    ;; Return unique entity key.
    (define/override (get-key)
      (list "host_disk" pkey))


    ;; Extract current state from block device details.
    (define/augment (appear info)
      (set! blkdev-name  (basename (hash-ref info 'DEVNAME)))
      (set! blkdev-size  (get-block-device-size blkdev-name))
      (set! blkdev-major (string->number (hash-ref info 'MAJOR)))
      (set! blkdev-minor (string->number (hash-ref info 'MINOR))))


    ;; Clear current state information.
    (define/augment (disappear)
      (set!-many (blkdev-name blkdev-major blkdev-minor blkdev-size) #f))


    ;; Apply the desired configuration.
    (define/augment (configure info)
      (let-from-dict ((size storage_pool) info)
        (set! storage-pool storage_pool)
        (set! minimal-size (* size (expt 2 20)))))


    ;; Remove the desired configuraiton.
    (define/augment (deconfigure)
      (set! storage-pool #f)
      (set! minimal-size +inf.0))


    ;; Return #t if the disk is present and it's size is at least as
    ;; much as have been configured.
    (define/public (ready?)
      (and present? (>= blkdev-size minimal-size)))


    ;; Report back with the storage pool the disk is assigned to.
    (define/augment (status)
      (hasheq 'storage_pool (or storage-pool 'null)
              'disk (second pkey)
              'size (floor (/ blkdev-size (expt 2 20)))))


    ;; Construct parent object.
    (super-new)))


;; Generic storage pool is just a formality.
(define/contract storage-pool%
                 entity/c
  (class entity%
    (inherit-field pkey)

    (define/override (get-key)
      (list "storage_pool" pkey))

    (super-new)))


;; Special storage pool type we use for anything we don't understand.
(define/contract unsupported-pool%
                 entity/c
  (class storage-pool%
    (inherit-field pkey)

    (define/augment (status)
      (hasheq 'uuid pkey
              'status "unsupported"))

    (super-new)))


;; pLVM storage pool just groups some disks together.
(define/contract plvm-pool%
                 entity/c
  (class storage-pool%
    ;; Inherit the generic fields.
    (inherit-field pkey)

    ;; Disks for this pool, in no particular order.
    (field (disks (set)))


    ;; Allow receival of disk events.
    (define/override (interested? other)
      (is-a? other host-disk%))


    ;; Add the disk to the set if intended for this storage pool.
    (define/override (other-changed disk)
      (when (equal? (get-field storage-pool disk) pkey)
        (set! disks (set-add disks disk))))


    ;; Remove disk from the pool.
    (define/override (other-removed disk)
      (set! disks (set-remove disks disk)))


    ;; Base pool availability on availability of it's individual disks.
    (define/augment (status)
      (let ((ready? (andmap (cut send <> ready?) disks)))
        (hasheq 'uuid pkey
                'status (if ready? "ready" "pending"))))


    ;; Construct parent object.
    (super-new)))


(define storage-manager/c
  (and/c manager/c
         (class/c
           (init-field (twilight
                         (instanceof/c
                           (class/c
                             (field uuid))))))))


(define/contract storage-manager%
                 storage-manager/c
  (class manager%
    ;; Inherit mapping with managed entities.
    (inherit-field entities)

    ;; Reference to the main Twilight manager that spawned us.
    (init-field twilight)

    ;; Inherit parent methods for entity manipulation.
    (inherit configure deconfigure appear disappear)


    ;; Extract host-disk% primary key from udev info.
    (define/private (host-disk-udev-pkey info)
      (list (get-field uuid twilight)
            (regexp-replace #rx"^mpath-" (hash-ref info 'DM_UUID) "")))

    ;; Extract host-disk% primary key from configuration.
    (define/private (host-disk-conf-pkey info)
      (list (get-field uuid twilight)
            (hash-ref info 'id)))


    ;; Extract storage-pool% primary key from configuration.
    (define/private (storage-pool-conf-pkey info)
      (hash-ref info 'uuid))


    ;; Find an existing or create a new host-disk% instance.
    (define/private (find-host-disk pkey)
      (let ((key (list "host_disk" pkey)))
        (hash-ref entities key (λ _ (new host-disk% (pkey pkey))))))


    ;; Find an existing or create a new storage-pool% instance.
    (define/private (find-storage-pool pkey)
      (let ((key (list "storage_pool" pkey)))
        (hash-ref entities key (λ _ (make-storage-pool pkey)))))


    ;; Process block device create/update event from udev.
    (define/public (disk-blkdev-changed info)
      (using (find-host-disk (host-disk-udev-pkey info))
        (appear <it> info)))


    ;; Process block device removal event from udev.
    (define/public (disk-blkdev-removed info)
      (using (find-host-disk (host-disk-udev-pkey info))
        (disappear <it>)))


    ;; Configure host-disk using disk information.
    (define/public (disk-configure info)
      (using (find-host-disk (host-disk-conf-pkey info))
        (configure <it> info)))


    ;; Deconfigure host-disk on withdrawal of disk configuration.
    (define/public (disk-deconfigure info)
      (using (find-host-disk (host-disk-conf-pkey info))
        (deconfigure <it>)))


    ;; Configure storage-pool using supplied information.
    (define/public (pool-configure info)
      (using (find-storage-pool (storage-pool-conf-pkey info))
        (configure <it> info)))


    ;; Remove configuration of a storage pool.
    (define/public (pool-deconfigure info)
      (using (find-storage-pool (storage-pool-conf-pkey info))
        (deconfigure <it>)))


    ;; Construct parent object.
    (super-new)))


;; Return string basename of specified (string) path.
(define/contract (basename path)
                 (-> path-string? path-string?)
  (path->string (file-name-from-path path)))


;; Create storage-pool% pool using type and primary key.
(define/contract (make-storage-pool type pkey)
                 (-> string? string? (is-a?/c storage-pool%))
  (match type
    ("plvm" (new plvm-pool% (pkey pkey)))
    (else   (new unsupported-pool% (pkey pkey)))))


; vim:set ts=2 sw=2 et:
