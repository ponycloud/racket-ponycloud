#lang racket/base
;
; Storage Configuration Component
;

(require racket/contract
         racket/sequence
         racket/class
         racket/match
         racket/path
         racket/list
         racket/set
         srfi/26)

(require misc1/syntax
         misc1/dict
         sysfs/block
         tasks)

(require "common.rkt"
         "entity.rkt")

(provide storage-manager%
         storage-manager/c)


(define/contract disk%
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
      (list "disk" pkey))


    ;; Extract current state from block device details.
    (define/augment (appear info)
      (set! blkdev-name  (basename (hash-ref info 'DEVNAME)))
      (set! blkdev-size  (get-block-device-size blkdev-name))
      (set! blkdev-major (string->number (hash-ref info 'MAJOR)))
      (set! blkdev-minor (string->number (hash-ref info 'MINOR))))


    ;; Clear current state information.
    (define/augment (disappear)
      (set! blkdev-name #f)
      (set! blkdev-major #f)
      (set! blkdev-minor #f)
      (set! blkdev-size #f))


    ;; Apply the desired configuration.
    (define/augment (configure info)
      (let-dict (((size storage_pool) info))
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
      (hasheq 'id pkey
              'storage_pool (or storage-pool 'null)
              'size (and blkdev-size (floor (/ blkdev-size (expt 2 20))))))


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
      (is-a? other disk%))


    ;; Add the disk to the set if intended for this storage pool.
    (define/override (other-changed disk)
      (when (equal? (get-field storage-pool disk) pkey)
        (set! disks (set-add disks disk))))


    ;; Remove disk from the pool.
    (define/override (other-removed disk)
      (set! disks (set-remove disks disk)))


    ;; Base pool availability on availability of it's individual disks.
    (define/augment (status)
      (let ((ready? (sequence-andmap (cut send <> ready?) disks)))
        (hasheq 'uuid pkey
                'status (if ready? "ready" "pending"))))


    ;; Construct parent object.
    (super-new)))


(define/contract image%
                 entity/c
  (class entity%
    ;; Inherit the generic fields.
    (inherit-field pkey)

    ;; Volumes backing the image.
    (field (volumes (set)))

    ;; If defined, the remote address of the image.
    (field (source-uri #f))


    ;; Setup the image entity using the information from controller.
    (define/augment (configure info)
      (let-dict (((source_uri) info))
        (set! source-uri source_uri)))


    ;; Allow receival of volume events to be able to add backing volumes.
    (define/override (interested? other)
      (is-a? other volume%))


    ;; React to volume changes by collecting the ones that are backing us.
    (define/override (other-changed volume)
      (when (equal? (get-field image volume) pkey)
        (set! volumes (set-add volumes volume))))


    ;; React to volume removal by removing them from the set of the ones
    ;; that are backing the image.
    (define/override (other-removed volume)
      (set! volumes (set-remove volumes volume)))


    ;; Return unique entity key.
    (define/override (get-key)
      (list "image" pkey))


    ;; Construct parent object.
    (super-new)))


(define/contract extent%
                 entity/c
  (class entity%
    ;; Inherit the generic fields.
    (inherit-field pkey)

    ;; Detailed information about the extent.
    (field (start #f)
           (size #f)
           (volume #f)
           (disk #f)
           (order #f))


    ;; Populate the extent information from configuration hash.
    (define/augment (configure info)
      (let-dict (((range volume disk order) info))
        (set-field! start this (first range))
        (set-field! size this (- (second range) (first range)))
        (set-field! volume this volume)
        (set-field! disk this disk)
        (set-field! order this order)))


    ;; Reset the extent information.
    (define/augment (deconfigure)
      (set!-values (start size volume disk order)
        (values #f #f #f #f #f)))


    ;; Return unique entity key.
    (define/override (get-key)
      (list "extent" pkey))


    ;; Construct parent object.
    (super-new)))


(define/contract volume%
                 entity/c
  (class entity%
    ;; Inherit the generic fields.
    (inherit-field pkey)

    ;; The image this volume is backing, if any.
    (field (image #f))


    ;; Configure the volume using the dict from controller.
    (define/augment (configure info)
      (let-dict (((image) info))
        (set-field! image this image)))


    ;; Deconfigure the volume.
    (define/augment (deconfigure)
      (set! image #f))


    ;; Return unique entity key.
    (define/override (get-key)
      (list "volume" pkey))


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


    ;; Extract disk% primary key from udev info.
    (define/private (disk-udev-pkey info)
      (regexp-replace #rx"^mpath-" (hash-ref info 'DM_UUID) ""))


    ;; Find an existing or create a new disk% instance.
    (define/private (find-disk pkey)
      (let ((key (list "disk" pkey)))
        (hash-ref entities key (λ _ (new disk% (pkey pkey))))))


    ;; Find an existing or create a new storage-pool% instance.
    (define/private (find-storage-pool info)
      (let ((pkey (hash-ref info 'uuid))
            (type (hash-ref info 'type)))
        (let ((key (list "storage_pool" pkey)))
          (hash-ref entities key (λ _ (make-storage-pool type pkey))))))


    ;; Find an existing or create a new image% instance.
    (define/private (find-image info)
      (let* ((pkey (hash-ref info 'uuid))
             (key  (list "disk" pkey)))
        (hash-ref entities key (λ _ (new image% (pkey pkey))))))


    ;; Find an existing or create a new extent% instance.
    (define/private (find-extent info)
      (let* ((pkey (hash-ref info 'uuid))
             (key  (list "extent" pkey)))
        (hash-ref entities key (λ _ (new extent% (pkey pkey))))))


    ;; Find an existing or create a new volume% instance.
    (define/private (find-volume info)
      (let* ((pkey (hash-ref info 'uuid))
             (key  (list "volume" pkey)))
        (hash-ref entities key (λ _ (new volume% (pkey pkey))))))


    ;; Process block device create/update event from udev.
    (define/public (disk-blkdev-changed info)
      (using (disk (find-disk (disk-udev-pkey info)))
        (appear disk info)))


    ;; Process block device removal event from udev.
    (define/public (disk-blkdev-removed info)
      (using (disk (find-disk (disk-udev-pkey info)))
        (disappear disk)))


    ;; Configure disk% using desired state.
    (define/public (disk-configure info)
      (using (disk (find-disk (hash-ref info 'id)))
        (configure disk info)))


    ;; Deconfigure disk on withdrawal of configuration.
    (define/public (disk-deconfigure info)
      (using (disk (find-disk (hash-ref info 'id)))
        (deconfigure disk)))


    ;; Configure storage-pool using supplied information.
    (define/public (pool-configure info)
      (using (sp (find-storage-pool info))
        (configure sp info)))


    ;; Remove configuration of a storage pool.
    (define/public (pool-deconfigure info)
      (using (sp (find-storage-pool info))
        (deconfigure sp)))


    ;; Configure image using supplied information.
    (define/public (image-configure info)
      (using (image (find-image info))
        (configure image info)))


    ;; Remove configuration of an image.
    (define/public (image-deconfigure info)
      (using (image (find-image info))
        (deconfigure image)))


    ;; Configure extent using supplied information.
    (define/public (extent-configure info)
      (using (extent (find-extent info))
        (configure extent info)))


    ;; Remove configuration of an extent.
    (define/public (extent-deconfigure info)
      (using (extent (find-extent info))
        (deconfigure extent)))


    ;; Configure volume using supplied information.
    (define/public (volume-configure info)
      (using (volume (find-volume info))
        (configure volume info)))


    ;; Remove configuration of an volume.
    (define/public (volume-deconfigure info)
      (using (volume (find-volume info))
        (deconfigure volume)))


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
