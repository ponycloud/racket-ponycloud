#lang racket/base
;
; Declarative Entity Management
;

(require racket/class
         racket/contract)

(require misc1/syntax
         misc1/dict
         misc1/list
         williams/uuid1/uuid
         json)

(require "common.rkt")

(provide entity/c
         entity%
         manager/c
         manager%)


(define entity/c
  (class/c
    ;; Primary key of the entity, be it uuid or something else.
    ;; Can be a composite key with multiple items.
    (init-field (pkey pkey/c))

    ;; Set to #t when present and #f when merely configured.
    (init-field (present? boolean?))

    ;; Set to #t when configured and #f when merely present.
    (init-field (configured? boolean?))

    ;; Return `(type pkey)` list that uniquely identifies the entity.
    (get-key (->m (list/c string? pkey/c)))

    ;; Return #t for entities to pass to the `other-` methods below.
    (interested? (->m (instanceof/c (recursive-contract entity/c)) boolean?))

    ;; Other entity has potentially changed it's properties.
    (other-changed (->m (instanceof/c (recursive-contract entity/c)) void?))

    ;; Other entity is in the process of being removed.
    (other-removed (->m (instanceof/c (recursive-contract entity/c)) void?))

    ;; The entity have (dis-)appeared, regardless of it's configuration status.
    ;; Once processed, other entities will have a chance to react using
    ;; their `other-` handlers.
    (appear (->m any/c void?))
    (disappear (->m void?))

    ;; The entity is to be (de-)configured.
    ;; Again, other entities will have a chance to react afterwards.
    (configure (->m (hash/c symbol? any/c) void?))
    (deconfigure (->m void?))

    ;; Return hash table with entity status.
    ;; Keep this brief, it goes over network.
    (status (->m (hash/c symbol? jsexpr?)))

    ;; Call `notify-sparkle` with status of the entity.
    (notify (->m void?))))


;; Implements the contract above with placeholder methods.
(define/contract entity%
                 entity/c
  (class object%
    (init-field pkey
                (present? #f)
                (configured? #f))

    (abstract get-key)

    (define/public (interested? other)
      #f)

    (define/public (other-changed other)
      (void))

    (define/public (other-removed other)
      (void))

    (define/pubment (configure info)
      (inner (void) configure info)
      (set! configured? #t)
      (notify))

    (define/pubment (deconfigure)
      (when configured?
        (inner (void) deconfigure))
      (set! configured? #f)
      (notify))

    (define/pubment (appear info)
      (inner (void) appear info)
      (set! present? #t)
      (notify))

    (define/pubment (disappear)
      (when present?
        (inner (void) disappear))
      (set! present? #f)
      (notify))

    (define/pubment (status)
      (define base
        (if (list? pkey)
            (hasheq 'status (if present? "present" "missing"))
            (hasheq 'status (if present? "present" "missing")
                    (if (uuid-string? pkey) 'uuid 'id) pkey)))
      (dict-merge base (inner null status)))

    (define/public-final (notify)
      (let-list (((name pkey) (get-key)))
        (notify-sparkle name pkey (and (or present? configured?) (status)))))

    (super-new)))

;;
;; Entity Manager
;;
;; The manager takes care of a collection of entities that can be
;; either configured?, present? or both.  Once an entity gets both
;; unconfigured and missing, manager removes it from it's collection.
;;
;; On every entity change all other entities are offered a chance to react.
;;
(define manager/c
  (class/c
    (field (entities (hash/c (list/c string? string?)
                             (instanceof/c entity/c))))

    (configure (->m (instanceof/c entity/c) any/c void?))
    (deconfigure (->m (instanceof/c entity/c) void?))

    (appear (->m (instanceof/c entity/c) any/c void?))
    (disappear (->m (instanceof/c entity/c) void?))))


(define/contract manager%
                 manager/c
  (class object%
    ;; Manager starts with no assigned entities.
    (field (entities (make-hash)))

    ;; Notify others about a change.
    (define/private (notify-changed entity)
      (for (((key other) entities))
        (when (send other interested? entity)
          (send other other-changed entity))))

    ;; Notify others about a removal.
    (define/private (notify-removed entity)
      (for (((key other) entities))
        (when (send other interested? entity)
          (send other other-removed entity))))

    ;; Add entity (if not already present) and notify others.
    (define/private (maybe-add entity)
      (let ((key (send entity get-key)))
        (hash-set! entities key entity))
      (notify-changed entity))

    ;; Remove entity (if deconfigured and missing) and notify others.
    (define/private (maybe-remove entity)
      (let ((key (send entity get-key)))
        (when (hash-has-key? entities entity)
          (unless (or (get-field configured? entity)
                      (get-field present? entity))
            (hash-remove! entities key)
            (notify-removed entity)))))

    (define/public-final (configure entity info)
      (maybe-add
        (producing (it entity)
          (send it configure info))))

    (define/public-final (deconfigure entity)
      (maybe-remove
        (producing (it entity)
          (send it deconfigure))))

    (define/public-final (appear entity info)
      (maybe-add
        (producing (it entity)
          (send it appear info))))

    (define/public-final (disappear entity)
      (maybe-remove
        (producing (it entity)
          (send it disappear))))

    (super-new)))


; vim:set ts=2 sw=2 et:
