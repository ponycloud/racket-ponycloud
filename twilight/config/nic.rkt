#lang racket/base
;
; Network Interface Configuration
;

(require unstable/contract
         racket/contract
         racket/match)

(require misc1/match
         kernel/link
         libuuid)

(require twilight/util
         twilight/unit
         twilight/config/types)

(provide
  (contract-out
    (struct nic-config
      ((hwaddr hwaddr?)
       (defined? boolean?)
       (link-name (maybe/c string?))
       (bond-uuid (maybe/c uuid?))))))


(define-logger nic)


(struct nic-config
  (hwaddr defined? link-name bond-uuid)
  #:transparent
  #:methods gen:config
  ((define (config-type self)
     'nic)

   (define (config-pkey self)
     (nic-config-hwaddr self))

   (define (config-delete? self)
     (and (not (nic-config-defined? self))
          (not (nic-config-link-name self))))

   (define (config-change self a-change)
     (let ((hwaddr (nic-config-hwaddr self)))
       (match a-change
         ((change 'nic (equal hwaddr) prev #f)
          (struct-copy nic-config self (defined? #f) (bond-uuid #f)))

         ((change 'nic (equal hwaddr) #f next)
          (struct-copy nic-config self
                       (defined? #t)
                       (bond-uuid (hash-ref next 'bond))))

         ((change 'dev/net pkey prev #f)
          (let ((device-hwaddr (props-hwaddr prev)))
            (if (equal? hwaddr device-hwaddr)
                (struct-copy nic-config self (link-name #f))
                self)))

         ((change 'dev/net pkey _ next)
          (let ((device-hwaddr (props-hwaddr next)))
            (if (equal? hwaddr device-hwaddr)
                (struct-copy nic-config self
                             (link-name (hash-ref next 'interface)))
                self)))

         (else self))))

   (define (config-spawn-unit self others)
     (match self
       ((nic-config hwaddr _ #f _)
        (spawn-unit
          (hasheq 'status "missing"
                  'link-name 'null
                  'bond-name 'null)))

       ((nic-config hwaddr #f link-name #f)
        (spawn-unit
          (hasheq 'status "ready"
                  'link-name link-name
                  'bond-name 'null)))

       ((nic-config hwaddr #t link-name #f)
        (spawn-unit
          (let ((link-index (link-name->index link-name)))
            (when (member 'up (link-flags link-index))
              (log-nic-info "set link ~a (~a) down" link-name link-index)
              (unset-link-flags! link-index 'up))

            (when (link-bond link-index)
              (log-nic-info "un-enslave link ~a (~a)" link-name link-index)
              (set-link-bond! link-index #f))))

          (hasheq 'status "inactive"
                  'link-name link-name
                  'bond-name 'null))

       ((nic-config hwaddr #t link-name bond-uuid)
        (spawn-unit
          (with-unit-result ((bond (hash-ref others `(bond ,bond-uuid))))
            (let* ((bond-name  (hash-ref bond 'bond-name))
                   (bond-index (link-name->index bond-name))
                   (link-index (link-name->index link-name)))
              (unless (equal? (link-bond link-index) bond-index)
                (log-nic-info "enslave link ~a (~a) to ~a (~a)"
                              link-name link-index bond-name bond-index)
                (set-link-bond! link-index bond-index))

              (unless (member 'up (link-flags link-index))
                (log-nic-info "set link ~a (~a) up" link-name link-index)
                (set-link-flags! link-index 'up))

              (hasheq 'status "active"
                      'link-name link-name
                      'bond-name bond-name)))))))

   (define (config-report self result)
     (match-let (((nic-config hwaddr _ _ _) self))
       (match result
         ((exn:break _ _ _)
          (list))

         ((exn message _)
          (list (change 'nic hwaddr #f
                        (hasheq 'status "broken"))))

         (else
          (let ((result (hash-set result 'hwaddr hwaddr)))
            (list (change 'nic hwaddr #f result)))))))))


; vim:set ts=2 sw=2 et:
