#lang racket/base
;
; Network Interface Configuration
;

(require unstable/contract
         racket/contract
         racket/match)

(require libuuid
         kernel/link)

(require twilight/util
         twilight/unit
         twilight/config/types)

(provide
  (contract-out
    (struct nic-config
      ((hwaddr hwaddr?)
       (link-name string?)
       (bond-uuid (maybe/c uuid?))))))


(struct nic-config
  (hwaddr link-name bond-uuid)
  #:transparent
  #:methods gen:config
  ((define (config-type self)
     'nic)

   (define (config-pkey self)
     (nic-config-hwaddr self))

   (define (config-delete? self)
     (and (not (nic-config-bond-uuid self))
          (not (nic-config-link-name self))))

   (define (config-change self a-change)
     (if (change-next a-change)
         (let ((bond-uuid (change-ref/next a-change 'bond)))
           (struct-copy nic-config self (bond-uuid bond-uuid)))
         (struct-copy nic-config self (bond-uuid #f))))

   (define (config-spawn-unit self others)
     (match self
       ((nic-config _ #f _)
        (spawn-unit
          (hasheq 'status "missing"
                  'link-name 'null
                  'bond-name 'null)))

       ((nic-config hwaddr link-name #f)
        (spawn-unit
          (let ((link-index (link-name->index link-name)))
            (unset-link-flags! link-index 'up)
            (set-link-bond! link-index #f))

          (hasheq 'status "present"
                  'link-name link-name
                  'bond-name 'null)))

       ((nic-config hwaddr link-name bond-uuid)
        (spawn-unit
          (with-unit-result ((bond (hash-ref others `(bond ,bond-uuid))))
            (let* ((bond-name  (hash-ref bond 'bond-name))
                   (bond-index (link-name->index bond-name))
                   (link-index (link-name->index link-name)))
              (set-link-bond! link-index bond-index)
              (set-link-flags! link-index 'up)

              (hasheq 'status "running"
                      'link-name link-name
                      'bond-name bond-name)))))))))


; vim:set ts=2 sw=2 et:
