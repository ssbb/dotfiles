(define-module (ssbb services bolt)
  #:use-module (gnu)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages linux)
  #:use-module (gnu services dbus)
  #:export (bolt-service-type))

(define %bolt-activation
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/lib/boltd")))

(define bolt-shepherd-service
  (list
   (shepherd-service
    (provision '(boltd))
    (requirement '(dbus-system udev))
    (documentation "Run the Thunderbolt device manager.")
    (start #~(make-forkexec-constructor
              (list #$(file-append bolt "/libexec/boltd"))))
    (stop #~(make-kill-destructor)))))

(define bolt-service-type
  (service-type
   (name 'bolt)
   (extensions (list
                (service-extension shepherd-root-service-type
                                   (const bolt-shepherd-service))
                (service-extension dbus-root-service-type
                                   (const (list bolt)))
                (service-extension polkit-service-type
                                   (const (list bolt)))
                (service-extension profile-service-type
                                   (const (list bolt)))
                (service-extension activation-service-type
                                   (const %bolt-activation))))
   (default-value #f)
   (description "Run boltd, the Thunderbolt 3 device manager.")))
