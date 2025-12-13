(define-module (ssbb services authentication)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system pam)
  #:use-module (gnu packages freedesktop)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (fprintd-pam-service-type
            fprintd-pam-configuration
            fprintd-pam-configuration?
            fprintd-pam-configuration-services))

(define-record-type* <fprintd-pam-configuration>
  fprintd-pam-configuration make-fprintd-pam-configuration
  fprintd-pam-configuration?
  (services fprintd-pam-configuration-services
            (default '("sudo" "login" "su" "greetd"))))

(define (fprintd-pam-extension config)
  (let ((pam-services (fprintd-pam-configuration-services config)))
    (pam-extension
     (transformer
      (lambda (pam)
        (if (member (pam-service-name pam) pam-services)
            (pam-service
             (inherit pam)
             (auth (cons (pam-entry
                          (control "sufficient")
                          (module (file-append fprintd
                                               "/lib/security/pam_fprintd.so")))
                         (pam-service-auth pam))))
            pam))))))

(define fprintd-pam-service-type
  (service-type
   (name 'fprintd-pam)
   (description "Add fingerprint authentication to PAM services.")
   (extensions
    (list (service-extension pam-root-service-type
                             (lambda (config)
                               (list (fprintd-pam-extension config))))))
   (default-value (fprintd-pam-configuration))))
