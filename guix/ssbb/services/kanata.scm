(define-module (ssbb services kanata)
  #:use-module (gnu)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages rust-apps)
  #:export (kanata-service-type))

(define (kanata-shepherd-service config-file)
  (shepherd-service
   (provision '(kanata))
   (requirement '(udev))  ; ensure input devices are ready
   (documentation "Run kanata keyboard remapper")
   (start #~(make-forkexec-constructor
             (list #$(file-append kanata "/bin/kanata")
                   "--cfg" #$config-file)
             #:user "kanata"
             #:group "kanata"
             #:log-file "/var/log/kanata.log"))
   (stop #~(make-kill-destructor))))

(define kanata-service-type
  (service-type
   (name 'kanata)
   (description "Kanata keyboard remapper")
   (extensions
    (list
     (service-extension shepherd-root-service-type
                        (lambda (config-file)
                          (list (kanata-shepherd-service config-file))))
     (service-extension account-service-type
                        (const (list (user-group (name "kanata") (system? #t))
                                     (user-account
                                      (name "kanata")
                                      (group "kanata")
                                      (system? #t)
                                      (home-directory "/var/empty")
                                      (shell (file-append shadow "/sbin/nologin"))))))
     (service-extension udev-service-type
                        (const (list (udev-rule
                                      "90-kanata.rules"
                                      "KERNEL==\"uinput\", MODE=\"0660\", GROUP=\"kanata\"\nSUBSYSTEM==\"input\", MODE=\"0640\", GROUP=\"kanata\""))))))
   (default-value #f)))
