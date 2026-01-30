(define-module (hosts surtr)
  #:use-module (ssbb system)
  #:use-module (ssbb services pm)
  #:use-module (ssbb packages pm)
  #:use-module (gnu)
  #:use-module (guix utils)
  #:use-module (gnu services pm)
  #:use-module (gnu services xorg))

(operating-system
 (inherit (base-operating-system 'intel))
 (kernel-arguments
  (append
   '("xe.force_probe=!a7a0"
     "i915.force_probe=a7a0"
     "modprobe.blacklist=mtk_t7xx")
   %default-kernel-arguments))


 (host-name "surtr")

 (mapped-devices (list (mapped-device
                        (source (uuid "4a875275-dfd6-4cf9-8491-9d0147e5dc80"))
                        (target "cryptroot")
                        (type luks-device-mapping)
                        (arguments '(#:key-file "/keyfile.bin")))))

 (file-systems (cons* (file-system
                       (mount-point "/boot/efi")
                       (device (uuid "9D5C-DCC7" 'fat32))
                       (type "vfat"))
                      (file-system
                       (mount-point "/")
                       (device "/dev/mapper/cryptroot")
                       (type "ext4")
                       (dependencies mapped-devices)) %base-file-systems))
 (packages (cons* throttled
                  (operating-system-packages base-operating-system)))

 (services
  (append
   (modify-services
    (operating-system-user-services base-operating-system)
    (tlp-service-type config =>
                      (tlp-configuration
                       (inherit config)
                       (runtime-pm-blacklist (list "08:00.0")))))
   (list
    (service throttled-service-type
             (throttled-configuration
              (ac-pl1-tdp-w 28)
              (ac-pl2-tdp-w 28)))))))
