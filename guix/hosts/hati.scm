(define-module (hosts hati)
  #:use-module (ssbb system)
  #:use-module (gnu)
  #:use-module (guix utils)
  #:use-module (gnu services pm)
  #:use-module (gnu services xorg))

(operating-system
 (inherit base-operating-system)
 (host-name "hati")

 (mapped-devices (list (mapped-device
                        (source (uuid ""))
                        (target "cryptroot")
                        (type luks-device-mapping)
                        (arguments '(#:key-file "/keyfile.bin")))))

 (file-systems (cons* (file-system
                       (mount-point "/boot/efi")
                       (device (uuid "" 'fat32))
                       (type "vfat"))
                      (file-system
                       (mount-point "/")
                       (device "/dev/mapper/cryptroot")
                       (type "ext4")
                       (dependencies mapped-devices)) %base-file-systems)))
