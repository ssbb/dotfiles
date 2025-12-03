;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.

;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu)
             (nongnu packages linux)
             (nongnu packages video)
             (nongnu system linux-initrd))

(use-service-modules cups desktop networking dns ssh xorg avahi dbus admin pm)
(use-package-modules shells cmake version-control fonts hardware video admin linux)

(operating-system
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))

 (locale "en_US.utf8")
 (timezone "Asia/Tbilisi")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "thinkbb")

 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets (list "/boot/efi"))
              (keyboard-layout keyboard-layout)
              (extra-initrd "/keyfile.cpio")))

 (mapped-devices (list (mapped-device
                        (source (uuid "4a875275-dfd6-4cf9-8491-9d0147e5dc80"))
                        (target "cryptroot")
                        (type luks-device-mapping)
                        (arguments '(#:key-file "/keyfile.bin")))))

 ;; The list of file systems that get "mounted".  The unique
 ;; file system identifiers there ("UUIDs") can be obtained
 ;; by running 'blkid' in a terminal.
 (file-systems (cons* (file-system
                       (mount-point "/boot/efi")
                       (device (uuid "9D5C-DCC7" 'fat32))
                       (type "vfat"))
                      (file-system
                       (mount-point "/")
                       (device "/dev/mapper/cryptroot")
                       (type "ext4")
                       (dependencies mapped-devices)) %base-file-systems))

 ;; The list of user accounts ('root' is implicit).
 (users (cons* (user-account
                (name "ssbb")
                (comment "Sviatoslav")
                (group "users")
                (home-directory "/home/ssbb")
                (shell (file-append fish "/bin/fish"))
                (supplementary-groups '("wheel" "netdev" "audio" "video" "realtime" "lp" "tty")))
               %base-user-accounts))

 (groups (cons (user-group (system? #t) (name "realtime"))
               %base-groups))

 (packages (cons* font-terminus
		              git
                  cmake
                  brillo
                  intel-media-driver/nonfree
                  libva-utils
                  igt-gpu-tools
                  bolt
                  fish
                  %base-packages))

 ;; Below is the list of system services.  To search for available
 ;; services, run 'guix system search KEYWORD' in a terminal.
 (services
  (append
   (modify-services %base-services
                    (delete login-service-type)
                    ;; (delete agetty-service-type)
                    (delete mingetty-service-type)
                    (delete console-font-service-type))

   (list
    ;; (service seatd-service-type)
    (service elogind-service-type)

    (service console-font-service-type
             (map (lambda (tty)
                    (cons tty (file-append
                               font-terminus
                               "/share/consolefonts/ter-132b")))
                  '("tty1" "tty2" "tty3")))

    (service greetd-service-type
             (greetd-configuration
              (greeter-supplementary-groups (list "video" "input"))
              (terminals
               (list
                (greetd-terminal-configuration
                 (terminal-vt "1")
                 (terminal-switch #t)
                 (default-session-command
                   (greetd-agreety-session
                    (command
                     (greetd-user-session
                      (command (file-append fish "/bin/fish")))))))
                (greetd-terminal-configuration (terminal-vt "2"))
                (greetd-terminal-configuration (terminal-vt "3"))))))

    (simple-service 'add-nonguix-substitutes
                    guix-service-type
                    (guix-extension
                     (substitute-urls
                      (cons* "https://substitutes.nonguix.org"
                             %default-substitute-urls))
                     (authorized-keys
                      (append (list (local-file "./signing-key.pub"))
                              %default-authorized-guix-keys))))

    polkit-wheel-service

    (service openssh-service-type)

    (service network-manager-service-type)
    (service wpa-supplicant-service-type)
    (service modem-manager-service-type)
    (service bluetooth-service-type
             (bluetooth-configuration
              (auto-enable? #t)))
    (service usb-modeswitch-service-type)

    (service avahi-service-type)
    (service udisks-service-type)
    (service upower-service-type)
    ;; (service accountsservice-service-type)
    (service vte-integration-service-type)
    (service colord-service-type)
    (service geoclue-service-type)
    fontconfig-file-system-service
    (service thermald-service-type)
    (service tlp-service-type
             (tlp-configuration
              (cpu-boost-on-ac? #t)
              (wifi-pwr-on-bat? #t)))

    (service polkit-service-type)
    (service dbus-root-service-type)

      (udev-rules-service 'brillo brillo)

    (service x11-socket-directory-service-type)

    (service ntp-service-type)))))
