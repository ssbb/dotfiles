(define-module (ssbb system)
  #:use-module (gnu)
  #:use-module (gnu system setuid)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages video)
  #:use-module (nongnu packages firmware)
  #:use-module (nongnu system linux-initrd)
  #:use-module (ssbb services kanata)
  #:use-module (ssbb services bolt)
  #:use-module (ssbb services authentication)
  #:use-module (ssbb packages xorg)
  #:export (base-operating-system))

(use-service-modules cups desktop networking dns ssh xorg avahi dbus admin pm authentication)
(use-package-modules shells cmake version-control fonts hardware video admin linux xorg rust-apps libusb nfs xdisorg freedesktop polkit)

(define base-operating-system
  (operating-system
   (kernel linux)
   (initrd microcode-initrd)
   (firmware (list linux-firmware sof-firmware))

   (host-name "placeholder")
   (locale "en_US.utf8")
   (timezone "Asia/Tbilisi")
   (keyboard-layout (keyboard-layout "us"))
   (file-systems (list))

   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)
                (extra-initrd "/keyfile.cpio")))

   (users (cons* (user-account
                  (name "ssbb")
                  (comment "Sviatoslav")
                  (group "users")
                  (home-directory "/home/ssbb")
                  (shell (file-append fish "/bin/fish"))
                  (supplementary-groups '("wheel" "netdev" "audio" "video" "realtime" "lp" "tty" "input")))
                 %base-user-accounts))

   (groups (cons (user-group (system? #t) (name "realtime"))
                 %base-groups))

   (packages (cons* font-terminus
		                git
                    cmake
                    fish
                    xkeyboard-config
                    polkit
                    brillo
                    libva-utils
                    bolt
                    intel-media-driver/nonfree
                    fwupd-nonfree
                    fprintd
                    powertop
                    igt-gpu-tools
                    tlp
                    bluez
                    %base-packages))

   (services
    (append
     (modify-services %base-services
                      (delete login-service-type)
                      (delete mingetty-service-type)
                      (delete console-font-service-type))

     (list
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
                        (append (list (local-file "files/nonguix-signing-key.pub"))
                                %default-authorized-guix-keys))))

      ;; Add udev rules for MTP devices so that non-root users can access them.
      (simple-service 'mtp udev-service-type (list libmtp))

      ;; Add udev rules for MTP devices so that non-root users can access them.
      polkit-wheel-service

      ;; Allow desktop users to also mount NTFS and NFS file systems
      ;; without root.
      (simple-service 'mount-setuid-helpers privileged-program-service-type
                      (map file-like->setuid-program
                           (list (file-append nfs-utils "/sbin/mount.nfs")
                                 (file-append ntfs-3g "/sbin/mount.ntfs-3g"))))

      ;; Provides a nicer experience for VTE-using terminal emulators such
      ;; as GNOME Console, Xfce Terminal, etc.
      (service vte-integration-service-type)

      ;; The global fontconfig cache directory can sometimes contain
      ;; stale entries, possibly referencing fonts that have been GC'd,
      ;; so mount it read-only.
      fontconfig-file-system-service

      ;; Network
      (service network-manager-service-type)
      (service wpa-supplicant-service-type)
      (service modem-manager-service-type)
      (service bluetooth-service-type
               (bluetooth-configuration
                (auto-enable? #t)))
      (service usb-modeswitch-service-type)

      ;; D-Bus
      (service avahi-service-type)
      (service udisks-service-type)
      (service upower-service-type)
      (service accountsservice-service-type)
      (service colord-service-type)
      (service geoclue-service-type)
      (service polkit-service-type)
      (service elogind-service-type)
      (service dbus-root-service-type)

      (service openssh-service-type)
      (service ntp-service-type)

      (service x11-socket-directory-service-type)
      (service startx-command-service-type
               (xorg-configuration
                (server xorg-server-tearfree)
                (drivers '("modesetting"))
                (extra-config (list "
Section \"OutputClass\"
    Identifier  \"Intel Graphics\"
    MatchDriver \"i915\"
    Driver      \"modesetting\"
    Option      \"TearFree\" \"true\"
EndSection
"))))

      (service screen-locker-service-type
               (screen-locker-configuration
                (name "xlock")
                (program (file-append xlockmore "/bin/xlock"))))

      (service bolt-service-type)
      (service thermald-service-type)
      (service tlp-service-type)

      (service fprintd-service-type)
      (service fprintd-pam-service-type)

      (simple-service 'fwupd-polkit
                      polkit-service-type
                      (list fwupd-nonfree))

      (simple-service 'fwupd-dbus
                      dbus-root-service-type
                      (list fwupd-nonfree))

      (udev-rules-service 'pipewire-add-udev-rules pipewire)
      (udev-rules-service 'brillo-add-udev-rules brillo)

      (service kanata-service-type
               (local-file "/home/ssbb/kanata.conf")))))))
