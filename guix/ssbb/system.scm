(define-module (ssbb system)
  #:use-module (gnu)
  #:use-module (gnu system setuid)
  #:use-module (gnu system privilege)
  #:use-module (guix utils)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages video)
  #:use-module (nongnu packages firmware)
  #:use-module (nongnu system linux-initrd)
  #:use-module (ssbb services kanata)
  #:use-module (ssbb services bolt)
  #:use-module (ssbb services authentication)
  #:use-module (ssbb packages xorg)
  #:export (base-operating-system))

(use-service-modules cups desktop networking dns ssh xorg avahi dbus admin pm authentication file-sharing)
(use-package-modules shells cmake version-control fonts hardware video admin linux xorg rust-apps libusb nfs xdisorg freedesktop polkit)

(define (read-relative-file filename)
  (let ((path (string-append (current-source-directory) "/" filename)))
    (call-with-input-file path get-string-all)))

(define (base-operating-system gpu)
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
                  (supplementary-groups '("wheel" "netdev" "audio" "video" "realtime" "lp" "tty" "input" "transmission")))
                 %base-user-accounts))

   (groups (cons (user-group (system? #t) (name "realtime"))
                 %base-groups))

   (packages (append
              (list font-terminus
		                git
                    cmake
                    fish
                    xkeyboard-config
                    polkit
                    brillo
                    libva-utils
                    bolt
                    fwupd-nonfree
                    fprintd
                    powertop
                    tlp
                    xsecurelock
                    bluez)
              (match gpu
                ('intel (list intel-media-driver/nonfree))
                ('amd '()))
              %base-packages))

   (privileged-programs
    (cons*
     (privileged-program
      (program (file-append xsecurelock "/libexec/xsecurelock/authproto_pam"))
      (setuid? #t))
      %default-privileged-programs))

   (services
    (append
     (modify-services %base-services
                      ;; (delete login-service-type)
                      (delete mingetty-service-type)
                      (delete console-font-service-type))

     (list
      ;; (service console-font-service-type
      ;;          (map (lambda (tty)
      ;;                 (cons tty (file-append
      ;;                            font-terminus
      ;;                            "/share/consolefonts/ter-132b")))
      ;;               '("tty1" "tty2" "tty3")))

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
                (server (if (eq? gpu 'intel) xorg-server-tearfree xorg-server))
                (drivers (if (eq? gpu 'intel) '("modesetting") '()))
                (extra-config
                 (list (read-relative-file "./files/xorg/video.conf")
                       (read-relative-file "./files/xorg/touchpad.conf")))))

      (service bolt-service-type)

      (service tlp-service-type
               (tlp-configuration
                (cpu-energy-perf-policy-on-ac "balance_performance")
                (cpu-energy-perf-policy-on-bat "balance_power")))

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

      (service transmission-daemon-service-type
               (transmission-daemon-configuration
                (incomplete-dir-enabled? #t)
                (incomplete-dir "/var/lib/transmission-daemon/incomplete")
                (watch-dir-enabled? #t)
                (watch-dir "/var/lib/transmission-daemon/watch")

                (rpc-enabled? #t)
                (rpc-whitelist-enabled? #t)
                (rpc-whitelist '("127.0.0.1" "::1"))))

      (service kanata-service-type
               (local-file "files/kanata.conf")))))))
