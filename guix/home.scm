(define-module (home)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services mpv)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services fontutils)
  #:use-module (gnu home services gnupg)
  #:use-module (nongnu packages fonts)
  #:use-module (ssbb home services dotfiles)
  #:use-module (ssbb packages emacs)
  #:use-module (ssbb packages fonts)
  #:use-module (ssbb packages fontutils))

(use-package-modules emacs-xyz tree-sitter xorg fonts gnome xdisorg freedesktop admin package-management video linux chromium shellutils rust-apps glib shells gnupg password-utils ncurses web-browsers polkit engineering gtk)

(define my-dotfiles-config
 	(home-dotfiles-configuration
	 (directories '("../stow"))
	 (layout 'stow)
	 (packages '("emacs" "x" "vc" "browser" "redshift" "qutebrowser"))))

(define (font-alias family prefer-list)
  `(alias
    (family ,family)
    (prefer ,@(map (lambda (f) `(family ,f)) prefer-list))))

(define (font-edit name value)
  (let ((val-elem (cond
                   ((boolean? value) `(bool ,(if value "true" "false")))
                   ((symbol? value) `(const ,(symbol->string value)))
                   (else `(const ,value)))))
    `(edit (@ (name ,name) (mode "assign"))
           ,val-elem)))

(define home-config
  (home-environment
   (packages (list emacs-lucid-xinput2
                   emacs-vterm
                   emacs-multi-vterm
                   emacs-exwm
                   xauth ;; used by emacs-xelb to get auth info
                   gnupg
                   polkit-gnome
                   ;; redshift home service is a bit annoying that it can't recover on X restart
                   redshift
                   tree-sitter-elixir
                   tree-sitter-heex
                   fontconfig-minimal-custom
                   font-iosevka-curly
                   font-nerd-symbols
                   font-google-noto
                   font-google-noto-emoji
                   font-liberation
                   font-awesome
                   font-apple-sf-pro
                   font-apple-new-york
                   font-apple-symbols
                   font-apple-color-emoji
                   ncurses ;; vterm shell integration uses tput from ncurses
                   xrandr
                   xrdb
                   xdg-user-dirs
                   colord-minimal
                   xiccd
                   gsettings-desktop-schemas
                   xdg-desktop-portal
                   xdg-desktop-portal-gtk
                   ;; gtk schemas does not appear in compiled schema file without including gtk excplicitly here
                   ;; probably there is better way to do it though?
                   gtk+
                   (list glib "bin")
                   binutils
                   ripgrep
                   mpv
                   password-store
                   pass-otp
                   tree
                   stow
                   libva
                   wireplumber
                   v4l-utils
                   ungoogled-chromium
                   qutebrowser
                   starship
                   kanata
                   htop

                   kicad kicad-symbols kicad-footprints kicad-packages3d kicad-templates

                   dbus))

   (services
    (append
     (list
      (service home-fish-service-type
	             (home-fish-configuration
		            (config
		             (list (local-file "../stow/shell/.config/fish/config.fish")))))

      (service home-files-service-type
	             `((".guile" ,%default-dotguile)
		             (".Xdefaults" ,%default-xdefaults)))

      (simple-service 'profile-env-vars-service
                      home-environment-variables-service-type
                      '(("PATH" . "$HOME/.local/bin:$PATH")
                        ("FREETYPE_PROPERTIES" . "truetype:interpreter-version=40")
                        ;; ("FREETYPE_PROPERTIES" . "truetype:interpreter-version=40 cff:no-stem-darkening=0 autofitter:no-stem-darkening=0")
                        ("XDG_SESSION_TYPE" . "x11")
                        ("XDG_CURRENT_DESKTOP" . "exwm")
                        ("XDG_SESSION_DESKTOP" . "exwm")))

      (service home-xdg-configuration-files-service-type
	             `(("gdb/gdbinit" ,%default-gdbinit)
		             ("nano/nanorc" ,%default-nanorc)))

      (service home-dotfiles-service-type my-dotfiles-config)
      (service dotfiles-manifest-service-type my-dotfiles-config)

      (service home-dbus-service-type)
      (service home-x11-service-type)

      ;; (service home-redshift-service-type
      ;;          (home-redshift-configuration
      ;;           (location-provider 'manual)
      ;;           (latitude 41.645895)
      ;;           (longitude 41.628414)
      ;;           (daytime-temperature 6500)
      ;;           (nighttime-temperature 4000)))

      (simple-service 'additional-fonts-service
                      home-fontconfig-service-type
                      (list (font-alias "serif" '("New York" "Apple Color Emoji"))
                            (font-alias "ui-serif" '("New York"))
                            (font-alias "sans-serif" '("SF Pro Text" "Apple Color Emoji"))
                            (font-alias "ui-sans-serif" '("SF Pro Display"))
                            (font-alias "monospace" '("Iosevka Curly"))
                            (font-alias "ui-monospace" '("Iosevka Curly"))
                            (font-alias "-apple-system" '("SF Pro Text"))
                            (font-alias "system-ui" '("SF Pro Text"))
                            (font-alias "emoji" '("Apple Color Emoji"))

                            `(match (@ (target "font"))
                               ,(font-edit "embeddedbitmap" #f)
                               ,(font-edit "autohint" #f)
                               ,(font-edit "antialias" #t)
                               ,(font-edit "hinting" #t)
                               ,(font-edit "rgba" 'rgb)
                               ,(font-edit "hintstyle" 'hintslight)
                               ,(font-edit "lcdfilter" 'lcddefault))))

      ;; (simple-service 'xiccd-service
      ;;                 home-shepherd-service-type
      ;;                 (list (shepherd-service
      ;;                        (provision '(xiccd))
      ;;                        (documentation "Run xiccd for colord integration")
      ;;                        (requirement '(x11-display dbus))
      ;;                        (modules '((srfi srfi-1)
      ;;                                   (srfi srfi-26)))
      ;;                        (start #~(lambda _
      ;;                                   (fork+exec-command
      ;;                                    (list #$(file-append xiccd "/bin/xiccd"))
      ;;                                    #:environment-variables
      ;;                                    (cons (string-append "DISPLAY=" (getenv "DISPLAY"))
      ;;                                          (remove (cut string-prefix? "DISPLAY=" <>)
      ;;                                                  (default-environment-variables))))))
      ;;                        (stop #~(make-kill-destructor)))))

      (simple-service 'transmission-symlinks
                      home-activation-service-type
                      #~(begin
                          (let ((target (string-append (getenv "HOME") "/torrents"))
                                (source "/var/lib/transmission-daemon/downloads"))
                            (unless (false-if-exception (lstat target))
                              (symlink source target)))))

      (service home-gpg-agent-service-type
               (home-gpg-agent-configuration
                (pinentry-program (file-append pinentry-emacs "/bin/pinentry-emacs"))
                (ssh-support? #t)))

      (service home-mpv-service-type
               (make-home-mpv-configuration
                #:global (make-mpv-profile-configuration
                          #:fullscreen? #t
                          #:hwdec '("auto")
                          #:sub-font "Iosevka Curly"
                          #:osd-font "Iosevka Curly"
                          #:alang '("rus" "eng"))))

      (service home-pipewire-service-type))

     %base-home-services))))

home-config
