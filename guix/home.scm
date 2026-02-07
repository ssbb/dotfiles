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
  #:use-module (ssbb packages fonts)
  #:use-module (ssbb packages fontutils))

(use-package-modules emacs-xyz tree-sitter xorg fonts gnome xdisorg freedesktop admin package-management video linux shellutils rust-apps glib shells gnupg password-utils ncurses web-browsers polkit engineering gtk emacs)

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
   (packages (list emacs
                   emacs-vterm
                   emacs-multi-vterm
                   emacs-exwm
                   emacs-pdf-tools
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
                   font-microsoft-web-core-fonts
                   font-google-noto
                   font-google-noto-emoji
                   font-google-noto-serif-cjk
                   font-google-noto-sans-cjk
                   font-google-roboto
                   font-liberation
                   font-awesome
                   ;; font-apple-sf-pro
                   ;; font-apple-new-york
                   ;; font-apple-symbols
                   ;; font-apple-color-emoji
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
                   qutebrowser
                   starship
                   kanata
                   htop
                   xset xprop xdotool
                   xss-lock xautolock
                   xsetroot
                   kicad kicad-symbols kicad-footprints kicad-packages3d kicad-templates
                   adwaita-icon-theme
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

      (simple-service 'additional-fonts-service
                      home-fontconfig-service-type
                      (list (font-alias "serif" '("Noto Serif" "Noto Color Emoji" "Noto Emoji"))
                            (font-alias "sans-serif" '("Noto Sans" "Noto Color Emoji" "Noto Emoji"))
                            (font-alias "sans" '("Noto Sans" "Noto Color Emoji" "Noto Emoji"))
                            (font-alias "monospace" '("Iosevka Curly" "Noto Mono" "Noto Color Emoji" "Noto Emoji"))
                            (font-alias "mono" '("Iosevka Curly" "Noto Mono" "Noto Color Emoji" "Noto Emoji"))

                            `(match (@ (target "font"))
                               ,(font-edit "embeddedbitmap" #f)
                               ,(font-edit "autohint" #f)
                               ,(font-edit "antialias" #t)
                               ,(font-edit "hinting" #t)
                               ,(font-edit "rgba" 'rgb)
                               ,(font-edit "hintstyle" 'hintslight)
                               ,(font-edit "lcdfilter" 'lcddefault))))

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
