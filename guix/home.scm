;; This is a sample Guix Home configuration which can help setup your
;; home directory in the same declarative manner as Guix System.
;; For more information, see the Home Configuration section of the manual.
(define-module (guix-home-config)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gnome)
  #:use-module (gnu services xorg)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages fontutils)
  #:use-module (nongnu packages mozilla)
  #:use-module (gnu packages package-management)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (gnu packages shellutils)

  ;; custom packages only
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system font)
  #:use-module ((guix licenses) #:prefix license:)
  )

(define-public font-nerd-symbols
  (package
   (name "font-nerd-symbols-only")
   (version "3.4.0")
   (source (origin
	          (method url-fetch)
	          (uri (string-append
		              "https://github.com/ryanoasis/nerd-fonts"
		              "/releases/download/v" version "/NerdFontsSymbolsOnly.tar.xz"))
	          (sha256
	           (base32
	            "0skirmz6rc0845960957b19kvlbfpg5k9gs6hq8agsmhlc6hk33z"))))
   (build-system font-build-system)
   (home-page "https://www.nerdfonts.com")
   (synopsis "Iosevka Nerd ")
   (license license:expat)
   (description "Nerd-Fonts is the iconic font aggregator, collection, and patcher. This is only the Iosevka font.")))

(define home-config
  (home-environment
   (packages (list emacs emacs-vterm emacs-multi-vterm xorg-server xauth xterm tree-sitter-elixir tree-sitter-heex font-iosevka-curly xrdb firefox binutils
                   font-google-noto font-google-noto-emoji font-liberation font-awesome colord-minimal xiccd font-nerd-symbols
                   fontconfig
                   tree
                   stow
                   starship
                    xrandr
                   ))

   (services
    (append
     (list
      (service home-fish-service-type
	             (home-fish-configuration
		            (config
		             (list (local-file "../files/fish/.config/fish/config.fish")))))

      (service home-files-service-type
	             `((".guile" ,%default-dotguile)
		             (".Xdefaults" ,%default-xdefaults)))

      (service home-xdg-configuration-files-service-type
	             `(("gdb/gdbinit" ,%default-gdbinit)
		             ("nano/nanorc" ,%default-nanorc)))

      (service home-dotfiles-service-type
	             (home-dotfiles-configuration
	              (directories '("../files"))
	              (layout 'stow)
	              (packages '("emacs" "x" "git"))))

      (service home-dbus-service-type)
      (service home-x11-service-type)
      (service home-startx-command-service-type
               (xorg-configuration))

      (service home-redshift-service-type
               (home-redshift-configuration
                (location-provider 'manual)
                (latitude 41.645895)
                (longitude 41.628414)
                (daytime-temperature 6500)
                (nighttime-temperature 4000)))

      (simple-service 'xiccd-service
                home-shepherd-service-type
                (list (shepherd-service
                       (provision '(xiccd))
                       (documentation "Run xiccd for colord integration")
                       (requirement '(x11-display dbus))
                       (modules '((srfi srfi-1)
                                  (srfi srfi-26)))
                       (start #~(lambda _
                                  (fork+exec-command
                                   (list #$(file-append xiccd "/bin/xiccd"))
                                   #:environment-variables
                                   (cons (string-append "DISPLAY=" (getenv "DISPLAY"))
                                         (remove (cut string-prefix? "DISPLAY=" <>)
                                                 (default-environment-variables))))))
                       (stop #~(make-kill-destructor)))))


)
     %base-home-services))))

home-config
