(define-module (ssbb packages emacs)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages xorg)
  :export (emacs-lucid-xinput2))

(define emacs-lucid-xinput2
  (package/inherit emacs-lucid
                   (name "emacs-lucid-xinput2")
                   (inputs (modify-inputs (package-inputs emacs-lucid)
                                          (prepend libxi)))
                   (arguments
                    (substitute-keyword-arguments
                     (package-arguments emacs-lucid)
                     ((#:configure-flags flags #~'())
                      #~(cons "--with-xinput2"
                              #$flags))))))
