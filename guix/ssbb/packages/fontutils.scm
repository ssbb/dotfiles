(define-module (ssbb packages fontutils)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages fontutils)
  #:export (fontconfig-minimal-custom))

(define fontconfig-minimal-custom
  (package
   (inherit fontconfig)
   (name "fontconfig-minimal-custom")
   (arguments
    (substitute-keyword-arguments
     (package-arguments fontconfig)
     ((#:phases phases)
      `(modify-phases ,phases
                      (add-after 'install 'remove-font-preference-configs
                                 (lambda* (#:key outputs #:allow-other-keys)
                                          (let ((conf-d (string-append (assoc-ref outputs "out")
                                                                       "/etc/fonts/conf.d")))
                                            (for-each (lambda (f)
                                                        (false-if-exception (delete-file (string-append conf-d "/" f))))
                                                      '("40-sansserif.conf"
                                                        "60-latin.conf"
                                                        "60-generic.conf"
                                                        "65-fonts-persian.conf"
                                                        "65-nonlatin.conf"
                                                        "69-unifont.conf"
                                                        "80-delicious.conf"
                                                        "90-synthetic.conf")))))))))))
