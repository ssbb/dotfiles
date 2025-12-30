(define-module (ssbb packages pm)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public throttled
  (package
   (name "throttled")
   (version "0.11")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/erpalma/throttled")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0acv5qsci4wsj29halmq1nxrr6l0lgly68f73ngz71pr9872sygv"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
                     (delete 'build)
                     (replace 'install
                              (lambda* (#:key inputs outputs #:allow-other-keys)
                                       (let* ((out (assoc-ref outputs "out"))
                                              (bin (string-append out "/bin"))
                                              (glib (assoc-ref inputs "glib"))
                                              (pciutils (assoc-ref inputs "pciutils"))
                                              (sitedir (site-packages inputs outputs))
                                              (python-sitedir
                                               (string-append out "/lib/python"
                                                              (python-version (assoc-ref inputs "python"))
                                                              "/site-packages")))
                                         (mkdir-p bin)
                                         (mkdir-p python-sitedir)
                                         (copy-file "throttled.py" (string-append bin "/throttled"))
                                         (chmod (string-append bin "/throttled") #o755)
                                         (copy-file "mmio.py" (string-append python-sitedir "/mmio.py"))
                                         (wrap-program (string-append bin "/throttled")
                                                       `("PATH" ":" prefix
                                                         (,(string-append pciutils "/sbin")))
                                                       `("GI_TYPELIB_PATH" ":" prefix
                                                         (,(string-append glib "/lib/girepository-1.0")))
                                                       `("GUIX_PYTHONPATH" ":" suffix
                                                         ,(list sitedir python-sitedir)))))))))
   (inputs
    (list glib
          gobject-introspection
          pciutils
          python-dbus
          python-pygobject))
   (home-page "https://github.com/erpalma/throttled")
   (synopsis "Workaround for Intel CPU throttling issues on Linux")
   (description
    "Throttled fixes CPU throttling issues on Lenovo ThinkPads and other
laptops by overriding power limits in MSR and MCHBAR registers.  It also
supports undervolting on older Intel CPUs (pre-10th gen).")
   (license license:expat)))
