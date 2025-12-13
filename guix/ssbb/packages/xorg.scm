(define-module (ssbb packages xorg)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages fontutils)
  #:export (xorg-server-tearfree))

(define-public xorg-server-tearfree
  (package
   (inherit xorg-server)
   (name "xorg-server-tearfree")
   (build-system meson-build-system)
   (source
    (origin
     (inherit (package-source xorg-server))
     (patches
      (list (origin
             (method url-fetch)
             (uri "https://www.linuxfromscratch.org/patches/blfs/12.4/xorg-server-21.1.18-tearfree_backport-1.patch")
             (sha256
              (base32 "11683ic4sas4g6cjnvkg9z8avvnkjddciqwmw309fvchzb10cd6z")))))))
   (arguments
    (list
     #:tests? #f
     #:configure-flags
     #~(list
        (string-append "-Dxkb_dir=" #$(this-package-input "xkeyboard-config") "/share/X11/xkb")
        "-Dxkb_output_dir=/tmp"
        (string-append "-Dxkb_bin_dir=" #$(this-package-input "xkbcomp") "/bin")
        "-Ddefault_font_path="
        "-Dxcsecurity=true"
        "-Dbuilder_string=GNU"
        "-Dvendor_name=Guix"
        "-Dxephyr=true"
        "-Dxvfb=true"
        "-Dglamor=true"
        "-Dsecure-rpc=false")))
   (native-inputs
    (modify-inputs (package-native-inputs xorg-server)
                   (prepend meson ninja font-util)))))
