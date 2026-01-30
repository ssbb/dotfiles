(define-module (ssbb packages fonts)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system font)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (font-nerd-symbols))

(define font-nerd-symbols
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
   (synopsis "Nerd Fonts Symbols")
   (license license:expat)
   (description "Nerd-Fonts is the iconic font aggregator, collection, and patcher. This is only the Symbols.")))
