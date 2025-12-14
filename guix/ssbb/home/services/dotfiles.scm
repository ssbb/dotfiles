(define-module (ssbb home services dotfiles)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:export (dotfiles-manifest-service-type))

(define (dotfiles-config->manifest-list config)
  "Extracts a list of (target . source) pairs from the config."
  (let ((files (home-dotfiles-configuration->files config)))
    (map (lambda (entry)
           (match entry
             ((target-rel file-obj)
              ;; cons pair: ("path/to/target" . "/abs/path/to/source")
              (cons target-rel (local-file-file file-obj)))))
         files)))

(define dotfiles-manifest-service-type
  (service-type
   (name 'dotfiles-manifest)
   (extensions
    (list (service-extension
           home-files-service-type
           (lambda (config)
             (let ((manifest (dotfiles-config->manifest-list config)))
               (list
                `(".dotfiles-manifest"
                  ,(plain-file "dotfiles-manifest"
                               (object->string manifest)))))))))
   (default-value (home-dotfiles-configuration))
   (description "Generates ~/.dotfiles-manifest mapping targets to source files.")))
