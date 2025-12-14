;;; init.el --- ssbb's personal configuration -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:
;;;

;;; Code:

(defconst ssbb/full-name "Sviatoslav Bulbakha")
(defconst ssbb/email "mail@ssbb.me")

(defconst ssbb/enable-modal-editing nil)
(defconst ssbb/enable-desktop (eq window-system 'x))

(defconst ssbb/guix-system-p
  (file-directory-p "/gnu/store"))

;; Benchmark Emacs startup time.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda ()
			                       (setq gc-cons-threshold (* 2 1000 1000))))

(dolist (dir '("site-lisp" "modules"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

(require 'ssbb-package)
(require 'ssbb-core)
(require 'ssbb-ui)
(require 'ssbb-completion)
(require 'ssbb-edit)
(require 'ssbb-text)
(require 'ssbb-dev)
(require 'ssbb-tools)

(when ssbb/enable-modal-editing
  (require 'ssbb-meow))

(when ssbb/enable-desktop
  (require 'ssbb-desktop))

(provide 'init)
;;; init.el ends here
