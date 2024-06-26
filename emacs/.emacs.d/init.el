;;; init.el -*- lexical-binding: t no-byte-compile: t -*-

;;; Code:

;; Benchmark Emacs startup time.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(setq gc-cons-threshold most-positive-fixnum)
 (add-hook 'after-init-hook (lambda ()
			                       (setq gc-cons-threshold (* 2 1000 1000))))

(dolist (dir '("site-lisp" "lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

(require 'init-const)
(require 'init-funcs)

(require 'init-package)
(require 'init-base)

(require 'init-ui)
(require 'init-dired)
(require 'init-treemacs)
(require 'init-completion)
(require 'init-edit)
(require 'init-highlight)
(require 'init-window)
(require 'init-dashboard)
(require 'init-utils)
(require 'init-org)
(require 'init-markdown)
(require 'init-tempel)
(require 'init-ai)

;; Programming
(require 'init-git)
(require 'init-lsp)
(require 'init-prog)
(require 'init-elixir)
(require 'init-elm)
(require 'init-haskell)
(require 'init-cad)
(require 'init-modus-exporter)
(require 'init-meow)
;; (require 'init-wm)
(require 'init-term)

(provide 'init)
