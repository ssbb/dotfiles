;;; ssbb-prog.el --- Development configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package subword
  :ensure nil
  :hook (prog-mode . subword-mode))

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode html-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-width 3))

(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package elixir-ts-mode
  :mode ("\\.ex\\'" "\\.exs\\'" "mix\\.lock\\'"))

(use-package heex-ts-mode
  :ensure t
  :mode "\\.heex\\'"
  :vc (:url "https://github.com/ssbb/heex-ts-mode" :branch "custom-font-lock"))

(use-package elm-mode)
(use-package haskell-mode)
(use-package scad-mode)

(use-package kconfig-mode
  :mode ("\\.defconfig\\'" "Kconfig" "Kconfig.board"))

(use-package dts-mode
  :mode ("\\.keymap\\'" "\\.dtsi\\'" "\\.overlay\\'")
  :hook ((dts-mode . ssbb/dts-mode-setup))
  :config
  (defun ssbb/dts-mode-setup ()
    (setq-local tab-width 4)))

(use-package plantuml-mode
  :mode "\\.puml\\'"
  :custom
  (plantuml-indent-level 2)
  :config
  (eval-and-compile
    (defun hex-encode (str)
		  (string-join
       (mapcar
        (lambda (c) (format "%02x" c))
        (encode-coding-string str 'utf-8)))))

  (defun plantuml-server-encode-url (string)
    "Encode the string STRING into a URL suitable for PlantUML server interactions."
    (let* ((encoded-string (hex-encode string)))
      (concat plantuml-server-url "/" plantuml-output-type "/~h" encoded-string))))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Formatting
(use-package apheleia
  :hook ((js-ts-mode . apheleia-mode)
         (c-ts-mode . apheleia-mode)
         (elm-mode . apheleia-mode)
         (dts-mode . apheleia-mode)
         (elixir-ts-mode . apheleia-mode)
         (heex-ts-mode . apheleia-mode))
  :config
  ;; See https://github.com/raxod502/apheleia/issues/30
  (defun ssbb/fix-apheleia-project-dir (orig-fn &rest args)
    (let ((project (project-current)))
      (if (not (null project))
          (let ((default-directory (project-root project))) (apply orig-fn args))
        (apply orig-fn args))))
  (advice-add 'apheleia-format-buffer :around #'ssbb/fix-apheleia-project-dir)

  ;; sometimes apheleia erase the whole buffer, which is pretty annoying.
  ;; fix it by detecting this scenario and simply doing no-op
  (defun ssbb/fix-apheleia-accidental-deletion
      (orig-fn old-buffer new-buffer &rest rest)
    (if (and (=  0 (buffer-size new-buffer))
             (/= 0 (buffer-size old-buffer)))
        ;; do not override anything
        nil
      (apply orig-fn old-buffer new-buffer rest)))

  (advice-add 'apheleia--create-rcs-patch :around #'ssbb/fix-apheleia-accidental-deletion)

  (cl-defun apheleia-indent-eglot-managed-buffer
      (&key buffer scratch callback &allow-other-keys)
    "Copy BUFFER to SCRATCH, then format scratch, then call CALLBACK."
    (with-current-buffer scratch
      (setq-local eglot--cached-server
                  (with-current-buffer buffer
                    (eglot-current-server)))
      (let ((buffer-file-name (buffer-local-value 'buffer-file-name buffer)))
        (eglot-format-buffer))
      (funcall callback)))

  (add-to-list 'apheleia-formatters
               '(eglot . apheleia-indent-eglot-managed-buffer))

  (defun ssbb/apheleia-prefer-eglot-h ()
    "Hook to prefer LSP-based formatting when available.
If ‘apheleia-formatter’ is set explicitly, do nothing. Intended for
‘eglot-managed-mode-hook’."
    (when (and (null apheleia-formatter)
               (eglot-managed-p)
               (eglot-server-capable :documentFormattingProvider))
      (setq-local apheleia-formatter 'eglot)))

  (add-hook 'eglot-managed-mode-hook #'ssbb/apheleia-prefer-eglot-h)

  (defun my/set-elixir-formatter ()
    (setq-local apheleia-formatter 'mix-format))

  ;; Elixir LSPs recompile frequently, causing formatting hangs.
  ;; Since they just proxy to `mix format`, skip the extra layer.
  (add-hook 'elixir-ts-mode-hook #'my/set-elixir-formatter)
  (add-hook 'heex-ts-mode-hook #'my/set-elixir-formatter))

;; Tree-sitter
(use-package treesit
  :ensure nil
  :config
  ;; (setq treesit-font-lock-level 4)
  (add-to-list 'treesit-language-source-alist '(devicetree "https://github.com/joelspadin/tree-sitter-devicetree")))

(unless ssbb/guix-system-p
  (use-package treesit-auto
    :custom
    (treesit-auto-install 'prompt)
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode)))

;; Diagnostics
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;; (use-package flycheck
;;   :config
;;   (global-flycheck-mode))

;; (use-package consult-flycheck
;;   :after (consult flycheck)
;;   :defer nil
;;   :bind (("C-c !" . consult-flycheck)))

;; LSP

(setq read-process-output-max (* 1024 1024))

;; Required by eglot for snippet expansion
(use-package yasnippet
  :hook (eglot-managed-mode . yas-minor-mode))

(use-package eglot
  :ensure t
  :hook (elixir-ts-mode . eglot-ensure)
  :custom
  (eglot-code-action-indicator "●")
  :config
  (add-to-list 'eglot-server-programs
               '((elixir-mode elixir-ts-mode heex-ts-mode) . ("expert-lsp" "--stdio"))))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-idle-delay 1.0))

(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
  :config
  (defun ssbb/eldoc-box-skip-keywords (orig-fun &rest args)
    "Skip eldoc-box for keyword faces."
    (let ((face (get-text-property (point) 'face)))
      (unless (if (listp face)
                  (memq 'font-lock-keyword-face face)
                (eq face 'font-lock-keyword-face))
        (apply orig-fun args))))


(advice-add 'eldoc-box--eldoc-message-function :around #'ssbb/eldoc-box-skip-keywords))

(provide 'ssbb-prog)
;;; ssbb-prog.el ends here
