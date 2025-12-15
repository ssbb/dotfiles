;;; ssbb-dev.el --- Development configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

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
         (dts-mode . apheleia-mode))
  :config
  ;; (add-to-list 'apheleia-mode-alist '(heex-ts-mode . mix-format))
  ;; (add-to-list 'apheleia-mode-alist '(dts-mode . clang-format))

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

  (advice-add 'apheleia--create-rcs-patch :around #'ssbb/fix-apheleia-accidental-deletion))

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

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
        ;; lsp-completion-provider nil
        lsp-enable-file-watchers nil
        lsp-lens-enable nil
        lsp-headerline-breadcrumb-enable nil)

  ;; (setq lsp-elixir-ls-version "v0.19.0")

  ;; (defun ssbb/lsp-mode-setup-completion ()
  ;;   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
  ;;         '(orderless)))

  (defun ssbb/lsp-setup ()
    (lsp-enable-which-key-integration))

  (defun ssbb/lsp-format-setup ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t))

  (defun ssbb/lsp-organize-setup ()
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (defun lsp-after-local-variables ()
    "Set up lsp after local variables have been loaded."
    (add-hook 'hack-local-variables-hook #'lsp-deferred nil t))

  (add-hook 'elixir-ts-mode-hook #'lsp-after-local-variables)
  (add-hook 'heex-ts-mode-hook #'lsp-after-local-variables)

  ;; (add-hook 'hack-local-variables-hook
  ;;           (lambda () (when (derived-mode-p 'elixir-ts-mode) (lsp))))

  :hook ((lsp-mode . lsp-enable-which-key-integration)
         ;; (lsp-completion-mode . ssbb/lsp-mode-setup-completion)

         ;; (elixir-ts-mode . lsp)
         (elixir-ts-mode . ssbb/lsp-format-setup)

         ;; (heex-ts-mode . lsp)
         (heex-ts-mode . ssbb/lsp-format-setup)

         (js-ts-mode . lsp)
         )

  :commands lsp)

(use-package consult-lsp
  :after (consult lsp-mode))

(use-package lsp-tailwindcss
  :after lsp-mode
  :vc (:url "https://github.com/merrickluo/lsp-tailwindcss"
            :branch "master")
  :custom
  (lsp-tailwindcss-add-on-mode t)

  :config
  (add-to-list 'lsp-tailwindcss-major-modes 'elixir-ts-mode)
  (add-to-list 'lsp-tailwindcss-major-modes 'heex-ts-mode))

(provide 'ssbb-dev)
;;; ssbb-dev.el ends here
