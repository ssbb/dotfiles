;;; init-lsp.el -*- lexical-binding: t -*-
;;; Code:

;; (setenv "LSP_USE_PLISTS" "true")
(setq read-process-output-max (* 1024 1024))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"

        lsp-completion-provider nil
        lsp-enable-file-watchers nil
        lsp-lens-enable nil
        lsp-headerline-breadcrumb-enable nil)

  (defun ssbb/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  (defun ssbb/lsp-setup ()
    (lsp-enable-which-key-integration))

  (defun ssbb/lsp-format-setup ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t))

  (defun ssbb/lsp-organize-setup ()
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . ssbb/lsp-mode-setup-completion)

         (elixir-ts-mode . lsp)
         (elixir-ts-mode . ssbb/lsp-format-setup)

         (heex-ts-mode . lsp)
         (heex-ts-mode . ssbb/lsp-format-setup)

         (js-ts-mode . lsp))

  :commands lsp)

(use-package consult-lsp)

;; Disable `lsp-mode' in `git-timemachine-mode'
(defun my-lsp--init-if-visible (fn &rest args)
  (unless (bound-and-true-p git-timemachine-mode)
    (apply fn args)))

(advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible)

(use-package lsp-tailwindcss
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t)

  :config
  (add-to-list 'lsp-tailwindcss-major-modes 'elixir-ts-mode)
  (add-to-list 'lsp-tailwindcss-major-modes 'heex-ts-mode))

(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1))

(provide 'init-lsp)

;;; init-lsp.el ends here
