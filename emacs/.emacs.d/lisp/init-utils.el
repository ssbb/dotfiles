;;; init-utils.el --- Utils -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package helpful
  :bind
  (("C-c h f" . helpful-callable)
   ("C-c h v" . helpful-variable)
   ("C-c h x" . helpful-command)
   ("C-c h F" . helpful-function)
   ("C-c h k" . helpful-key)))

(use-package which-key
  :config
  (which-key-mode))

(use-package ag)

(use-package asdf
  :vc (:url "https://github.com/tabfugnic/asdf.el"
            :branch "main")
  :config
  (asdf-enable))

(use-package rg)

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))


(use-package disk-usage)
(use-package memory-usage)

(use-package chatgpt-shell
  ;; :custom
  ;; ((chatgpt-shell-openai-key
  ;;   (lambda ()
  ;;     (auth-source-pass-get 'secret "openai-key")))))
  )

(use-package dts-mode
  :mode ("\\.keymap\\'" "\\.dtsi\\'"))

(use-package kconfig-mode
  :mode ("\\.defconfig\\'" "Kconfig" "Kconfig.board"))

(provide 'init-utils)

;;; init-utils.el ends here
