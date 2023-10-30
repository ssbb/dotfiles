;;; init-utils.el --- Utils -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package which-key
  :config
  (which-key-mode))

(use-package ag)

(use-package asdf
  :vc (:fetcher github :repo tabfugnic/asdf.el)
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
