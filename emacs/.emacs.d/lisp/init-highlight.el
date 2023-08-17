;;; init-highlight.el -*- lexical-binding: t -*-
;;; Code:

;; Highlight current line
(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1))

;; Highlight parens
(use-package paren
  :ensure nil
  :config
  (show-paren-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-highlight)

;;; init-highlight.el ends here
