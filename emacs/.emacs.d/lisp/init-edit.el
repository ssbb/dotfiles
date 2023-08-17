;;; init-edit.el -*- lexical-binding: t -*-
;;; Code:

(use-package vundo
  :bind ("C-x u" . vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; Display line numbers in prog modes
(setq-default display-line-numbers-width 3)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Make minified files viewable
(use-package so-long
  :config
  (global-so-long-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package sudo-edit)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package subword
  :ensure nil
  :hook (prog-mode . subword-mode))

(provide 'init-edit)

;;; init-edit.el ends here
