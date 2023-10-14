;;; init-edit.el -*- lexical-binding: t -*-
;;; Code:

(use-package vundo
  :bind (("C-x u" . vundo)
         :map vundo-mode-map
         ("n" . vundo-backward)
         ("i" . vundo-forward)
         ("u" . vundo-previous)
         ("e" . vundo-next)
         ("q" . vundo-quit)
         ("SPC" . vundo-confirm))

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
  :bind ("C-=" . er/expand-region)
  :config
  (setq er/try-expand-list
        '(er/mark-inside-quotes
          er/mark-outside-quotes
          er/mark-inside-pairs
          er/mark-outside-pairs)))

(use-package subword
  :ensure nil
  :hook (prog-mode . subword-mode))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)))



(use-package simple
  :ensure nil
  :hook (((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

(provide 'init-edit)

;;; init-edit.el ends here
