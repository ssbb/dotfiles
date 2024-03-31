;;; init-term.el --- Terminal emulation  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package vterm
  :config
  ;; Disable hl line mode to avoid cursor blinking.
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local global-hl-line-mode nil))))

(provide 'init-term)

;;; init-term.el ends here
