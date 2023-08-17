;;; init-dired.el -*- lexical-binding: t -*-
;;; Code:

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-alh"))

;; Colorful dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(provide 'init-dired)

;;; init-dired.el ends here
