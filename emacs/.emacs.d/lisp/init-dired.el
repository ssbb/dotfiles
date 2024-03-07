;;; init-dired.el -*- lexical-binding: t -*-
;;; Code:

(use-package dired
  :ensure nil
  :config
  (setq insert-directory-program "gls"
        dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group"))

(use-package nerd-icons-dired
  :hook ((dired-mode . nerd-icons-dired-mode)))

(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

(provide 'init-dired)

;;; init-dired.el ends here
