;;; init-treemacs.el -*- lexical-binding: t -*-
;;; Code:

(use-package treemacs
  :bind (("C-c /" . treemacs-select-window))
  :config
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-hide-gitignored-files-mode -1)

  ;; (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)

  (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
        treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc))

(use-package treemacs-magit)

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(provide 'init-treemacs)

;;; init-treemacs.el ends here
