;;; init-treemacs.el -*- lexical-binding: t -*-
;;; Code:

(use-package treemacs
  :bind (("C-x t t" . treemacs))
  :config
  (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
        treemacs-missing-project-action 'remove
        treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-width 50
        treemacs-wide-toggle-width 80
        treemacs-width-increment 5
        treemacs-git-mode 'simple
        treemacs-sorting 'alphabetic-case-insensitive-asc)

  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-hide-gitignored-files-mode -1)

  (treemacs-fringe-indicator-mode 'always))

(use-package treemacs-magit)

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(provide 'init-treemacs)

;;; init-treemacs.el ends here
