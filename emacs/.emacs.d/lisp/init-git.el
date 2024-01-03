;;; init-git.el

;;; Code:

(use-package magit
  :bind ("C-x g" . magit-status))

;; (use-package magit-todos
;;   :config
;;   (magit-todos-mode 1))

(use-package git-timemachine)

(provide 'init-git)

;;; init-git.el ends here
