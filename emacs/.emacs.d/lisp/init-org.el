;;; init-org.el -*- lexical-binding: t -*-
;;; Code:

(use-package org
  :config
  (setq org-log-done 'time)
  (add-to-list 'org-modules 'org-habit t))

(provide 'init-org)

;;; init-org.el ends here
