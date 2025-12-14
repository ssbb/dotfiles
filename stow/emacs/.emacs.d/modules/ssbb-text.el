;;; ssbb-text.el --- Markup and Writing configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . org-display-inline-images))

  :custom
  (org-log-done 'time)

  :config
  (org-display-inline-images)
  (add-to-list 'org-modules 'org-habit t))

(use-package org-ql)

(use-package htmlize)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)))

(provide 'ssbb-text)
;;; ssbb-text.el ends here
