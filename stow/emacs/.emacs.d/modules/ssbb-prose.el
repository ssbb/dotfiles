;;; ssbb-prose.el --- Markup and Writing configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . org-display-inline-images))

  :custom
  (org-log-done 'time)
  (org-imenu-depth 100)
  (org-id-method 'org)
  :config
   (add-to-list 'org-modules 'org-habit))

(use-package org-ql)

(use-package htmlize)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)))

(provide 'ssbb-prose)
;;; ssbb-prose.el ends here
