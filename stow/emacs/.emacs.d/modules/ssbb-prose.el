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
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-ellipsis "…")
  (org-insert-heading-respect-content t)
  (org-special-ctrl-a/e t)
  :config
  (add-to-list 'org-modules 'org-habit))

(use-package org-modern-indent
  :vc (:url "https://github.com/jdtsmith/org-modern-indent")
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-modern
  :custom
  (org-modern-star 'replace)
  (org-modern-hide-stars nil)
  (org-modern-table nil)
  :config
  (global-org-modern-mode))

(use-package org-ql)

(use-package htmlize)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)))

(provide 'ssbb-prose)
;;; ssbb-prose.el ends here
