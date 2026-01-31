;;; ssbb-package.el --- Package management -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; Automatically install packages
(setq use-package-always-ensure t
      use-package-enable-imenu-support t)

(require 'use-package)
(require 'use-package-ensure)

(provide 'ssbb-package)
;;; ssbb-package.el ends here
