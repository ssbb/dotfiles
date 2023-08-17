;;; init-package.el
;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Automatically install packages
(require 'use-package-ensure)
(setq use-package-always-ensure t
      use-package-enable-imenu-support t)

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

(provide 'init-package)

;;; init-package.el ends here
