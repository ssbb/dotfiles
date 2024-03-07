;;; init-package.el
;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Automatically install packages
(require 'use-package-ensure)
(setq use-package-always-ensure t
      use-package-enable-imenu-support t)

(provide 'init-package)

;;; init-package.el ends here
