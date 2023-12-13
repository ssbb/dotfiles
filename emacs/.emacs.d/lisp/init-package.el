;;; init-package.el
;;; Code:

;; (require 'package)

;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))

(setq use-package-enable-imenu-support t
      straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Automatically install packages
;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t
;;       use-package-enable-imenu-support t)

;; (unless (package-installed-p 'vc-use-package)
;;   (package-vc-install "https://github.com/slotThe/vc-use-package"))

(provide 'init-package)

;;; init-package.el ends here
