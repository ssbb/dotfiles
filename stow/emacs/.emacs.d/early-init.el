;;; early-init.el

;;; Code:

;; Disable package initialize in early init to speed up load
(setq package-enable-at-startup nil)

(setq gc-cons-threshold most-positive-fixnum)
(setq native-comp-async-report-warnings-errors nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(if (string= (getenv "XDG_CURRENT_DESKTOP") "exwm")
    (progn
      (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
      (add-to-list 'default-frame-alist '(fullscreen . maximized)))

  (progn
    (add-to-list 'initial-frame-alist '(width . 128))
    (add-to-list 'initial-frame-alist '(height . 48))))

(provide 'early-init)

;;; early-init.el ends here
