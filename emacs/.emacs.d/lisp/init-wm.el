;;; init-wm.el --- EXWM setup  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package exwm
  :if (memq window-system '(x))
  :config
  (require 'exwm)
  (require 'exwm-config)
  ;; (require 'exwm-randr)

  (setq exwm-manage-configurations '((t char-mode t))
        exwm-workspace-number 4
        exwm-workspace-index-map (lambda (i) (number-to-string (1+ i))))

  (defun ssbb/exwm-rename-buffer ()
    (exwm-workspace-rename-buffer
     (concat "*EXWM* " exwm-class-name)))

  (add-hook 'exwm-update-class-hook 'ssbb/exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook 'ssbb/exwm-rename-buffer)

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer (concat "*WM* " exwm-class-name))))

  (add-hook 'exwm-update-title-hook
            (lambda ()
              (exwm-workspace-rename-buffer (concat "*WM* " exwm-title))))

  (setq exwm-input-global-keys
        `((,(kbd "<XF86Launch9>") . meow-keypad)
          ([?\s-r] . exwm-reset)
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ,@(mapcar (lambda (i)
                       `(,(kbd (format "s-%d" i)) .
                         (lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))
                    (number-sequence 1 9))))

  ;; (setq exwm-randr-workspace-output-plist '(1 "DP-3" 1 "DP-4"))

  ;; (exwm-randr-enable)
  (exwm-enable))

;; (use-package lemon
;;   :straight (:host codeberg :repo "emacs-weirdware/lemon"))

(provide 'init-wm)

;;; init-wm.el ends here
