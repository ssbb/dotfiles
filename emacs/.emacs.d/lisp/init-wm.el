;;; init-wm.el --- EXWM setup  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package exwm
  :if (memq window-system '(x))
  :straight (:host github :repo "emacs-exwm/exwm")
  :config
  (require 'exwm)
  (require 'exwm-config)
  ;; (require 'exwm-randr)

  ;; Initial number of workspaces
  (setq exwm-workspace-number 4)

  ;; Start workspaces from 1
  (setq exwm-workspace-index-map (lambda (i) (number-to-string (1+ i))))

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (add-hook 'exwm-update-title-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-title)))

  (setq exwm-input-global-keys
        `(
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

(provide 'init-wm)

;;; init-wm.el ends here
