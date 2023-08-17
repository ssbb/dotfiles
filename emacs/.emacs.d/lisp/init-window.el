;;; init-window.el -*- lexical-binding: t -*-
;;; Code:

(use-package windmove
  :ensure nil
  :config
  (windmove-default-keybindings 'super))

(use-package ace-window
  :defer nil
  :bind (([remap other-window] . ace-window))
  
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :foreground unspecified :bold t :height 3.0))))
  (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  
  :config
  (ace-window-display-mode)
  (ace-window-posframe-mode)
  
  ;; Select widnow via `M-1'...`M-9'
  (defun aw--select-window (number)
    "Slecet the specified window."
    (when (numberp number)
      (let ((found nil))
        (dolist (win (aw-window-list))
          (when (and (window-live-p win)
                     (eq number
                         (string-to-number
                          (window-parameter win 'ace-window-path))))
            (setq found t)
            (aw-switch-to-window win)))
        (unless found
          (message "No specified window: %d" number)))))
  
  (dotimes (n 9)
    (bind-key (format "M-%d" (1+ n))
              (lambda ()
                (interactive)
                (aw--select-window (1+ n))))))

(use-package switch-window)

(provide 'init-window)

;;; init-window.el ends here
