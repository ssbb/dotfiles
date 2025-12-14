;;; ssbb-desktop.el --- EXWM configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun ssbb/audio-volume-up ()
  "Increase volume by 5% asynchronously."
  (interactive)
  (start-process "volume-up" nil "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "5%+"))

(defun ssbb/audio-volume-down ()
  "Decrease volume by 5% asynchronously."
  (interactive)
  (start-process "volume-down" nil "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "5%-"))

(defun ssbb/audio-mute-toggle ()
  "Toggle mute."
  (interactive)
  (start-process "volume-mute" nil "wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"))

(defun ssbb/mic-mute-toggle ()
  "Toggle microphone."
  (interactive)
  (start-process "mic-mute" nil "wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle"))

(defun ssbb/brightness-up ()
  "Increase brightness by 5% asynchronously using brillo."
  (interactive)
  (start-process "brightness-up" nil "brillo" "-A" "5"))

(defun ssbb/brightness-down ()
  "Decrease brightness by 5% asynchronously using brillo."
  (interactive)
  (start-process "brightness-down" nil "brillo" "-U" "5"))

(defun ssbb/toggle-dark-mode ()
  (interactive)
  (let* ((current (string-trim
                   (shell-command-to-string
                    "gsettings get org.gnome.desktop.interface color-scheme")))
         (new-scheme (if (string-match-p "dark" current)
                         "prefer-light"
                       "prefer-dark")))
    (call-process "gsettings" nil nil nil
                  "set" "org.gnome.desktop.interface" "color-scheme" new-scheme)
    (message "Color scheme: %s" new-scheme)))

(use-package exwm
  :custom
  (exwm-workspace-number 4)
  (exwm-workspace-index-map
   (lambda (i) (number-to-string (1+ i))))

  :config
  (require 'exwm)

  (defun ssbb/exwm-rename-buffer ()
    (exwm-workspace-rename-buffer
     (concat "*EXWM* " exwm-title)))

  (add-hook 'exwm-update-class-hook 'ssbb/exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook 'ssbb/exwm-rename-buffer)

  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\C-d] . [delete])))

  (setq exwm-input-global-keys
        `(([?\s-r] . exwm-reset)
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-!] . consult-omni)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 1 9))
          ([XF86AudioRaiseVolume] . ssbb/audio-volume-up)
          ([XF86AudioLowerVolume] . ssbb/audio-volume-down)
          ([XF86AudioMute]        . ssbb/audio-mute-toggle)
          ([XF86AudioMicMute]     . ssbb/mic-mute-toggle)
          ([XF86MonBrightnessUp]   . ssbb/brightness-up)
          ([XF86MonBrightnessDown] . ssbb/brightness-down)
          ([XF86Display] . ssbb/toggle-dark-mode)))

  (with-eval-after-load 'consult
    (defvar +consult-exwm-filter "\\`\\*WM*")
    (add-to-list 'consult-buffer-filter +consult-exwm-filter)

    (defvar consult-source-exwm
      `(:name      "EXWM"
                   :narrow    ?x
                   ;; :hidden t
                   :category  buffer
                   :face      consult-buffer
                   :history   buffer-name-history
                   ;; Specify either :action or :state
                   :action    ,#'consult--buffer-action ;; No preview
                   ;; :state  ,#'consult--buffer-state  ;; Preview
                   :items
                   ,(lambda () (consult--buffer-query
                                :sort 'visibility
                                :as #'buffer-name
                                :exclude (remq +consult-exwm-filter consult-buffer-filter)
                                :mode 'exwm-mode)))
      "EXWM buffer source.")
    (add-to-list 'consult-buffer-sources 'consult-source-exwm 'append))

  (exwm-wm-mode))

(use-package screenshot
  :vc (:url "https://github.com/tecosaur/screenshot"))

(use-package disk-usage)
(use-package memory-usage)

(use-package consult-desktop-app
  :after consult
  :ensure nil
  :config
  (add-to-list 'consult-buffer-sources 'consult--source-desktop-apps 'append))

(use-package guix-dotfiles
  :ensure nil
  :bind (("C-c j" . guix-dotfiles-jump)))

(provide 'ssbb-desktop)
;;; ssbb-desktop.el ends here
