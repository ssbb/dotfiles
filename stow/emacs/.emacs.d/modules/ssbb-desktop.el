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
  "Toggle dark mode via DBus."
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

(defun ssbb/lock-screen ()
  "Lock the screen via xset."
  (interactive)
  (start-process "lock" nil "xset" "s" "activate"))

(use-package exwm
  :config
  (require 'exwm)

  (defun ssbb/exwm-rename-buffer ()
    (exwm-workspace-rename-buffer
     (concat "*EXWM* " exwm-title)))

  (add-hook 'exwm-update-class-hook 'ssbb/exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook 'ssbb/exwm-rename-buffer)

  (setq exwm-workspace-number 1)

  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
          ;; ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])))

  (setq exwm-input-global-keys
        `(([?\s-r] . exwm-reset)
          ([?\s-!] . consult-omni)
          ([?\s-l] . ssbb/lock-screen)
          ;; s-N: Switch to certain tab
          ;; it's handled by tab-bar-mode already but we repeat here to make them global
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (tab-bar-select-tab i))))
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

  (setq exwm-xsettings-theme '("Adwaita" . "Adwaita-dark")
        exwm-xsettings `(("Gtk/FontName" . "system-ui 10")
                         ("Xft/RGBA" . "rgb")
                         ("Xft/lcdfilter" . "lcddefault")
                         ("Xft/Antialias" . 1)
                         ("Xft/DPI" . ,(* 120 1024))
                         ("Xft/Hinting" . 1)
                         ("Xft/HintStyle" . "hintslight")))
  (exwm-xsettings-mode 1)
  (exwm-background-mode 1)
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

(use-package ednc
  :config
  (ednc-mode))

(use-package ednc-popup
  :vc (:url "https://codeberg.org/akib/emacs-ednc-popup.git")
  :config
  (add-hook 'ednc-notification-presentation-functions
            #'ednc-popup-presentation-function))

(use-package battery
  :after nerd-icons
  :init
  (defvar ssbb/battery-mode-line-string "")

  (defun ssbb/battery-update ()
    (let* ((data (funcall battery-status-function))
           (status (alist-get ?L data))
           (charging (or (string= status "AC")
                         (string= status "on-line")))
           (percent (string-to-number (or (alist-get ?p data) "0")))
           (icon (cond
                  ((>= percent 90) (if charging "nf-md-battery_charging_90" "nf-md-battery_90"))
                  ((>= percent 80) (if charging "nf-md-battery_charging_80" "nf-md-battery_80"))
                  ((>= percent 70) (if charging "nf-md-battery_charging_70" "nf-md-battery_70"))
                  ((>= percent 60) (if charging "nf-md-battery_charging_60" "nf-md-battery_60"))
                  ((>= percent 50) (if charging "nf-md-battery_charging_50" "nf-md-battery_50"))
                  ((>= percent 40) (if charging "nf-md-battery_charging_40" "nf-md-battery_40"))
                  ((>= percent 30) (if charging "nf-md-battery_charging_30" "nf-md-battery_30"))
                  ((>= percent 20) (if charging "nf-md-battery_charging_20" "nf-md-battery_20"))
                  ((>= percent 10) (if charging "nf-md-battery_charging_10" "nf-md-battery_10"))
                  (t (if charging "nf-md-battery_charging_outline" "nf-md-battery_alert")))))
      (setq ssbb/battery-mode-line-string
            (format " %s %d%%%% " (nerd-icons-mdicon icon :v-adjust 0.1) percent))))

  :config
  (setq battery-mode-line-format "")
  (add-to-list 'global-mode-string '(:eval ssbb/battery-mode-line-string) t)
  (advice-add #'battery-update :after #'ssbb/battery-update)
  (display-battery-mode))

(use-package time
  :ensure nil
  :custom
  (display-time-format "%a %b %d, %I:%M%p")
  (display-time-default-load-average nil)
  :config
  (display-time-mode))

(use-package qutebrowser
  :vc (:url "https://github.com/lrustand/qutebrowser.el" :rev "master")
  :config
  (setq qutebrowser-pass--username
        (apply-partially #'auth-source-pass-get "login"))

  (qutebrowser-rpc-ensure-installed)
  (qutebrowser-theme-export-mode 1)
  (global-qutebrowser-doom-modeline-mode 1)
  (global-qutebrowser-exwm-mode))

(use-package pinentry
  :config
  (pinentry-start))

(provide 'ssbb-desktop)
;;; ssbb-desktop.el ends here
