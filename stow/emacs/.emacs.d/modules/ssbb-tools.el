;;; ssbb-tools.el --- Tools configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package magit
  :bind (("C-c v" . magit-status)
         ("C-x g" . magit-status)))

(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package ag)
(use-package rg)

(unless ssbb/guix-system-p
  (use-package vterm
    :ensure t)

  (use-package multi-vterm
    :ensure t))

(use-package vterm
  :ensure nil
  :defer t
  :custom
  ;; TODO breaks multi-vterm ability to toggle dedicated buffer
  ;; (vterm-buffer-name-string "*vterm* %s")
  (vterm-shell "fish")
  (vterm-term-environment-variable "eterm-color")
  (vterm-kill-buffer-on-exit t)

  :config
  ;; Force redraw buffer on theme change
  (add-hook 'enable-theme-functions
            (lambda (_theme)
              (dolist (buf (buffer-list))
                (with-current-buffer buf
                  (when (derived-mode-p 'vterm-mode)
                    (let* ((inhibit-read-only t)
                           (height (window-body-height))
                           (width (window-body-width)))
                      (vterm--set-size vterm--term (1- height) width)
                      (vterm--set-size vterm--term height width)))))))

  ;; Disable hl line mode to avoid cursor blinking.
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local global-hl-line-mode nil))))

(use-package multi-vterm
  :defer t
  :ensure nil
  :bind (("C-c d" . multi-vterm-dedicated-toggle)
         ("C-c v" . multi-vterm))
  :after vterm)

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("q" . (lambda () (interactive) (quit-window t))))  ;; kill dired bauffer on `q'
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-mouse-drag-files t))

(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

(use-package guix
  :ensure nil
  :defer t)

(provide 'ssbb-tools)
;;; ssbb-sools.el ends here
