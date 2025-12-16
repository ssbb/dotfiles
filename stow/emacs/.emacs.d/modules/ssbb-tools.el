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
  :hook (vterm-mode . ssbb/vterm-setup)
  :config
  (defun ssbb/vterm-setup ()
    (global-hl-line-mode -1))

  (defun ssbb/vterm-refresh-on-theme (_theme)
    (dolist (buf (buffer-list))
      (when (eq (buffer-local-value 'major-mode buf) 'vterm-mode)
        (with-current-buffer buf
          (let* ((inhibit-read-only t)
                 (height (window-body-height))
                 (width (window-body-width)))
            (vterm--set-size vterm--term (1- height) width)
            (vterm--set-size vterm--term height width))))))

  (add-hook 'enable-theme-functions #'ssbb/vterm-refresh-on-theme))

(use-package multi-vterm
  :defer t
  :ensure nil
  :bind (("C-c d" . multi-vterm-dedicated-toggle)
         ("C-c v" . multi-vterm))
  :after vterm)

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("q" . ssbb/dired-quit))
  :custom
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-mouse-drag-files t)
  (mouse-drag-and-drop-region-cross-program t)

  :config
  (defun ssbb/dired-quit ()
    (interactive)
    (quit-window t)))

(use-package diredfl
  :hook ((dired-mode . diredfl-mode)
         (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package dirvish
  :bind (("C-c f" . dirvish)
         ("C-c b" . dirvish-side)
         :map dirvish-mode-map
         ("?" . dirvish-dispatch)
         ("N" . dirvish-narrow)
         ("TAB" . dirvish-subtree-toggle)
         (";" . dired-up-directory))
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-side-width 50)
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons collapse file-size))
  (setq dirvish-large-directory-threshold 20000)

  (dirvish-side-follow-mode))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

;; (use-package guix
;;   :ensure nil
;;   :defer t)

(use-package transmission)

(provide 'ssbb-tools)
;;; ssbb-tools.el ends here
