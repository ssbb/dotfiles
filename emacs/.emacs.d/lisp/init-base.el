;;; init-base.el -*- lexical-binding: t -*-
;;; Code:

(setq user-full-name ssbb/full-name
      user-mail-address ssbb/email)

(when (eq system-type 'darwin)
  (setq ns-right-command-modifier 'control)
)

;; Garbage collector hack
(use-package gcmh
  :hook (emacs-startup . gcmh-mode))

;; Use UTF-8
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq system-time-locale "C")

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Short y/n
(setq use-short-answers t)

(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 2
              indent-tabs-mode nil)

(setq visible-bell t
      inhibit-compacting-font-caches t ;; Don't compact font caches during GC
      delete-by-moving-to-trash t      ;; Delete by moving to OS trash
      make-backup-files nil            ;; Do not create backup files
      auto-save-default nil)           ;; Disable auto save

;; History
(use-package savehist
  :ensure nil
  :config
  (savehist-mode))

(use-package recentf
  :ensure nil
  :config
  (recentf-mode))

(use-package simple
  :ensure nil
  :config
  (setq column-number-mode t))

;; Global keybindings
(global-set-key (kbd "M-<up>") 'ssbb/move-line-up)
(global-set-key (kbd "M-<down>") 'ssbb/move-line-down)

(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

(provide 'init-base)

;;; init-base.el ends here
