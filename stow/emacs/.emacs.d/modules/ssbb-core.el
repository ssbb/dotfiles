;;; ssbb-core.el --- Core configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq user-full-name ssbb/full-name
      user-mail-address ssbb/email

      use-short-answers t
      vc-follow-symlinks t
      visible-bell t
      inhibit-compacting-font-caches t  ;; Don't compact font caches during GC
      delete-by-moving-to-trash t       ;; Delete by moving to OS trash
      make-backup-files t)

(column-number-mode t)
(indent-tabs-mode nil)
(savehist-mode)
(recentf-mode)

;; don't ask to kill processes on exit
(setq confirm-kill-processes nil)

;; Encoding
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Keep filesystem clean
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(make-directory (expand-file-name "tmp/locks/" user-emacs-directory) t)

(setq backup-directory-alist
        `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(setq auto-save-list-file-prefix
        (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory))

(setq auto-save-file-name-transforms
        `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(setq lock-file-name-transforms
        `((".*" ,(expand-file-name "tmp/locks/" user-emacs-directory) t)))

;; Garbage collector hack
(use-package gcmh
  :vc (:url "https://github.com/emacsmirror/gcmh")
  :hook (emacs-startup . gcmh-mode))

;; Paths from shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package asdf
  :vc (:url "https://github.com/tabfugnic/asdf.el" :branch "main")
  :config
  (asdf-enable))

(use-package helpful
  :bind
  (("C-c h f" . helpful-callable)
   ("C-c h v" . helpful-variable)
   ("C-c h x" . helpful-command)
   ("C-c h F" . helpful-function)
   ("C-c h k" . helpful-key)))

;; Don't change font size on C-<scroll>
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

;; (use-package desktop
;;   :ensure nil
;;   :custom
;;   (desktop-save t)
;;   (desktop-load-locked-desktop t)
;;   :config
;;   (make-directory (expand-file-name "tmp/desktop/" user-emacs-directory) t)
;;   (setq desktop-dirname (expand-file-name "tmp/desktop/" user-emacs-directory))
;;   (setq desktop-path    (list desktop-dirname))
;;   (desktop-save-mode 1))

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'ssbb-core)
;;; ssbb-core.el ends here
