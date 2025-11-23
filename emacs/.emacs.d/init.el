;;; init.el --- ssbb's personal configuration -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:
;;;

;;; Code:

(defconst ssbb/full-name "Sviatoslav Bulbakha")
(defconst ssbb/email "mail@ssbb.me")

;;; Stratup

;; Benchmark Emacs startup time.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda ()
			                       (setq gc-cons-threshold (* 2 1000 1000))))

;;; Package management

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; Automatically install packages
(setq use-package-always-ensure t
      use-package-enable-imenu-support t)

(require 'use-package-ensure)

;;; Base

(setq user-full-name ssbb/full-name
      user-mail-address ssbb/email)

(dolist (dir '("site-lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

;; Garbage collector hack
(use-package gcmh
  :vc (:url "https://github.com/emacsmirror/gcmh")
  :hook (emacs-startup . gcmh-mode))

(setq use-short-answers t)

(setq visible-bell t
      inhibit-compacting-font-caches t  ;; Don't compact font caches during GC
      delete-by-moving-to-trash t)      ;; Delete by moving to OS trash

;; Use UTF-8
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(prefer-coding-system 'utf-8)

(setq locale-coding-system 'utf-8
      system-time-locale "C")

;; Shell $PATH
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Keep my filesystem clean
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(setq auto-save-list-file-prefix
      (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory))

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(setq lock-file-name-transforms
      `((".*" ,(expand-file-name "tmp/locks/" user-emacs-directory) t)))

(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 2
              indent-tabs-mode nil)

(use-package simple
  :ensure nil
  :config
  (setq column-number-mode t))

;; Handle large files
(use-package so-long
  :config
  (global-so-long-mode 1))

(use-package savehist
  :ensure nil
  :config
  (savehist-mode))

(use-package recentf
  :ensure nil
  :config
  (recentf-mode))


;;; Edit

(setq save-interprogram-paste-before-kill t)

(use-package subword
  :ensure nil
  :hook (prog-mode . subword-mode))

(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

;; Delete selection on insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Save with sudo
(use-package sudo-edit)

(use-package vundo
  :bind (("C-x u" . vundo))
  :config (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package simple
  :ensure nil
  :hook (((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :config
  (setq er/try-expand-list
        '(er/mark-inside-quotes
          er/mark-outside-quotes
          er/mark-inside-pairs
          er/mark-outside-pairs)))

(use-package tempel
  ;; :custom
  ;; (tempel-trigger-prefix "<")
  :bind (:map tempel-map
              ("TAB" . tempel-next)
              ("<escape>" . my/tempel-done))

  :init
  (defun my/tempel-done ()
    (interactive)
    (tempel-done)
    (meow-insert-exit))

  ;; Setup completion at point
  (defun my/tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'my/tempel-setup-capf)
  (add-hook 'prog-mode-hook 'my/tempel-setup-capf)
  (add-hook 'text-mode-hook 'my/tempel-setup-capf)

  ;; return tempel-expand as #1 function in capf after lsp-mode wipes it.
  (add-hook 'lsp-completion-mode-hook 'my/tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode))
  )

(use-package tempel-collection)


(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(defun ssbb/move-line-up ()
  "Move line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun ssbb/move-line-down ()
  "Move line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(keymap-global-set "M-<up>" 'ssbb/move-line-up)
(keymap-global-set "M-<down>" 'ssbb/move-line-down)

(keymap-global-set "<home>" 'move-beginning-of-line)
(keymap-global-set "<end>" 'move-end-of-line)

;;; EXWM

(when (memq window-system '(x))
  (use-package exwm
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

    (exwm-enable))

  (use-package screenshot
    :vc (:url "https://github.com/tecosaur/screenshot"))

  (use-package disk-usage)
  (use-package memory-usage))

;;; Meow

(use-package meow
  :demand t
  :custom
  (meow-selection-command-fallback
   '((meow-change . meow-change-char)
     (meow-kill . meow-C-k)
     (meow-save . kill-ring-save)
     (meow-cancel-selection . keyboard-quit)
     (meow-pop-selection . meow-pop-grab)
     (meow-beacon-change . meow-beacon-change-char)
     (meow-expand . meow-digit-argument)

     ;; Fallback replace to yank.
     (meow-replace . meow-yank)
     (meow-replace-pop . meow-yank-pop)))
  :config
  (setq meow-char-thing-table
        ;; rofnd/square/curly/angle things maps to my keyboard SYM layer
        '((?p . round)
          (?b . square)
          (?f . curly)
          (?g . angle)
          (?s . string)
          (?r . line)
          (?t . buffer)))

  (setq meow--kbd-backward-line "<up>"
        meow--kbd-forward-line  "<down>"
        meow--kbd-backward-char "<left>"
        meow--kbd-forward-char  "<right>")

  (setq meow-visit-sanitize-completion t
        meow-keypad-self-insert-undefined nil
        meow-keypad-start-keys '((?c . ?c) (?x . ?x))
        ;; meow-keypad-leader-dispatch "C-c"
        meow-use-clipboard t)

  (setq meow-normal-state-keymap (make-sparse-keymap))
  (setq meow-leader-state-keymap (make-sparse-keymap))

  (setf (alist-get 'normal meow-keymap-alist) meow-normal-state-keymap)
  (setf (alist-get 'leader meow-keymap-alist) meow-leader-state-keymap)

  ;; (defvar my-meow-shift-map (make-sparse-keymap))
  ;; (dolist (char (number-sequence ?a ?z))
  ;;   (define-key my-meow-shift-map
  ;;               (char-to-string char)
  ;;               `(lambda () (interactive) (call-interactively (lookup-key meow-motion-state-keymap ,(char-to-string (- char 32)))))))
  ;; (define-key meow-motion-state-keymap (kbd "i") my-meow-shift-map)

  (meow-define-keys 'leader
    (cons "p" project-prefix-map)
    '("z" . meow-universal-argument)
    '("l" . consult-goto-line)
    '("q" . kill-buffer)
    '("b" . switch-to-buffer)
    '("w" . ace-window)
    '("n" . comment-dwim)
    '("." . treemacs-select-window)
    '("z" . meow-universal-argument)
    '(",n" . "H-n")
    '(",a" . "H-a")
    '(",u" . "H-u")
    '(",e" . "H-e")
    )

  (meow-define-keys 'motion
    '("n" . meow-left)
    '("a" . meow-right)
    '("u" . meow-prev)
    '("e" . meow-next))

  (meow-define-keys 'normal
    '("0" . meow-expand-0)
    '("1" . meow-expand-1)
    '("2" . meow-expand-2)
    '("3" . meow-expand-3)
    '("4" . meow-expand-4)
    '("5" . meow-expand-5)
    '("6" . meow-expand-6)
    '("7" . meow-expand-7)
    '("8" . meow-expand-8)
    '("9" . meow-expand-9)

    '("a"  . meow-right)
    '("A" . meow-right-expand)
    '("b"  . meow-join)
    '("c"  . meow-append)
    '("C" . meow-open-below)
    '("d"  . meow-change)
    '("D" . meow-change-line)
    '("e"  . meow-next)
    '("E" . meow-next-expand)
    '("f"  . meow-back-word)
    '("F" . meow-back-symbol)
    '("g"  . meow-replace)
    '("G" . meow-replace-pop)
    '("h"  . meow-undo)
    '("H" . undo-redo)
    '("i" . meow-delete)
    '("k"  . meow-till)
    '("K" . meow-find)
    '("l"  . meow-insert)
    '("L" . meow-open-above)
    '("m"  . avy-goto-char)
    '("n"  . meow-left)
    '("N" . meow-left-expand)
    '("o"  . meow-next-word)
    '("O" . meow-next-symbol)
    '("p"  . er/expand-region)
    '("P"  . er/shrink-region)
    '("q"  . execute-extended-command)
    '("r"  . meow-kill)
    '("R" . meow-kill-whole-line)
    '("s"  . meow-mark-word)
    '("S" . meow-mark-symbol)
    '("t"  . meow-line)
    '("u"  . meow-prev)
    '("U" . meow-prev-expand)
    '("v"  . meow-quit)
    '("w"  . meow-grab)
    '("W" . meow-pop-grab)
    '("x"  . meow-save)
    '("X" . meow-save-clipboard)
    '("y"  . meow-reverse)
    '("z"  . meow-reverse)

    '("'"  . consult-line)
    '(","  . meow-beginning-of-thing)
    '("."  . meow-end-of-thing)
    '("<" . meow-inner-of-thing)
    '("""" . meow-bounds-of-thing)

    '("<escape>" . meow-cancel-selection))

  (defun meow-save-clipboard ()
    "Copy in clipboard."
    (interactive)
    (let ((meow-use-clipboard t))
      (meow-save)))

  (defun meow-change-line ()
    "Kill till end of line and switch to INSERT state."
    (interactive)
    (meow--cancel-selection)
    (meow-end-of-thing
     (car (rassoc 'line meow-char-thing-table)))
    (meow-change))

  (defun er/shrink-region ()
    (interactive)
    (er/expand-region -1))

  (defun ssbb/meow-hide-cursor ()
    (progn
      (meow--set-cursor-type nil)))

  (add-to-list 'meow-update-cursor-functions-alist
               '((lambda () (and (meow-motion-mode-p)
                                 (eq major-mode 'vundo-mode)))
                 . ssbb/meow-hide-cursor))

  ;; Allow meow-keypad to take inputs when it's started.
  (add-hook 'meow-keypad-mode-hook
            (lambda ()
              (when (derived-mode-p 'exwm-mode)
                (if meow-keypad-mode
                    (exwm-input-grab-keyboard exwm--id)
                  (exwm-input-release-keyboard exwm--id)))))

  (with-eval-after-load 'meow
    (push '(vterm-mode . insert) meow-mode-state-list)
    (add-hook 'vterm-mode-hook
              (lambda ()
                (add-hook 'meow-insert-enter-hook
                          (lambda () (vterm-copy-mode -1))
                          nil t)
                (add-hook 'meow-insert-exit-hook
                          (lambda () (vterm-copy-mode 1))
                          nil t))))

  ;; (require 'meow)
  ;; (meow-global-mode t)
  )

;; (use-package meow-state-bridge
;;   :after meow
;;   :load-path "~/.emacs.d/site-lisp/"
;;   :hook (meow-global-mode . meow-state-bridge-toggle)
;;   :init
;;   (setq meow-state-bridge-script-path "/Users/ssbb/bridge.py"
;;         meow-state-bridge-debug t)
;;   :config
;;   (defun meow-state-bridge-toggle ()
;;     (meow-state-bridge-mode (if meow-global-mode 1 -1))))

;;; Org & Markdown

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . org-display-inline-images))
  :config
  (setq org-log-done 'time)
  (org-display-inline-images)
  (add-to-list 'org-modules 'org-habit t))

(use-package org-ql)

(use-package htmlize)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)))

;;; Navigation

(use-package avy
  :bind (("C-:" . avy-goto-char))
  :config
  (setq avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o)  ;; Colemak DH
        avy-all-windows nil))

(setq split-width-threshold 160
      split-height-threshold nil)

(use-package ace-window
  :defer nil
  :bind (([remap other-window] . ace-window))

  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :foreground unspecified :bold t :height 3.0))))
  (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))

  :config
  (setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o)  ;; Colemak DH
        aw-dispatch-always t)

  (ace-window-display-mode)
  (when window-system
    (ace-window-posframe-mode)))

(use-package windmove
  :ensure nil
  :config
  (windmove-default-keybindings 'super))

;;; Programming

(use-package elixir-ts-mode)

(use-package heex-ts-mode
  :ensure t
  :vc (:url "https://github.com/ssbb/heex-ts-mode" :branch "custom-font-lock"))

(use-package elm-mode)
(use-package haskell-mode)
(use-package scad-mode)

(use-package kconfig-mode
  :mode ("\\.defconfig\\'" "Kconfig" "Kconfig.board"))

(use-package dts-mode
  :mode ("\\.keymap\\'" "\\.dtsi\\'" "\\.overlay\\'")
  :config
  (add-hook 'dts-mode-hook
            (lambda () (setq-local tab-width 4))))

(use-package plantuml-mode
  :mode "\\.puml\\'"
  :config
  (setq plantuml-indent-level 2)

  (eval-and-compile (defun hex-encode (str)
		                  (string-join (mapcar (lambda (c) (format "%02x" c)) (encode-coding-string str 'utf-8)))))

  (defun plantuml-server-encode-url (string)
    "Encode the string STRING into a URL suitable for PlantUML server interactions."
    (let* ((encoded-string (hex-encode string)))
      (concat plantuml-server-url "/" plantuml-output-type "/~h" encoded-string))))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Formatting
(use-package apheleia
  :hook ((js-ts-mode . apheleia-mode)
         (c-ts-mode . apheleia-mode)
         (elm-mode . apheleia-mode)
         (dts-mode . apheleia-mode))
  :config
  (add-to-list 'apheleia-mode-alist '(heex-ts-mode . mix-format))
  ;; (add-to-list 'apheleia-mode-alist '(dts-mode . clang-format))

  ;; See https://github.com/raxod502/apheleia/issues/30
  (defun ssbb/fix-apheleia-project-dir (orig-fn &rest args)
    (let ((project (project-current)))
      (if (not (null project))
          (let ((default-directory (project-root project))) (apply orig-fn args))
        (apply orig-fn args))))
  (advice-add 'apheleia-format-buffer :around #'ssbb/fix-apheleia-project-dir)

  ;; sometimes apheleia erase the whole buffer, which is pretty annoying.
  ;; fix it by detecting this scenario and simply doing no-op
  (defun ssbb/fix-apheleia-accidental-deletion
      (orig-fn old-buffer new-buffer &rest rest)
    (if (and (=  0 (buffer-size new-buffer))
             (/= 0 (buffer-size old-buffer)))
        ;; do not override anything
        nil
      (apply orig-fn old-buffer new-buffer rest)))

  (advice-add 'apheleia--create-rcs-patch :around #'ssbb/fix-apheleia-accidental-deletion))

;; Tree-sitter
(use-package treesit
  :ensure nil
  :config
  ;; (setq treesit-font-lock-level 4)
  (add-to-list 'treesit-language-source-alist '(devicetree "https://github.com/joelspadin/tree-sitter-devicetree")))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Diagnostics
(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package consult-flycheck
  :after (consult flycheck)
  :defer nil
  :bind (("C-c !" . consult-flycheck)))

;; LSP

(setq read-process-output-max (* 1024 1024))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
        ;; lsp-completion-provider nil
        lsp-enable-file-watchers nil
        lsp-lens-enable nil
        lsp-headerline-breadcrumb-enable nil)

  ;; (setq lsp-elixir-ls-version "v0.19.0")

  ;; (defun ssbb/lsp-mode-setup-completion ()
  ;;   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
  ;;         '(orderless)))

  (defun ssbb/lsp-setup ()
    (lsp-enable-which-key-integration))

  (defun ssbb/lsp-format-setup ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t))

  (defun ssbb/lsp-organize-setup ()
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (defun lsp-after-local-variables ()
    "Set up lsp after local variables have been loaded."
    (add-hook 'hack-local-variables-hook #'lsp-deferred nil t))

  (add-hook 'elixir-ts-mode-hook #'lsp-after-local-variables)
  (add-hook 'heex-ts-mode-hook #'lsp-after-local-variables)

  ;; (add-hook 'hack-local-variables-hook
  ;;           (lambda () (when (derived-mode-p 'elixir-ts-mode) (lsp))))

  :hook ((lsp-mode . lsp-enable-which-key-integration)
         ;; (lsp-completion-mode . ssbb/lsp-mode-setup-completion)

         ;; (elixir-ts-mode . lsp)
         (elixir-ts-mode . ssbb/lsp-format-setup)

         ;; (heex-ts-mode . lsp)
         (heex-ts-mode . ssbb/lsp-format-setup)

         (js-ts-mode . lsp)
         )

  :commands lsp)

(use-package consult-lsp
  :after (consult lsp-mode))

(use-package lsp-tailwindcss
  :after lsp-mode
  :vc (:url "https://github.com/merrickluo/lsp-tailwindcss"
            :branch "master")

  ;; :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  ;; lsp-tailwindcss-skip-config-check t)

  :config
  (add-to-list 'lsp-tailwindcss-major-modes 'elixir-ts-mode)
  (add-to-list 'lsp-tailwindcss-major-modes 'heex-ts-mode))

;;; Completion system

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Only list the commands of the current modes
  (when (boundp 'read-extended-command-predicate)
    (setq read-extended-command-predicate
          #'command-completion-default-include-p))

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package completion-preview
  :ensure nil
  :hook (prog-mode . completion-preview-mode))

;; For now disabled in flavor of completion-preview
;; (use-package corfu
;;   :defer nil
;;   :bind (:map corfu-map
;;               ("M-m" . ssbb/corfu-move-to-minibuffer)
;;               ("SPC" . corfu-insert-separator))
;;   :config
;;   (defun ssbb/corfu-move-to-minibuffer ()
;;     (interactive)
;;     (when completion-in-region--data
;;       (let ((completion-extra-properties corfu--extra)
;;             completion-cycle-threshold completion-cycling)
;;         (apply #'consult-completion-in-region completion-in-region--data))))

;;   ;; (setq corfu-cycle t
;;   ;;       corfu-auto t
;;   ;;       corfu-auto-prefix 2
;;   ;;       corfu-separator ?\s
;;   ;;       corfu-quit-no-match 'separator
;;   ;;       corfu-quit-at-boundary t
;;   ;;       corfu-preview-current 'insert
;;   ;;       corfu-preselect 'valid
;;   ;;       corfu-on-exact-match 'insert
;;   ;;       corfu-scroll-margin 5)

;;   (setq corfu-popupinfo-delay 0.5
;;         corfu-popupinfo-max-height 32)

;;   (global-corfu-mode)
;;   (corfu-popupinfo-mode)
;;   (corfu-history-mode))

;; (use-package nerd-icons-corfu
;;   :after corfu
;;   :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package consult
  :bind (("C-c i" . consult-imenu)
         ("C-x p s" . consult-ripgrep)
         ;; ("C-x p f" . consult-find)
	       ([remap goto-line] . consult-goto-line)
	       ([remap switch-to-buffer] . consult-buffer)
	       ([remap yank-pop] . consult-yank-pop)
	       ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
	       ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         )

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  (setq consult-ripgrep-args "rg --hidden --glob=!.git/* --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip")

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

)

(use-package vertico
  :config
  (vertico-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

;; Use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ([remap describe-bindings] . embark-bindings))

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (consult embark)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :bind (:map minibuffer-mode-map
              ("C-c C-o" . embark-export)))

(use-package consult-project-extra
  :after consult
  :bind (([remap project-find-file] . consult-project-extra-find)
         ("C-x p o" . consult-project-extra-find-other-window)))

(use-package consult-todo
  :after consult
  :bind (("C-c t" . consult-todo)))

;;; UI

;; Increase idle delay
(setq idle-update-delay 1.0)

;; Increase default frame size
(add-to-list 'initial-frame-alist '(height . 48))
(add-to-list 'initial-frame-alist '(width . 128))

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; Supress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Smooth scrolling
(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package posframe)

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode html-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :init
  (setq-default display-line-numbers-width 3))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1))

(use-package paren
  :ensure nil
  :config
  (show-paren-mode 1))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; Don't change font size on C-<scroll>
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

;; Don't prompt to load theme on first load.
(setq custom-safe-themes t)

(use-package modus-themes
  :demand t
  :ensure t
  :bind (("<f5>" . modus-themes-toggle))
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui nil
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-completions '((t . (extrabold)))
        modus-themes-prompts '(bold)
        modus-themes-to-toggle '(modus-operandi-tritanopia modus-vivendi-tritanopia))

  (setq modus-themes-common-palette-overrides
        `((fringe unspecified)

          ;; Make line numbers less intense and add a shade of cyan
          ;; for the current line number.
          (fg-line-number-inactive "gray50")
          (fg-line-number-active red-warmer)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)

          (bg-mode-line-active bg-dim)
          (fg-mode-line-active fg-dim)

          (bg-mode-line-inactive bg-inactive)
          (fg-mode-line-inactive fg-inactive)

          (border-mode-line-active bg-dim)
          (border-mode-line-inactive bg-dim)

          ;; Make the current line of `hl-line-mode' a fine shade of
          ;; gray (though also see my `lin' package).
          (bg-hl-line bg-dim)))

  (load-theme 'modus-operandi-tritanopia t)

  (defun my/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'modus-operandi-tritanopia t))
      ('dark (load-theme 'modus-vivendi-tritanopia t))))

  (add-hook 'ns-system-appearance-change-functions #'my/apply-theme))

(use-package nyan-mode
  :config
  (nyan-mode))

;; Use Iosevka font
(set-frame-font "Iosevka Curly 14" nil t)
(add-to-list 'default-frame-alist '(font . "Iosevka Curly 14"))

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package doom-modeline
  :config
  (setq doom-modeline-height 30)
  :init
  (doom-modeline-mode 1))

(use-package hide-mode-line
  :hook (((embark-collect-mode) . hide-mode-line-mode)))

(use-package nerd-icons)
(use-package nerd-icons-dired
  :hook ((dired-mode . nerd-icons-dired-mode)))

;;; VC

(setq vc-follow-symlinks nil)

(use-package magit
  :bind (("C-c v" . magit-status)
         ("C-x g" . magit-status)))

;;; Utils

(use-package helpful
  :bind
  (("C-c h f" . helpful-callable)
   ("C-c h v" . helpful-variable)
   ("C-c h x" . helpful-command)
   ("C-c h F" . helpful-function)
   ("C-c h k" . helpful-key)))

(use-package which-key
  :config
  (which-key-mode))

(use-package ag)
(use-package rg)

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(use-package asdf
  :vc (:url "https://github.com/tabfugnic/asdf.el"
            :branch "main")
  :config
  (asdf-enable))

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server))

(use-package eterm-256color)

(use-package vterm
  :custom
  ;; TODO breaks multi-vterm ability to toggle dedicated buffer
  ;; (vterm-buffer-name-string "*vterm* %s")
  (vterm-shell "fish")
  (vterm-term-environment-variable "eterm-color")
  (vterm-kill-buffer-on-exit)

  :config
  ;; Disable hl line mode to avoid cursor blinking.
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local global-hl-line-mode nil))))

(use-package multi-vterm
  :bind (("C-c d" . multi-vterm-dedicated-toggle)
         ("C-c v" . multi-vterm))
  :after vterm)

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("q" . (lambda () (interactive) (quit-window t))))  ;; kill dired bauffer on `q'
  :custom
  (dired-kill-when-opening-new-dired-buffer t))

(provide 'init)
;;; init.el ends here

;; Local Variables:
;; outline-minor-mode-cycle: t
;; outline-regexp: ";;; "
;; eval: (outline-minor-mode)
;; eval: (outline-hide-body)
;; End:
