;;; ssbb-edit.el --- Edit system configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

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

(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 2
              indent-tabs-mode nil)

(setq save-interprogram-paste-before-kill t)

(use-package avy
  :bind (("C-:" . avy-goto-char))
  :custom
  (avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
  (avy-all-windows nil))

(use-package so-long
  :config
  (global-so-long-mode 1))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

;; Delete selection on insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package sudo-edit)

(use-package vundo
  :bind (("C-x u" . vundo))
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package simple
  :ensure nil
  :hook ((prog-mode markdown-mode conf-mode) . ssbb/enable-trailing-whitespace)
  :init
  (defun ssbb/enable-trailing-whitespace ()
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
              ("<escape>" . ssbb/tempel-done))

  :init
  (defun ssbb/tempel-done ()
    (interactive)
    (tempel-done)
    (meow-insert-exit))

  ;; Setup completion at point
  (defun ssbb/tempel-setup-capf ()
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

  (add-hook 'conf-mode-hook 'ssbb/tempel-setup-capf)
  (add-hook 'prog-mode-hook 'ssbb/tempel-setup-capf)
  (add-hook 'text-mode-hook 'ssbb/tempel-setup-capf)

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

(provide 'ssbb-edit)
;;; ssbb-edit.el ends here
