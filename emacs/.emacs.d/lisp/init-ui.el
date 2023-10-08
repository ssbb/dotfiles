;;; init-ui.el -*- lexical-binding: t -*-
;;; Code:

;; Increase idle delay
(setq idle-update-delay 1.0)

;; Increase default frame size
(add-to-list 'initial-frame-alist '(height . 48))
(add-to-list 'initial-frame-alist '(width . 128))

;; Supress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t)

;; Disable toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Disable scrollbar
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Smooth scroll
(pixel-scroll-precision-mode t)

;; Make certain buffers grossly incandescent
(use-package solaire-mode
  :hook (after-load-theme . solaire-global-mode))

(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui nil
        modus-themes-bold-constructs t
        modus-themes-completions '((t . (extrabold)))
        modus-themes-prompts nil)

  ;; (setq modus-themes-common-palette-overrides
  ;;       '((fringe unspecified)
  ;;         ;; Make line numbers less intense and add a shade of cyan
  ;;         ;; for the current line number.
  ;;         (fg-line-number-inactive "gray50")
  ;;         (fg-line-number-active cyan-cooler)
  ;;         (bg-line-number-inactive unspecified)
  ;;         (bg-line-number-active unspecified)

  ;;         ;; Make the current line of `hl-line-mode' a fine shade of
  ;;         ;; gray (though also see my `lin' package).
  ;;         (bg-hl-line bg-dim)

  ;;         ;; Make the active mode line a fine shade of lavender
  ;;         ;; (purple) and tone down the gray of the inactive mode
  ;;         ;; lines.
  ;;         (bg-mode-line-active bg-cyan-nuanced)
  ;;         (border-mode-line-active bg-cyan-nuanced)

  ;;         (bg-mode-line-inactive bg-dim)
  ;;         (border-mode-line-inactive bg-inactive)))

  ;; (setq modus-themes-common-palette-overrides
  ;;     '((builtin magenta-faint)
  ;;       (comment yellow-faint)
  ;;       (constant red-cooler)
  ;;       (fnname yellow-warmer)
  ;;       (keyword olive)
  ;;       (preprocessor green-warmer)
  ;;       (docstring cyan-faint)
  ;;       (string yellow-warmer)
  ;;       (type slate)
  ;;       (variable yellow-warmer)))


  (load-theme 'modus-vivendi t)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(use-package doom-themes)
(use-package ef-themes)
(use-package color-theme-sanityinc-tomorrow)
(use-package gruvbox-theme)
(use-package darktooth-theme)

(use-package posframe)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

(use-package nerd-icons)
(use-package hide-mode-line
  :hook (((embark-collect-mode) . hide-mode-line-mode)))

;; Don't prompt to load theme on first load.
(setq custom-safe-themes t)

;; Use Iosevka font
(set-frame-font "Iosevka 13" nil t)
(add-to-list 'default-frame-alist '(font . "Iosevka 13"))

(provide 'init-ui)

;;; init-ui.el ends here
