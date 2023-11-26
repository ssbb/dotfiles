;;; init-ui.el -*- lexical-binding: t -*-

;; Increase idle delay
(setq idle-update-delay 1.0)


(setq split-width-threshold 160
      split-height-threshold nil)

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
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-completions '((t . (extrabold)))
        modus-themes-prompts nil)

  (setq modus-themes-common-palette-overrides
        '((fringe unspecified)

          ;; Make line numbers less intense and add a shade of cyan
          ;; for the current line number.
          (fg-line-number-inactive "gray50")
          (fg-line-number-active red-warmer)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)

          ;; Make the current line of `hl-line-mode' a fine shade of
          ;; gray (though also see my `lin' package).
          (bg-hl-line bg-dim)

          ;; Tomorrow Theme
          (builtin blue-faint)
          (comment fg-dim)
          (constant red-faint)
          (fnname blue-faint)
          (keyword magenta-faint)
          (preprocessor magenta-faint)
          (docstring fg-dim)
          (string olive)
          (type yellow-warmer)
          (variable red-warmer)))

  (load-theme 'modus-vivendi)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;; (use-package doom-themes)
;; (use-package ef-themes)

;; (use-package color-theme-sanityinc-tomorrow
;;   :config
;;   (load-theme 'sanityinc-tomorrow-night))

;; (use-package gruvbox-theme)
;; (use-package darktooth-theme)

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
