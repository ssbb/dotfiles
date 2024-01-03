;;; init-ui.el -*- lexical-binding: t -*-

;; Increase idle delay
(setq idle-update-delay 1.0)

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

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

(use-package display-line-numbers
  :straight nil
  :hook ((prog-mode html-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :init
  (setq-default display-line-numbers-width 3))


;; Make certain buffers grossly incandescent
(use-package solaire-mode
  :hook (after-load-theme . solaire-global-mode))

(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui nil
        modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-completions '((t . (extrabold)))
        modus-themes-prompts '(bold))

  (setq modus-themes-common-palette-overrides
        '((fringe unspecified)

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

  (setq modus-vivendi-palette-overrides
        '((comment fg-dim)
          (constant red-faint)
          (fnname slate)
          (keyword magenta-faint)
          (preprocessor magenta-faint)
          (docstring fg-dim)
          (string olive)
          (type yellow-warmer)
          (variable red-warmer)))

  (load-theme 'modus-vivendi t)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(use-package doom-themes)
(use-package gruvbox-theme)
(use-package color-theme-sanityinc-tomorrow)

;; (use-package doom-themes)
;; (use-package ef-themes)

;; (use-package color-theme-sanityinc-tomorrow
;;   :config
;;   (load-theme 'sanityinc-tomorrow-night))

;; (use-package gruvbox-theme)
;; (use-package darktooth-theme)

(use-package posframe)

(use-package doom-modeline
  :config
  (setq doom-modeline-height 30)

  :init
  (doom-modeline-mode 1))

(use-package nerd-icons)
(use-package hide-mode-line
  :hook (((embark-collect-mode) . hide-mode-line-mode)))

;; Don't prompt to load theme on first load.
(setq custom-safe-themes t)

;; Use Iosevka font
(set-frame-font "Iosevka 13" nil t)
;; (set-frame-font "Iosevka Comfy 13" nil t)
(add-to-list 'default-frame-alist '(font . "Iosevka 13"))

(use-package nyan-mode
  :config
  (nyan-mode))

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

(provide 'init-ui)

;;; init-ui.el ends here
