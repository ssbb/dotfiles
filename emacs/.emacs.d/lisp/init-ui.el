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

(use-package modus-themes
  ;; :straight (:host github :repo "protesilaos/modus-themes")
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

          ;; ;; Mimic Gruvbox
          ;; (builtin red-faint)
          ;; (comment fg-dim)
          ;; (constant maroon)
          ;; (fnname olive)
          ;; (keyword red)
          ;; (preprocessor fg-main)
          ;; (docstring fg-dim)
          ;; (string olive)
          ;; (type yellow-warmer)
          ;; (variable slate)

          ;; ;; Mmimic Tomorrow theme. It's the same for both Day/Night variants.
          ;; (builtin blue-faint)
          ;; (comment fg-dim)
          ;; (constant red-faint)
          ;; (fnname slate)
          ;; (keyword magenta-faint)
          ;; (preprocessor magenta-faint)
          ;; (docstring fg-dim)
          ;; (string olive)
          ;; (type yellow-warmer)
          ;; (variable red)

          ;; Make the current line of `hl-line-mode' a fine shade of
          ;; gray (though also see my `lin' package).
          (bg-hl-line bg-dim)))
  ;; ,@modus-themes-preset-overrides-faint))

  ;; (setq modus-vivendi-palette-overrides
  ;;       '((fg-main "#ebdbb2")))
  ;; (setq modus-operandi-palette-overrides
  ;;       '((fg-main "#282828")))

  (load-theme 'modus-operandi-tritanopia t)

  ;; (defun my/apply-theme (appearance)
  ;;   "Load theme, taking current system APPEARANCE into consideration."
  ;;   (mapc #'disable-theme custom-enabled-themes)
  ;;   (pcase appearance
  ;;     ('light (load-theme 'modus-operandi t))
  ;;     ('dark (load-theme 'modus-vivendi t))))

  ;; (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
  )

;; User themes should live in .emacs.d/themes, not ~/.emacs.d
(setq custom-theme-directory (concat user-emacs-directory "themes/"))

;; Third party themes add themselves to `custom-theme-load-path', but the themes
;; living in $DOOMDIR/themes should always have priority.
(setq custom-theme-load-path
      (cons 'custom-theme-directory
            (delq 'custom-theme-directory custom-theme-load-path)))

(use-package ef-themes)
(use-package doom-themes)
(use-package doom-themes
  :config
  ;; (require 'my-gruvbox-dark-theme)

  (setq doom-themes-padded-modeline -1)
  ;; (load-theme 'my-gruvbox-dark t)

  ;; (doom-themes-visual-bell-config)
  ;; (doom-themes-treemacs-config)
  ;; (doom-themes-org-config))
  )
(use-package gruvbox-theme)
(use-package base16-theme)

;; (use-package spacious-padding)
;; (use-package color-theme-sanityinc-tomorrow)

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
(set-frame-font "Iosevka 14" nil t)
(add-to-list 'default-frame-alist '(font . "Iosevka 14"))

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
