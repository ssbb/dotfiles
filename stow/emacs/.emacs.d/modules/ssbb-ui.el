;;; ssbb-ui.el --- UI configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq custom-safe-themes t
      use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      frame-inhibit-implied-resize t  ;; Don't resize frame based on font (for exwm especially)
      idle-update-delay 1.0)

(setq split-width-threshold 160
      split-height-threshold nil)

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; FIXME Move to some per-system variables?
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :font "Iosevka Curly" :height 140)
  (set-face-attribute 'default nil :font "Iosevka Curly" :height 100))

;; FIXME conflicts with desktop-save mode.
;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook))

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1))

(use-package paren
  :ensure nil
  :config
  (show-paren-mode 1))

(use-package posframe)

(use-package doom-modeline
  :custom
  (doom-modeline-height (round (* (frame-char-height) 1.5)))
  (doom-modeline-battery nil)
  (doom-modeline-time nil)
  :init
  (doom-modeline-mode 1))

(use-package hide-mode-line
  :hook (((embark-collect-mode) . hide-mode-line-mode)))

(use-package nerd-icons)
;; (use-package nerd-icons-dired
;;   :hook ((dired-mode . nerd-icons-dired-mode)))

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

  (load-theme 'modus-operandi-tritanopia t))

(use-package auto-dark
  :if (eq window-system 'x)
  :custom
  (auto-dark-themes '((modus-vivendi-tritanopia)
                      (modus-operandi-tritanopia)))
  :init
  (auto-dark-mode))

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

(use-package which-key
  :config
  (which-key-mode))

(use-package spacious-padding
  :config
  (setq spacious-padding-widths
        '(:internal-border-width 15
          :header-line-width 4
          :mode-line-width 6
          :custom-button-width 3
          :tab-width 4
          :right-divider-width 30
          :scroll-bar-width 8
          :fringe-width 8))

  (setq spacious-padding-subtle-mode-line t)

  (spacious-padding-mode t))

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-tab-hints t)
  (tab-bar-select-tab-modifiers '(super))
  :config
  (add-to-list 'tab-bar-format 'tab-bar-format-align-right t)
  (add-to-list 'tab-bar-format 'tab-bar-format-global t)

  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

(provide 'ssbb-ui)
;;; ssbb-ui.el ends here
