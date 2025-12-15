;;; ssbb-ui.el --- UI configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq custom-safe-themes t
      use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      frame-inhibit-implied-resize t  ;; Don't resize frame based on font (for exwm especially)
      idle-update-delay 1.0)

(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(menu-bar-mode -1)

(setq split-width-threshold 160
      split-height-threshold nil)

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; FIXME Move to some per-system variables?
(if ssbb/guix-system-p
    (set-face-attribute 'default nil :font "Iosevka Curly" :height 100)
  (progn
    (set-frame-font "Iosevka Curly 14" nil t)
    (add-to-list 'default-frame-alist '(font . "Iosevka Curly 14"))))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode html-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-width 3))

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

(use-package posframe)

(use-package nyan-mode
  :config
  (nyan-mode))

(use-package doom-modeline
  :custom
  (doom-modeline-height (round (* (frame-char-height) 1.5)))
  :init
  (doom-modeline-mode 1))

(use-package hide-mode-line
  :hook (((embark-collect-mode) . hide-mode-line-mode)))

(use-package nerd-icons)
(use-package nerd-icons-dired
  :hook ((dired-mode . nerd-icons-dired-mode)))

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

(provide 'ssbb-ui)
;;; ssbb-ui.el ends here
