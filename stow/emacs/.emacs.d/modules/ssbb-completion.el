;;; ssbb-completion.el --- Completion configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq completion-cycle-threshold 3  ;; TAB cycle if there are only few candidates
      tab-always-indend 'complete)

;; Only list the commands of the current modes
(when (boundp 'read-extended-command-predicate)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p))

(use-package completion-preview
  :ensure nil
  :hook (prog-mode . completion-preview-mode)
  :bind (:map completion-preview-active-mode-map
              ("M-n" . completion-preview-next-candidate)
              ("M-p" . completion-preview-prev-candidate)
              ("M-i" . completion-preview-insert))
  :custom
  (completion-preview-minimum-symbol-length 2))

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


(provide 'ssbb-completion)
;;; ssbb-completion.el ends here
