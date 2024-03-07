;;; init-prog.el -*- lexical-binding: t -*-
;;; Code:

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package treesit-auto
  :init
  (setq treesit-auto-install 'prompt)

  :config
  (global-treesit-auto-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package apheleia
  :hook ((js-ts-mode . apheleia-mode)
         (c-ts-mode . apheleia-mode)
         (elm-mode . apheleia-mode))
  :config
  (add-to-list 'apheleia-mode-alist '(heex-ts-mode . mix-format))

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

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package consult-flycheck
  :after (consult flycheck)
  :defer nil
  :bind (("C-c !" . consult-flycheck)))

;; (use-package outline
;;   :ensure nil
;;   :hook (prog-mode . outline-minor-mode))

;; (use-package outshine
;;   :hook (outline-minor-mode . outshine-mode))

(provide 'init-prog)

;;; init-prog.el ends here
