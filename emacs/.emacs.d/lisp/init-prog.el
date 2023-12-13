;;; init-prog.el -*- lexical-binding: t -*-
;;; Code:

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

;; (use-package eldoc-box
;;   :hook ((emacs-lisp-mode . eldoc-box-hover-at-point-mode)
;;          (eglot-managed-mode . eldoc-box-hover-at-point-mode)))

(use-package indent-bars
  :straight (:host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode))

;; (use-package combobulate
;;   :vc (:fetcher github :repo "mickeynp/combobulate")
;;   :preface
;;   (setq combobulate-key-prefix "C-c o")

;;   :hook ((python-ts-mode . combobulate-mode)
;;          (js-ts-mode . combobulate-mode)
;;          (css-ts-mode . combobulate-mode)
;;          (yaml-ts-mode . combobulate-mode)
;;          (typescript-ts-mode . combobulate-mode)
;;          (tsx-ts-mode . combobulate-mode)))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package consult-flycheck
  :after (consult flycheck)
  :defer nil
  :bind (("C-c e" . consult-flycheck)))

(provide 'init-prog)

;;; init-prog.el ends here
