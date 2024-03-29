;;; init-tempel.el --- Tempel setup  -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(use-package tempel
  ;; :custom
  ;; (tempel-trigger-prefix "<")
  :bind (:map tempel-map
              ("TAB" . tempel-next)
              ("<escape>" . my/tempel-done))

  :init
  (defun my/tempel-done ()
    (interactive)
    (tempel-done)
    (meow-insert-exit))

  ;; Setup completion at point
  (defun my/tempel-setup-capf ()
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

  (add-hook 'conf-mode-hook 'my/tempel-setup-capf)
  (add-hook 'prog-mode-hook 'my/tempel-setup-capf)
  (add-hook 'text-mode-hook 'my/tempel-setup-capf)

  ;; return tempel-expand as #1 function in capf after lsp-mode wipes it.
  (add-hook 'lsp-completion-mode-hook 'my/tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode))
  )

(use-package tempel-collection)

(provide 'init-tempel)

;;; init-tempel.el ends here
