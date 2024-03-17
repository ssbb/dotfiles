;;; init-elixir.el -*- lexical-binding: t -*-
;;; Code:

;; Elixir formatting done via LSP in init-lsp.el

;; (use-package elixir-ts-mode)

;; (use-package heex-ts-mode
;;   :vc (:url "https://github.com/ssbb/heex-ts-mode"))

;; (add-to-list 'load-path "/Users/ssbb/Workspace/heex-ts-mode")
;;(require 'heex-ts-mode)

(defun my/elixir-mod-name ()
  ;; (interactive)
  (file-name-base (buffer-file-name)))

(provide 'init-elixir)

;;; init-elixir.el ends here
