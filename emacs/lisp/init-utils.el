;;; init-utils.el --- Utils -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package which-key
  :config
  (which-key-mode))

(use-package ag)

(use-package asdf
  :vc (:fetcher github :repo tabfugnic/asdf.el)
  :config
  (asdf-enable))

(use-package disk-usage)
(use-package memory-usage)

(use-package chatgpt-shell
  ;; :custom
  ;; ((chatgpt-shell-openai-key
  ;;   (lambda ()
  ;;     (auth-source-pass-get 'secret "openai-key")))))
  )

(provide 'init-utils)

;;; init-utils.el ends here
