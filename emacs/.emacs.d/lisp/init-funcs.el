;;; init-funcs.el -*- lexical-binding: t -*-
;;; Code:

(defun ssbb/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun ssbb/move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun ssbb/move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(provide 'init-funcs)

;;; init-funcs.el ends here
