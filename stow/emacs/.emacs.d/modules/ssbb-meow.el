;;; ssbb-meow.el --- Meow modal configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package meow
  :demand t
  :custom
  (meow-selection-command-fallback
   '((meow-change . meow-change-char)
     (meow-kill . meow-C-k)
     (meow-save . kill-ring-save)
     (meow-cancel-selection . keyboard-quit)
     (meow-pop-selection . meow-pop-grab)
     (meow-beacon-change . meow-beacon-change-char)
     (meow-expand . meow-digit-argument)

     ;; Fallback replace to yank.
     (meow-replace . meow-yank)
     (meow-replace-pop . meow-yank-pop)))

  (meow-use-clipboard t)
  (meow-char-thing-table
   '((?p . round)
     (?b . square)
     (?f . curly)
     (?g . angle)
     (?s . string)
     (?r . line)
     (?t . buffer)))

  (meow-visit-sanitize-completion t)
  (meow-keypad-self-insert-undefined nil)
  (meow-keypad-start-keys '((?c . ?c) (?x . ?x)))

  :config
  (setq meow--kbd-backward-line "<up>"
        meow--kbd-forward-line  "<down>"
        meow--kbd-backward-char "<left>"
        meow--kbd-forward-char  "<right>")

  (setq meow-normal-state-keymap (make-sparse-keymap))
  (setq meow-leader-state-keymap (make-sparse-keymap))

  (setf (alist-get 'normal meow-keymap-alist) meow-normal-state-keymap)
  (setf (alist-get 'leader meow-keymap-alist) meow-leader-state-keymap)

  (meow-define-keys 'leader
    (cons "p" project-prefix-map)
    '("z" . meow-universal-argument)
    '("l" . consult-goto-line)
    '("q" . kill-buffer)
    '("b" . switch-to-buffer)
    '("w" . ace-window)
    '("n" . comment-dwim)
    '("." . treemacs-select-window)
    '("z" . meow-universal-argument)
    '(",n" . "H-n")
    '(",a" . "H-a")
    '(",u" . "H-u")
    '(",e" . "H-e")
    )

  (meow-define-keys 'motion
    '("n" . meow-left)
    '("a" . meow-right)
    '("u" . meow-prev)
    '("e" . meow-next))

  (meow-define-keys 'normal
    '("0" . meow-expand-0)
    '("1" . meow-expand-1)
    '("2" . meow-expand-2)
    '("3" . meow-expand-3)
    '("4" . meow-expand-4)
    '("5" . meow-expand-5)
    '("6" . meow-expand-6)
    '("7" . meow-expand-7)
    '("8" . meow-expand-8)
    '("9" . meow-expand-9)

    '("a"  . meow-right)
    '("A" .  meow-right-expand)
    '("b"  . meow-join)
    '("c"  . meow-append)
    '("C" .  meow-open-below)
    '("d"  . meow-change)
    '("D" .  ssbb/meow-change-line)
    '("e"  . meow-next)
    '("E" .  meow-next-expand)
    '("f"  . meow-back-word)
    '("F" .  meow-back-symbol)
    '("g"  . meow-replace)
    '("G" .  meow-replace-pop)
    '("h"  . meow-undo)
    '("H" .  undo-redo)
    '("i" .  meow-delete)
    '("k"  . meow-till)
    '("K" .  meow-find)
    '("l"  . meow-insert)
    '("L" .  meow-open-above)
    '("m"  . avy-goto-char)
    '("n"  . meow-left)
    '("N" .  meow-left-expand)
    '("o"  . meow-next-word)
    '("O" .  meow-next-symbol)
    '("p"  . er/expand-region)
    '("P"  . ssbb/shrink-region)
    '("q"  . execute-extended-command)
    '("r"  . meow-kill)
    '("R" .  meow-kill-whole-line)
    '("s"  . meow-mark-word)
    '("S" .  meow-mark-symbol)
    '("t"  . meow-line)
    '("u"  . meow-prev)
    '("U" .  meow-prev-expand)
    '("v"  . meow-quit)
    '("w"  . meow-grab)
    '("W" .  meow-pop-grab)
    '("x"  . meow-save)
    '("X" .  ssbb/meow-save-clipboard)
    '("y"  . meow-reverse)
    '("z"  . meow-reverse)

    '("'"  . consult-line)
    '(","  . meow-beginning-of-thing)
    '("."  . meow-end-of-thing)
    '("<" . meow-inner-of-thing)
    '("""" . meow-bounds-of-thing)

    '("<escape>" . meow-cancel-selection))

  (defun ssbb/meow-save-clipboard ()
    "Copy in clipboard."
    (interactive)
    (let ((meow-use-clipboard t))
      (meow-save)))

  (defun ssbb/meow-change-line ()
    "Kill till end of line and switch to INSERT state."
    (interactive)
    (meow--cancel-selection)
    (meow-end-of-thing
     (car (rassoc 'line meow-char-thing-table)))
    (meow-change))

  (defun ssbb/shrink-region ()
    (interactive)
    (er/expand-region -1))

  (defun ssbb/meow-hide-cursor ()
    (progn
      (meow--set-cursor-type nil)))

  (add-to-list 'meow-update-cursor-functions-alist
               '((lambda () (and (meow-motion-mode-p)
                                 (eq major-mode 'vundo-mode)))
                 . ssbb/meow-hide-cursor))

  ;; Allow meow-keypad to take inputs when it's started.
  (add-hook 'meow-keypad-mode-hook
            (lambda ()
              (when (derived-mode-p 'exwm-mode)
                (if meow-keypad-mode
                    (exwm-input-grab-keyboard exwm--id)
                  (exwm-input-release-keyboard exwm--id)))))

  (with-eval-after-load 'meow
    (push '(vterm-mode . insert) meow-mode-state-list)
    (add-hook 'vterm-mode-hook
              (lambda ()
                (add-hook 'meow-insert-enter-hook
                          (lambda () (vterm-copy-mode -1))
                          nil t)
                (add-hook 'meow-insert-exit-hook
                          (lambda () (vterm-copy-mode 1))
                          nil t))))

  (require 'meow)
  (meow-global-mode t))

;; (use-package meow-state-bridge
;;   :after meow
;;   :load-path "~/.emacs.d/site-lisp/"
;;   :hook (meow-global-mode . meow-state-bridge-toggle)
;;   :init
;;   (setq meow-state-bridge-script-path "/Users/ssbb/bridge.py"
;;         meow-state-bridge-debug t)
;;   :config
;;   (defun meow-state-bridge-toggle ()
;;     (meow-state-bridge-mode (if meow-global-mode 1 -1))))

(provide 'ssbb-meow)
;;; ssbb-meow.el ends here
