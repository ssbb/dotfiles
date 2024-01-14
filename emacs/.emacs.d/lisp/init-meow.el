;;; init-meow.el -*- lexical-binding: t -*-
;;; Code:

(use-package meow
  :demand t
  :bind (:map meow-keypad-state-keymap
              ("<XF86Launch9>" . meow-keypad-self-insert))

  :config
  (setq meow-char-thing-table
        ;; rofnd/square/curly/angle things maps to my keyboard SYM layer
        '((?p . round)
          (?b . square)
          (?f . curly)
          (?g . angle)
          (?s . string)
          (?l . line)
          (?/ . buffer)))

  (setq meow--kbd-backward-line  "<up>"
        meow--kbd-forward-line   "<down>"
        meow--kbd-backward-char  "<left>"
        meow--kbd-forward-char  "<right>")

  (setq meow-visit-sanitize-completion t
        meow-keypad-self-insert-undefined nil
        meow-keypad-start-keys '((?c . ?c) (?x . ?x))
        meow-keypad-leader-dispatch "C-c"
        meow-use-clipboard t)

  (meow-define-keys 'leader
    (cons "p" project-prefix-map)
    '("b" . switch-to-buffer)
    '("o" . ace-window))

  (meow-define-keys 'motion
    '("n" . meow-left)
    '("i" . meow-right)
    '("u" . meow-prev)
    '("e" . meow-next)
    '("<espace>" . ignore))

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

    '("a" . meow-mark-word)
    '("A" . meow-mark-symbol)
    '("b" . er/expand-region)
    '("B" . (lambda ()
              (interactive)
              (er/expand-region -1)))
    '("c" . meow-save)
    ;; '("d" . meow-indent)
    '("e" . meow-next)
    '("E" . meow-next-expand)
    '("f" . meow-search)
    '("g" . meow-cancel-selection)
    '("G" . meow-grab)
    '("h" . meow-undo)
    '("H" . vundo)
    '("i" . meow-right)
    '("I" . meow-right-expand)
    '("j" . meow-join)
    '("k" . meow-kill-whole-line)
    '("l" . meow-back-word)
    '("L" . meow-back-symbol)
    '("m" . meow-line)
    '("M" . meow-goto-line)
    '("n" . meow-left)
    '("N" . meow-left-expand)
    '("o" . meow-delete)
    '("p" . meow-till)
    '("P" . meow-yank-pop)
    '("q" . meow-quit)
    '("Q" . kill-current-buffer)
    '("r" . meow-insert)
    '("R" . meow-open-above)
    '("s" . meow-change)
    '("t" . meow-append)
    '("T" . meow-open-below)
    '("u" . meow-prev)
    '("U" . meow-prev-expand)
    '("v" . meow-yank)
    '("V" . meow-yank-pop)
    '("w" . meow-find)
    '("x" . meow-kill)
    '("y" . meow-next-word)
    '("Y" . meow-next-symbol)
    '("z" . meow-replace)
    ;; '("Z" . meow-swap-grab)

    ;; '("ds" . meow-swap-grab)
    ;; '("dd" . meow-sync-grab)

    '("'" . meow-reverse)
    '("/" . consult-line)
    '("?" . meow-visit)

    '(";" . evilnc-comment-or-uncomment-lines)

    '("[" . indent-rigidly-left-to-tab-stop)
    '("]" . indent-rigidly-right-to-tab-stop)

    '("," . meow-beginning-of-thing)
    '("." . meow-end-of-thing)
    '("<" . meow-inner-of-thing)
    '(">" . meow-bounds-of-thing)

    '("<escape>" . ignore)
    )

  ;; (add-to-list 'meow-mode-state-list '(git-commit-mode . insert))

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
  (meow-global-mode 1))

(provide 'init-meow)

;;; init-meow.el ends here
