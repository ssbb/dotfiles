;;; meow-state-bridge.el --- Bridge Meow state to external processes -*- lexical-binding: t -*-

;; Author: Sviatoslav Bulbakha
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (meow "1.0"))

;;; Commentary:

;; This package provides a bridge between Meow modal states and external processes.
;; It tracks whether the user is in normal/motion mode vs insert mode and sends
;; updates to a Python process via stdin.

;;; Code:

(require 'meow)

(defvar meow-state-bridge--current-state nil
  "Current state sent to external process.
Either `normal' or `insert'.")

(defvar meow-state-bridge--buffer-name "*meow-state-bridge*"
  "Name of the buffer for process output and debug messages.")

(defvar meow-state-bridge--buffer-name "*meow-state-bridge*"
  "Name of the buffer for process output and debug messages.")

(defvar meow-state-bridge-script-path nil
  "Path to the Python script to run.
Set this before starting the process.")

(defvar meow-state-bridge-command "python"
  "Python command to use (e.g., `python', `python3', or full path).")

(defvar meow-state-bridge-debug nil
  "Enable debug logging.")

(defun meow-state-bridge--log (format-string &rest args)
  "Log a debug message if debug mode is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (when meow-state-bridge-debug
    (let ((message (apply #'format format-string args))
          (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
      (with-current-buffer (get-buffer-create meow-state-bridge--buffer-name)
        (goto-char (point-max))
        (insert (format "[%s] %s\n" timestamp message))))))

(defun meow-state-bridge--get-effective-state ()
  "Determine the effective state based on current context."
  (when (frame-focus-state)
    (meow--current-state)))

(defun meow-state-bridge--notify (state)
  "Notify external program of STATE (`normal' or `insert')."
  (let ((payload (format "mode %s" (symbol-name state)))
        (proc (get-buffer-process meow-state-bridge--buffer-name)))
    (if (and proc (process-live-p proc))
        (progn
          (meow-state-bridge--log "Sending payload %s" payload)
          (process-send-string proc (concat payload "\n"))))))

(defun meow-state-bridge--update (&rest _ignored)
  "Update and send state if it has changed.
This function is called by various hooks and ignores all arguments."
  ;; Check it's an active visible buffer (and not eg eldoc).
  (when (eq (current-buffer) (window-buffer (selected-window)))
    (let ((new-state (meow-state-bridge--get-effective-state)))
      ;; Ignore if state not changed.
      (when (not (eq new-state meow-state-bridge--current-state))
        (meow-state-bridge--log "State changed: %s -> %s"
                                meow-state-bridge--current-state new-state)
        (setq meow-state-bridge--current-state new-state)
        (meow-state-bridge--notify new-state)))))

(defun meow-state-bridge--process-filter (proc string)
  "Handle output from the Python process."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert string))))

(defun meow-state-bridge--start-process ()
  "Start the external Python process."
  (let ((buffer (get-buffer-create meow-state-bridge--buffer-name)))
    (with-current-buffer buffer
      (when-let ((proc (get-buffer-process buffer)))
        (delete-process proc))
      (erase-buffer)
      (let ((proc (start-file-process "meow-state-bridge"
                                      buffer
                                      meow-state-bridge-command
                                      "-u"
                                      (expand-file-name meow-state-bridge-script-path))))
        (unless proc
          (error "Failed to start meow-state-bridge process"))

        ;; Configure process
        (set-process-filter proc #'meow-state-bridge--process-filter)
        (set-process-query-on-exit-flag proc nil))
      (display-buffer buffer))))

(defun meow-state-bridge--stop-process ()
  "Stop the external Python process."
  (let ((proc (get-buffer-process meow-state-bridge--buffer-name)))
    (meow-state-bridge--log "Stopping process")
    (delete-process proc)
    (setq meow-state-bridge--current-state nil)))


;;; Hooks

(defun meow-state-bridge--install-hooks ()
  "Install all necessary hooks for state tracking."
  ;; Meow state changes
  (add-hook 'meow-switch-state-hook #'meow-state-bridge--update)

  ;; Buffer and window changes
  (add-hook 'window-selection-change-functions #'meow-state-bridge--update)
  (add-hook 'buffer-list-update-hook #'meow-state-bridge--update)

  ;; Frame focus changes
  (add-function :after after-focus-change-function #'meow-state-bridge--update))

(defun meow-state-bridge--remove-hooks ()
  "Remove all hooks installed for state tracking."
  ;; Meow state changes
  (remove-hook 'meow-switch-state-hook #'meow-state-bridge--update)

  ;; Buffer and window changes
  (remove-hook 'window-selection-change-functions #'meow-state-bridge--update)
  (remove-hook 'buffer-list-update-hook #'meow-state-bridge--update)

  ;; Frame focus changes
  (remove-function after-focus-change-function #'meow-state-bridge--update))

;;;###autoload
(define-minor-mode meow-state-bridge-mode
  "Bridge Meow state to external processes.
When enabled, tracks whether the user is in normal/motion mode
vs insert mode and sends updates to a Python process."
  :global t
  :group 'meow
  (if meow-state-bridge-mode
      (progn
        (meow-state-bridge--start-process)
        (meow-state-bridge--install-hooks)
        (meow-state-bridge--update))
    (meow-state-bridge--remove-hooks)
    (meow-state-bridge--stop-process)))

(provide 'meow-state-bridge)

;;; meow-state-bridge.el ends here
