(defvar consult--source-desktop-apps
  `(:name "Desktop Apps"
          :narrow ?a
          :category file
          :face consult-file
          :history file-name-history
          :items ,#'consult--desktop-apps-items
          :action ,#'consult--desktop-app-launch))

(defun consult--desktop-apps-dirs ()
  "Return list of directories containing .desktop files."
  (let ((xdg-data-dirs (or (getenv "XDG_DATA_DIRS")
                           "/usr/local/share:/usr/share"))
        (xdg-data-home (or (getenv "XDG_DATA_HOME")
                           (expand-file-name "~/.local/share"))))
    (mapcar (lambda (dir) (expand-file-name "applications" dir))
            (cons xdg-data-home (split-string xdg-data-dirs ":")))))

(defun consult--desktop-apps-items ()
  "Return list of desktop application names."
  (let (apps)
    (dolist (dir (consult--desktop-apps-dirs))
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t "\\.desktop$"))
          (when-let* ((name (consult--desktop-app-name file)))
            (push (propertize name 'desktop-file file) apps)))))
    (delete-dups (nreverse apps))))

(defun consult--desktop-app-name (file)
  "Extract Name= from desktop FILE, skip hidden entries."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((hidden (or (re-search-forward "^NoDisplay=true" nil t)
                      (progn (goto-char (point-min))
                             (re-search-forward "^Hidden=true" nil t)))))
      (unless hidden
        (goto-char (point-min))
        (when (re-search-forward "^Name=\\(.+\\)$" nil t)
          (match-string 1))))))

(defun consult--desktop-app-launch (app)
  "Launch desktop APP."
  (let* ((file (get-text-property 0 'desktop-file app))
         (exec (consult--desktop-app-exec file)))
    (when exec
      (start-process-shell-command app nil exec))))

(defun consult--desktop-app-exec (file)
  "Extract Exec= from desktop FILE, cleaning up field codes."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward "^Exec=\\(.+\\)$" nil t)
      ;; Remove %f %F %u %U etc. field codes
      (replace-regexp-in-string " ?%[fFuUdDnNickvm]" "" (match-string 1)))))

;;;###autoload

(defun consult-desktop-app ()
  "Launch a desktop application."
  (interactive)
  (consult--multi (list consult--source-desktop-apps)
                  :prompt "App: "
                  :sort nil))

(provide 'consult-desktop-app)
