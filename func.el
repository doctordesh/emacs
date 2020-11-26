(defun er/cudate ()
 "Prints todays date and time in ER format"
 (interactive)
 (insert (format-time-string "%a, %Y-%m-%d: %H:")))

(defun er/backup ()
  "Backup org and emacs files"
  (interactive)
  (shell-command "sh ~/bin/backup.sh"))

(defun er/backup ()
  "alskdjf"
  (interactive)
  (start-process-shell-command "backup" nil "sh ~/bin/backup.sh"))
