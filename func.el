;; --------------------------------------------------
;; Custom functions
;; --------------------------------------------------

(defun er/date ()
 "Prints todays date and time in ER format"
 (interactive)
 (insert (format-time-string "%a, %Y-%m-%d: %H:")))

(defun er/backup ()
  "Backup org and emacs files"
  (interactive)
  (start-process-shell-command "backup" "*Messages*" "sh ~/bin/backup.sh"))


(defun er/clone (repo)
  "Clone repo from KITS SW Gitlab and put it in the right place"
  (interactive "sWhich repo? (gitlab.maxiv.lu.se/kits-maxiv/ ")
  (setq repo-url (format "git@gitlab.maxiv.lu.se:kits-maxiv/%s" repo))
  (setq download-dir (expand-file-name (format "~/Development/%s" repo)))
  (setq command (format "git clone %s %s" repo-url download-dir))
  (setq buffer (generate-new-buffer (format "*clone %s*" repo-url)))
  (message "Repo: %s" repo-url)
  (message download-dir)
  (switch-to-buffer buffer)
  (start-process-shell-command "clone" buffer command)
  (projectile-switch-project)
  )
