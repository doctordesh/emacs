;; --------------------------------------------------
;; Custom functions
;; --------------------------------------------------

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

(defun er/day ()
    "Insert the daily template"
    (interactive)
    (insert (concat
	     "** "
	     (format-time-string "%a, %Y-%m-%d: %H:")
	     "\n"
	     "*** Meta [0/3]\n"
	     "- [ ] Read mail and create inbox items for each thing\n"
	     "- [ ] Plan the day. Look up what should be done according to previous\n"
	     "lists and add things to the 'Task' header\n"
	     "- [ ] Write down achievements of the day\n"
             "*** Tasks [%]\n"
             "Add tasks here about what should be done today. Add links to todos etc\n"
             "- [ ] ...\n"
             "**** Unplanned\n"
             "*** Achievements\n"
             "Add a comment at the end of the day about what was achieved, good and bad.\n"
             "Or insights, or thoughts, or something else\n"
	     ))
    )
