;; --------------------------------------------------
;; Custom functions
;; --------------------------------------------------

(defun er/backup ()
  "Backup org and emacs files"
  (interactive)
  (start-process-shell-command "backup" "*Messages*" "sh ~/.emacs.d/backup.sh"))


(defun er/clone (repo)
  "Clone repo from KITS SW Gitlab and put it in the right place"
  (interactive "sWhich repo? (gitlab.maxiv.lu.se/kits-maxiv/ ")
  (setq repo-url (format "git@gitlab.maxiv.lu.se:kits-maxiv/%s" repo))
  (setq download-dir (expand-file-name (format "~/Development/%s" repo)))
  (setq command (format "git clone %s %s" repo-url download-dir))
  (message "Repo: %s" repo-url)
  (message download-dir)
  (call-process "git" nil nil nil "clone" repo-url download-dir)
  (projectile-add-known-project download-dir)
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
	   "- [ ] Sort inbox items to either todo, someday or trash\n"
	   "- [ ] Plan the day and add things to the 'Task' header\n"
	   "  - [ ] Check left over tasks from yesterday\n"
	   "  - [ ] Check Calendar for any meetings\n"
	   "  - [ ] Check 'todo' for good tasks\n"
	   "  - [ ] Check sprint\n"
	   "- [ ] Write down thoughts of the day\n"
           "*** Tasks [%]\n"
           "Add tasks here about what should be done today. Add links to todos etc\n"
           "- [ ] ...\n"
           "**** Unplanned\n"
           "*** Achievements\n"
           "Add a comment at the end of the day about what was achieved, good and bad.\n"
           "Or insights, or thoughts, or something else\n"
	   ))
  )

(defun er/ksync ()
  "Uses 'ksync' to sync to kitslab"
  (interactive)
  (start-process-shell-command "ksync" "*Messages*" "cd ~/Development && ksync")
  )
