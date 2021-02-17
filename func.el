;; --------------------------------------------------
;; Custom functions
;; --------------------------------------------------

(defun er/date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d"))
  )

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
	   "*** Meta\n"
	   "**** Prepare the day [/]\n"
	   "- [ ] Read previous days reflections\n"
	   "- [ ] Read mail and create inbox items for each thing\n"
	   "- [ ] Sort inbox items to either todo, someday or trash\n"
	   "- [ ] Plan the day and add things to the 'Task' header\n"
	   "  - [ ] Check left over tasks from yesterday\n"
	   "  - [ ] Check Calendar for any meetings\n"
	   "  - [ ] Check 'todo' for good tasks\n"
	   "  - [ ] Check sprint\n"
	   "**** Reflection and follow-up [/]\n"
	   "- [ ] Check of the Focus scale\n"
	   "- [ ] Write down thoughts of the day\n"
           "*** Tasks [%]\n"
           "Add tasks here about what should be done today. Add links to todos etc\n"
           "- [ ] ...\n"
           "**** Unplanned\n"
	   "*** Focus scale [0%]\n"
	   "- [ ] No wasted time on YouTube\n"
	   "- [ ] No wasted time on Reddit\n"
	   "- [ ] No wasted time on Facebook\n"
           "*** Reflections\n"
           "Add a comment at the end of the day about what was achieved, good and bad.\n"
           "Or insights, or thoughts, or something else\n"
	   ))
  )

(defun er/ksync ()
  "Uses 'ksync' to sync to kitslab"
  (interactive)
  (start-process-shell-command "ksync" "*Messages*" "cd ~/Development && ksync")
  )

(defun er/duplicate-line ()
  "Duplicates current line"
  (interactive)
  (save-mark-and-excursion
    (beginning-of-line)
    (insert (thing-at-point 'line t))))

(defun er/move-line-down ()
  "Move current line down"
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun er/move-line-up ()
  "Move current line up"
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (forward-line -2)
    (move-to-column col)))

(defun er/upper ()
  "Move up 3 lines"
  (interactive)
  (previous-line 3)
  )

(defun er/downer ()
  "Move down 3 lines"
  (interactive)
  (next-line 3)
  )
