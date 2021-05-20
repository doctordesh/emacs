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

(defun er/proj (repo)
  "Create new directory with git and add to projetile"
  (interactive "sName: ")
  (setq dir (format "~/Development/%s" repo))
  (make-directory dir)
  (message "Repo: %s" dir)
  (shell-command (format "git init %s" dir))
  (projectile-add-known-project dir)
  (projectile-switch-project)
  )

(defun er/vterm (key)
  "Open a new vterm with a name"
  (interactive "sName: *vterm-[cli] ")
  (if (equal "" key) (setq key "cli"))
  (setq buffer-name (format "*vterm-%s*" key))
  (vterm buffer-name)
  )

(defun er/day ()
  "Insert the daily template"
  (interactive)
  (insert (concat
	   "** "
	   (format-time-string "%a, %Y-%m-%d")
	   "\n"
	   ":WORKHOURS:\n"
	   (format-time-string "%H:")
	   "\n"
	   ":END:\n"
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
	   "- [ ] Fill in hours\n"
	   "- [ ] Write down thoughts of the day\n"
           "*** Tasks [%]\n"
           "Add tasks here about what should be done today. Add links to todos etc\n"
           "- [ ] ...\n"
           "**** Unplanned\n"
           "*** Reflections\n"
           "Add a comment at the end of the day about what was achieved, good and bad.\n"
           "Or insights, or thoughts, or something else\n"
	   "**** Focus\n"
	   "**** Thoughts\n"
	   ))
  (previous-line 24)
  (move-end-of-line 1)
  )

(defun er/ksync ()
  "Uses 'ksync' to sync to kitslab"
  (interactive)
  (start-process-shell-command "ksync" "*Messages*" "cd ~/Development && ksync")
  (message "ksynced")
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

(defun er/prev-frame ()
  "Previous frame"
  (interactive)
  (other-frame -1)
  )

;; --------------------------------------------------
;; Split window functions
;; Four flavours
;; 1. Split and then open file (f)
;; 2. Split and then open vterm (t)
;; 3. Split and then open buffer (b)
;; 4. Split and then do 'default' (d)
;; --------------------------------------------------

(defun er/split-window-right ()
  "Split window to the right and move to new window"
  (interactive)
  (split-window-right)
  (other-window 1)
  )

(defun er/file-split-window-right ()
  "Split and open file"
  (interactive)
  (split-window-right)
  (other-window 1)
  (projectile-find-file)
  )

(defun er/vterm-split-window-right (key)
  "Split and open vterm"
  (interactive "sName: *vterm-[cli] ")
  (if (equal "" key) (setq key "cli"))
  (split-window-right)
  (other-window 1)
  (er/vterm key)
  )

(defun er/buffer-split-window-right ()
  "Split and open buffer"
  (interactive)
  (split-window-right)
  (other-window 1)
  (ivy-switch-buffer)
  )

(defun er/split-window-below ()
  "Split window below and move to new window"
  (interactive)
  (split-window-below)
  (other-window 1)
  )

(defun er/file-split-window-below ()
  "Split and open file"
  (interactive)
  (split-window-below)
  (other-window 1)
  (projectile-find-file)
  )

(defun er/vterm-split-window-below (key)
  "Split and open vterm"
  (interactive "sName: *vterm-[cli] ")
  (if (equal "" key) (setq key "cli"))
  (split-window-below)
  (other-window 1)
  (er/vterm key)
  )

(defun er/buffer-split-window-below ()
  "Split and open buffer"
  (interactive)
  (split-window-below)
  (other-window 1)
  (ivy-switch-buffer)
  )

;; --------------------------------------------------
;; Frame
;; --------------------------------------------------

(defun er/make-frame ()
  "New frame"
  (interactive)
  (make-frame)
  (toggle-frame-maximized)
  )
