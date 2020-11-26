(defun er/cudate ()
 "Prints todays date and time in ER format"
 (interactive)
 (insert (format-time-string "%a, %Y-%m-%d: %H:")))
