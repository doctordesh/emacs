;; Removes default keys because they are not in use, because Ergodox ROCKS
(global-unset-key (kbd "C-v")); end-of-buffer
(global-unset-key (kbd "C-a")); move-beginning-of-line
(global-unset-key (kbd "C-e")); move-end-of-line
(global-unset-key (kbd "C-f")); forward-char
(global-unset-key (kbd "C-b")); backward-char
(global-unset-key (kbd "C-d")); delete-char
(global-unset-key (kbd "C-n")); next-line
(global-unset-key (kbd "C-p")); previous-line
(global-unset-key (kbd "C-q")); quoted-insert
(global-unset-key (kbd "C-t")); transpose-chars
(global-unset-key (kbd "C-z")); suspend-frame
(global-unset-key (kbd "C-s")); search-forward
(global-unset-key (kbd "C-r")); search-backward,
(global-unset-key (kbd "C-w")); kill-region (same as cmd-x) - cut
(global-unset-key (kbd "M-w")); kill-ring-save (same as cmd-c) - copy
(global-unset-key (kbd "C-x C-c")); exit emacs (use cmd-q instead)
