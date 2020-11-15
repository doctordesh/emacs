;; Use yaml-mode when running *.yml files
(use-package yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; --------------------------------------
;; Golang

;; Setup PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; Set my path as gopath
(setenv "GOPATH" "/Users/emil/Development/go")

;; Gofmt before save
(defun custom-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding
  (local-set-key (kbd "C-.") 'godef-jump)
  (local-set-key (kbd "C-,") 'pop-tag-mark)
  )
(add-hook 'go-mode-hook 'custom-go-mode-hook)
