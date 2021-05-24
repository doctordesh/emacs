;; --------------------------------------
;; Restclient (https://github.com/pashky/restclient.el)
(use-package restclient)

;; Adds a custom file extension
(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))

;; ----------------------------------------
;; Org-mode

;; Create links by id instead of 'file'. Makes destionation headings portable
;; and they can be changed and refiled.
(setq org-id-link-to-org-use-id t)

;; Nice looking bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Indents contents below a header to match header level
(add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))

;; Hides the **, //, __ when doing bold, italic
;; and underscore emphaisis
(setq org-hide-emphasis-markers 1)

;; Capture template
(setq org-capture-templates
      '(
	("t" "Todo" entry (file "~/.org/inbox.org")
         "* TODO %?\n:LOGBOOK:\n- Added at %U\n:END: ")
	("T" "Task" entry (file "~/.org/inbox.org")
	 "* TODO %?\nhttps://agile.maxiv.lu.se/project/vinhar-max-iv-machine-cs/us/%^{TaskID}\n:LOGBOOK:\n- Added at %U\n:END:")
	))

(define-key global-map (kbd "C-c c") 'org-capture)

;; Make sure that state transitions are logged in LOGBOOK drawer
(setq org-log-into-drawer 1)

;; Colors and faces
(setq org-todo-keyword-faces
      '(
	("PROJECT"     . (:background "#292B2E" :foreground "#BBBBBB" :weight bold))
	("NEXT"        . (:background "#292B2E" :foreground "#FA9746" :weight bold))
	("TODO"        . (:background "#292B2E" :foreground "#FA9746" :weight bold))
	("IN-PROGRESS" . (:background "#292B2E" :foreground "#259FA1" :weight bold))
	("ON-HOLD"     . (:background "#292B2E" :foreground "#C77150" :weight bold))
	("WAITING"     . (:background "#292B2E" :foreground "#C77150" :weight bold))
	("DONE"        . (:background "#292B2E" :foreground "#17BA0F" :weight bold))
	("REJECTED"    . (:background "#292B2E" :foreground "#C94D1C" :weight bold))
	))

;; Refile targets
(setq org-reverse-note-order t)
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets '(
			   ("~/.org/todo.org" :level . 0)
			   ("~/.org/archive.org" :level . 0)
			   ("~/.org/someday.org" :level . 0)
			   ))

;; --------------------------------------
;; YAML

(use-package yaml-mode)

;; Use yaml-mode when running *.yml files
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))


;; --------------------------------------
;; Golang

(use-package go-mode)

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

;; use goimports instead of gofmt
(setq gofmt-command "goimports")

;; Gofmt before save
(defun custom-go-mode-hook ()
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; Godef jump key binding
  (local-set-key (kbd "C-.") 'godef-jump)
  (local-set-key (kbd "C-,") 'pop-tag-mark)
  )
(remove-hook 'go-mode-hook 'custom-go-mode-hook)
(add-hook 'go-mode-hook 'custom-go-mode-hook)

;; ----------------------------------------
;; ;; lsp-mode
;; (use-package lsp-mode
;;   :ensure t
;;   :commands (lsp lsp-deferred)
;;   :hook ((python-mode) . lsp-deferred))

;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp)

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode
;;   :init
;; )

;; (defun lsp-go-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


;; --------------------------------------
;; Dockerfile
(use-package dockerfile-mode)

;; Adds a custom file extension
(add-to-list 'auto-mode-alist '("\\.docker\\'" . dockerfile-mode))


;; --------------------------------------
;; Docker (not Dockerfile)
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))


;; --------------------------------------
;; Markdown
(use-package markdown-mode)


;; --------------------------------------
;; Python

;; Regular python-mode is build-in

(use-package conda)

;; Set PYTHONPATH inside emacs
(setenv "PYTHONPATH" (shell-command-to-string "echo $PYTHONPATH"))


;; -------------------------------------
;; VTerm
(use-package vterm)
(custom-set-faces
 `(vterm-color-black   ((t (:foreground ,"#000000"))))
 `(vterm-color-white   ((t (:foreground ,"#F0F0F0"))))
 `(vterm-color-blue    ((t (:foreground ,"#6096D2"))))
 `(vterm-color-cyan    ((t (:foreground ,"#2DACBA"))))
 `(vterm-color-magenta ((t (:foreground ,"#C983BB"))))
 `(vterm-color-green   ((t (:foreground ,"#2B896A"))))
 `(vterm-color-red     ((t (:foreground ,"#C94D1C"))))
 `(vterm-color-yellow  ((t (:foreground ,"#FBB13C"))))
 )
