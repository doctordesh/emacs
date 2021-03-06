;; -------------------------------------
;; Initialise package manager
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;; -------------------------------------
;; Package installs
(use-package exec-path-from-shell) ; A GNU Emacs library to ensure
				   ; environment variables inside
				   ; Emacs look the same as in the
				   ; user's shell.


;; -------------------------------------
;; Basic config
(setq inhibit-startup-message t)

;; Disable menu bar, tool bar, scroll bar (vertical and horizontal)
(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; Set theme
(package-install 'spacemacs-theme)
(load-theme 'spacemacs-dark)

;; Set ~/Development as default directory
(setq default-directory "~/Development/")

;; Show column position in file
(column-number-mode 1)

;; Set line spacing
(setq-default line-spacing 0.35)
(setq-default org-cycle-separator-line 3)

;; Line numbers in all prod-modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Highlight line in all prod-modes
(add-hook 'prog-mode-hook 'hl-line-mode)

;; Delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-quit)

;; Show time
(display-time-mode 1)
(setq display-time-24hr-format 1)
(setq display-time-default-load-average nil)

;; Don't ask yes or no, but y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; When text is marked, it gets deleted when overwritten
(delete-selection-mode 1)

;; Error when scroll hits top
(setq scroll-error-top-bottom t)

;; Line length before break for fill-paragraph (M-q)
(setq-default fill-column 80)

;; ----------------------------------------
;; Start emacs server deamon
(server-start)


;; ----------------------------------------
;; Window placement

(use-package emacs
  :custom
  (display-buffer-alist))

;; this works, but i'm not sure how I want it.
;; (use-package emacs
;;   :custom
;;   (display-buffer-alist '(
;; 			  (
;; 			   "\\*Help\\*"
;; 			   (display-buffer-in-side-window)
;; 			   (window-height . 0.25)
;; 			   (side . bottom)
;; 			   (slot . 0)
;; 			   )

;; 			  (
;; 			   "\\*vterm-cli\\*"
;; 			   (display-buffer-in-side-window)
;; 			   (window-height . 0.25)
;; 			   (side . bottom)
;; 			   (slot . 1)
;; 			  )
;; 			  )))

;; ----------------------------------------
;; Setup Ivy (auto-complete framework)
(use-package ivy
  :diminish
  :bind (("C-w" . swiper)) ; swiper is better search
  :config
  (ivy-mode 1))

;; ----------------------------------------
;; Setup Counsel (Ivy-enhanced versions
;; of common Emacs commands)
(use-package counsel
  :diminish counsel-mode
  :init
  (counsel-mode 1))


;; ---------------------------------------
;; ivy-rich supplies more information in ivy listings
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))


;; --------------------------------------
;; Setup Doom Mode Line. It's the bar at
;; the bottom of the screen that shows
;; line number and file etc.
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (
	   (doom-modeline-height 40)))

;; Hide modification icon
(setq doom-modeline-buffer-modification-icon nil)


;; --------------------------------------
;; Set directory of backup and autosave
;; files to be /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; --------------------------------------
;; Setup rainbow delimiters
;; Colors partner delimiters in the same color
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; --------------------------------------
;; Which-key, helps with alternatives to key-bindings
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))


;; -------------------------------------
;; Projectile - project management tool
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  (add-to-list 'projectile-globally-ignored-directories "*__pycache__")
  (add-to-list 'projectile-globally-ignored-file-suffixes "pyc")
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Development")
    (setq projectile-project-search-path '("~/Development")))
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-globally-ignored-directories
   '(".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".ccls-cache" ".clangd" "__pycache__" "*__pycache__"))
  )


;; --------------------------------------
;; Magit (Maahhh git!)
(use-package magit)


;; --------------------------------------
;; JSON mode
(use-package json-mode)


;; --------------------------------------
;; Bind keys to files for easy access
(defun er/open-inbox ()
    "Open inbox"
    (interactive)
    (find-file "~/.org/inbox.org"))

(defun er/open-daily ()
    "Open daily"
    (interactive)
    (find-file "~/.org/daily.org"))

(defun er/open-todo ()
    "Open todo"
    (interactive)
    (find-file "~/.org/todo.org"))

(defun er/open-notes ()
    "Open notes"
    (interactive)
    (find-file "~/.org/notes.org"))

(defun er/open-meetings ()
    "Open meetings"
    (interactive)
    (find-file "~/.org/meetings.org"))

(defun er/open-someday ()
    "Open meetings"
    (interactive)
    (find-file "~/.org/someday.org"))

(defun er/open-setup ()
    "Open setup"
    (interactive)
    (find-file "~/.emacs.d/setup.el"))

(global-set-key (kbd "C-c f i") 'er/open-inbox)
(global-set-key (kbd "C-c f d") 'er/open-daily)
(global-set-key (kbd "C-c f t") 'er/open-todo)
(global-set-key (kbd "C-c f n") 'er/open-notes)
(global-set-key (kbd "C-c f m") 'er/open-meetings)
(global-set-key (kbd "C-c f y") 'er/open-someday)
(global-set-key (kbd "C-c f s") 'er/open-setup)

;; ----------------------------------------
;; dired
(use-package dired
  :ensure nil
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
          insert-directory-program "/usr/local/bin/gls"))
  :custom
  (dired-listing-switches "-aBhgo --group-directories-first"))

;; ----------------------------------------
;; company (in-buffer completion)
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  )
(add-hook 'prog-mode-hook 'company-mode)


;; ----------------------------------------
;; yasnippet
(use-package yasnippet)
(setq yas-snippet-dirs '(
			 "~/.emacs.d/snippets"
			 ))
(yas-global-mode 1)

;; ----------------------------------------
;; elfeed (RSS reader)

(use-package elfeed)
(setq elfeed-feeds
      '(
	("https://www.joelonsoftware.com/rss" software)
	("http://feeds.feedburner.com/typepad/krisdedecker/lowtechmagazineenglish" tech)
	("https://www.slowernews.com/rss.xml" news)
	))


;; ----------------------------------------
;; multiple cursors
(use-package multiple-cursors)
(global-set-key (kbd "C-:") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(define-key mc/keymap (kbd "<return>") nil)

;; ----------------------------------------
;; Personal key bindings

;; Setup key binding for full screen
(define-key global-map (kbd "<s-return>") 'toggle-frame-maximized)

;; Setup keybindings for home and end
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)

;; More keybinding
(define-key global-map (kbd "C-f") 'find-file)
(define-key global-map (kbd "C-S-f") 'projectile-find-file)
(define-key global-map (kbd "C-b") 'ivy-switch-buffer)
(define-key global-map (kbd "C-S-b") 'projectile-switch-to-buffer)
(define-key global-map (kbd "C-s") 'save-buffer)
(define-key global-map (kbd "C-r") 'query-replace)
(define-key global-map (kbd "C-S-r") 'query-replace-regexp)
(define-key global-map (kbd "C-x K") 'ibuffer)
(define-key global-map (kbd "s-w") 'delete-window)
(define-key global-map (kbd "s-W") 'delete-other-windows)
(define-key global-map (kbd "C-d") 'dired-other-window)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "s-1") 'other-frame)
(define-key global-map (kbd "s-!") 'er/prev-frame)

(define-key global-map (kbd "<C-s-left>") 'shrink-window-horizontally)
(define-key global-map (kbd "<C-s-right>") 'enlarge-window-horizontally)
(define-key global-map (kbd "<C-s-down>") 'shrink-window)
(define-key global-map (kbd "<C-s-up>") 'enlarge-window)

(define-key global-map (kbd "s-P") 'projectile-switch-project)
(define-key global-map (kbd "s-p") 'projectile-find-file)

;; Use cmd up-down-left-right to move between windows
(windmove-default-keybindings 'super)

;; Keybindings for custom functions
(global-set-key (kbd "C-S-d") 'er/duplicate-line)
(global-set-key (kbd "C-S-k") 'er/move-line-up)
(global-set-key (kbd "C-S-j") 'er/move-line-down)
(global-set-key (kbd "<M-up>") 'er/upper)
(global-set-key (kbd "<M-down>") 'er/downer)
(global-set-key (kbd "C-c t") 'er/vterm)

;; Split window into different 'types' of buffers
(global-unset-key (kbd "s-d")) ; remove otherwise can't use as leader key
(global-set-key (kbd "s-d d") 'er/split-window-right)
(global-set-key (kbd "s-d f") 'er/file-split-window-right)
(global-set-key (kbd "s-d t") 'er/vterm-split-window-right)
(global-set-key (kbd "s-d b") 'er/buffer-split-window-right)
(global-set-key (kbd "s-d p") 'er/project-split-window-right)

(global-unset-key (kbd "s-D")) ; remove otherwise can't use as leader key
(global-set-key (kbd "s-D d") 'er/split-window-below)
(global-set-key (kbd "s-D f") 'er/file-split-window-below)
(global-set-key (kbd "s-D t") 'er/vterm-split-window-below)
(global-set-key (kbd "s-D b") 'er/buffer-split-window-below)
(global-set-key (kbd "s-D p") 'er/project-split-window-below)

;; New frame
(global-set-key (kbd "s-n") 'er/make-frame)

;; ----------------------------------------
(use-package tramp)
(setq tramp-default-method "ssh")


;; ----------------------------------------
;; Ripgrep
(use-package ripgrep)
(use-package projectile-ripgrep)


;; ----------------------------------------
;; Setup ksync save hook
(add-hook 'after-save-hook 'er/ksync)
