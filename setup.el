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
(use-package exec-path-from-shell) ; A GNU Emacs library to ensure environment variables inside Emacs look the same as in the user's shell.

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

;; Show column position in file
(column-number-mode 1)

;; Set line spacing
(setq-default line-spacing 0.35)
(setq-default org-cycle-separator-line 3)

;; Line numbers in all prod-modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Setup key binding for full screen
(define-key global-map (kbd "<s-return>") 'toggle-frame-fullscreen)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; -------------------------------------
;; Personal key bindings

;; Setup keybindings for home and end
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)

;; More keybinding(define-key global-map (kbd "C-f") 'find-file)
(define-key global-map (kbd "C-b") 'ivy-switch-buffer)
(define-key global-map (kbd "C-s") 'save-buffer)
(define-key global-map (kbd "C-r") 'query-replace)
(define-key global-map (kbd "C-R") 'query-replace-regexp)
(define-key global-map (kbd "C-x K") 'ibuffer)

;; Use cmd up-down-left-right to move between windows
(windmove-default-keybindings 'super)

;; --------------------------------------
;; Setup Ivy (auto-complete framework)
(use-package ivy
  :diminish
  :bind (("C-w" . swiper)) ; swiper is better search
  :config
  (ivy-mode 1))

;; --------------------------------------
;; Setup Counsel (Ivy-enhanced versions
;; of common Emacs commands)
(use-package counsel
  :diminish counsel-mode
  :init
  (counsel-mode 1))

;; -------------------------------------
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
  )

(define-key global-map (kbd "s-P") 'projectile-switch-project)
(define-key global-map (kbd "s-p") 'projectile-find-file)


;; --------------------------------------
;; Magit (Maahhh git!)
(use-package magit)


;; --------------------------------------
;; Start emacs server deamon
(server-start)


;; --------------------------------------
;; Set emacs to max frame
(toggle-frame-maximized)


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

(defun er/open-setup ()
    "Open setup"
    (interactive)
    (find-file "~/.emacs.d/setup.el"))

(global-set-key (kbd "C-c f i") 'er/open-inbox)
(global-set-key (kbd "C-c f d") 'er/open-daily)
(global-set-key (kbd "C-c f t") 'er/open-todo)
(global-set-key (kbd "C-c f n") 'er/open-notes)
(global-set-key (kbd "C-c f m") 'er/open-meetings)
(global-set-key (kbd "C-c f s") 'er/open-setup)
