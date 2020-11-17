;; Beginning of emacs config
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
(load-theme 'spacemacs-dark)

;; Show column position in file
(column-number-mode 1)

;; Setup keybindings for home and end
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)

;; More keybindings
(define-key global-map (kbd "C-f") 'find-file)
(define-key global-map (kbd "C-b") 'ivy-switch-buffer)

;; Use shift up-down-left-right to move between windows
(windmove-default-keybindings)

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

;; --------------------------------------
;; Setup Counsel
(use-package counsel
  :diminish counsel-mode
  :init
  (counsel-mode 1))

;; --------------------------------------
;; Setup Ivy (auto-complete framework)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper))
  :config
  (ivy-mode 1))

;; --------------------------------------
;; Setup Doom Mode Line. It's the bar at
;; the bottom of the screen that shows
;; line number and file etc.
(use-package doom-modeline
  :ensure t
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
;; ivy-rich supplies more information in ivy listings
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))


;; -------------------------------------
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Development/kits-projects")
    (setq projectile-project-search-path '("~/Development/kits-projects")))
  (setq projecttile-switch-project-action #'projectile-dired)
  )
