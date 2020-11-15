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

;; Set line spacing
(setq-default line-spacing 0.35)
(setq-default org-cycle-separator-line 3)

;; Line numbers in all prod-modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Setup key binding for full screen
(define-key global-map (kbd "<s-return>") 'toggle-frame-fullscreen)

;; ========================================
;; Initialise package manager
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ---------------------------------------
;; Setup Ivy (auto-complete framework)
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))
;; --------------------------------------
;; Setup Doom Mode Line. It's the bar at
;; the bottom of the screen that shows
;; line number and file etc.
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
)
;; --------------------------------------
;; Set directory of backup and autosave
;; files to be /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; -------------------------------------
