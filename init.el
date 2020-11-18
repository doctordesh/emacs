(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "8b58ef2d23b6d164988a607ee153fd2fa35ee33efc394281b1028c2797ddeebb" default))
 '(package-selected-packages
   '(magit conda projectile exec-path-from-shell spacemacs-theme markdown-mode go-mode yaml-mode doom-modeline use-package ivy))
 '(scroll-error-top-bottom t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Load manually downloaded plugins
(add-to-list 'load-path "~/.emacs.d/packages")

;; Load main configuration file
(load-file "~/.emacs.d/setup.el")

;; Load main configuration for org-mode
(load-file "~/.emacs.d/org.el")

;; Load file extension modes
(load-file "~/.emacs.d/file-modes.el")

;; Load custom functions
(load-file "~/.emacs.d/func.el")
