(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(conda-anaconda-home "/Users/emil/Development/miniconda3")
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "8b58ef2d23b6d164988a607ee153fd2fa35ee33efc394281b1028c2797ddeebb" default))
 '(line-spacing 0.35)
 '(package-selected-packages
   '(neotree iedit dockerfile-mode magit conda projectile exec-path-from-shell spacemacs-theme markdown-mode go-mode yaml-mode doom-modeline use-package ivy))
 '(projectile-globally-ignored-directories
   '(".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".ccls-cache" ".clangd" "__pycache__" "*__pycache__"))
 '(projectile-indexing-method 'hybrid)
 '(scroll-error-top-bottom t)
 '(spacemacs-theme-custom-colors nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Remove default key bindings
(load-file "~/.emacs.d/unbind.el")

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
