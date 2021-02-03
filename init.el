(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(conda-anaconda-home "/Users/emil/Development/miniconda3")
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "8b58ef2d23b6d164988a607ee153fd2fa35ee33efc394281b1028c2797ddeebb" default))
 '(display-time-24hr-format nil)
 '(display-time-default-load-average nil)
 '(line-spacing 0.35)
 '(org-hide-emphasis-markers t)
 '(org-refile-targets '(("~/.org/todo.org" :level . 0)))
 '(package-selected-packages
   '(yasnippet multiple-cursors restclient elfeed which-key rainbow-delimiters counsel ivy-rich neotree iedit dockerfile-mode magit conda projectile exec-path-from-shell spacemacs-theme markdown-mode go-mode yaml-mode doom-modeline use-package ivy))
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

;; Load custom functions
(load-file "~/.emacs.d/func.el")

;; Load main configuration file
(load-file "~/.emacs.d/setup.el")

;; Load file extension modes
(load-file "~/.emacs.d/major-modes.el")

;; --------------------------------------------------
;; Setup done, prep for usage

;; Set emacs to full screen
(toggle-frame-fullscreen)

;; Open the daily.org and todo.org file
(find-file "~/.org/daily.org")
(find-file-other-window "~/.org/todo.org")
(other-window 1) ; prev line focuses new buffer, so move back
