(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(conda-anaconda-home "/Users/emil/Development/miniconda3")
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "8b58ef2d23b6d164988a607ee153fd2fa35ee33efc394281b1028c2797ddeebb" default))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(projectile-ripgrep ripgrep epresent go-playground internal window\.el window-system window docker vterm json-mode lsp-ui company-lsp lsp-mode company yasnippet multiple-cursors restclient elfeed which-key rainbow-delimiters counsel ivy-rich neotree iedit dockerfile-mode magit conda projectile exec-path-from-shell spacemacs-theme markdown-mode go-mode yaml-mode doom-modeline use-package ivy))
 '(spacemacs-theme-custom-colors nil))

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
(toggle-frame-maximized)

;; Open the daily.org and todo.org file
(find-file "~/.org/daily.org")
(emacs-lock-mode)

(split-window-right)
(other-window 1)
(find-file "~/.org/todo.org")
(emacs-lock-mode)

(split-window-below)
(other-window 1)
(vterm)
(rename-buffer "*cli*")
(emacs-lock-mode)

(other-window 1) ; prev line focuses new buffer, so move back
;; --------------------------------------------------

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(vterm-color-black ((t (:foreground "#000000"))))
 '(vterm-color-blue ((t (:foreground "#6096D2"))))
 '(vterm-color-cyan ((t (:foreground "#2DACBA"))))
 '(vterm-color-green ((t (:foreground "#2B896A"))))
 '(vterm-color-magenta ((t (:foreground "#C983BB"))))
 '(vterm-color-red ((t (:foreground "#C94D1C"))))
 '(vterm-color-white ((t (:foreground "#F0F0F0"))))
 '(vterm-color-yellow ((t (:foreground "#FBB13C")))))
