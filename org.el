;; Nice looking bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Indents contents below a header to match header level
(add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
