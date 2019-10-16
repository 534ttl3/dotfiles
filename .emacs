(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; according to http://wikemacs.org/wiki/User's_Initialization_File#With_org-mode
(require 'org)
(require 'ob-tangle)
(message "one")
(org-babel-load-file (expand-file-name "~/.emacs.d/dotemacs.org"))
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.

;;  '(package-selected-packages
;;    (quote
;;     (elscreen-tab elscreen golden-ratio org-noter interleave flash-region multiple-cursors auto-dim-other-buffers tabbar free-keys helm-mode klin elisp-slime-nav org-elisp-help beacon smooth-scrolling origami shell-pop darkroom hide-mode-line neotree dired-sidebar py-autopep8 flycheck elpy material-theme multi-term centered-window org-ref org-download transpose-frame evil-collection evil org-pdfview auctex-lua auctex-latexmk auctex yasnippet linum-relative exec-path-from-shell projectile use-package))))
