(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   (quote
    ("43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" default)))
 '(helm-ff-auto-update-Zinitial-value t)
 '(helm-ff-auto-update-initial-value t)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(org-export-show-temporary-export-buffer t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-startup-truncated t)
 '(package-selected-packages
   (quote
    (doom-modeline doom-themes magit counsel swiper framemove rotate which-key guide-key elscreen org-noter multiple-cursors ws-butler use-package transpose-frame tabbar stickyfunc-enhance srefactor shell-pop py-autopep8 projectile origami org-ref org-pdfview org-elisp-help org-download neotree multi-term material-theme linum-relative ialign hide-mode-line helm-gtags golden-ratio ggtags function-args free-keys flycheck flash-region evil-paredit evil-collection evil-cleverparens elpy elisp-slime-nav desktop+ darkroom crux company-c-headers centered-window beacon auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-page 'disabled nil)
