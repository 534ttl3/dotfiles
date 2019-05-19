
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; according to http://wikemacs.org/wiki/User's_Initialization_File#With_org-mode
(require 'org)
(require 'ob-tangle)
(message "one")
(org-babel-load-file (expand-file-name "~/.emacs.d/myemacs.org"))
