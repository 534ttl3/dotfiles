;;; cs-desktop-plus.el --- configuration for desktop+  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  chris

;; Author: chris <chris@chris-lenovo>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'desktop+)

(defconst cs-cloud-dir (expand-file-name "~/Dropbox/"))
(defconst cs-cloud-linkedapps-dir (concat cs-cloud-dir "1LinkedApps/"))
(defconst cs-cloud-emacs-dir (concat cs-cloud-linkedapps-dir "emacs/"))
(defconst cs-cloud-emacs-desktops-dir (concat cs-cloud-emacs-dir "desktops/"))

;; the given directory should be a link to dropbox
;; with a central managed git repository

(defconst cs-emacsd-desktops-dir "~/.emacs.d/desktops/")
(if (file-directory-p cs-emacsd-desktops-dir)
    (progn (message (format "%s exists" cs-emacsd-desktops-dir)))
  (progn (message (format "%s doesn't exit, linking it to %s" cs-emacsd-desktops-dir cs-cloud-emacs-desktops-dir))
         (shell-command-to-string
          (format "ln -s %s %s" cs-cloud-emacs-desktops-dir (file-name-directory (directory-file-name cs-emacsd-desktops-dir))))))

(setq desktop+-base-dir cs-emacsd-desktops-dir)

;; ;; BEGIN remember last session
;; (defun read-lines (filePath)
;;   "Return a list of lines of a file at filePath."
;;   (with-temp-buffer
;;     (insert-file-contents filePath)
;;     (split-string (buffer-string) "\n" t)))
;;
;; (setq last-session-file-name ".lastsessionname")
;;
;; (defun load-last-session ()
;;   (interactive)
;;   (desktop+-load (nth 0 (read-lines last-session-file-name)))
;;   )

;; ;; (add-hook 'kill-emacs-hook
;; ;; 	  '(lambda ()
;; ;; 	     (write-region (file-name-nondirectory (directory-file-name desktop-dirname)) nil last-session-file-name))
;; ;; 	  )
;; ;;
;; ;; (global-set-key (kbd "C-c C-l C-l") 'load-last-session)
;; ;; ;; END remember last session

(provide 'cs-desktop-plus)
;;; cs-desktop-plus.el ends here
