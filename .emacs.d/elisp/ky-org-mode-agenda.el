;;; ky-org-mode-agenda.el --- org-mode agenda helper functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  chris

;; Author: chris <chris@chris-thinkpad>
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

(require 'org)
(require 'org-agenda)

(setq org-agenda-files-file (expand-file-name "~/Dropbox/org/agendafiles"))

(defun ky-reload-org-agenda-files ()
  (interactive)
  (setq org-agenda-files (remove ""
                                 (split-string (with-temp-buffer
                                                 (insert-file-contents org-agenda-files-file)
                                                 (buffer-string))
                                               "\n"))))

(defun ky-set-agenda-file (&optional path)
  (interactive)
  (unless path
    (if (string-equal (file-name-extension (buffer-file-name)) "org")
        (setq path (buffer-file-name))
      (user-error "Not in an org file")))
  (ky-reload-org-agenda-files)
  (unless (member (expand-file-name path) (mapcar (lambda (p)
                                                    (expand-file-name p))
                                                  org-agenda-files))
    (write-region (concat "\n" path) nil org-agenda-files-file 'append))
  (ky-reload-org-agenda-files))

(define-key org-mode-map (kbd "C-c a a") 'ky-set-agenda-file)


;; at startup reload the org agenda files
(ky-reload-org-agenda-files)

(provide 'ky-org-mode-agenda)
;;; ky-org-mode-agenda.el ends here
