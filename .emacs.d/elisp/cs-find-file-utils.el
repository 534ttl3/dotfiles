;;; cs-find-file-utils.el --- Utils for find-file    -*- lexical-binding: t; -*-

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

(defun cs-find-file-from-clipboard ()
  (interactive)
  (let* ((clipboard-contents (with-temp-buffer
                               (yank)
                               (buffer-string))))
    (if (file-exists-p clipboard-contents)
        (counsel-find-file clipboard-contents)
      (user-error "Clipbord does not contain file name that exists."))))

(global-set-key (kbd "C-M-, f") 'cs-find-file-from-clipboard)

(provide 'cs-find-file-utils)
;;; cs-find-file-utils.el ends here
