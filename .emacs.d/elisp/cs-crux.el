;;; cs-crux.el --- my ridiculously useful functions  -*- lexical-binding: t; -*-
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

(defun cs-move-to-beginning-of-visual-line ()
    ""
    (interactive)
    (let* (pos-first-non-ws-char-in-cur-vis-line)
      (save-excursion
        (beginning-of-visual-line)
        (setq pos-first-non-ws-char-in-cur-vis-line
              (if (string-match "\s" (string (char-after (point))))
                  (search-forward-regexp "\s+"
                                         (save-excursion
                                           (end-of-visual-line)
                                           (point))
                                         t)
                (point))))
      (if (equal pos-first-non-ws-char-in-cur-vis-line (point))
          (beginning-of-visual-line)
        (goto-char pos-first-non-ws-char-in-cur-vis-line))))

(global-set-key (kbd "C-a") #'cs-move-to-beginning-of-visual-line)

(provide 'cs-crux)
;;; cs-crux.el ends here
