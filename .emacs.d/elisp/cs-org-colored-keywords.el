;;; cs-org-colored-keywords.el --- colored keywords in latex export from org  -*- lexical-binding: t; -*-

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

(defun org-latex-format-headline-colored-keywords-function (todo todo-type priority text tags info)
  (concat (cond
           ((string= todo "TODO")
            (and todo
                 (format "{\\color{red}\\bfseries\\sffamily %s} "
                         todo)))
           ((string= todo "DONE")
            (and todo
                 (format "{\\color{green}\\bfseries\\sffamily %s} "
                         todo))))
          (and priority
               (format "\\framebox{\\#%c} " priority))
          text
          (and tags
               (format "\\hfill{}\\textsc{%s}"
                       (mapconcat (lambda (tag)
                                    (org-latex-plain-text tag info))
                                  tags
                                  ":")))))

(setq org-latex-format-headline-function 'org-latex-format-headline-colored-keywords-function)

;; the default actually is this:
;; (defun org-latex-format-headline-default-function
;;     (todo _todo-type priority text tags _info)
;;   "Default format function for a headline.
;; See `org-latex-format-headline-function' for details."
;;   (concat
;;    (and todo (format "{\\bfseries\\sffamily %s} " todo))
;;    (and priority (format "\\framebox{\\#%c} " priority))
;;    text
;;    (and tags
;; 	(format "\\hfill{}\\textsc{%s}"
;; 		(mapconcat #'org-latex--protect-text tags ":")))))

(provide 'cs-org-colored-keywords)
;;; cs-org-colored-keywords.el ends here
