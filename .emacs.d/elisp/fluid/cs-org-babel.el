;;; cs-org-babel.el --- org babel settings           -*- lexical-binding: t; -*-

;; Copyright (C) 2020  chris

;; Author: chris <chris@chris-lenovo>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESSnn FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'org)

;; ---- org-babel ----
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   (haskell . t)
   ;; (ein . t)
   ))

(setq org-confirm-babel-evaluate nil)
(setq org-export-use-babel t)

(setq org-src-tab-acts-natively t)


(defun cs-jump-to-def-from-inside-org-code-block ()
  "From an `org-mode` buffer's code block, jump to definition."
  (interactive)
  (let* (original-org-buffer
         original-org-buffer-point
         code-block-buffer
         code-block-buffer-point
         new-file-buffer
         new-file-buffer-point)
    (setq original-org-buffer (current-buffer))
    (setq original-org-buffer-point (point))
    (org-edit-special)
    (setq code-block-buffer (current-buffer))
    (setq code-block-buffer-point (point))
    (with-demoted-errors
        (call-interactively (global-key-binding (kbd "C-, d"))))
    (if (not (eq code-block-buffer (current-buffer)))
        (progn
          (setq new-file-buffer (current-buffer))
          (setq new-file-buffer-point (point))
          (switch-to-buffer code-block-buffer)
          (goto-char code-block-buffer-point)))
    (org-edit-src-exit)
    (if new-file-buffer
        (progn
          (switch-to-buffer new-file-buffer)
          (goto-char new-file-buffer-point)))))

(provide 'cs-org-babel)
;;; cs-org-babel.el ends here
