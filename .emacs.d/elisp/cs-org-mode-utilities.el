;;; cs-org-mode-utilities.el --- org utilities       -*- lexical-binding: t; -*-

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

(require 'hydra)

(defun my-org-scale-image (&optional minus)
  "Scale an image."
  (interactive)
  (save-excursion
    (let* ((org-width-pos (re-search-backward "#\\+attr_org:\s+:width\s+\\([0-9]+\\)"))
           (beg (match-beginning 1))
           (end (match-end 1))
           (num (string-to-number (buffer-substring-no-properties beg end))))
      (delete-region beg end)
      (goto-char beg)
      (if minus
          (insert (number-to-string (- num 10)))
        (insert (number-to-string (+ 10 num))))
      (org-redisplay-inline-images))))


(defun my-run-org-image-scaler-hydra ()
  (interactive)
  (let* ((hydra-body (eval (remove nil
                                   `(defhydra hydra-my-org-image-scaler
                                      (:columns 1)
                                      "scale org inline image"
                                      ("+"
                                       (lambda ()
                                         (interactive)
                                         (my-org-scale-image))
                                       "enlarge")
                                      ("-"
                                       (lambda ()
                                         (interactive)
                                         (my-org-scale-image 'minus))
                                       "shrink")
                                      ("q" nil "cancel"))))))
    (hydra-my-org-image-scaler/body)
    (fmakunbound 'hydra-my-org-image-scaler/body)
    (setq hydra-my-org-image-scaler/body nil)))

(define-key org-mode-map (kbd "C-c s") 'my-run-org-image-scaler-hydra)
(define-key org-mode-map (kbd "C-c C-+") 'my-org-scale-image)
(define-key org-mode-map (kbd "C-c C--") (lambda ()
                                           (interactive) (my-org-scale-image 'minus)))

(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename (concat (make-temp-name (concat (buffer-file-name)
                                                 "_"
                                                 (format-time-string "%Y%m%d_%H%M%S_")))
                         ".png"))
  (call-process "import" nil nil nil filename)
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))



(defun org-archive-done-tasks-subtree ()
  (interactive)
  (org-map-entries (lambda ()
                     (org-archive-subtree)
                     (setq org-map-continue-from (outline-previous-heading)))
                   "/DONE"
                   'tree))

(defun org-archive-done-tasks-file ()
  (interactive)
  (org-map-entries (lambda ()
                     (org-archive-subtree)
                     (setq org-map-continue-from (outline-previous-heading)))
                   "/DONE"
                   'file))

(provide 'cs-org-mode-utilities)
;;; cs-org-mode-utilities.el ends here
