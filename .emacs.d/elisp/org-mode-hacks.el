;;; org-mode-hacks.el --- Hacks for org-mode         -*- lexical-binding: t; -*-

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

(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (buffer-file-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "import" nil nil nil filename)
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

(defun my-toggle-margins ()
"Set margins in current buffer."
(interactive)
  (if (or (> left-margin-width 0) (> right-margin-width 0))
    (progn
      (setq left-margin-width 0)
      (setq right-margin-width 0)
      (set-window-buffer (selected-window) (current-buffer)))
    (setq left-margin-width 26)
    (setq right-margin-width 26)
    (set-window-buffer (selected-window) (current-buffer))))

(global-set-key [f5] 'my-toggle-margins)

(defun update-org-latex-fragments ()
  ;; (org-toggle-latex-fragment '(16))
  ;; (plist-put org-format-latex-options :scale text-scale-mode-amount)
  ;; (org-toggle-latex-fragment '(16))

  ;; readjust latex fragment size parameter based on text zoom size
  (set-latex-fragment-rendering-size-based-automatically)
  ;; plainly disable latex fragments
  (turn-off-latex-toggling-and-render-all-previews)
  ;; (when (org-remove-latex-fragment-image-overlays ,(point-min)
  ;;                                                 ,(point-max))
  ;;   (message "LaTeX fragment images removed from section")
  ;;   (turn-off-latex-toggling-and-render-all-previews)
  ;;   (if (not (buffer-narrowed-p))
  ;;       (org-global-prop-set render-latex-preview-prop-key
  ;;                            "f")
  ;;     (user-error "Global property not edited. This buffer just is a clone and prbably narrowed.")))
  )

(add-hook 'text-scale-mode-hook 'update-org-latex-fragments)

(defun remove-org-latex-fragments-folder ()
  (interactive)
  (delete-directory "ltximg" t t))


;; (defun update-org-latex-fragments ()
;;   (org-toggle-latex-fragment '(16))
;;   (plist-put org-format-latex-options :scale text-scale-mode-amount)
;;   (org-toggle-latex-fragment '(16)))
;; (add-hook 'text-scale-mode-hook 'update-org-latex-fragments)

(provide 'org-mode-hacks)
;;; org-mode-hacks.el ends here
