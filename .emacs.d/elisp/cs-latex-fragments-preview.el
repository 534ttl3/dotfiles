;;; cs-latex-fragments-preview.el --- toggle on/off automatic toggling of latex preview fragments in org-mode buffers when cursor enters or leaves a latex object  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  chris

;; Author: chris <chris@chris-IdeaPad-U330p>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'org)
(require 'cl)

;; ------- latex fragments preview, automate
(defvar kk/org-latex-fragment-last nil
  "Holds last fragment/environment you were on.")

(defun kk/org-in-latex-fragment-p ()
  "Return the point where the latex fragment begins, if inside one.
Else return false."
  (let* ((el (org-element-context))
         (el-type (car el)))
    (and (or (eq 'latex-fragment el-type) (eq 'latex-environment el-type))
         (org-element-property :begin el))))

(defun kk/org-latex-fragment-toggle ()
  "Toggle a latex fragment image."
  (set-latex-fragment-rendering-size-based-automatically)
  (and (eq 'org-mode major-mode)
       (let ((begin (kk/org-in-latex-fragment-p)))
         (cond
          ;; were on a fragment and now on a new fragment
          ((and
            ;; fragment we were on
            kk/org-latex-fragment-last
            ;; and are on a fragment now
            begin

            ;; but not on the last one this is a little tricky. as you edit the
            ;; fragment, it is not equal to the last one. We use the begin
            ;; property which is less likely to change for the comparison.
            (not (and kk/org-latex-fragment-last
                      (= begin
                         kk/org-latex-fragment-last))))
           ;; go back to last one and put image back, provided there is still a fragment there
           (save-excursion
             (goto-char kk/org-latex-fragment-last)
             (when (kk/org-in-latex-fragment-p) (org-preview-latex-fragment))

             ;; now remove current image
             (goto-char begin)
             (let ((ov (loop for ov in (org--list-latex-overlays)
                             if
                             (and
                              (<= (overlay-start ov) (point))
                              (>= (overlay-end ov) (point)))
                             return ov)))
               (when ov
                 (delete-overlay ov)))
             ;; and save new fragment
             (setq kk/org-latex-fragment-last begin)))

          ;; were on a fragment and now are not on a fragment
          ((and
            ;; not on a fragment now
            (not begin)
            ;; but we were on one
            kk/org-latex-fragment-last)
           ;; put image back on, provided that there is still a fragment here.
           (save-excursion
             (goto-char kk/org-latex-fragment-last)
             (when (kk/org-in-latex-fragment-p) (org-preview-latex-fragment)))

           ;; unset last fragment
           (setq kk/org-latex-fragment-last nil))

          ;; were not on a fragment, and now are
          ((and
            ;; we were not one one
            (not kk/org-latex-fragment-last)
            ;; but now we are
            begin)
           ;; remove imagex
           (save-excursion
             (goto-char begin)
             (let ((ov (loop for ov in (org--list-latex-overlays)
                             if
                             (and
                              (<= (overlay-start ov) (point))
                              (>= (overlay-end ov) (point)))
                             return ov)))
               (when ov
                 (delete-overlay ov))))
           (setq kk/org-latex-fragment-last begin))))))

;; (defvar auto-compile-cursor-traversed-latex-fragments t
;;   "Toggle variable nil or t.")

(defun toggle-org-dynamic-preview-latex-fragment ()
  "Toggle on/off the `post-command-hook' `kk/org-latex-fragment-toggle'."
  (interactive)
  (if (not (member 'kk/org-latex-fragment-toggle post-command-hook))
      (add-hook 'post-command-hook 'kk/org-latex-fragment-toggle t 'local)
    (remove-hook 'post-command-hook 'kk/org-latex-fragment-toggle 'local)))

(defun cs-turn-on-org-dynamic-preview-latex-fragment ()
  (interactive)
  (add-hook 'post-command-hook 'kk/org-latex-fragment-toggle t 'local))

(defun cs-turn-off-org-dynamic-preview-latex-fragment ()
  (interactive)
  (remove-hook 'post-command-hook 'kk/org-latex-fragment-toggle 'local))

(defun org-renumber-environment (orig-func &rest args)
  (let ((results '())
        (counter -1)
        (numberp))

    (setq results (loop for (begin .  env) in
                        (org-element-map (org-element-parse-buffer) 'latex-environment
                          (lambda (env)
                            (cons
                             (org-element-property :begin env)
                             (org-element-property :value env))))
                        collect
                        (cond
                         ((and (string-match "\\\\begin{equation}" env)
                               (not (string-match "\\\\tag{" env)))
                          (incf counter)
                          (cons begin counter))
                         ((string-match "\\\\begin{align}" env)
                          (prog2
                              (incf counter)
                              (cons begin counter)
                            (with-temp-buffer
                              (insert env)
                              (goto-char (point-min))
                              ;; \\ is used for a new line. Each one leads to a number
                              (incf counter (count-matches "\\\\$"))
                              ;; unless there are nonumbers.
                              (goto-char (point-min))
                              (decf counter (count-matches "\\nonumber")))))
                         (t
                          (cons begin nil)))))

    (when (setq numberp (cdr (assoc (point) results)))
      (setf (car args)
            (concat
             (format "\\setcounter{equation}{%s}\n" numberp)
             (car args)))))

  (apply orig-func args))



;; ;; TODO: awesome possibility of getting closer to visual in-line latex editing
;; ;; get string of underlying latex with (org-element-property :value context)
;; (defun my-insert-latex-preview-cursor ()
;;   (interactive)
;;   (save-excursion (insert "\\textcolor{green}{\\textbf{|}}")))

;; (advice-add 'org-create-formula-image :before #'my-insert-latex-preview-cursor)
;; (advice-add 'org-create-formula-image :before #'my-insert-latex-preview-cursor)
(advice-add 'org-create-formula-image :around #'org-renumber-environment)
;; -------

;; (advice-remove 'org-create-formula-image #'org-renumber-environment)


(defun render-org-mode-buffer-latex-previews (&optional only-run-if-in-this-buffer cursor-position-before)
  (interactive)
  (let* ((run-it t)
         do-buffers-match)
    (when only-run-if-in-this-buffer
      (progn
        (setq run-it nil)
        (if (eq (current-buffer) only-run-if-in-this-buffer)
            (progn
              (setq do-buffers-match t)
              (setq run-it t))
          ;; dont
          (message "Not in the right buffer for latex preview rendering!"))))

    (when run-it
      (org-format-latex "ltximg/org-ltximg"
                                        ; prefix
                        nil ; beg
                        nil ; end
                        (file-name-directory (buffer-file-name)) ; dir

                        'overlays ; overlays
                        "Creating images for org-noter widened document..."
                                        ; msg

                        'forbuffer ; forbuffer

                        'dvipng ; processing-type
                        )
      (when (and cursor-position-before only-run-if-in-this-buffer
                 do-buffers-match)
        ;; restore cursor position
        (goto-char cursor-position-before)))))

(defun set-latex-fragment-rendering-size-based-automatically ()
  (interactive)
  (let* ((offset-num 1.5)
         (latex-fragment-scale-per-text-scale 0.3))
    (if (equal text-scale-mode-amount 0)
        (plist-put org-format-latex-options :scale offset-num)
      (plist-put org-format-latex-options :scale (+ offset-num (* text-scale-mode-amount latex-fragment-scale-per-text-scale))))))

(defun turn-on-latex-toggling-and-render-all-previews (&optional rendering-delay-in-seconds)
  "And restore your cursor position."
  (interactive)
  (set-latex-fragment-rendering-size-based-automatically)
  (let* ((cursor-position-before (point))
         (buffer-before (current-buffer))
         command-to-render)
    (if rendering-delay-in-seconds
        (setq command-to-render `(run-with-idle-timer ,rendering-delay-in-seconds
                                                      nil
                                                      (lambda ()
                                                        (render-org-mode-buffer-latex-previews (current-buffer) ,cursor-position-before))))
      (setq command-to-render `(run-with-idle-timer 1
                                                    nil
                                                    (lambda ()
                                                      (render-org-mode-buffer-latex-previews (current-buffer) ,cursor-position-before)))))
    (cs-turn-on-org-dynamic-preview-latex-fragment)
    (eval command-to-render)))

(defun turn-off-latex-toggling-and-render-all-previews (&optional rendering-delay-in-seconds)
  "And restore your cursor position."
  (interactive)
  (let* ((cursor-position-before (point))
         (buffer-before (current-buffer)))
    (org-remove-latex-fragment-image-overlays (point-min) (point-max))
    (cs-turn-off-org-dynamic-preview-latex-fragment)))

(defun org-toggle-latex-fragment-with-prefix-arg ()
  "This only toggles it. TODO: I want a function that deliberately enables/disables
programmatically."
  (interactive)
  (setq current-prefix-arg '(4)) ; C-u
  (call-interactively 'org-toggle-latex-fragment))

(defun do-not-render-latex-previews-p ()
    "Determine if in the org file, a property instruction is set at the top that aims at preventing
the rendering of inline latex previews."
    (interactive)
    (let* ((result (org-global-prop-value render-latex-preview-prop-key)))
        (or (not result)
            (string-equal result "f")
            (string-equal result "false")
            (string-equal result "False")
            (string-equal result "F")
            (string-equal result "FALSE")
            (string-equal result "nil")
            (string-equal result "NIL"))))

(provide 'cs-latex-fragments-preview)
;;; cs-latex-fragments-preview.el ends here
