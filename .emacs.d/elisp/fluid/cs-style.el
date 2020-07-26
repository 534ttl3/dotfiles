;;; cs-style.el --- setting the style of emacs (e.g. when visiting from nw mode)  -*- lexical-binding: t; -*-

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



;; ------- set theme -----------

(require 'doom-themes)

(defun cs-set-style-emacs ()
  ""
  (interactive)
  (if (display-graphic-p)
      (let* ()
        (require 'doom-themes)
        ;; Global settings (defaults)
        (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
              doom-themes-enable-italic t) ; if nil, italics is universally disabled

        ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
        ;; may have their own settings.
        ;; (load-theme 'doom-one t)
        (load-theme 'doom-city-lights t)
        ;; Enable flashing mode-line on errors
        (doom-themes-visual-bell-config)
        ;; Enable custom neotree theme (all-the-icons must be installed!)
        (doom-themes-neotree-config)
        ;; or for treemacs users
        (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
        (doom-themes-treemacs-config)
        ;; Corrects (and improves) org-mode's native fontification.
        (doom-themes-org-config))
    (let* ()
      (load-theme 'wheatgrass))))


;; ------ set line spacing (e.g. for org mode) -------

(defun xah-toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height.
  URL `http://ergoemacs.org/emacs/emacs_toggle_line_spacing.html'
  Version 2017-06-02"
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing 0.5))
  (redraw-frame (selected-frame)))

(defun cs-set-line-spacing ()
  (interactive)
  (setq line-spacing 0.5)
  (redraw-frame (selected-frame)))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'cs-set-line-spacing))


;; -------- line truncation in txt files ---------

(add-hook 'text-mode-hook
          (lambda ()
            (toggle-truncate-lines 1)))

(provide 'cs-style)
;;; cs-style.el ends here
