;;; cs-org-mode-general.el --- general org mode settings  -*- lexical-binding: t; -*-

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

;;; Code:

;; (add-hook 'after-init-hook 'org-agenda-list)
;; (global-set-key (kbd "C-x i s") 'my-org-screenshot)

(setq org-image-actual-width nil)

(setq org-startup-indented t)
(setq org-startup-folded nil)

(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode 1)))

(add-hook 'org-mode-hook
          (lambda ()
            (show-paren-mode 1)))

(setq org-todo-keywords '((sequence "TODO" "MAYBE" "NEXT" "STARTED"
                                    "STUCK"
                                    "TOASK" "WAITING" "DELEGATED" "|" "DONE" "DEFERRED"
                                    "CANCELLED" "TOREVIEW")))

;; (custom-set-faces '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
;;                   '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
;;                   ;; '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
;;                   ;; '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
;;                   ;; '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
;;                   )

(setq org-todo-keyword-faces '(("PROJ" :background "blue"
                                :foreground "black"
                                :weight bold
                                :box (:line-width 2 :style released-button))
                               ("TODO" :background "red1"
                                :foreground "black"
                                :weight bold
                                :box (:line-width 2 :style released-button))
                               ("STUCK" :background "orange"
                                :foreground "black"
                                :weight bold
                                :box (:line-width 2 :style released-button))
                               ("NEXT" :background "red1"
                                :foreground "black"
                                :weight bold
                                :box (:line-width 2 :style released-button))
                               ("STARTED" :background "orange"
                                :foreground "black"
                                :weight bold
                                :box (:line-width 2 :style released-button))
                               ("WAITING" :background "yellow"
                                :foreground "black"
                                :weight bold
                                :box (:line-width 2 :style released-button))
                               ("TOASK" :background "orange"
                                :foreground "black"
                                :weight bold
                                :box (:line-width 2 :style released-button))
                               ("DEFERRED" :background "gold"
                                :foreground "black"
                                :weight bold
                                :box (:line-width 2 :style released-button))
                               ("DELEGATED" :background "gold"
                                :foreground "black"2
                                :weight bold
                                :box (:line-width 2 :style released-button))
                               ("MAYBE" :background "gray"
                                :foreground "black"
                                :weight bold
                                :box (:line-width 2 :style released-button))
                               ("APPT" :background "red1"
                                :foreground "black"
                                :weight bold
                                :box (:line-width 2 :style released-button))
                               ("DONE" :background "forest green"
                                :weight bold
                                :box (:line-width 2 :style released-button))
                               ("CANCELLED" :background "lime green"
                                :foreground "black"
                                :weight bold
                                :box (:line-width 2 :style released-button))
                               ("TOREVIEW" :background "green"
                                :foreground "black"
                                :weight bold
                                :box (:line-width 2 :style released-button))))

(provide 'cs-org-mode-general)
;;; cs-org-mode-general.el ends here
