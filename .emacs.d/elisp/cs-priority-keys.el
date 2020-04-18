;;; cs-priority-keys.el --- minor mode for priority key combinations  -*- lexical-binding: t; -*-

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

;; https://stackoverflow.com/a/683575
;; the minor mode
(defvar cs-keys-minor-mode-map (let ((map (make-sparse-keymap)))
                                 ;; (define-key map (kbd "C-o")
                                 ;;                 ;; 'evil-jump-backward
                                 ;;                 )
                                 ;; (define-key map (kbd "C-i")
                                 ;;                 ;; 'evil-jump-forward
                                 ;;                 )
                                 map)
  "cs-keys-minor-mode keymap.")

(define-minor-mode cs-keys-minor-mode "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " cs-keys")

(defun my-minibuffer-setup-hook ()
  (cs-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(cs-keys-minor-mode 1)

;; https://stackoverflow.com/a/5340797
;; make precendence last
(add-hook 'after-load-functions 'cs-my-keys-have-priority)

(defun cs-my-keys-have-priority (_file)
  "Try to ensure that my keybindings retain priority over other minor modes.
Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'cs-keys-minor-mode)
    (let ((mykeys (assq 'cs-keys-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'cs-keys-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))


(provide 'cs-priority-keys)
;;; cs-priority-keys.el ends here
