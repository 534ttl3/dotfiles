;;; cs-evil.el --- evil mode settings                -*- lexical-binding: t; -*-

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

(require 'use-package)

(let* (;; it may use preceding-sexp, which is obsolete
       ;; and that produces an annoying warning
       (warning-minimum-level :emergency))
  (use-package evil
    :init
    (progn
      (setq evil-want-C-u-scroll t)
      (add-to-list 'load-path "~/.emacs.d/evil")
      (setq evil-want-integration t)
      (setq evil-want-keybinding nil)
      (setq evil-toggle-key "<C-S-dead-grave>"))
    :config
    (progn
      (evil-mode 1)
      ;; map all modes to emacs-state-modes, since I only want
      ;; access to some evil functions, but not any keybindings, please
      (dolist (item evil-normal-state-modes)
        (add-to-list 'evil-emacs-state-modes item))
      (setq evil-normal-state-modes nil)
      (dolist (item evil-motion-state-modes)
        (add-to-list 'evil-emacs-state-modes item))
      (setq evil-motion-state-modes nil)
      (add-to-list 'evil-emacs-state-modes 'mhtml-mode)
      (add-to-list 'evil-emacs-state-modes 'custom-mode)
      (add-to-list 'evil-emacs-state-modes 'org-mode)
      (add-to-list 'evil-emacs-state-modes 'nav-mode)
      (add-to-list 'evil-emacs-state-modes 'pdf-occur-buffer-mode)
      (add-to-list 'evil-emacs-state-modes 'emacs-lisp-mode)
      (add-to-list 'evil-emacs-state-modes 'prog-mode)
      (add-to-list 'evil-emacs-state-modes 'grep-mode)
      (add-to-list 'evil-emacs-state-modes 'bibtex-mode)
      (add-to-list 'evil-emacs-state-modes 'dired-mode)
      (add-to-list 'evil-emacs-state-modes 'debugger-mode)
      (add-to-list 'evil-emacs-state-modes 'package-menu-mode)
      ;; (add-to-list 'evil-emacs-state-modes 'pdf-view-mode)
      (add-to-list 'evil-emacs-state-modes 'ediff-mode)
      (add-to-list 'evil-emacs-state-modes 'minibuffer-inactive-mode)
      (add-to-list 'evil-emacs-state-modes 'latex-mode)
      (add-to-list 'evil-emacs-state-modes 'eshell-mode)
      ;; (add-to-list 'evil-emacs-state-modes 'term-mode)  ;; this didn't work
      (add-to-list 'evil-emacs-state-modes 'process-menu-mode)
      (add-to-list 'evil-emacs-state-modes 'tabulated-list-mode)
      (add-to-list 'evil-emacs-state-modes 'fundamental-mode)
      (add-to-list 'evil-emacs-state-modes 'text-mode)
      (add-to-list 'evil-emacs-state-modes 'inferior-python-mode)
      (evil-set-initial-state 'help-mode 'emacs)
      ;; (evil-set-initial-state 'term-mode 'emacs)  ;; this didn't work as well
      ;; (add-hook 'term-mode-hook (lambda () (evil-mode -1)))  ;; this finally worked

      ;; only ever go up/down visual lines
      (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
      (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
      ;; now the functions that emacs doesn't have a good ootb solution for,
      ;; and therefore I have to use evil:
      (global-set-key (kbd "C-M-s-k")
                      'evil-window-move-very-top)
      (global-set-key (kbd "C-M-s-j")
                      'evil-window-move-very-bottom)
      (global-set-key (kbd "C-M-s-h")
                      'evil-window-move-far-left)
      (global-set-key (kbd "C-M-s-l")
                      'evil-window-move-far-right)
      (global-set-key (kbd "C-o")
                      'evil-jump-backward)
      (global-set-key (kbd "C-i")
                      'evil-jump-forward)
      (define-key my-keys-minor-mode-map (kbd "C-o") 'evil-jump-backward)
      (define-key my-keys-minor-mode-map (kbd "C-i") 'evil-jump-forward)
      ;; elscreen could use these two, so unset them
      (define-key evil-normal-state-map (kbd "C-z") nil)
      (define-key evil-emacs-state-map (kbd "C-z") nil))))

(provide 'cs-evil)
;;; cs-evil.el ends here
