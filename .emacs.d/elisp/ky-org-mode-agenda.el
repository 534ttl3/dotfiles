;;; ky-org-mode-agenda.el --- org-mode agenda helper functions  -*- lexical-binding: t; -*-

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

(require 'org)
(require 'org-agenda)
(require 'hydra)

;; (setq org-agenda-files-file (expand-file-name "~/Dropbox/org/agendafiles/"))

;; (defun ky-reload-org-agenda-files ()
;;   (interactive)
;;   (setq org-agenda-files (remove ""
;;                                  (split-string (with-temp-buffer
;;                                                  (insert-file-contents org-agenda-files-file)
;;                                                  (buffer-string))
;;                                                "\n"))))

;; (defun ky-set-agenda-file (&optional path)
;;   (interactive)
;;   (unless path
;;     (if (string-equal (file-name-extension (buffer-file-name)) "org")
;;         (setq path (buffer-file-name))
;;       (user-error "Not in an org file")))
;;   (ky-reload-org-agenda-files)
;;   (unless (member (expand-file-name path) (mapcar (lambda (p)
;;                                                     (expand-file-name p))
;;                                                   org-agenda-files))
;;     (write-region (concat "\n" path) nil org-agenda-files-file 'append))
;;   (ky-reload-org-agenda-files))

;; (define-key org-mode-map (kbd "C-c a a") 'ky-set-agenda-file)


;; at startup reload the org agenda files
;; (ky-reload-org-agenda-files)

;; Hydra for org agenda (graciously taken from Spacemacs)
(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                 :post (setq which-key-inhibit nil)
                                 :hint none)
  "
Org agenda (_q_uit)
^^
^Basic Commands^
^--------------^---------
_A_ agenda
_W_ list (scheduled dates)
_T_ todo list (all todos)
_C_ cycle agenda files
_[_ add this file to agenda
_]_ remove this file from agenda
^^             ^^

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; lists
  ("T" org-todo-list :exit t) ;; see the todo's
  ("W" org-agenda-list :exit t) ;; see the schedule (week)
  ("A" org-agenda :exit t) ;; agenda menu
  ("C" org-cycle-agenda-files) ;; agenda cycle
  ("[" org-agenda-file-to-front)
  ("]" org-remove-file)
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))

(global-set-key (kbd "C-M-, a") 'hydra-org-agenda/body)


(provide 'ky-org-mode-agenda)
;;; ky-org-mode-agenda.el ends here
