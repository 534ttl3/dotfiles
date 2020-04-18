;;; cs-python-tmux-debugger.el --- focuses a terminal window with a tmux session running in it and sends text into it (like compile or debug)  -*- lexical-binding: t; -*-

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

;; (cl-defstruct cs-tmux-session name)

;; (defconst cs-cur-tmux-session nil
;;   "Tmux session to send keys to.")

(defconst cs-cur-tmux-session-window-id nil
  "Window id for the debugging session.")

(defun cs-tmux-get-tmux-session-name-for-buffer (&optional buffer)
  ""
  (concat "cs-tmux--" (file-name-base (buffer-name))))

(defun cs-tmux-session-exists-p (&optional session-name)
  ""
  (interactive)
  (unless session-name
    (setq session-name "Test01"))

  (let* ((return-str (shell-command-to-string "tmux list-sessions")))
    (string-match (regexp-quote (concat session-name)) return-str)))

(defun cs-tmux-test-open ()
  ""
  (interactive)
  (shell-command "gnome-terminal -e 'tmux attach -t 0' >/dev/null")
  ;; (shell-command (concat "gnome-terminal -e 'tmux attach -t "
  ;;                        "hey" "' >/dev/null"))
  (shell-command "gnome-terminal -e 'tmux attach -t hey' >/dev/null"))

(defun run-gnome-terminal-here ()
  (interactive "@")
  (shell-command (concat "gnome-terminal "
                         (file-name-directory (or load-file-name buffer-file-name))
                         ;; " -e 'tmux attach -t hey' "
                         " > /dev/null 2>&1 & disown")
                 nil
                 nil))

(defun cs-tmux-set-focus-to-window (window-id)
  ""
  (shell-command-to-string (concat "wmctrl -i -a " window-id))
  window-id)

(defun cs-tmux-open-new-terminal-and-attach (tmux-session-name)
  ""
  ;; log all terminal windows id's
  (let* (terminal-windows-ids-before
         terminal-windows-ids-after
         new-terminal-window-id
         term-window-id)
    (setq terminal-windows-ids-before
          (remove ""
                  (split-string (shell-command-to-string "xdotool search --onlyvisible -class gnome-terminal")
                                "\n")))

    (shell-command (concat "gnome-terminal -e 'tmux attach -t "
                           tmux-session-name "' >/dev/null"))

    (setq terminal-windows-ids-after
          (remove ""
                  (split-string (shell-command-to-string "xdotool search --onlyvisible -class gnome-terminal")
                                "\n")))
    (setq term-window-id
          (let* ((tmp (-difference terminal-windows-ids-after terminal-windows-ids-before)))
            (assert (equal (length tmp) 1))
            (car tmp)))

    (cs-tmux-set-focus-to-window term-window-id)))

(defun cs-tmux-terminal-window-exists-p (window-id)
  ""
  (let* ((terminal-windows-ids (remove ""
                                       (split-string (shell-command-to-string "xdotool search --onlyvisible -class gnome-terminal")
                                                     "\n"))))
    (member window-id terminal-windows-ids)))

(defun python-call ()
  "From within a python buffer, make a call to run that file in a tmux session."
  (interactive)

  (let* ((tmux-session-for-cur-buffer (cs-tmux-get-tmux-session-name-for-buffer)))
    (if (cs-tmux-session-exists-p tmux-session-for-cur-buffer)
        (shell-command-to-string "tmux send-keys -t 0 \"python3 main.py\" ENTER")
      ;; create it
      (shell-command-to-string (concat "tmux new -s " tmux-session-for-cur-buffer
                                       " -d")))

    ;; check if the window associated to this session has already been opened from inside
    ;; this program, i.e. if there is a window id
    (if (and cs-cur-tmux-session-window-id
             (cs-tmux-terminal-window-exists-p cs-cur-tmux-session-window-id))
        ;; only set focus to it
        (cs-tmux-set-focus-to-window cs-cur-tmux-session-window-id)
      ;; connect to it and open it in external gnome-terminal
      ;; then, set the window id and the name
      (setq cs-cur-tmux-session-window-id (cs-tmux-open-new-terminal-and-attach tmux-session-for-cur-buffer)))))

(provide 'cs-python-tmux-debugger)
;;; cs-python-tmux-debugger.el ends here
