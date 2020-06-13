;;; cs-crux.el --- my ridiculously useful functions  -*- lexical-binding: t; -*-
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

(defun find-next-file (&optional backward)
  "Find the next file (of the same type, sorted by name) in the current directory.
With prefix arg, find the previous file. Adapted from https://emacs.stackexchange.com/a/12164"
  (interactive "P")
  (when buffer-file-name
    (let* ((file (expand-file-name buffer-file-name))
           (files (cl-remove-if (lambda (file_c)
                                  (not (string-equal (file-name-extension file_c)
                                                     (file-name-extension file))))
                                (cl-remove-if (lambda (file_c)
                                                (and (cl-first (file-attributes file_c))))
                                              (sort (directory-files (file-name-directory file)
                                                                     t
                                                                     nil
                                                                     t)
                                                    'string<))))
           (pos (mod (+ (cl-position file files :test 'equal)
                        (if backward -1 1))
                     (length files))))
      (find-file (nth pos files)))))

(define-key pdf-view-mode-map (kbd "M-n") 'find-next-file)
(define-key pdf-view-mode-map (kbd "M-p") (lambda () (interactive)
                                            (let ((current-prefix-arg 4))
                                              (call-interactively 'find-next-file))))

(defun cs-dired-open-file-externally ()
    "In dired, open the filepath named on this line."
    (interactive)
    (let* ((filepath (dired-get-filename nil t)))
      (message "Opening %s..." filepath)
      (cond ((string-equal (file-name-extension filepath) "ipynb")
             (cs-dired-open-notebook filepath))
            (t (call-process "xdg-open" nil 0 nil filepath)))))

(define-key dired-mode-map (kbd "C-c C-o") 'cs-dired-open-file-externally)

(cl-defstruct jupyter-notebook-list-item url pre-question-mark-url-part port past-question-mark-url-part base-dir)

(defun cs-dired-find-jupyter-servers ()
  "Returns tuples of (server url, token, server base dir)."
  (let* ((str-output (shell-command-to-string "jupyter notebook list"))
         urls-base-dirs)
    ;; compile a list of urls and base-dirs
    (setq urls-base-dirs (remove nil
                                 (mapcar (lambda (str)
                                           (when (string-match ;; "\\(^http.+\\)\s+::\s+\\(.+\\)$"
                                                  "\\(?1:\\(?2:^http.+:\\(?3:[0-9]+\\).+\\)\\(?4:\\?.+\\)\\)\s+::\s+\\(?5:.+\\)$"
                                                  ;; "\\(?1:\\(?2:^http.+\\)\\(?3:\\?.+\\)\\)\s+::\s+\\(?4:.+\\)$"
                                                               str)
                                             (make-jupyter-notebook-list-item
                                              :url
                                              (match-string 1 str)
                                              :pre-question-mark-url-part
                                              (match-string 2 str)
                                              :port
                                              (match-string 3 str)
                                              :past-question-mark-url-part
                                              (match-string 4 str)
                                              :base-dir
                                              (match-string 5 str))))
                                         (split-string str-output "\n"))))))

(defun file-exists-somewhere-within-folder-p (file-path root-path)
  "check if file-path is in a subdirectory under root-path and not somewhere else."
  (let* ((rel-dir-path (file-relative-name file-path root-path)))
    (if (or (not (file-exists-p root-path))
            (not (file-exists-p file-path))
            (string-match-p (regexp-quote "..") rel-dir-path))
        nil
      rel-dir-path)))


(defconst new-server-candidate (list "new server" "new server"))


(defun cs-my-source-open-with-server (filepath)
  (if (equal (length (helm-marked-candidates)) 1)
      (if (equal (car new-server-candidate) (car (car (helm-marked-candidates))))
          (progn
            (message "Open in new server selected.")
            (cs-open-file-with-new-server filepath))
        (cs-open-file-with-running-server filepath
                                          (car (car (helm-marked-candidates)))))
    (message "Please select only one server for this action.")))

(defun cs-my-source-shutdown-servers (filepath)
  (remove (car new-server-candidate)
          (helm-marked-candidates))
  (cs-shutdown-these-servers filepath))

(defun cs-dired-open-notebook (&optional filepath)
  "This will open a new jupyter server. "
  (unless filepath
    (setq filepath (dired-get-filename nil t)))
    ;; compile a list of jupyter notebook servers
  ;; with a base-dir that is an (nth-order) parent
  ;; of the file's directory is already running
  (let* ((list-of-jupyter-servers (cs-dired-find-jupyter-servers))
         (helm-source-candidates (remove nil
                                         (mapcar (lambda (jupyter-server)
                                                   (when (file-exists-somewhere-within-folder-p filepath
                                                                                                (jupyter-notebook-list-item-base-dir jupyter-server))
                                                     jupyter-server))
                                                 list-of-jupyter-servers))))
    (helm :sources (helm-build-sync-source "select possible parent jupyter server"
                     :header-name (lambda (_)
                                    (format "header name"))
                     :candidates
                     (lambda ()
                       (append (mapcar (lambda (candidate)
                                         (list ;; (prin1-to-string candidate)
                                          (concat "running on port "
                                                  (jupyter-notebook-list-item-port candidate)
                                                  ", base dir: "
                                                  (jupyter-notebook-list-item-base-dir candidate))
                                          candidate))
                                       helm-source-candidates)
                               (list new-server-candidate)))
                     :action (helm-make-actions "Open the file with this server. "
                                                (lambda (_)
                                                  (cs-my-source-open-with-server filepath))
                                                "Shutdown these servers"
                                                (lambda (_)
                                                  (cs-my-source-shutdown-servers filepath)))))))


(defun cs-open-file-with-running-server (filepath &optional server-candidate)
  (let* ((compiled-url (concat (jupyter-notebook-list-item-pre-question-mark-url-part
                                server-candidate)
                               "notebooks/"
                               (file-relative-name filepath
                                                   (jupyter-notebook-list-item-base-dir server-candidate))
                               (jupyter-notebook-list-item-past-question-mark-url-part
                                server-candidate))))
    (browse-url-default-browser compiled-url)))

(defun cs-open-file-with-new-server (filepath)
  ;; no servers is yet running for this file, so create new server
  (call-process "jupyter-notebook" nil 0 nil
                filepath))

(defun cs-shutdown-these-servers (server-candidates)
  (message "stopping server")
  (mapcar (lambda (server-candidates)
            (setq server-candidates (car server-candidates))
            ;; TODO shutdown the notebook at port
            (call-process "jupyter"
                          nil
                          0
                          nil
                          "notebook"
                          "stop"
                          (jupyter-notebook-list-item-port server-candidates)))
          (helm-marked-candidates)))

(defun cs-move-to-beginning-of-visual-line ()
    ""
    (interactive)
    (let* (pos-first-non-ws-char-in-cur-vis-line)
      (save-excursion
        (beginning-of-visual-line)
        (setq pos-first-non-ws-char-in-cur-vis-line
              (if (string-match "\s" (string (char-after (point))))
                  (search-forward-regexp "\s+"
                                         (save-excursion
                                           (end-of-visual-line)
                                           (point))
                                         t)
                (point))))
      (if (equal pos-first-non-ws-char-in-cur-vis-line (point))
          (beginning-of-visual-line)
        (goto-char pos-first-non-ws-char-in-cur-vis-line))))

(global-set-key (kbd "C-a") #'cs-move-to-beginning-of-visual-line)

(defun my-toggle-margins (&optional enable-thick-margin)
  "Set margins in current buffer."
  (interactive)
  (if (and (or (> left-margin-width 0)
               (> right-margin-width 0))
           (not enable-thick-margin))
      (progn
        (setq left-margin-width 0)
        (setq right-margin-width 0)
        (set-window-buffer (selected-window)
                           (current-buffer)))
    (setq left-margin-width 26)
    (setq right-margin-width 26)
    (set-window-buffer (selected-window)
                       (current-buffer))))

(global-set-key [f5]
                'my-toggle-margins)

(defun cs-make-all-writable ()
  "Sometimes sections (e.g. properties of org files)
  are not writeable. This makes them writeable."
  (interactive)
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min)
                            (point-max)
                            '(read-only t))))

(global-set-key (kbd "C-x w") 'cs-make-all-writable)

(defun list-packages-and-versions ()
  "Returns a list of all installed packages and their versions"
  (mapcar
   (lambda (pkg)
     `(,pkg ,(package-desc-version
              (cadr (assq pkg package-alist)))))
   package-activated-list))

(defun google-quickly()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

(global-set-key (kbd "C-x C-g") 'google-quickly)


(defun outside-terminal-with-tmux ()
  (interactive)
  (shell-command "gnome-terminal -e 'tmux new' >/dev/null"))

(global-set-key (kbd "C-x C-m C-t") 'outside-terminal-with-tmux)

(defun outside-explorer ()
  (interactive)
  (setq s (concat "nautilus " (file-name-directory buffer-file-name) " & "))
  (message s)
  (call-process-shell-command s nil 0))

(global-set-key (kbd "C-x C-m C-f") 'outside-explorer)  ; open gui file explorer

(defun outside-browser ()
  (interactive)
  (setq s (concat "chromium-browser " (file-name-directory buffer-file-name) " & "))
  (message s)
  (call-process-shell-command s nil 0))

(global-set-key (kbd "C-x C-m C-b") 'outside-browser)  ; open browser at that file

(defun kill-non-visible-buffers ()
  "Kill all buffers not currently shown in a window somewhere."
  (interactive)
  (dolist (buf  (buffer-list))
    (unless (get-buffer-window buf 'visible) (kill-buffer buf))))

(defun new-buffer-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

(global-set-key (kbd "C-c n") #'new-buffer-frame)

;; search for the current folder's desktop-setup.el file, load it and execute the create-project-desktop-setup function


;; ------- put filename to clipboard --------

(defun cs-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(global-set-key (kbd "C-, c f c") 'cs-put-file-name-on-clipboard)


;; ---- open file from clipboard

(defun cs-open-file-from-clipboard ()
  (interactive)
  (find-file
   (helm-read-file-name
    "open filepath from clipboard: "
    :initial-input (with-temp-buffer (yank) (buffer-string)))))

(global-set-key (kbd "C-, c f o") 'cs-open-file-from-clipboard)


;; ---- drag and drop files (as links) from explorer into org-mode -----

(defun my-dnd-func (event)
  (interactive "e")
  (goto-char (nth 1 (event-start event)))
  (x-focus-frame nil)
  (let* ((payload (car (last event)))
         (type (car payload))
         (fname (cadr payload))
         (img-regexp "\\(png\\|jp[e]?g\\)\\>"))
    (cond
     ;; insert image link
     ((and  (eq 'drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; insert image link with caption
     ((and  (eq 'C-drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert "#+ATTR_ORG: :width 300\n")
      (insert (concat  "#+CAPTION: " (read-input "Caption: ") "\n"))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; C-drag-n-drop to open a file
     ((and  (eq 'C-drag-n-drop (car event))
            (eq 'file type))
      (find-file fname))
     ((and (eq 'M-drag-n-drop (car event))
           (eq 'file type))
      (insert (format "[[attachfile:%s]]" fname)))
     ;; regular drag and drop on file
     ((eq 'file type)
      (insert (format "[[%s]]\n" fname)))
     (t
      (error "I am not equipped for dnd on %s" payload)))))

(define-key org-mode-map (kbd "<drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<C-drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<M-drag-n-drop>") 'my-dnd-func)

(provide 'cs-crux)
;;; cs-crux.el ends here
