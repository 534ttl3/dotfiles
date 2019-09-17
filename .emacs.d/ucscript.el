(require 'frame-cmds)

(defun open-project-desktop (file-or-directory-name)
  "start a custom routine specific for the project in order to lauch a favorite window configuration. see if in the current directory, there is a desktop-setup.el file with create-project-desktop-setup declared inside. if yes, load it and run the function "
  (interactive
   (list (read-file-name "dir or file in dir: ")))
  (message (concat "dir or file in dir is: " file-or-directory-name))
  (setq sp-old-create-project-desktop-setup-routine-file (symbol-file 'create-project-desktop-setup))
  (if (load-file (concat (file-name-directory file-or-directory-name) "desktop-setup.el"))
      (if (symbol-file 'create-project-desktop-setup)
          (create-project-desktop-setup)
        (message "no (defun create-project-desktop-setup () ... ) found in %s" (concat dname "desktop-setup.el")))))

(defun xx-display-filename (path)
  ""
  (interactive (list (read-file-name "dir or file in dir: " nil nil nil (helm-current-directory))))
  (message (concat "dir or file in dir is: " path)))


(defun ff ()
  "Prompt user to enter a file name, with completion and history support."
  (interactive)
  (message "String is %s" (read-file-name "Enter file name:")))

(defun do-something (x y)
  "Ask name and age"
  (interactive
   ;; complex code here that returns a list
   (list "Mary" 22))
  (message "Name is: %s, Age is: %d" x y))


(defun open-pdf-in-new-frame-if-not-already-open ()
  ;; useful functions
  ;; (find-file-existing (setq filename (expand-file-name "~/Dropbox/2TextBooks/1-Bible/elberfelder-1905-deuelo_a4.pdf")))
  ;; (iconify-frame (nth 0 (frame-list)))
  ;; (buffer-list)
  ;; (visible-frame-list)

  (setq buffer (get-buffer "elberfelder-1905-deuelo_a4.pdf"))
  (setq buffer-window (get-buffer-window buffer 0))

  (if buffer
      (if buffer-window
          (progn
            (setq framewithpdf (window-frame buffer-window))
            (if (frcmds-frame-iconified-p framewithpdf)
                (raise-frame framewithpdf))
            (make-frame-visible framewithpdf)
            (raise-frame framewithpdf))
        (switch-to-buffer-other-frame))
    (find-file-other-frame (setq filename (expand-file-name "~/Dropbox/2TextBooks/1-Bible/elberfelder-1905-deuelo_a4.pdf"))))
  )

;; TODO: because un-iconify for some reason doesn't work
;; in gnome through and Emacs 25, 26 (at least through raise-frame)
;; i do it only with visible and invisible frames, (which btw. don't show up in
;; gnome's window switcher.)


;; (make-frame-visible (window-frame (get-buffer-window (get-buffer "elberfelder-1905-deuelo_a4.pdf") t)))
;; (frame-visible-p (window-frame (get-buffer-window (get-buffer "elberfelder-1905-deuelo_a4.pdf") t)))
;; (make-frame-invisible (window-frame (get-buffer-window (get-buffer "elberfelder-1905-deuelo_a4.pdf") t)))

;; (defun get-all-pdf-buffers)
