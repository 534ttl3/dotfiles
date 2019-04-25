;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.


;; Prevent the cursor from blinking
(blink-cursor-mode 0)
;; Don't use messages that you don't read
;; (setq initial-scratch-message "")
;; (setq inhibit-startup-message t)
;; Don't let Emacs hurt your ears
(setq visible-bell t)

;; You need to set `inhibit-startup-echo-area-message' from the
;; customization interface:
;; M-x customize-variable RET inhibit-startup-echo-area-message RET
;; then enter your username
;; (setq inhibit-startup-echo-area-message "guerry")

;; This is bound to f11 in Emacs 24.4
;; (toggle-frame-fullscreen) 
;; Who use the bar to scroll?

;; You can also set the initial frame parameters
;; (setq initial-frame-alist
;;       '((menu-bar-lines . 0)
;;         (tool-bar-lines . 0)))

;; If you want to hide the mode-line in all new buffers
;; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

;; Alternatively, you can paint your mode-line in White but then
;; you'll have to manually paint it in black again
;; (custom-set-faces
;;  '(mode-line-highlight ((t nil)))
;;  '(mode-line ((t (:foreground "white" :background "white"))))
;;  '(mode-line-inactive ((t (:background "white" :foreground "white")))))

;; A small minor mode to use a big fringe
(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to hide the mode-line in the ucrrent buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 100 (frame-char-width)))
        2))))

;; Now activate this global minor mode
;; (bzg-big-fringe-mode 1)

(global-set-key (kbd "C-, f") 'bzg-big-fringe-mode)

;; To activate the fringe by default and deactivate it when windows
;; are split vertically, uncomment this:
;; (add-hook 'window-configuration-change-hook
;;           (lambda ()
;;             (if (delq nil
;;                       (let ((fw (frame-width)))
;;                         (mapcar (lambda(w) (< (window-width w) fw))
;;                                 (window-list))))
;;                 (bzg-big-fringe-mode 0)
;;               (bzg-big-fringe-mode 1))))

;; Use a minimal cursor
;; (setq cursor-type 'hbar)

;; Get rid of the indicators in the fringe
;; (mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
;;         fringe-bitmaps)

;; Set the color of the fringe
;; (custom-set-faces
;;  '(fringe ((t (:background "white")))))

;; (custom-set-faces
;;   '(default ((t (:background "black" :foreground "grey"))))
;;   '(fringe ((t (:background "black")))))

;; Command to toggle the display of the mode-line as a header
(defvar-local header-line-format nil)
(defun mode-line-in-header ()
  (interactive)
  (if (not header-line-format)
      (setq header-line-format mode-line-format
            mode-line-format nil)
    (setq mode-line-format header-line-format
          header-line-format nil))
  (set-window-buffer nil (current-buffer)))

(global-set-key (kbd "C-, SPC") 'mode-line-in-header)


;; (global-set-key (kbd "C-, w") (lambda () (interactive)
;;   (bzg-big-fringe-mode)
;; ))

;; (if (not (bound-and-true-p hide-mode-line-mode))
;;     (hide-mode-line-mode)
;;   (hide-mode-line-mode -1)
;;   )
