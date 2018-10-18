(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   (quote
    ("43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" default)))
 '(package-selected-packages
   (quote
    (evil-collection evil org-pdfview pdf-tools auctex-lua auctex-latexmk auctex yasnippet-snippets yasnippet linum-relative exec-path-from-shell projectile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ---- use-package initialization, make sure use-package.el is cloned into ~/.emacs.d
;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;; (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

;; use-package
(setq use-package-always-ensure t)


" ---- BEGIN general emacs settings "

(global-auto-revert-mode 1)

(add-hook 'prog-mode-hook 'visual-line-mode)

;; googling quickly
(defun prelude-google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

(global-set-key (kbd "C-x g") 'prelude-google)

(tool-bar-mode -1)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

(savehist-mode 1)

;; disable startup screen when opening a file
(defun my-inhibit-startup-screen-always ()
  ;; Startup screen inhibitor for `command-line-functions`.
  ;; Inhibits startup screen on the first unrecognised option.
  (ignore (setq inhibit-startup-screen t)))

(add-hook 'command-line-functions #'my-inhibit-startup-screen-always)

;; insert current buffers file path into minibuffer 
(define-key minibuffer-local-map [f3]
  (lambda () (interactive) 
     (insert (buffer-name (window-buffer (minibuffer-selected-window))))))

" ---- END general emacs settings "


" --- BEGIN packages + package-specific settings "

(use-package winner
  :config
    (when (fboundp 'winner-mode)
      (winner-mode 1))
    (define-key winner-mode-map (kbd "C-c h") 'winner-undo)
    (define-key winner-mode-map (kbd "C-c l") 'winner-redo)
    )

(use-package desktop
  :config
  (setq desktop-path (list "~/.emacs.d/desktop-confs/standard"))
    (desktop-save-mode 1)
  )

(use-package org
  :config
    (add-hook 'org-mode-hook 'visual-line-mode)
    (define-key org-mode-map (kbd "C-c i") 'org-latex-export-to-pdf)

    ;; org-mode leuven theme
    ;;(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/emacs-leuven-theme")
    ;;(add-hook 'after-init-hook (lambda () (load-theme 'leuven t)))
    (defun org-archive-done-tasks-subtree ()
      (interactive)
      (org-map-entries
      (lambda ()
      (org-archive-subtree)
      (setq org-map-continue-from (outline-previous-heading)))
      "/DONE" 'tree))
    
    (defun org-archive-done-tasks-file ()
      (interactive)
      (org-map-entries
      (lambda ()
      (org-archive-subtree)
      (setq org-map-continue-from (outline-previous-heading)))
      "/DONE" 'file))

    ;; make sure that python and elisp code
    ;; blocks can be evaluated in org-mode
    (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (python . t)
        (shell . t)
        (haskell . t))
      )

    ;; add koma-article to org-mode
    (with-eval-after-load "ox-latex"
      (add-to-list 'org-latex-classes
                   '("koma-article" "\\documentclass{scrartcl}"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
    
    (setq org-latex-pdf-process 
	  '("latexmk -pdf -pdflatex=lualatex -bibtex %f"))

    ;; (setq org-latex-create-formula-image-program 'imagemagick)

    (require 'org-inlinetask)  ;; new inline-todo with C-c C-x t
)

(use-package evil
  :init 
    (setq evil-want-C-u-scroll t) ;; do this before you require evil
    (add-to-list 'load-path "~/.emacs.d/evil")

    ;; evil-collection, see https://github.com/emacs-evil/evil-collection#installation
    (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
    (setq evil-want-keybinding nil)
  :config
    (evil-mode 1)
    (add-to-list 'evil-emacs-state-modes 'nav-mode)
    (add-to-list 'evil-emacs-state-modes 'pdf-occur-buffer-mode)

    ;; only ever go up/down visual lines
    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package linum-relative
  :config
    (add-hook 'prog-mode-hook 'linum-on)
    (setq linum-relative-current-symbol "")
    (linum-relative-mode)
    ;; only for files, not for regular other buffers
    ;; (add-hook 'find-file-hook 'linum-mode)
)

(use-package tex
  :defer t
  :ensure auctex
  :config
    (setq TeX-auto-save t)
    ;; in latex-mode with auctex, don't use fancy fontification for math
    (setq tex-fontify-script nil)
    (setq font-latex-fontify-script nil)
    
    ;; also don't use big ugly headings
    (setq font-latex-fontify-sectioning 'color)
    (setq font-latex-fontify-sectioning 1.0)
)

(use-package yasnippet
  :config
    (yas-global-mode)
    (defun my-org-latex-yas ()
      ;; Activate org and LaTeX yas expansion in org-mode buffers.
      (yas-minor-mode)
      (yas-activate-extra-mode 'latex-mode))
    
    (add-hook 'org-mode-hook #'my-org-latex-yas)
    )

(use-package yasnippet-snippets)

(use-package pdf-tools
  :config
    (define-key pdf-view-mode-map (kbd "C-c C-l") 'org-store-link)
)


(use-package org-pdfview
  ;; org-pdfview: it's not a minor-mode, just a few functions that adapt
  ;; orgs behavior if pdf-view-mode is enabled, e.g. for storing links,
  ;; a special function is called
  :config
    (pdf-tools-install)
    ;; (pdf-loader-install)
    
    ;; override a function in org-pdfview so that the description is not the whole file path
    (eval-after-load "org-pdfview"
      (defun org-pdfview-store-link ()
        "  Store a link to a pdfview buffer."
        (when (eq major-mode 'pdf-view-mode)
          ;; This buffer is in pdf-view-mode
          (let* ((path buffer-file-name)
         	  (page (pdf-view-current-page))
         	  (link (concat "pdfview:" path "::" (number-to-string page))))
            (org-store-link-props
             :type "pdfview"
             :link link
             :description (concat (nth 0 (split-string (file-name-nondirectory buffer-file-name) "-")) "::" (number-to-string (pdf-view-current-page)))
             ))))
    )
)



