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
 '(custom-enabled-themes (quote (misterioso)))
 '(package-selected-packages
   (quote
    (org-pdfview pdf-tools auctex-lua auctex-latexmk auctex yasnippet-snippets yasnippet linum-relative exec-path-from-shell projectile clojure-mode-extra-font-locking cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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



" --- BEGIN Plugin-specific settings --- "


(require 'org)


" EVIL-MODE "
(setq evil-want-C-u-scroll t) " do this before you require evil "
(add-to-list 'load-path "~/.emacs.d/evil")

(require 'evil)
(evil-mode 1)
(add-to-list 'evil-emacs-state-modes 'nav-mode)
(add-to-list 'evil-emacs-state-modes 'pdf-occur-buffer-mode)


(tool-bar-mode -1)


(require 'linum-relative)
(add-hook 'prog-mode-hook 'linum-on)
(setq linum-relative-current-symbol "")
(linum-relative-mode)
" only for files, not for regular other buffers "
" (add-hook 'find-file-hook 'linum-mode) "


(yas-global-mode)


" in latex-mode with auctex, don't use fancy fontification for math "
(setq tex-fontify-script nil)
(setq font-latex-fontify-script nil)

" also don't use big ugly headings "
(setq font-latex-fontify-sectioning 'color)
(setq font-latex-fontify-sectioning 1.0)

" save the command history of the preceding emacs sessions "
(require 'yasnippet)


(defun my-org-latex-yas ()
  "Activate org and LaTeX yas expansion in org-mode buffers."
  (yas-minor-mode)
  (yas-activate-extra-mode 'latex-mode))

(add-hook 'org-mode-hook #'my-org-latex-yas)


" googling quickly "

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


" visual-line-mode "
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)

" only ever go up/down visual lines "
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

" pdf-tools "
(require 'pdf-tools)

" org-pdfview "
(require 'org-pdfview)
(pdf-tools-install)
" (pdf-loader-install) "
