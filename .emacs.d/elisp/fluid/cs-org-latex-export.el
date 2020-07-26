;;; cs-org-latex-export.el --- org latex export settings  -*- lexical-binding: t; -*-

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


(setq org-latex-pdf-process '("latexmk -pdf -pdflatex=lualatex -bibtex %f"))

;; export body shortcut
(fset 'latex-body-export
      (lambda (&optional arg)
        "Keyboard macro for LaTeX body export only."
        (interactive "p")
        (kmacro-exec-ring-item (quote ("ll" 0 "%d"))
                               arg)))


;; add koma-article to org-mode
(with-eval-after-load "ox-latex"
  (add-to-list 'org-latex-classes
               '("koma-article" "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


;; ---- beamer presentations with org mode
(eval-after-load "ox-latex"
  ;; update the list of LaTeX classes and associated header (encoding, etc.)
  ;; and structure

  '(add-to-list 'org-latex-classes
                `("beamer" ,(concat "\\documentclass[presentation]{beamer}\n"
                                    "[DEFAULT-PACKAGES]" "[PACKAGES]" "[EXTRA]\n")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))



;; nicer code-blocks with listings instead of verbatim
;; FIXME: use minted instead
(setq org-latex-listings t)


;; make keywords colored by loading this module
(require 'cs-org-colored-keywords)

(provide 'cs-org-latex-export)
;;; cs-org-latex-export.el ends here
