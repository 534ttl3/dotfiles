;;; cs-org-blog-html-backend.el --- custom html backend to export to jekyll blog  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  chris

;; Author: chris <chris@chris-lenovo>
;; Keywords: abbrev

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


(require 'cl-lib)
(require 'ox)
(require 'ox-publish)
(require 'ox-html)

(defvar org-latex-default-packages-alist)
(defvar org-latex-packages-alist)
(defvar orgtbl-exp-regexp)

(org-export-define-derived-backend 'my-html 'html
  :translate-alist '((template . my-org-html-template)
                     ;; (latex-environment . my-org-html-latex-environment)
                     ;; (latex-fragment . my-org-html-latex-fragment)
                     )
  :menu-entry
  '(?y "Export to blog using my-html"
       (;; (?H "As HTML buffer" org-html-export-as-html)
	(?h "As HTML file" my-org-html-export-to-html)
	;; (?o "As HTML file and open"
	;;     (lambda (a s v b)
	;;       (if a (org-html-export-to-html t s v b)
	;; 	(org-open-file (org-html-export-to-html nil s v b)))))
    )))

;; (defun my-org-html-export-to-html
;;     (&optional async subtreep visible-only body-only ext-plist)
;;   "Export current buffer to a HTML file.

;; If narrowing is active in the current buffer, only export its
;; narrowed part.

;; If a region is active, export that region.

;; A non-nil optional argument ASYNC means the process should happen
;; asynchronously.  The resulting file should be accessible through
;; the `org-export-stack' interface.

;; When optional argument SUBTREEP is non-nil, export the sub-tree
;; at point, extracting information from the headline properties
;; first.

;; When optional argument VISIBLE-ONLY is non-nil, don't export
;; contents of hidden elements.

;; When optional argument BODY-ONLY is non-nil, only write code
;; between \"<body>\" and \"</body>\" tags.

;; EXT-PLIST, when provided, is a property list with external
;; parameters overriding Org default settings, but still inferior to
;; file-local settings.

;; Return output file's name."
;;   (interactive)

;;   (let* ((extension (concat "." (or (plist-get ext-plist :html-extension)
;; 				    org-html-extension
;; 				    "html")))
;;          (file ;; (if (bound-and-true-p jekyll-posts-directory)
;;                ;;     (helm-read-file-name "Select and type target file or dir: "
;;                ;;                          :initial-input (if (file-exists-p jekyll-posts-directory)
;;                ;;                                             (concat
;;                ;;                                              (expand-file-name jekyll-posts-directory)
;;                ;;                                              ;; (file-name-directory (buffer-file-name))
;;                ;;                                              (format-time-string "%Y-%m-%d")
;;                ;;                                              "-"
;;                ;;                                              (file-name-base (buffer-file-name))
;;                ;;                                              extension)))
;;                ;;   )
;;                (org-export-output-file-name extension subtreep))
;;          (org-export-coding-system org-html-coding-system))
;;     (org-export-to-file 'my-html file
;;       async subtreep visible-only body-only ext-plist)))

(defvar my-html-preamble-str
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />
<title></title>
<style type=\"text/css\">
  @font-face {
    font-family: \"Computer Modern\";
    src: url('http://mirrors.ctan.org/fonts/cm-unicode/fonts/otf/cmunss.otf');
  }
  @font-face {
    font-family: \"Computer Modern\";
    src: url('http://mirrors.ctan.org/fonts/cm-unicode/fonts/otf/cmunsx.otf');
    font-weight: bold;
  }
  @font-face {
    font-family: \"Computer Modern\";
    src: url('http://mirrors.ctan.org/fonts/cm-unicode/fonts/otf/cmunsi.otf');
    font-style: italic, oblique;
  }
  @font-face {
    font-family: \"Computer Modern\";
    src: url('http://mirrors.ctan.org/fonts/cm-unicode/fonts/otf/cmunbxo.otf');
    font-weight: bold;
    font-style: italic, oblique;
  }

  body {
    font-family: \"Computer Modern\", sans-serif;
  }

/* 2: min-width */
@media screen and (min-width: 800px) {
	#content {
        margin: auto;
        width: 800px;
	}
}

.equation-container {
    display: table;
    text-align: center;
    width: 100%;
}

.equation-label {
    display: table-cell;
    text-align: right;
    vertical-align: middle;
}
.org-svg { width: auto; }
</style>
</head>
<body>")


(defvar my-html-postamble-str "</body></html>")

(defun my-org-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (interactive)
  (concat
   ;; (let ((link-up (org-trim (plist-get info :html-link-up)))
   ;;   (link-home (org-trim (plist-get info :html-link-home))))
   ;;   (unless (and (string= link-up "") (string= link-home ""))
   ;;     (format (plist-get info :html-home/up-format)
   ;;         (or link-up link-home)
   ;;         (or link-home link-up))))
   ;; Preamble.
   ;; (org-html--build-pre/postamble 'preamble info)
    my-html-preamble-str
   "\n<div id=\"content\">\n"
   contents
   "\n</div>"
   ;; (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; Postamble.
   ;; (org-html--build-pre/postamble 'postamble info)
   ;; Possibly use the Klipse library live code blocks.
   ;; (when (plist-get info :html-klipsify-src)
   ;;   (concat "<script>" (plist-get info :html-klipse-selection-script)
   ;;       "</script><script src=\""
   ;;       org-html-klipse-js
   ;;       "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
   ;;       org-html-klipse-css "\"/>"))
   ;; Closing document.
   ;; "</body>\n</html>"
   my-html-postamble-str
   ))



(provide 'cs-org-blog-html-backend)
;;; cs-org-blog-html-backend.el ends here
