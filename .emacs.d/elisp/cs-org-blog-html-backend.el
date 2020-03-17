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

(setq my-html-preamble-str
  (concat
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

    .slidingtopbar {
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

.topbar {
  width: 100%;
  margin: 0;
  padding: 0;
  background-color: #f1f1f1;
  overflow: auto;
  position: relative;
}

.topbar a {
  display: block;
  color: black;
  padding: 16px;
  text-decoration: none;
  float: left;
}

.topbar a:hover:not(.active){
  background-color: #555;
  color: white;
}

@media screen and (max-width: 1000px) {
  .topbar a {float: left;}
  div#content {margin-left: 0;}
}

@media screen and (max-width: 400px) {
  .topbar a {
    text-align: center;
    float: none;
  }
}
"
   "</style>
</head>
<body>
<div class=\"topbar\">
  <div class=\"slidingtopbar\">
    <a href=\"https://534ttl3.github.io/index.html>Home</a>
    <a href=\"sitemap.html\">Sitemap</a>
    <a href=\"#home\">Edit this page on GitHub</a>
  </div>
</div>"))

(setq my-html-postamble-str
      (concat
       "</body></html>"))

(defun my-org-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (interactive)
  (concat
   (let ((link-up (org-trim (plist-get info :html-link-up)))
     (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format (plist-get info :html-home/up-format)
           (or link-up link-home)
           (or link-home link-up))))
   ;; Preamble.
   (org-html--build-pre/postamble 'preamble info)

    my-html-preamble-str
   "\n<div id=\"content\">\n"
   contents
   "\n</div>"
   my-html-postamble-str
   ))

(provide 'cs-org-blog-html-backend)
;;; cs-org-blog-html-backend.el ends here
