;;; cs-org-blog-html-backend.el --- custom html backend to export to blog  -*- lexical-binding: t; -*-

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

(require 'cs-org-publish)

;; (progn
;;   (defun imalison:org-inline-css-hook (exporter)
;;     "Insert custom inline css to automatically set the
;; background of code to whatever theme I'm using's background"
;;     (when (eq exporter 'my-html)
;;       (let* ((my-pre-bg (face-background 'default))
;;              (my-pre-fg (face-foreground 'default)))
;;         (setq org-html-head-extra (concat org-html-head-extra
;;                                           (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
;;                                                   my-pre-bg my-pre-fg))))))
;;   (add-hook 'org-export-before-processing-hook
;;             'imalison:org-inline-css-hook))


(org-export-define-derived-backend 'my-html 'html
  :translate-alist '((template . my-org-html-template))
  :menu-entry
  '(?y "Export to blog using my-html"
       (;; (?H "As HTML buffer" org-html-export-as-html)
	(?h "As HTML file" my-org-html-publish-to-my-html)
	;; (?o "As HTML file and open"
	;;     (lambda (a s v b)
	;;       (if a (org-html-export-to-html t s v b)
	;; 	(org-open-file (org-html-export-to-html nil s v b)))))
    )))

(defun cs-my-html-insert-get-org-head-content (info)
  ""
  (org-html--build-meta-info info)
  (org-html--build-head info)
  (org-html--build-mathjax-config info))

(defun get-my-html-preamble-str (&optional org-info no-org sliding-topbar-verbose-html)
  "This is a general html preamble string.
It can be called by org to publish an html page,
but also just from any function which wants to create an html file."
  (concat
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />\n"
   "<head>\n"
   (unless no-org
     (cs-my-html-insert-get-org-head-content org-info))
   "<title>" "</title>"
   "<style type=\"text/css\">
  body {
      font-family: 'Helvetica', 'Arial', sans-serif;
      margin: 0px;
  }

  #content {
      padding: 5px;
  }

  .slidingtopbar {
      overflow: hidden;
      width: 800px;
  }

/* 2: min-width */
@media screen and (min-width: 800px) {
	#content {
        margin: auto;
        width: 775px;
	}

    .slidingtopbar {
        margin: auto;
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
  background-color: #242424;
  overflow: auto;
  position: relative;
}

.topbar a {
  display: block;
  color: white;
  padding: 16px;
  text-decoration: none;
  float: left;
  border-bottom: 5px solid cornflowerblue;
}

.topbar a:hover:not(.active){
  background-color: #555;
  color: white;
}

@media screen and (max-width: 400px) {
  .topbar a {
    text-align: center;
    float: none;
  }

  .slidingtopbar {
      overflow: auto;
      width: auto;
  }
}


.posted, .lastedited{
color: #646769;
font-family: -apple-system,BlinkMacSystemFont,\"Roboto\",\"Segoe UI\",\"Helvetica Neue\",\"Lucida Grande\",Arial,sans-serif;
font-size: .75em;
margin-right: 1em;
}

.post-container-div {
border-left: 5px solid cornflowerblue;
padding-left: 25px;
padding-top: 5px;
padding-bottom: 10px;
border-right: 5px solid cornflowerblue;
background: aliceblue;
margin-bottom: 25px;
margin-top: 25px;
}

pre.src {
width: auto;
position: absolute;
overflow: visible;
}

.container{
    width: 100%;
    text-align: center;
}

.project-container-div{
    background: whitesmoke;
    width: 260px;
    padding-top: 10px;
    padding-bottom: 10px;
    padding-left: 50px;
    padding-right: 10px;
    margin-left: 10px;
    margin-right: 10px;
    display: inline-block;
    margin-bottom: 25px;
    margin-top: 25px;
    border-right: 5px solid cornflowerblue;
}

"
   "</style>
</head>
<body>
<div class=\"topbar\">
  <div class=\"slidingtopbar\">"
   (if sliding-topbar-verbose-html
       sliding-topbar-verbose-html
     (concat "<a href=\"" (cs-relative-paths-relative-link-to-index cur-rel-paths) "\">Home</a>"
             "<a href=\"" (cs-relative-paths-relative-link-to-sitemap cur-rel-paths) "\">Sitemap</a>"
             "<a href=\"" (cs-relative-paths-absolute-path-to-github-org-file cur-rel-paths) "\">Edit on Github</a>"))
   "</div>
</div>"))

(defun get-my-html-print-title-str (info)
  (when (plist-get info :with-title)
     (let ((title (and (plist-get info :with-title)
		       (plist-get info :title)))
	   (subtitle (plist-get info :subtitle))
	   (html5-fancy (org-html--html5-fancy-p info)))
       (when title
	 (format
	  (if html5-fancy
	      "<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
	    "<h1 class=\"title\">%s%s</h1>\n")
	  (org-export-data title info)
	  (if subtitle
	      (format
	       (if html5-fancy
		   "<p class=\"subtitle\">%s</p>\n"
		 (concat "\n" (org-html-close-tag "br" nil info) "\n"
			 "<span class=\"subtitle\">%s</span>\n"))
	       (org-export-data subtitle info))
	    ""))))))

(defun get-my-footer ()
  ""
  (concat "<footer><div class=\"slidingtopbar\"><span>Contact: 534ttl3@gmail.com</span></div></footer>"))

(defun get-my-html-postamble-str ()
  (concat
   ;; (get-my-footer)
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

   (get-my-html-preamble-str info)

   "\n<div id=\"content\">\n"
   ;; Print the title (if it's there)
   (get-my-html-print-title-str info)

   ;; Print the date (if it's there)
   (let* ((spec (org-html-format-spec info))
          (date (cdr (assq ?d spec))))
     (concat
      (and (plist-get info :with-date)
           (org-string-nw-p date)
           (format "<span class=\"posted\">%s: %s</span>\n"
                   (org-html--translate "posted" info)
                   date))
      ;; (and (plist-get info :time-stamp-file)
      ;;      (format
      ;;       "<span class=\"lastedited\">%s: %s</span>\n"
      ;;       (org-html--translate "last edited" info)
      ;;       (format-time-string
      ;;        (plist-get info :html-metadata-timestamp-format))))
      ))

   ;; (prin1-to-string cur-rel-paths)

   contents
   "\n</div>"
   ;; (org-html--build-pre/postamble 'postamble info)
   (get-my-html-postamble-str)))

(defun my-html-template-plain (&optional sliding-topbar-html title-html
                                         content-html)
  "Return the HTML of my template as a string."
  (concat (get-my-html-preamble-str nil t sliding-topbar-html)
          "\n<div id=\"content\">\n"
          title-html
          content-html
          "\n</div>"
          (get-my-html-postamble-str)))



(provide 'cs-org-blog-html-backend)
;;; cs-org-blog-html-backend.el ends here
