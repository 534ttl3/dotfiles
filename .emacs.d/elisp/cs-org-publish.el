;;; cs-org-publish.el --- publish website            -*- lexical-binding: t; -*-

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


(require 'ox)
(require 'ox-html)
(require 'ox-publish)

(defun my-org-html-publish-to-my-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'my-html filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-html-extension
				      "html"))
		      plist pub-dir))


(defun publish-project (&optional current-directory)
  "Always publish the whole project.
   TODO: - check that only those files that are linked from org documents
           (and are in the org subfolder or the assets subfolder) are published as attachments
           use/modify the publishing function for that
         - operate on latex export blocks (which have a special tag),
           run latex on them, convert them to svg (if they aren't more than 1 page in total)
           and embed them into the html with a set width.
           if you want them to be exported to html as they would be to latex
           - to do this, you could use sth similar to
             #+ATTR_BEAMER: :options [Lagrange]
             but invent your own ATTR, e.g.:
             #+ATTR_KLIN: :options try_convert_svg_html_export width:300px

           Interesting links:
           https://emacs.stackexchange.com/questions/45751/org-export-to-different-directory
           - check if there is some way of running latex blocks and exporting their output to html
             and exporing them to latex as-is
           "
  (interactive)
  (if (not current-directory)
      (setq current-directory (expand-file-name "~")))

  (let* ((project-name "site")
         (project-component-doc-name (concat project-name "org"))
         (project-component-other-name (concat project-name "other"))
         (project-component-all (concat project-name "all"))
         (project-base-dir (helm-read-file-name "Publish: Select base dir:"
                                                :initial-input (expand-file-name (expand-file-name (file-name-directory (if (buffer-file-name)
                                                                                                                            (buffer-file-name)
                                                                                                                          (concat (file-name-directory current-directory) "org/")))))))
         (project-publish-dir (helm-read-file-name "Select publishing dir:"
                                                   :initial-input (expand-file-name ;; (file-name-directory (if (buffer-file-name)
                                                                                    ;;                          (buffer-file-name)
                                                                   ;;                        (concat (file-name-directory current-directory) "public/")))
                                                                   (concat project-base-dir "../public/")
                                                                                    )))
         ;; (project-publish-dir (expand-file-name (concat "~/projects/" project-name "/" "public/")))
         ;; (project-base-dir (expand-file-name (concat "~/projects/" project-name "/" "org/")))
         )
    (setq org-publish-project-alist `((,project-component-doc-name :base-directory ,project-base-dir
                                                                   :base-extension "org"
                                                                   :publishing-directory ,project-publish-dir
                                                                   :recursive t
                                                                   :publishing-function my-org-html-publish-to-my-html
                                                                   :auto-sitemap t
                                                                   )
                                      (,project-component-other-name :base-directory ,project-base-dir
                                                                     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
                                                                     :publishing-directory ,project-publish-dir
                                                                     :recursive t
                                                                     :publishing-function org-publish-attachment)
                                      (,project-component-all
                                       :components (,project-component-doc-name ,project-component-other-name))))
    (org-publish-reset-cache)
    (org-publish-remove-all-timestamps)
    (let* ()
      (org-publish project-component-all t t))))



(defun publish-file (&optional filepath)
  (interactive)

  (setq filepath "/home/chris/Dropbox/org/notes/software/site.org")

  ;; (if (and (not filepath) (string-equal (file-name-extension (buffer-file-name)) "org"))
  ;;     (setq filepath (buffer-file-name))
  ;;   (setq filepath (helm-read-file-name "Publish this file: ")))

  (let* (project-base-dir
         project-publish-dir
         project-component-doc-name
         project-component-other-name)
    (setq org-publish-project-alist `(("project"
                                       :base-directory ,(setq project-base-dir (helm-read-file-name (concat "Select base directory for publishing " filepath ": ")
                                                                                                    :initial-input (file-name-directory filepath)))
                                       :publishing-function my-org-html-publish-to-my-html
                                       :publishing-directory ,(setq project-publish-dir (helm-read-file-name (concat "Select publishing directory for publishing "
                                                                                                                     filepath ": ")
                                                                                                             :initial-input (expand-file-name (file-name-directory "~/Desktop/test-publish-target-dir/"))))
                                       :exclude ".*"
                                       :include [,filepath])
                                      ("project-attachments"
                                       :base-directory ,project-base-dir
                                       :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
                                       :publishing-directory ,project-publish-dir
                                       :recursive t
                                       :publishing-function org-publish-attachment)
                                      (,"project-all"
                                       :components ("project" "project-attachments")))))
  (org-publish-reset-cache)
  (org-publish-remove-all-timestamps)
  (let* ()
    (org-publish "project-all" t t)))

(provide 'cs-org-publish)
;;; cs-org-publish.el ends here
