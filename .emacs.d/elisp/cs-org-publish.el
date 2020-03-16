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
(require 'org-publish)

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


(defun publish-my-project ()
  "Always publish the whole project."
  (interactive)
  (let* ((project-name "blog")
         (project-component-doc-name (concat project-name "org"))
         (project-component-other-name (concat project-name "other"))
         (project-component-all (concat project-name "all"))
         (project-base-dir (expand-file-name (concat "~/projects/" project-name "/" "org/")))
         (project-publish-dir (expand-file-name (concat "~/projects/" project-name "/" "public/"))))
    (setq org-publish-project-alist `((,project-component-doc-name :base-directory ,project-base-dir
                                                                   :base-extension "org"
                                                                   :publishing-directory ,project-publish-dir
                                                                   :recursive t
                                                                   :publishing-function my-org-html-publish-to-my-html)
                                      (,project-component-other-name :base-directory ,project-base-dir
                                                                     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
                                                                     :publishing-directory ,project-publish-dir
                                                                     :recursive t
                                                                     :publishing-function org-publish-attachment)
                                      (,project-component-all
                                       :components (,project-component-doc-name ,project-component-other-name))))
    (org-publish project-component-all)))

(provide 'cs-org-publish)
;;; cs-org-publish.el ends here
