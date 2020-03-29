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
(require 'cs-org-publish-utils)
(require 'cs-org-transfer)


;; --- setting up a custom org-publish backend ---

(defun my-org-html-publish-to-my-html (plist filename pub-dir ;; &optional anotherone
                                             )
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  ;; (message (concat (prin1-to-string anotherone)))
  (org-publish-org-to 'my-html filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-html-extension
				      "html"))
		      plist pub-dir))

(defun publish-project (&optional current-directory git-root for-preview)
  "Publish the whole project.
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

  (let* ((some-variable "hey")
         (project-name "site")
         (project-component-doc-name (concat project-name "org"))
         (project-component-other-name (concat project-name "other"))
         (project-component-all (concat project-name "all"))
         (project-base-dir (helm-read-file-name "Publish: Select project's base dir:"
                                                :initial-input (get-projects-base-dir (buffer-file-name))))
         (project-publish-dir (if for-preview
                                  (helm-read-file-name "Select project's publishing buffer (for preview) dir:"
                                                       :initial-input (get-projects-publish-dir (buffer-file-name)))
                                ;; publish it directly
                                (get-publish-dir-from-git-root nil cs-my-public-website-root-dir))))

    (setq org-publish-project-alist `((,project-component-doc-name :base-directory ,project-base-dir
                                                                   :base-extension "org"
                                                                   :publishing-directory ,project-publish-dir
                                                                   :recursive t
                                                                   :publishing-function my-org-html-publish-to-my-html
                                                                   :auto-sitemap t
                                                                   :sitemap-title "Sitemap")
                                      (,project-component-other-name :base-directory ,project-base-dir
                                                                     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
                                                                     :publishing-directory ,project-publish-dir
                                                                     :recursive t
                                                                     :publishing-function org-publish-attachment)
                                      (,project-component-all
                                       :components (,project-component-doc-name ,project-component-other-name))))

    (org-publish-reset-cache)
    (org-publish-remove-all-timestamps)
    (org-publish project-component-all t nil)

    (when (yes-or-no-p "Do you want to open the preview folder? ")
      (shell-command (concat "nautilus " (prin1-to-string project-publish-dir) " &") publish-buffer-name publish-buffer-name))))



;;----- high-level, pushing to website functions -------

(defun publish-project-one-whack (&optional root-dir)
  (interactive)
  (unless root-dir
    (setq root-dir (get-next-git-root))
    (unless root-dir
      (user-error "Root git dir not found")))

  (cs-clean-project-publish-buffer root-dir)
  (update-index-and-publish-project root-dir)
  (publish-override-directory-commit-and-push (get-publish-dir-from-git-root t (get-next-git-root))
                                              (get-publish-dir-from-git-root nil cs-my-public-website-root-dir)))

(defun publish-override-directory-commit-and-push (&optional source-dir target-dir)
  (interactive)
  (let* ()
    (unless source-dir
      (setq source-dir (helm-read-file-name "Select buffer publishing dir: "
                                            :initial-input (get-publish-dir-from-git-root t
                                                                                          (get-next-git-root)))))
    (unless target-dir
      (setq target-dir (helm-read-file-name "Select public publishing dir: "
                                            :initial-input (get-publish-dir-from-git-root nil cs-my-public-website-root-dir))))

    ;; override to website's git repository directory
    (with-output-to-temp-buffer publish-buffer-name
      (shell-command (read-shell-command "Run the publishing command like this: "
                                         (concat "cp -af "
                                                 (prin1-to-string (concat (file-name-as-directory source-dir)
                                                                          "."))
                                                 " "
                                                 (prin1-to-string target-dir)))
                     publish-buffer-name))
    (pop-to-buffer publish-buffer-name)

    ;; add all, commit and push in one step
    (if (yes-or-no-p "Continue to add all, commit and push?")
        (progn
          (with-output-to-temp-buffer publish-buffer-name
            (shell-command (read-shell-command "Run the pushing command like this: "
                                               (concat " cd " (prin1-to-string target-dir) " ; "
                                                       " git add . ; git commit -m 'pushing html' ; git push ; "))
                           publish-buffer-name))
          ;; go to the website to preview
          (when (yes-or-no-p "Wanna open the website in browser?")
            (browse-url (concat "https://"
                                (file-name-nondirectory (directory-file-name (file-name-directory cs-my-public-website-root-dir)))))
            (browse-url (concat cs-my-github-page-url
                                (file-name-nondirectory (directory-file-name (file-name-directory cs-my-public-website-root-dir)))))))

      ;; else, just open the directory in dired
      (dired target-dir))))


;; ----- creating/updating the index.html of a project from it's set of org files ----

(defun get-index-as-org-file (&optional root-dir)
  "Publish the index.org (overview of recent posts) as an org file first.
Then, it can also be converted to an html file, together with
all other org files."
  (interactive)
  (with-temp-buffer
    (org-mode)
    (org-element-parse-buffer)
    (insert "#+OPTIONS: tex:dvisvgm\n")
    (insert "#+OPTIONS: num:nil\n")
    (insert "#+TITLE: Home\n")

    (let* ((base-dir (get-org-dir-from-git-root root-dir))
           (pm-list (get-all-post-metadatas base-dir)) sorted-list)
      ;; ;; sort after last modified date and print
      ;; ;; this doesn't really work yet, since org-publish touches every org file when publishing
;;       (setq sorted-list (reverse (my-sort-for-what pm-list 'post-metadata-last-modified-date)))
;;       (when sorted-list
;;         (insert "\n")
;;         ;; sort
;;         (insert "*Most recently updated pages:*")
;;         (insert "\n")
;;         (insert "\n")
;;         (let* ((ctr 0))
;;           (while (and (nth ctr sorted-list)
;;                       (< ctr 3))
;;             (print-post-metadata-into-org (nth ctr sorted-list))
;;             (insert "\n")
;;             (insert "\n")
;;             (setq ctr (+ 1 ctr))))
;;         (insert "
;; #+BEGIN_EXPORT html
;; <hr>
;; #+END_EXPORT"))
      ;; sort after last posted date and print
      (setq sorted-list (reverse (my-sort-for-what (copy-list pm-list) 'post-metadata-date)))
      (when sorted-list
        (insert "\n")
        ;; sort
        (insert "#+BEGIN_EXPORT html"
                "\n"
                "<h2>Most recent posts:</h2>"
                "\n"
                "#+END_EXPORT"
                "\n"
                )
        (insert "\n")
        (insert "\n")
        ;; if it's not officially marked as post, don't post it!
        (let* ((ctr 0))
          (while (and (nth ctr sorted-list)
                      (< ctr 3))
            (print-post-metadata-into-org (nth ctr sorted-list))
            (insert "\n")
            (insert "\n")
            (setq ctr (+ 1 ctr))))
        (insert "
#+BEGIN_EXPORT html
<hr style=\"height: 8px;background: black;border: none;\">
#+END_EXPORT"))
      ;; sort after title
      (setq sorted-list (reverse (my-sort-for-what (copy-list pm-list) 'post-metadata-title)))
      (when sorted-list
        (insert "\n")
        ;; sort
        (insert "#+BEGIN_EXPORT html"
                "\n"
                "<h2>All posts, sorted after title:</h2>"
                "\n"
                "#+END_EXPORT"
                "\n"
                )
        (insert "\n")
        (insert "\n")
        (let* ((ctr 0))
          (while (nth ctr sorted-list)
            (print-post-metadata-into-org (nth ctr sorted-list))
            (insert "\n")
            (insert "\n")
            (setq ctr (+ 1 ctr)))))
      ;; (write-file (expand-file-name "/home/chris/Desktop/demo.org"))
      (write-file (helm-read-file-name "Write the index file to: "
                                       :initial-input (concat base-dir "index.org"))))))

(defun produce-index-org (root-dir)
  "Called typically from within the git subdirectory
 of the org project, after the org files in that project have been updated"
  (interactive)
  (unless root-dir
    (setq root-dir (get-next-git-root)))
  (get-index-as-org-file root-dir))

(defun update-index-and-publish-project (&optional root-dir)
  (interactive)
  (unless root-dir
    (setq root-dir (get-next-git-root)))
  (produce-index-org root-dir)
  (publish-project nil root-dir t))

;; --- importing an org file and it's needed assets into a project from outside -----

(defun cs-org-integrate-into-project (&optional org-file-path)
  "This means to select certain links in the org file (first level links) and
back them up into an assets directory, at the same level as the copied
org file, but also to check if that org file already links
to resources inside a project. There links are not copied, but are merely
adjusted in the org file."
  (interactive)
  (unless org-file-path
    (setq org-file-path (buffer-file-name)))
  (cs-transfer-single-org-file org-file-path t))

(provide 'cs-org-publish)
;;; cs-org-publish.el ends here
