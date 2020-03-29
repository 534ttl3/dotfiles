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


;; (defconst foo "foo-global-value")

;; (defun otherfunc ()
;;   ""
;;   (prin1-to-string foo))

;; (defun myfunc ()
;;   ""
;;   (interactive)
;;   (prin1-to-string foo)
;;   (let* ((foo "foo-changed-value"))
;;     (prin1-to-string foo)
;;     (otherfunc)
;;     (prin1-to-string foo))
;;   (prin1-to-string foo))


;; --- setting up a custom org-publish backend ---



(cl-defstruct cs-relative-paths filepath relative-link-to-sitemap relative-link-to-index absolute-path-to-github-org-file)

;; (defconst cs-relative-paths "global-value")

(defconst cur-rel-paths (make-cs-relative-paths :filepath nil
                                                :relative-link-to-sitemap
                                                nil
                                                :relative-link-to-index
                                                nil
                                                :absolute-path-to-github-org-file
                                                nil))



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

(defun cs-org-publish-project (&optional project-root-dir)
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
             and exporing them to latex as-is"

  (interactive)

  (unless project-root-dir
    (setq project-root-dir (get-next-project-root (buffer-file-name)
                                                  t)))

  (let* (;; (some-variable "hey")
         (project-name "site")
         (project-component-doc-name (concat project-name "org"))
         (project-component-other-name (concat project-name "other"))
         (project-component-all (concat project-name "all"))
         (project-base-dir
          (if project-root-dir
              (get-projects-base-dir-from-root-dir project-root-dir)
            (helm-read-file-name "Publish: Select project's base dir:"
                                 :initial-input (get-projects-base-dir (buffer-file-name)))))
         (project-publish-dir
          (if project-root-dir
              (get-projects-publish-dir-from-root-dir project-root-dir)
            (helm-read-file-name "Select project's publishing buffer (for preview) dir:"
                                 :initial-input (get-projects-publish-dir (buffer-file-name))))
                              ;; publish it directly
                              ;; (get-publish-dir-from-git-root nil cs-my-public-website-root-dir)
                              ))

    (setq org-publish-project-alist
          `((,project-component-doc-name
             :base-directory ,project-base-dir
             :base-extension "org"
             :publishing-directory ,project-publish-dir
             :recursive t
             :publishing-function
             ((lambda (plist filename pub-dir)
                (let* ((cur-rel-paths
                        (make-cs-relative-paths :filepath filename
                                                :relative-link-to-sitemap
                                                (concat (file-relative-name ,project-base-dir
                                                                            (file-name-directory filename))
                                                        "sitemap.html")
                                                :relative-link-to-index
                                                (concat (file-relative-name ,project-base-dir
                                                                            (file-name-directory filename))
                                                        "index.html")
                                                :absolute-path-to-github-org-file
                                                (if (or (string-equal (file-name-nondirectory filename) "index.org")
                                                        (string-equal (file-name-nondirectory filename) "sitemap.org"))
                                                    (get-project-repo-url (get-project-repo-name ,project-root-dir))
                                                  (get-edit-on-github-link (get-project-repo-name ,project-root-dir)
                                                                           ,project-root-dir
                                                                           filename)))))
                  (my-org-html-publish-to-my-html plist filename
                                                  pub-dir))))
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
      (shell-command (concat "nautilus " (prin1-to-string project-publish-dir) " &") publish-buffer-name publish-buffer-name))
    ))



;;----- high-level, pushing to website functions -------

(defun publish-project-offline (&optional project-root-dir)
  (interactive)
  (unless project-root-dir
    (setq project-root-dir (get-next-project-root (buffer-file-name)
                                                  t))
    (unless project-root-dir
      (user-error "Root git dir not found")))
  ;; project
  (cs-clean-project-publish-buffer project-root-dir)
  (produce-index-org project-root-dir)
  (cs-org-publish-project project-root-dir))

(defun publish-project-to-website-repo-offline (&optional project-root-dir)
  (interactive)
  (unless project-root-dir
    (setq project-root-dir (get-next-project-root (buffer-file-name)  t))
    (unless project-root-dir
      (user-error "Root git dir not found")))

  ;; project
  (cs-clean-project-publish-buffer project-root-dir)

  (produce-index-org project-root-dir)

  (cs-org-publish-project)

  ;; website
  (cs-website-project-clear-and-paste
   ;; project publish dir
   (get-publish-dir-from-git-root t
                                  (get-next-git-root))
   ;; website's publish dir
   (get-publish-dir-from-git-root nil cs-my-public-website-root-dir))

  (when (yes-or-no-p "Preview the website's local repo? ")
    (shell-command (concat "nautilus " cs-my-public-website-root-dir " &") publish-buffer-name publish-buffer-name)))


(defun cs-website-project-clear-and-paste (&optional source-dir target-dir)
  "The website project could in the future be composed of several projects.
But it can also just be a plain directory (with a .git and .gitignore)
in which the html of a project (with an index.html is pasted)"
  (interactive)
  (let* ()
    (unless source-dir
      (setq source-dir (helm-read-file-name "Select buffer publishing dir: "
                                            :initial-input (get-publish-dir-from-git-root t
                                                                                          (get-next-git-root)))))
    (unless target-dir
      (setq target-dir (helm-read-file-name "Select public publishing dir: "
                                            :initial-input (get-publish-dir-from-git-root nil cs-my-public-website-root-dir))))

    ;; remove everything from the website's git repository directory (except the configuration files and the .git)
    (with-output-to-temp-buffer publish-buffer-name
      (shell-command (read-shell-command "Trash the website repo's html (except dotfiles): "
                                         (concat " cd " (prin1-to-string target-dir) " ; "
                                                 " trash -rf ./* ; "))
                     publish-buffer-name))

    ;; override to website's git repository directory
    (with-output-to-temp-buffer publish-buffer-name
      (shell-command (read-shell-command "Copy the html to the website repo: "
                                         (concat "cp -af "
                                                 (prin1-to-string (concat (file-name-as-directory source-dir)
                                                                          "."))
                                                 " "
                                                 (prin1-to-string target-dir)))
                     publish-buffer-name))
    ;; (pop-to-buffer publish-buffer-name)
    ))

(defun cs-deploy-website-with-git ()
  (interactive)
  "Push the website's generated html to github pages."
  (let* ((big-project-root-dir cs-my-public-website-root-dir))
    (with-output-to-temp-buffer publish-buffer-name
      (shell-command (read-shell-command "Run the pushing command like this: "
                                         (concat " cd " (prin1-to-string big-project-root-dir) " ; "
                                                 " git add . ; git commit -m 'pushing html' ; git push ; "))
                     publish-buffer-name))
    ;; go to the website to preview
    ;; (when (yes-or-no-p "Wanna open the website in browser?")
    ;;   (browse-url cs-my-github-website-url)
    ;;   (browse-url cs-my-github-website-repo-url))
    ))


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
      (setq sorted-list (reverse
                         (remove nil (mapcar (lambda (metadata)
                                               (when (post-metadata-date metadata)
                                                 metadata))
                                             (my-sort-for-what (copy-list pm-list) 'post-metadata-date)))))
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

;; (defun update-index-and-publish-project (&optional root-dir)
;;   (interactive)
;;   (unless root-dir
;;     (setq root-dir (get-next-git-root)))
;;   (produce-index-org root-dir)
;;   (cs-org-publish-project nil root-dir t))

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



(defun cs-org-publish-run-hydra ()
  ""
  (interactive)
  (let* ((hydra-body (eval (remove nil
                                   `(defhydra hydra-cs-org-publish
                                      (:columns 1 :exit t)
                                      "cs-org-publish: options for publishing a blog post"
                                      ("t s o"
                                       (lambda ()
                                         (interactive)
                                         (cs-transfer-single-org-file))
                                       "transfer single org file")
                                      ("p l f"
                                       (lambda ()
                                         (interactive)
                                         (pull-files-into-asset-dir))
                                       "pull linked files into asset directory")
                                      ("p p o"
                                       (lambda ()
                                         (interactive)
                                         (publish-project-offline))
                                       "publish the project offline for preview")
                                      ("p w r"
                                       (lambda ()
                                         (interactive)
                                         (publish-project-to-website-repo-offline))
                                       "publish the project offline to website repo")
                                      ("d w g"
                                       (lambda ()
                                         (interactive)
                                         (cs-deploy-website-with-git))
                                       "deploy website with git")
                                      ("q" nil "cancel")))))))
    (hydra-cs-org-publish/body)
    (fmakunbound 'hydra-cs-org-publish/body)
    (setq hydra-cs-org-publish/body nil))

(define-key org-mode-map (kbd "C-M-, P") ; process
  'cs-org-publish-run-hydra)


(provide 'cs-org-publish)
;;; cs-org-publish.el ends here
