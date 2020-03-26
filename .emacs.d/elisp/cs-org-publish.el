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

  (unless filepath
    (setq filepath (helm-read-file-name "Select org file to publish: " :initial-input (buffer-file-name))))

  ;; (if (and (not filepath) (string-equal (file-name-extension (buffer-file-name)) "org"))
  ;;     (setq filepath (buffer-file-name))
  ;;   (setq filepath (helm-read-file-name "Publish this file: ")))

  (let* ((project-base-dir (helm-read-file-name (concat "Select base directory for publishing "
                                                        filepath ": ")
                                                :initial-input (file-name-directory filepath)))
         (project-publish-dir
          (helm-read-file-name (concat "Select publishing directory for publishing "
                                       filepath " to html: ")
                               :initial-input
                               ;; (expand-file-name (file-name-directory "~/Desktop/test-publish-target-dir/"))
                               (concat (get-base-dir-as-eg-next-git-root) "www-buf")))
                          project-component-doc-name
                          project-component-other-name)

    (when (not (file-exists-p project-publish-dir))
      (make-directory project-publish-dir t))

    (setq org-publish-project-alist `(("project" :base-directory ,project-base-dir
                                       :publishing-function (lambda ()
                                                              (my-org-html-publish-to-my-html)
                                                              (get-home-as-org-file))
                                       :publishing-directory,project-publish-dir
                                       :exclude ".*"
                                       :include [,filepath])
                                      ("project-attachments" :base-directory ,project-base-dir
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





(cl-defstruct post-metadata title date last-modified-date file-name-base relative-dir-path)

(defun my-sort-for-what (mylist sort-for-what)
  (let* (;; (foo (make-post-metadata :title "dis foo title"))
         ;; (bar (make-post-metadata :title "dis bar title"))
         ;; (mylist (list foo bar))
         )
    (sort mylist `(lambda (elem1 elem2) (string-lessp (,sort-for-what elem1) (,sort-for-what elem2))))))


(defun get-all-post-metadatas (base-dir &optional forbidden-file-names)
  (interactive)
  (setq forbidden-file-names (list "sitemap.org" "home.org" "index.org"))
  ;; (setq base-dir (expand-file-name "/home/chris/Dropbox/1Projects/programming/derivations-site"))
  (let* ((file-path-list (remove nil
                                 (mapcar (lambda (file-path)
                                           (when (not (member (file-name-nondirectory file-path) forbidden-file-names))
                                             file-path))
                                         (remove nil
                                                 (mapcar (lambda (file-path)
                                                           (when (and (not (string-equal file-path ""))
                                                                      (file-exists-p file-path))
                                                             file-path))
                                                         (split-string (shell-command-to-string (concat "find "
                                                                                                        (prin1-to-string base-dir)
                                                                                                        " -name \"*.org\" -type f"))
                                                                       "\n")))))))
    (mapcar (lambda (path)
              (parse-file path base-dir))
            file-path-list)))

(defun get-base-dir-as-eg-next-git-root ()
  (helm-read-file-name "Select base-dir (e.g. a git directory root?):  "
                       :initial-input (file-name-as-directory (car (split-string (shell-command-to-string "git rev-parse --show-toplevel")
                                                                                 "\n")))))

(defun parse-file (&optional file-path base-dir)
  (interactive)
  (unless file-path
    ;; (setq file-path (expand-file-name "~/Dropbox/1Projects/programming/derivations-site/testpost.org"))
    ;; (setq file-path (expand-file-name "/home/chris/Dropbox/org/notes/software/site.org"))
    )

  (let* (relative-dir-path)
    (unless base-dir
      (setq base-dir (get-base-dir-as-eg-next-git-root)))
    (setq relative-dir-path (file-relative-name file-path base-dir))
    (with-temp-buffer
      (insert-file-contents file-path)
      (org-mode)
      (org-element-parse-buffer)
      (let* ((title (org-element-property :value (car (org-global-props-get-plist "TITLE"))))
             (date (org-element-property :value (car (org-global-props-get-plist "DATE"))))
             (last-modified-date (format-time-string "%Y-%m-%d"
                                                     (file-attribute-status-change-time (file-attributes file-path)))))
        `(,title
          ,date
          ,last-modified-date
          ,(file-name-base file-path)
          ,(file-name-directory file-path))
        (make-post-metadata :title title
                            :date date
                            :last-modified-date last-modified-date
                            :file-name-base (file-name-base file-path)
                            :relative-dir-path (file-name-directory file-path))))))

(defun print-post-metadata-into-org (pm-instance)
  ""
  (insert "[["
          (concat (file-name-as-directory (post-metadata-relative-dir-path pm-instance))
                  (post-metadata-file-name-base pm-instance)
                  ".org")
          "]["
          (post-metadata-title pm-instance)
          "]]"))

(defun get-home-as-org-file (&optional publish-dir)
  "Publish the sitemap as an org file first.
Then, it can also be converted to an html file, together with
all other org files."
  (interactive)
  (with-temp-buffer
    (org-mode)
    (org-element-parse-buffer)
    (insert "#+OPTIONS: tex:dvisvgm\n")
    (insert "#+OPTIONS: num:nil\n")
    (insert "#+TITLE: Home\n")

    (insert "
#+BEGIN_EXPORT html
<br></br>
#+END_EXPORT
")

    (let* ((base-dir (get-base-dir-as-eg-next-git-root))
           (pm-list (get-all-post-metadatas base-dir))
           sorted-list)
      ;; sort after last modified date and print
      (setq sorted-list (reverse (my-sort-for-what pm-list 'post-metadata-last-modified-date)))
      (when sorted-list
        (insert "\n")
        ;; sort
        (insert "*Most recently updated pages:*")
        (insert "\n")
        (insert "\n")
        (let* ((ctr 0))
          (while (and (nth ctr sorted-list)
                      (< ctr 3))
            (print-post-metadata-into-org (nth ctr sorted-list))
            (insert "\n")
            (insert "\n")
            (setq ctr (+ 1 ctr))))
        (insert "
#+BEGIN_EXPORT html
<hr>
#+END_EXPORT"))
      ;; sort after last posted date and print
      (setq sorted-list (reverse (my-sort-for-what pm-list 'post-metadata-date)))
      (when sorted-list
        (insert "\n")
        ;; sort
        (insert "*Most recent posts:*")
        (insert "\n")
        (insert "\n")
        (let* ((ctr 0))
          (while (and (nth ctr sorted-list)
                      (< ctr 3))
            (print-post-metadata-into-org (nth ctr sorted-list))
            (insert "\n")
            (insert "\n")
            (setq ctr (+ 1 ctr))))
        (insert "
#+BEGIN_EXPORT html
<hr>
#+END_EXPORT"))
      ;; sort after title
      (setq sorted-list (reverse (my-sort-for-what pm-list 'post-metadata-title)))
      (when sorted-list
        (insert "\n")
        ;; sort
        (insert "*All posts, sorted after title:*")
        (insert "\n")
        (insert "\n")
        (let* ((ctr 0))
          (while (nth ctr sorted-list)
            (print-post-metadata-into-org (nth ctr sorted-list))
            (insert "\n")
            (insert "\n")
            (setq ctr (+ 1 ctr)))))
      ;; (write-file (expand-file-name "/home/chris/Desktop/demo.org"))
      (write-file (helm-read-file-name "Publish the home.org to: " :initial-input (concat base-dir "home.org"))))))

(defun org-global-props-get-plist (&optional property buffer)
  "Get the plists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'cs-org-publish)
;;; cs-org-publish.el ends here
