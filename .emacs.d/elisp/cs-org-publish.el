
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

;; This you might need to change depending on your installation
(setq cs-my-public-website-root-dir (file-name-as-directory (expand-file-name "~/Dropbox/1Projects/programming/534ttl3.github.io")))
(setq cs-my-github-page-url "https://github.com/534ttl3/")
(defconst project-properties-filename ".project-properties")

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


(defun get-projects-base-dir (buffname)
  (expand-file-name (concat (file-name-as-directory (if (get-next-project-root buffname)
                                                        (get-next-project-root buffname)
                                                      (file-name-directory buffname)))
                            "org/")))

(defun get-projects-publish-dir (buffname)
  (expand-file-name (concat (file-name-as-directory (if (get-next-project-root buffname)
                                                        (get-next-project-root buffname)
                                                      (file-name-directory buffname)))
                            "publish-buffer/")))

(defun publish-project (&optional current-directory git-root for-preview)
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
    (org-publish project-component-all t t)
    ;; ask by using a hydra if you want to see the preview
    ;; (browse-url (concat "file://" project-publish-dir))
    ;; (defun run-dis-hydra ()
    ;;   (interactive)
    ;;   (let* ((hydra-body (eval (remove nil
    ;;                                    `(defhydra dis-hydra
    ;;                                       (:columns 1)
    ;;                                       ""
    ;;                                       ("p"
    ;;                                        (lambda ()
    ;;                                          (interactive)
    ;;                                          )
    ;;                                        "preview your output")
    ;;                                       ("m b"
    ;;                                        (lambda ()
    ;;                                          (interactive)
    ;;                                          (switch-to-buffer "*Messages*" t))
    ;;                                        "go to messages buffer")
    ;;                                       ("q" nil "cancel this hydra"))))))
    ;;     (dis-hydra/body)
    ;;     (fmakunbound 'dis-hydra/body)
    ;;     (setq dis-hydra/body nil)))
    ;; (run-dis-hydra)
    ))


(defun publish-file (&optional filepath)
  (interactive)

  (unless filepath
    (setq filepath (helm-read-file-name "Select org file to publish: " :initial-input (buffer-file-name))))

  (let* ((project-base-dir (helm-read-file-name (concat "Select base directory for publishing "
                                                        filepath ": ")
                                                :initial-input (file-name-directory filepath)))
         (project-publish-dir
          (helm-read-file-name (concat "Select publishing directory for publishing "
                                       filepath " to html: ")
                               :initial-input
                               ;; (expand-file-name (file-name-directory "~/Desktop/test-publish-target-dir/"))
                               (concat (get-next-git-root) "www-buf")))
                          project-component-doc-name
                          project-component-other-name)

    (when (not (file-exists-p project-publish-dir))
      (make-directory project-publish-dir t))

    (setq org-publish-project-alist `(("project" :base-directory ,project-base-dir
                                       :publishing-function my-org-html-publish-to-my-html
                                       :publishing-directory ,project-publish-dir
                                       :exclude ".*"
                                       :include [,filepath])
                                      ("project-attachments" :base-directory ,project-base-dir
                                       :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
                                       :publishing-directory ,project-publish-dir
                                       :recursive t
                                       :publishing-function org-publish-attachment)
                                      (,"project-all"
                                       :components ("project" "project-attachments"))))


    (org-publish-reset-cache)
    (org-publish-remove-all-timestamps)
    (let* ()
      (org-publish "project-all" t nil))

    (get-index-as-org-file project-publish-dir)))



(cl-defstruct post-metadata title date last-modified-date file-name-base relative-dir-path post-type)

(defun my-sort-for-what (mylist sort-for-what)
  (let* (;; (foo (make-post-metadata :title "dis foo title"))
         ;; (bar (make-post-metadata :title "dis bar title"))
         ;; (mylist (list foo bar))
         )
    (sort mylist
          `(lambda (elem1 elem2)
             (string-lessp (,sort-for-what elem1)
                           (,sort-for-what elem2))))))


(defun get-all-post-metadatas (base-dir &optional forbidden-file-names)
  (interactive)
  (setq forbidden-file-names (list "sitemap.org" "index.org" "index.org"))
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
    (remove nil
            (mapcar (lambda (path)
                      (let* ((metadata (parse-org-file-to-metadata path base-dir)))
                        (when (char-or-string-p (post-metadata-post-type metadata))
                          (when (not (string-equal ""
                                                   (post-metadata-post-type metadata)))
                            metadata))))
                    file-path-list))))


(defun get-next-git-root (&optional ask)
  (let* ((automatically-found-dir (file-name-as-directory (car (split-string (shell-command-to-string "git rev-parse --show-toplevel")
                                                                             "\n")))))
    (if (not ask)
        (if automatically-found-dir
            automatically-found-dir
          (user-error "No git dir was automatically found"))
      (helm-read-file-name "Select git directory root: "
                           :initial-input (file-name-as-directory (car (split-string (shell-command-to-string "git rev-parse --show-toplevel")
                                                                                     "\n"))))))
  ;; TODO: give error or warning if it's not a git dir root
  )


(defun parse-org-file-to-metadata (&optional file-path base-dir)
  "Get metadata from an org file at FILE-PATH as a function of BASE-DIR."
  (interactive)
  (unless file-path
    ;; (setq file-path (expand-file-name "~/Dropbox/1Projects/programming/derivations-site/testpost.org"))
    ;; (setq file-path (expand-file-name "/home/chris/Dropbox/org/notes/software/site.org"))
    )

  (let* (relative-dir-path)

    (unless base-dir
      (setq base-dir (file-name-as-directory (concat (get-next-git-root) "org"))))

    (setq relative-dir-path (file-relative-name file-path base-dir))
    (with-temp-buffer
      (insert-file-contents file-path)
      (org-mode)
      (org-element-parse-buffer)
      (let* ((title (org-element-property :value (car (org-global-props-get-plist "TITLE"))))
             (date (org-element-property :value (car (org-global-props-get-plist "DATE"))))
             (post-type (org-element-property :value (car (org-global-props-get-plist "POST"))))
             (last-modified-date (format-time-string "%Y-%m-%d"
                                                     (file-attribute-status-change-time (file-attributes file-path)))))
        `(,title
          ,date
          ,last-modified-date
          ,(file-name-base file-path)
          ,(file-name-directory file-path)
          ,post-type)
        (make-post-metadata :title title
                            :date date
                            :last-modified-date last-modified-date
                            :file-name-base (file-name-base file-path)
                            :relative-dir-path (file-name-directory file-path)
                            :post-type post-type)))))

(defun print-post-metadata-into-org (pm-instance)
  (let* ((date-str (post-metadata-date pm-instance)))
    (insert "#+BEGIN_EXPORT html"
            "\n"
            "<div class=\"post-container-div\">"
            "\n"
            "#+END_EXPORT"
            "\n"
            "[["
            (concat (file-name-as-directory (post-metadata-relative-dir-path pm-instance))
                    (post-metadata-file-name-base pm-instance)
                    ".org")
            "]["
            (post-metadata-title pm-instance)
            "]]"
            "\n"
            (when date-str
              (concat "#+BEGIN_EXPORT html"
                      "\n"
                      (format "<p class=\"posted\"> %s </p>"
                              date-str)
                      "\n"
                      "#+END_EXPORT"
                      "\n"))
            "#+BEGIN_EXPORT html"
            "\n"
            "</div>"
            "\n"
            "#+END_EXPORT"
            "\n"
            )))

(defun get-org-dir-from-git-root (&optional git-root)
  (unless (and git-root (file-exists-p git-root))
    (setq git-root (get-next-git-root)))
  (file-name-as-directory (concat git-root "org")))

(defun get-publish-dir-from-git-root (publish-to-buffer &optional git-root)
  (unless (and git-root (file-exists-p git-root))
    (setq git-root (get-next-git-root)))
  (if publish-to-buffer
      (file-name-as-directory (concat git-root "publish-buffer"))
    ;; not publish to buffer means: publish index.html into the root, i.e. publish directly
    (file-name-as-directory git-root)
    ))

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

    ;; (insert "
;; #+BEGIN_EXPORT html
;; <br></br>
;; #+END_EXPORT
;; ")

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

(defun org-global-props-get-plist (&optional property buffer)
  "Get the plists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun produce-index-org (root-dir)
  "Called typically from within the git subdirectory
 of the org project, after the org files in that project have been updated"
  (interactive)
  (unless root-dir
    (setq root-dir (get-next-git-root)))
  (get-index-as-org-file root-dir))

(defun cs-clean-project-publish-buffer (&optional root-dir)
  (interactive)

  (unless root-dir
    (setq root-dir (get-next-git-root))
    (unless root-dir
      (user-error "Root git dir not found")))

  (with-output-to-temp-buffer "*cleaning-publish-buffer*"
      (shell-command (read-shell-command "Run the cleaning command like this: "
                                         (concat "(cd " (prin1-to-string root-dir)
                                                 ;; " && rm -rvf *.elc 2>/dev/null "
                                                 " && rm -rvf " (prin1-to-string (get-publish-dir-from-git-root t root-dir))
                                                 " && rm -rvf ~/.org-timestamps/* "
                                                 ")"))
                     "*cleaning-publish-buffer*")))

(defun update-index-and-publish-project (&optional root-dir)
  (interactive)
  (unless root-dir
    (setq root-dir (get-next-git-root)))
  (produce-index-org root-dir)
  (publish-project nil root-dir t))

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
    (with-output-to-temp-buffer "*publishing-website*"
      (shell-command (read-shell-command "Run the publishing command like this: "
                                         (concat "cp -af "
                                                 (prin1-to-string (concat (file-name-as-directory source-dir)
                                                                          "."))
                                                 " "
                                                 (prin1-to-string target-dir)))
                     "*publishing-website*"))
    (pop-to-buffer "*publising-website*")

    ;; add all, commit and push in one step
    (if (yes-or-no-p "Continue to add all, commit and push?")
        (progn
          (with-output-to-temp-buffer "*pushing-website*"
            (shell-command (read-shell-command "Run the pushing command like this: "
                                               (concat "(cd "
                                                       (prin1-to-string target-dir)
                                                       " && git add . && git commit -m 'pushing html' && git push)"))
                           "*publishing-website*"))
          ;; go to the website to preview
          (browse-url (concat "https://"
                              (file-name-nondirectory (directory-file-name (file-name-directory cs-my-public-website-root-dir)))))
          (browse-url (concat cs-my-github-page-url
                              (file-name-nondirectory (directory-file-name (file-name-directory cs-my-public-website-root-dir))))))
      ;; else, just open the directory in dired
      (dired target-dir))))


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



;; ------- make a custom org lint checker --------
(require 'org-lint)

(setq my-link-checker (make-org-lint-checker
                       :name 'cs-link-to-local-file
                       :description "Report links to non-existent files under a specific subdirectory. "
                       :categories '(link)
                       :trust 'low))

(defun file-exists-somewhere-within-folder-p (file-path root-path)
  "check if file-path is in a subdirectory under root-path and not somewhere else."
  (let* ((rel-dir-path (file-relative-name file-path root-path)))
    (if (or (not (file-exists-p root-path))
            (not (file-exists-p file-path))
            (string-match-p (regexp-quote "..") rel-dir-path))
        nil
      rel-dir-path)))

(defun file-in-project-p (filepath)
  "Check if a file is in a project, i.e.:
  - the project's root directory is a git root
  - the project's root directory contains a file .project-properties"
  (let* ((project-root-dir (get-next-git-root)))
    (and (file-exists-p (concat project-root-dir project-properties-filename))
         (file-exists-somewhere-within-folder-p filepath
                                                project-root-dir))))

(defun get-next-project-root (filepath &optional ask)
  "From inside a path, get the project's root."
  (let* ((project-root-dir (get-next-git-root ask)))
    (when (and (file-exists-p (concat project-root-dir project-properties-filename))
               (file-exists-somewhere-within-folder-p filepath
                                                      project-root-dir))
      project-root-dir)))

(defun files-under-same-project-p (file-path-1 file-path-2)
  "Check if files are under the same project, i.e.:
  - the project's root directory is a git root
  - the project's root directory contains a file .project-properties"
  ;; identify the current project's root directory
  (let* ((project-root-dir (get-next-git-root)))
    (and (file-exists-p (concat project-root-dir project-properties-filename))
         (file-exists-somewhere-within-folder-p file-path-1
                                                project-root-dir)
         (file-exists-somewhere-within-folder-p file-path-2
                                                project-root-dir))))

(defun org-lint-cs-link-to-local-file (ast)
  (org-element-map ast
      'link
    (lambda (l)
      (when (equal "file" (org-element-property :type l))
        (let ((file (org-element-property :path l)))
          (and (not (files-under-same-project-p (buffer-file-name) file))
               (list (org-element-property :begin l)
                     (format (if (org-element-lineage l
                                                      '(link))
                                 "CS: image file \"%s\"\
 not found in the same project"
                               "CS: local file \"%s\" not found in the same project")
                             file))))))))


(defun cs-org-toggle-link-display (&optional show-full)
  "Toggle the literal or descriptive display of links."
  (interactive)
  (if (or org-link-descriptive show-full)
      (remove-from-invisibility-spec '(org-link))
    (add-to-invisibility-spec '(org-link)))
  (org-restart-font-lock)
  (setq org-link-descriptive (not (or org-link-descriptive show-full))))


(defun cs-org-check-for-broken-links (;; root-dir org-file-path
                                               )
  "Check for broken links of an org file ORG-FILE-PATH.
All internal links should be to files placed in a subdirectory
of ROOT-DIR.
This file will check from the top down each link, and will halt
at the first broken link to be managed.  Thus, it needs to be run
multiple times to get to all the broken links."
  (interactive)
  (let* ((org-buffer (current-buffer)))
    (add-to-list 'org-lint--checkers my-link-checker
                 1)
    (cs-org-toggle-link-display t)
    (call-interactively 'org-lint)
    (delete my-link-checker org-lint--checkers)))

(defun get-assets-dir-from-org-file (org-filepath)
  "Get the assets directory associated to an org file ORG-FILEPATH."
  (let* ((org-file-dir (file-name-directory org-filepath))
         (org-file-base (file-name-base org-filepath)))
    (file-name-as-directory (concat (file-name-as-directory (concat org-file-dir org-file-base))
                                    "assets"))))

(defun cs-org-create-assets-dir (&optional org-filepath)
  "For an org file myfile.org, create
a directory myfile and a directory myfile/assets"
  (interactive)
  (unless org-filepath
    (setq org-filepath (buffer-file-name)))

  ;; check if it's an org file and if it exists
  (unless (and (file-exists-p org-filepath) (string-equal (file-name-extension org-filepath) "org"))
    (user-error (concat "Org file does not exist, or file is not an org file: " org-filepath)))

  (let* ((assets-dir (get-assets-dir-from-org-file org-filepath)))
    (if (yes-or-no-p (concat "create asset directory " assets-dir " ?"))
        (progn
          (make-directory assets-dir t))
      (message "asset directory was not created"))))

(defun cs-org-get-linked-files ()
  "Gets linked file paths, but in their formatted version.
That means not in their full expanded version."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (let* ((is-pdfview (string= (org-element-property :type link)
                                  "pdfview"))
             (link-str (org-element-property :path link)))
        (when (or (string= (org-element-property :type link)
                           "file")
                  is-pdfview)
          (if is-pdfview
              (car (split-string link-str "::"))
            link-str))))))


;; ---- helm source to pick out the custom list of files ----


(defun get-links-to-move (org-buffer &optional only-if-not-in-project)
  "Get those links in an org file that you want to move.
E.g.:
- you could want to GTHOOT (get the hell out of there, i.e. you want to save all
linked files safely away to a dedicated folder, making a particular org file
more or less standalone) -> get all links (standard)

- you could want to only get the links of those files that are not under the
same project as the org file.  This is useful if you want to e.g. publish
a website and you want to share some of the resources, but also have certain
files dedicated to a specific org file -> ONLY-IF-NOT-IN-PROJECT

ORG-BUFFER refers to the org buffer the links of which should be extracted."
  (interactive)
  (with-current-buffer org-buffer
    (message (concat "FROM GET_DATA_2: " (prin1-to-string (current-buffer))
                     " " (prin1-to-string org-buffer)))

    (if only-if-not-in-project
        (mapcar (lambda (filepath)
                  (cons filepath filepath))
                (remove nil
                        (mapcar (lambda (filepath)
                                  (unless (files-under-same-project-p filepath
                                                                      (buffer-file-name org-buffer))
                                    filepath)
                                  filepath)
                                (cs-org-get-linked-files))))
      (mapcar (lambda (filepath)
                (cons filepath filepath))
              (cs-org-get-linked-files)))))


(defun get-target-filepath-in-assets-dir (assets-dir original-filepath)
  "When a file is to be copied to the assets dir, it may need to be
renamed or placed in a subdirectory, if there's already a file named
equally in the assets directory. This function figures out where to put
and what to call the new file."
  (let* ((flat-in-assets-dir-target-path (concat assets-dir (file-name-nondirectory original-filepath)))
         (target-file-name-base (file-name-base original-filepath))
         (target-path flat-in-assets-dir-target-path)
         (extension (file-name-extension original-filepath))
         files-equal-after-diff-p)

    (while (and (file-exists-p target-path)
                (not
                 (setq
                  files-equal-after-diff-p
                  (string-equal ""
                                (shell-command-to-string (concat "diff "
                                                                 (prin1-to-string original-filepath)
                                                                 " "
                                                                 (prin1-to-string target-path)))))))
      (setq target-file-name-base (concat target-file-name-base "-2"))
      (setq target-path (concat assets-dir target-file-name-base "."
                                extension)))
    (if files-equal-after-diff-p
        (progn
          ;; car: target path, cadr: override file
          ;; if two equally named files are equal, give back the target-path
          ;; for substitution, but indicate that there is no need
          ;; to write the file again
          (list target-path nil))
      (list target-path t))))


(defun copy-and-relink (org-buffer candidate-filepath-as-printed-in-org)
  "Copy the file at candidate-filepath-as-printed-in-org to the ORG-BUFFER's assets dir."

  (with-current-buffer org-buffer
    (let* ((candidate-filepath (expand-file-name candidate-filepath-as-printed-in-org))
           (org-file-path (buffer-file-name org-buffer))
           (target-dir (get-assets-dir-from-org-file org-file-path))
           (target-path-results
            (get-target-filepath-in-assets-dir target-dir
                                               candidate-filepath)))

      (if (or (car target-path-results)
              (cadr target-path-results))
          ;; create assets directory if it's not already there
          (if (not (file-exists-p target-dir))
              (make-directory target-dir t)))

      (when (cadr target-path-results)
        ;; write the file to the target path
        (call-process-shell-command (read-shell-command "write to assets: "
                                                        (concat "cp -af "
                                                                (prin1-to-string candidate-filepath)
                                                                " "
                                                                (prin1-to-string (car target-path-results))))
                                    nil
                                    "*writing to asset dir*"
                                    t))
      (when (car target-path-results)
        ;; now replace the links in the buffer
        (cs-org-toggle-link-display t)
        (save-excursion
          (goto-char (point-min))
          (query-replace-regexp (regexp-quote candidate-filepath-as-printed-in-org)
                                (file-relative-name (car target-path-results)
                                                    (file-name-directory org-file-path))))))))


(defun pull-files-into-asset-dir (&optional org-buffer only-if-not-in-project)
  "From within an org file, scan it's links one by one.
Act on them to pull them into the file's assets directory."
  ;; make helm source with the linked files inside
  ;; then, a helm action to post them into the assets directory
  (interactive)

  (unless org-buffer
    (if (and (string-equal (file-name-extension (buffer-file-name)) "org")
             ;; (file-in-project-p (buffer-file-name))
             )
        (setq org-buffer (current-buffer))
      (user-error "Not in the position to pull files")))

  (let* ((candidates-that-need-treatment (get-links-to-move org-buffer only-if-not-in-project)))
    (if candidates-that-need-treatment
        (helm :sources
              (helm-build-sync-source "Copy over"
                :header-name (lambda (_)
                               (format "header name"))
                :candidates (lambda ()
                              candidates-that-need-treatment)
                :action (helm-make-actions
                         "Copy this to org file assets and re-link"
                         (lambda (_)
                           (message
                            (concat
                             "writing to assets results: "
                             (prin1-to-string
                              (mapcar (lambda (candidate)
                                        (copy-and-relink org-buffer candidate))
                                      (helm-marked-candidates)))))
                           (pull-files-into-asset-dir org-buffer only-if-not-in-project))
                                           "Copy this to general assets and re-link"
                                           (lambda (_)
                                             (pull-files-into-asset-dir
                                              org-buffer only-if-not-in-project)))))
      (message "no candidates need treatment"))))

(defun cs-org-gthoot (&optional org-file-path)
  "Get the hell out of there!
This means to grab every link in the org file (first level link) and
back them up into an assets directory, at the same level as the copied
org file."
  (interactive)
  (unless org-file-path
    (setq org-file-path (buffer-file-name)))
  (cs-transfer-single-org-file org-file-path nil))


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


(defun cs-transfer-single-org-file (&optional org-file-path only-if-not-in-project)
  (interactive)
    "Integrate a notes file into the website.
If you want to publish something from your notes, you move it into the
project's git directory.  This function makes you aware of what you
need to do to properly integrate that org file.

Things to consider are:

- if you have links to other org files in there, they will break.  So,
  edit the final file to either not include them, or if the file to be
  linked to is already integrated into the project, change the link
  manually.

- if you embed links like images/pdfs/source code files specific to
  only that org file, they will need to be copied to the file's own
  specifc directory.  You can put these files, if they are of general
  interest and you might want to share them between different org
  files into an assets directory or into a subdirectory in there.

  Hard:
- if you have a link to an org file in a different project, and you
  want to include it, you must locate that org file in it's directory
  and calculate a relative link, according to the website's
  conventions."
    (unless org-file-path
      (setq org-file-path (buffer-file-name)))

    ;; now check if it's actually an org file
    (unless (string-equal (file-name-extension org-file-path)
                          "org")
      (user-error "Not an org file"))


    (let* (actually-write-now
           target-file-path)
      ;; now check if the file is already placed in a project and
      ;; in that case issue a warning
      ;; (if (file-in-project-p org-file-path)
      ;;   (when (yes-or-no-p (concat org-file-path " is aleady part of a project. "
      ;;                              "Do you still want to transfer it to a project, again?"))
      ;;     (setq actually-write-now t))
      ;;   (setq actually-write-now t))

      (setq actually-write-now t)

      (when actually-write-now
        (copy-file org-file-path
                   (setq target-file-path
                         (helm-read-file-name "Write file to: "
                                              :initial-input (expand-file-name "~/Dropbox/")))
                   t)
        (find-file target-file-path)
        (pull-files-into-asset-dir (current-buffer) only-if-not-in-project))))

(provide 'cs-org-publish)
;;; cs-org-publish.el ends here
