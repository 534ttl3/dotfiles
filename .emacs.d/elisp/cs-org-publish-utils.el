;;; cs-org-publish-utils.el --- Utils for cs-org-publish     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  chris

;; Author: chris <chris@chris-lenovo>
;; Keywords: abbrev, abbrev, abbrev

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

;; ------- conventions ----------
;; This you might need to change depending on your installation
(defconst cs-my-public-website-root-dir (file-name-as-directory (expand-file-name "~/Dropbox/1Projects/programming/534ttl3.github.io")))
(defconst cs-my-github-page-url "https://github.com/534ttl3/")
(defconst cs-my-youtube-page-url "https://youtube.com/s41b0tproductions")
(defconst cs-my-github-website-repo-name "534ttl3.github.io")
(defconst cs-my-github-website-url (concat "https://" cs-my-github-website-repo-name "/"))
(defconst cs-my-github-website-about-link
  (concat cs-my-github-website-url "www/about.html"))
(defconst cs-my-github-website-repo-url (concat cs-my-github-page-url cs-my-github-website-repo-name))
(defconst project-properties-filename ".project-properties")
(defconst www-dir-name (file-name-as-directory "www"))
(defconst cs-org-publish-within-single-project-base-dir-name (file-name-as-directory "org"))
(defconst publish-buffer-name "*publish*")
(defconst cs-github-edit-master-subdir-path-name "./edit/master/")
(defconst cs-github-blob-master-subdir-path-name "./blob/master/")
;; a link to a website could look like this:
;; https://github.com/534ttl3/derivations-site/blob/master/org/example.org

;; ------- getting relevant project directories or vcs directories automatically, or from file paths --------
(defun get-projects-base-dir (buffname)
  (expand-file-name (concat (file-name-as-directory (if (get-next-project-root buffname)
                                                        (get-next-project-root buffname)
                                                      (file-name-directory buffname)))
                            cs-org-publish-within-single-project-base-dir-name)))

(defun get-projects-publish-dir (buffname)
  (expand-file-name (concat (file-name-as-directory (if (get-next-project-root buffname)
                                                        (get-next-project-root buffname)
                                                      (file-name-directory buffname)))
                            www-dir-name)))

(defun get-projects-base-dir-from-root-dir (project-root-dir)
  (let* ((standard (concat project-root-dir cs-org-publish-within-single-project-base-dir-name)))
    (if (file-exists-p standard)
        standard
      (user-error (concat "Base dir " standard " does not exist")))))

(defun get-projects-publish-dir-from-root-dir (project-root-dir)
  (let* ((standard (concat project-root-dir www-dir-name)))
    standard
    ;; (if (file-exists-p standard)
    ;;     standard
    ;;   (user-error (concat "Publish for preview dir " standard " does not exist")))
    ))

(defun get-project-repo-name (project-root)
  (file-name-nondirectory
   (directory-file-name
    (file-name-directory (file-name-as-directory project-root)))))

(defun get-project-repo-url (project-repo-name)
  (concat cs-my-github-page-url project-repo-name "/"))

(defun get-project-view-file-in-repo-url (project-repo-name)
  (concat cs-my-github-page-url project-repo-name "/"))

(defun get-edit-on-github-link (project-repo-name project-root-dir file-path)
  (concat cs-my-github-page-url (file-name-as-directory project-repo-name)
          cs-github-blob-master-subdir-path-name (file-relative-name file-path project-root-dir))
  )

(defun get-next-git-root (&optional ask prompt filepath)
  (let* ((automatically-found-dir (file-name-as-directory (car (split-string (shell-command-to-string
                                                                              (concat (when (and filepath (if (file-exists-p filepath)
                                                                                                              t
                                                                                                            (user-error (concat "Filepath " (prin1-to-string filepath) " given to get-next-git-root doesn't exist"))))
                                                                                        (concat " cd "
                                                                                                (prin1-to-string (file-name-directory filepath))
                                                                                                " ; "))
                                                                                      "git rev-parse --show-toplevel"))
                                                                             "\n")))))
    (if (not ask)
        (if automatically-found-dir
            automatically-found-dir
          (user-error "No git dir was automatically found"))
      (helm-read-file-name (if prompt
                               prompt
                             "Select git directory root: ")
                           :initial-input automatically-found-dir)))
  ;; TODO: give error or warning if it's not a git dir root
  )

(defun get-org-dir-from-git-root (&optional git-root)
  (unless (and git-root (file-exists-p git-root))
    (setq git-root (get-next-git-root)))
  (file-name-as-directory (concat git-root "org")))

(defun get-publish-dir-from-git-root (publish-to-buffer &optional git-root subproject-name)
  (unless (and git-root
               (file-exists-p git-root))
    (setq git-root (get-next-git-root)))
  (if publish-to-buffer
      (let* ((www-dir (file-name-as-directory (concat git-root www-dir-name))))
        (if subproject-name
            (file-name-as-directory (concat www-dir subproject-name))
          www-dir))
    ;; not publish to buffer means: publish index.html into the root, i.e. publish directly
    ;; FIXME: maybe remove this option eventually
    (file-name-as-directory git-root)))

(defun get-next-project-root (filepath &optional ask prompt)
  "From inside a path, get the project's root."
  (let* ((project-root-dir (get-next-git-root ask prompt filepath)))
    (when (and (file-exists-p (concat project-root-dir project-properties-filename))
               (file-exists-somewhere-within-folder-p filepath
                                                      project-root-dir))
      project-root-dir)))


;; ---- checking utils ------

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


;; ---- metadata handling -------

(cl-defstruct post-metadata title date last-modified-date file-name-base relative-dir-path post-type)

(defun my-sort-for-what (mylist sort-for-what)
  (let* ()
    (sort mylist
          `(lambda (elem1 elem2)
             (string-lessp (,sort-for-what elem1)
                           (,sort-for-what elem2))))))

(defun get-all-post-metadatas (base-dir &optional forbidden-file-names)
  (interactive)
  (setq base-dir (expand-file-name base-dir))
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

(defun org-global-props-get-plist (&optional property buffer)
  "Get the plists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

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
    (insert (concat "#+BEGIN_EXPORT html"
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
                              (format "<p class=\"posted\"> %s </p>" date-str)
                              "\n"
                              "#+END_EXPORT"
                              "\n"))
                    "#+BEGIN_EXPORT html"
                    "\n"
                    "</div>"
                    "\n"
                    "#+END_EXPORT"
                    "\n"))))


;; ------- cleaning out the produced html of a projcet --------
(defun cs-clean-project-publish-buffer (&optional root-dir prompt subdir-rel-path)
  (interactive)
  (unless root-dir
    (setq root-dir (get-next-git-root))
    (unless root-dir
      (user-error "Root git dir not found")))
  (with-output-to-temp-buffer publish-buffer-name
    (shell-command
     (read-shell-command (if prompt prompt "Run the cleaning command like this: ")
                         (concat "cd "
                                 (prin1-to-string root-dir)
                                 " ; "
                                 ;; " && rm -rvf *.elc 2>/dev/null "
                                 " find "
                                 (prin1-to-string (concat (file-name-as-directory www-dir-name)
                                                          subdir-rel-path))
                                 " -type f,d -exec rm -rf {} + "
                                 " ; "
                                 " rm -rvf ~/.org-timestamps/* ; "))
                   publish-buffer-name)))





(provide 'cs-org-publish-utils)
;;; cs-org-publish-utils.el ends here
