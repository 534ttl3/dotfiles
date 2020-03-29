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
(defconst project-properties-filename ".project-properties")
(defconst publish-for-preview-dir-name (file-name-as-directory "publish-buffer"))
(defconst cs-org-publish-within-single-project-base-dir-name (file-name-as-directory "org"))
(defconst publish-buffer-name "*publish*")


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
                            publish-for-preview-dir-name)))

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

(defun get-org-dir-from-git-root (&optional git-root)
  (unless (and git-root (file-exists-p git-root))
    (setq git-root (get-next-git-root)))
  (file-name-as-directory (concat git-root "org")))

(defun get-publish-dir-from-git-root (publish-to-buffer &optional git-root)
  (unless (and git-root
               (file-exists-p git-root))
    (setq git-root (get-next-git-root)))
  (if publish-to-buffer
      (file-name-as-directory (concat git-root "publish-buffer"))
    ;; not publish to buffer means: publish index.html into the root, i.e. publish directly
    (file-name-as-directory git-root)))

(defun get-next-project-root (filepath &optional ask)
  "From inside a path, get the project's root."
  (let* ((project-root-dir (get-next-git-root ask)))
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
  (let* ( ;; (foo (make-post-metadata :title "dis foo title"))
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


;; ------- cleaning out the produced html of a projcet --------
(defun cs-clean-project-publish-buffer (&optional root-dir)
  (interactive)
  (unless root-dir
    (setq root-dir (get-next-git-root))
    (unless root-dir
      (user-error "Root git dir not found")))
  (with-output-to-temp-buffer publish-buffer-name
    (shell-command (read-shell-command "Run the cleaning command like this: "
                                       (concat "cd "
                                               (prin1-to-string root-dir)
                                               " ; "
                                               ;; " && rm -rvf *.elc 2>/dev/null "
                                               " rm -rvf "
                                               (prin1-to-string (get-publish-dir-from-git-root t root-dir))
                                               " ; "
                                               " rm -rvf ~/.org-timestamps/* ; "))
                   publish-buffer-name)))





(provide 'cs-org-publish-utils)
;;; cs-org-publish-utils.el ends here
