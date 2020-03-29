;;; cs-org-publish-code-dump.el --- Code dump for cs-org-publish  -*- lexical-binding: t; -*-

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





;; (defun publish-file (&optional filepath)
;;   (interactive)

;;   (unless filepath
;;     (setq filepath (helm-read-file-name "Select org file to publish: " :initial-input (buffer-file-name))))

;;   (let* ((project-base-dir (helm-read-file-name (concat "Select base directory for publishing "
;;                                                         filepath ": ")
;;                                                 :initial-input (file-name-directory filepath)))
;;          (project-publish-dir
;;           (helm-read-file-name (concat "Select publishing directory for publishing "
;;                                        filepath " to html: ")
;;                                :initial-input
;;                                ;; (expand-file-name (file-name-directory "~/Desktop/test-publish-target-dir/"))
;;                                (concat (get-next-git-root) "www-buf")))
;;                           project-component-doc-name
;;                           project-component-other-name)

;;     (when (not (file-exists-p project-publish-dir))
;;       (make-directory project-publish-dir t))

;;     (setq org-publish-project-alist `(("project" :base-directory ,project-base-dir
;;                                        :publishing-function my-org-html-publish-to-my-html
;;                                        :publishing-directory ,project-publish-dir
;;                                        :exclude ".*"
;;                                        :include [,filepath])
;;                                       ("project-attachments" :base-directory ,project-base-dir
;;                                        :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
;;                                        :publishing-directory ,project-publish-dir
;;                                        :recursive t
;;                                        :publishing-function org-publish-attachment)
;;                                       (,"project-all"
;;                                        :components ("project" "project-attachments"))))


;;     (org-publish-reset-cache)
;;     (org-publish-remove-all-timestamps)
;;     (let* ()
;;       (org-publish "project-all" t nil))

;;     (get-index-as-org-file project-publish-dir)))














(provide 'cs-org-publish-code-dump)
;;; cs-org-publish-code-dump.el ends here
