;;; cs-frame-resize.el --- function to resize frame  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  chris

;; Author: chris <chris@chris-thinkpad>
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

;; make-frame-almost-fit-desktop is intended to make the current frame be almost at
;; the edge of the window, but not totally full-screen.

;;; Code:

(defun cs-frame-get-frames-monitor ()
  "Get the monitor of the FRAME."
  (car (remove nil
               (mapcar (lambda (monitor)
                         (when (member (selected-frame) (assq 'frames monitor))
                           monitor))
                       (display-monitor-attributes-list)))))

(defun make-frame-almost-fit-desktop (&optional monitor)
  "Resize a frame so that it is almost fullscreen, but leave small margins.
If MONITOR is nil, use the current frame's monitor."
  (let* ((down-scale-vertical 0.925)
         (down-scale-horizontal 0.97)
         (cur-monitor (if monitor monitor (cs-frame-get-frames-monitor)))
         (monitor-pixel-width ;; (x-display-pixel-width)
          (nth 3 (assq 'geometry cur-monitor)))
         (monitor-pixel-height ;; (x-display-pixel-height)
          (nth 4 (assq 'geometry cur-monitor))))
    (modify-frame-parameters (selected-frame)
                             `((width . (text-pixels . ,(round (* down-scale-horizontal
                                                                  monitor-pixel-width))))
                               (height . (text-pixels . ,(round (* down-scale-vertical
                                                                   monitor-pixel-height))))
                               (left . ,(round (* (/ (- 1.0 down-scale-horizontal)
                                                     2.0)
                                                  monitor-pixel-width)))
                               (top . ,(round (* (/ (- 1.0 down-scale-vertical)
                                                    2.0)
                                                 monitor-pixel-height)))))))


(provide 'cs-frame-resize)
;;; cs-frame-resize.el ends here
