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


(defun make-frame-almost-fit-desktop ()
  "Makes a frame that is almost as big as the screen."
  (let* ((down-scale-vertical 0.925)
         (down-scale-horizontal 0.97))
    (modify-frame-parameters (selected-frame)
                             `((width . (text-pixels . ,(round (* down-scale-horizontal
                                                                  (x-display-pixel-width)))))
                               (height . (text-pixels . ,(round (* down-scale-vertical
                                                                   (x-display-pixel-height)))))
                               (left . ,(round (* (/ (- 1.0 down-scale-horizontal)
                                                     2.0)
                                                  (x-display-pixel-width))))
                               (top . ,(round (* (/ (- 1.0 down-scale-vertical)
                                                    2.0)
                                                 (x-display-pixel-height))))))))


(provide 'cs-frame-resize)
;;; cs-frame-resize.el ends here
