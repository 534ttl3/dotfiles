;;; cs-python-flycheck-refactor.el --- using the flycheck linter error messages to make completions  -*- lexical-binding: t; -*-

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

;; (defun propose-autocompletion)
;; Idea: for a specific function (say a data analysis function that you want to plot in various ways, after outsourcing, get all undefined variables and compile a list of them automatically, then paste that list into the arguments of the function (may contain e.g. many different matplotlib axes, plotting parameters, but also most importantly the supplied data from the main function)



(provide 'cs-python-flycheck-refactor)
;;; cs-python-flycheck-refactor.el ends here
