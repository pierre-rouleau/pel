;;; pel-cc-navigate.el --- C/C++ Specialized Navigation Commands.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, January  2 2024.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2024-01-02 19:00:48 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2024  Pierre Rouleau
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; Very preliminary code.  For test of potentially future specialized
;; navigation commands.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; TODO - protect against commented-out or string switch statements.

(defun pel-cc-inside-switch-p ()
  "Return t if point is inside a switch statement, nil otherwise."
  (let ((current-position (point))
        (top nil)
        (bottom nil))
    (save-excursion
      (when (and (re-search-backward "switch\\s-*(" nil t)
                 (search-forward "{" nil t))
        (left-char)
        (setq top (point))
        (forward-sexp)
        (setq bottom (point))))
    (and (< top current-position)
         (> bottom current-position))))

(defun pel-cc-to-switch-begin ()
  "Move point to the beginning of current switch statement."
  (interactive)
  (if (pel-cc-inside-switch-p)
      (progn
        (push-mark)
        (re-search-backward "switch\\s-*(" nil t)
        (search-forward "{" nil t)
        (left-char))
    (user-error "Point is not inside a switch statement!")))

(defun pel-cc-to-switch-end ()
  "Move point to the end of current switch statement."
  (interactive)
  (pel-cc-to-switch-begin)
  (forward-sexp))
;;; --------------------------------------------------------------------------
(provide 'pel-cc-navigate)

;;; pel-cc-navigate.el ends here
