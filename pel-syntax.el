;;; pel-syntax.el --- Syntax processing helper functions.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, September 29 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-09-29 14:39:30, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021  Pierre Rouleau
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
;;  This provides a set of utilities to provide syntax-table based utilities
;;  which may be used to enhance features such as electric behaviour of keys.


;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'syntax)     ; syntax always available, even in emacs -Q

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; Macros to write self-documenting code.
(defmacro pel--inside-string (syntax)
  "Return non-nil if point is inside string according to SYNTAX list."
  `(nth 3 ,syntax))

(defmacro pel--inside-block (syntax)
  "Return non-nil if point is inside matching pair block according to SYNTAX."
  `(>= (nth 0 ,syntax) 0))


;; Predicates
;; ----------

(defun pel-inside-block-p (&optional pos)
  "Return non-nil if point is between a code matched-pair block of characters.

Return nil otherwise.  Return nil when point is inside string or comment."
  (let ((syntax (syntax-ppss pos)))
    (unless (pel--inside-string syntax)
      (pel--inside-block syntax))))


;; Electric keys helper functions
;; ------------------------------

(defun pel-insert-space-in-enclosing-block ()
  "Insert a space if point is in between a block pair."
  (when (pel-inside-block-p)
    (insert " ")))

;;; --------------------------------------------------------------------------
(provide 'pel-syntax)

;;; pel-syntax.el ends here
