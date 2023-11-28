;;; pel--syntax-macros.el --- Syntax helper macros.  -*- lexical-binding: t; -*-

;; Created   : Saturday, October 15 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2023-11-28 14:56:09 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2022, 2023  Pierre Rouleau
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
;;  This file provides a set of macros used to document the use of the list
;;  returned by the `syntax-ppss' function.
;;
;; Internal Macros for self documenting code:
;; - `pel--inside-string-p'
;; - `pel--inside-block-p'
;; - `pel--inside-comment-p'
;; - `pel--open-parens-pos'

;; Predicate utilities:
;; - `pel-inside-block-p'
;; - `pel-inside-comment-p'
;; - `pel-inside-string-p'
;; - `pel-inside-code-p'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;
;; Macros for self documenting code
;; --------------------------------
;;
;; These macros are useful to reduce the need to call functions that create a
;; let-bound variable while providing meaningful names.

(defmacro pel--inside-string-p (syntax)
  "Return non-nil if point is inside string according to SYNTAX list."
  `(nth 3 ,syntax))

(defmacro pel--inside-block-p (syntax)
  "Return non-nil if point is inside matching pair block according to SYNTAX."
  `(> (nth 0 ,syntax) 0))

(defmacro pel--inside-comment-p (syntax)
  "Return non-nil if point is inside comment according to SYNTAX."
  `(nth 4 ,syntax))

(defmacro pel--open-parens-pos (syntax)
  "Return list of position of open parens according to SYNTAX.

Each integer in the list is the position of the open parens,
starting with the outermost one.  Return nil if not outside parens."
  `(nth 9 ,syntax))

;; Predicate utilities
;; -------------------

(defun pel-inside-block-p (&optional pos)
  "Return non-nil if POS, or point, is between a code matched-pair block.

Return nil otherwise.  Return nil when point is inside string or comment."
  (let ((syntax (syntax-ppss pos)))
    (unless (pel--inside-string-p syntax)
      (pel--inside-block-p syntax))))

(defun pel-inside-comment-p (&optional pos)
  "Return non-nil if POS, or point, is inside a comment, nil otherwise.

When inside comment, return t if inside non-nestable comment,
otherwise return an integer indicating the current comment nesting."
  (pel--inside-comment-p (syntax-ppss pos)))

(defun pel-inside-string-p (&optional pos)
  "Return non-nil if POS, or point, is inside a string, nil otherwise."
  (pel--inside-string-p (syntax-ppss pos)))

(defun pel-inside-code-p (&optional pos)
  "Return t if POS, or point, is inside code; not in string nor comment."
  (let ((syntax-sexp (syntax-ppss pos)))
    (and (not (nth 3 syntax-sexp))
         (not (nth 4 syntax-sexp)))))

;;; --------------------------------------------------------------------------
(provide 'pel--syntax-macros)

;;; pel--syntax-macros.el ends here
