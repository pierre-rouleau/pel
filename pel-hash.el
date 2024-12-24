;;; pel-hash.el --- Hash utilities.  -*- lexical-binding: t; -*-

;; Created   : Monday, December 23 2024.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2024-12-23 18:39:29 EST, updated by Pierre Rouleau>

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
;; This module supports creation and operations on a hash of lists.
;; The key can be any object type, but the type must support comparison.
;;
;; The creation function, `pel-make-hash-of-lists' specifies the function used
;; to compare the keys.
;;
;; Add elements to the list specified by a key with `pel-addto-hash-of-lists'.
;;
;; Get the complete list for a key with `pel-get-list-from-hash-of-lists-for'.
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-make-hash-of-lists (&optional test)
  "Create and return a hash table of lists.

The optional TEST argument identifies the test function used to
compare keys.
The possible TEST predicates include the same predefined
as `make-hash-table', with  a different default:
- eq   : for symbol and integer keys.  This is the default.
- eql  : like eq but also for floats.
- equal: like eql but also for strings.

User-supplied test and hash functions can be specified via
`define-hash-table-test’."
  (make-hash-table :test (or test 'eq)))

(defun pel-addto-hash-of-lists (hash-table key element)
  "Add ELEMENT to the list associated to the KEY of the HASH-TABLE.

Return the value of the updated list.
The elements in that list are last entered first."
  (puthash key (cons element (gethash key hash-table '())) hash-table))

(defun pel-get-list-from-hash-of-lists-for (hash-table key)
  "Return the list of element associated to the KEY from the HASH-TABLE.

The returned list holds elements in the same order as they were
entered: the first element entered in the list is the first
element in the returned list."
  (reverse (gethash key hash-table '())))

;;; --------------------------------------------------------------------------
(provide 'pel-hash)

;;; pel-hash.el ends here
