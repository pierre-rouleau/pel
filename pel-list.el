;;; pel-list.el --- List utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2021, 2022, 2023, 2024  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

;; This file is part of the PEL package
;; This file is not part of GNU Emacs.

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
;; This file defines a set of functions that operate on lists:
;;
;; - `pel-insert-in-list'
;; - `pel-insert-list-in-list'
;; - `pel-join'
;; - `pel-list-split'
;;   - `pel-list-index'
;; - `pel-mapmapcar'

;;; --------------------------------------------------------------------------
;;; Dependencies:
(eval-when-compile
  (require 'cl-macs))                      ; use: cl-dolist and cl-return
(require 'pel--base)                      ; use: `pel-require'
(pel-require 'seq :install-when-missing)   ; use: seq-subseq
(declare-function seq-subseq "seq")
;; ---------------------------------------------------------------------------
;;; Code:

(defun pel-insert-in-list (elem n list)
  "Insert single ELEM just before Nth element of LIST.
LIST is not modified.
Return the new list.
Performance: O(n) - of LIST size."
  (append
   (butlast list (- (length list) n))
   (cons elem (nthcdr n list))))

(defun pel-insert-list-in-list (seq n list)
  "Insert list SEQ just before Nth element of LIST.
LIST is not modified.
Return the new list.
Performance: O(n) - of LIST size."
  (append
   (butlast list (- (length list) n))
   seq
   (nthcdr n list)))

(defun pel-join (elems sep &optional max-per-line prefix)
  "Return a string joining each string in ELEMS.
Use SEP, a string,  as separator.
Allow up to MAX-PER-LINE elements per line.
For second and following lines, put leading PREFIX string.
Note that SEP does not need to be a newline to place each element
on a separate line; for that set MAX-PER-LINE to 1."
  (when (and max-per-line (<= max-per-line 0))
    (error "max-per-line must either be nil or >= 1"))
  (let ((text  "")
        (rem-count (length elems))
        (done-count 0))
    (dolist (elem elems text)
      (setq rem-count (1- rem-count))
      (setq done-count (1+ done-count))
      (setq text (concat text
                         elem
                         (if (> rem-count 0)
                             (if (and max-per-line
                                      (eq (% done-count max-per-line) 0))
                                 (concat sep "\n" prefix)
                               sep)
                           ""))))))

(defun pel-list-index (object list)
  "Return index of first OBJECT found inside LIST, nil if not present."
  (let ((index -1))
    (cl-dolist (elt list)
      (setq index (1+ index))
      (when (equal elt object)
        (cl-return index)))))

(defun pel-list-split (object list &optional include-object)
  "Split LIST in 2 lists.

Break the list at the first instance of OBJECT.

If INCLUDE-OBJECT is non-nil it may be set to :in-first or
:in-last to indicate to include OBJECT at the end of the first
list or at the first element of the second list, respectively."
  (let ((split-idx (pel-list-index object list)))
    (if split-idx
        (list
         (seq-subseq list 0 (if (eq include-object :in-first)
                                (+ 1 split-idx)
                              split-idx))
         (seq-subseq list (if (eq include-object :in-last)
                              split-idx
                            (+ 1 split-idx))))
      (list list nil))))

(defun pel-mapmapcar (func list-of-list)
  "Apply function FUNC one each element of LIST-OF-LIST.

Return list of list with each element processed by FUNC.
CAUTION: does NOT work with more complicated data structures (eg. tree)."
  (mapcar (lambda (inner-list)
            (mapcar func inner-list))
          list-of-list))

;; ---------------------------------------------------------------------------
;;
(provide 'pel-list)

;;; pel-list.el ends here
