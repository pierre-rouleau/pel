;;; pel-list.el --- List utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Pierre Rouleau

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

;; ---------------------------------------------------------------------------
;;; Commentary:
;;
;; This file defines a set of functions that operate on lists:
;;
;; - `pel-insert-in-list'
;; - `pel-insert-list-in-list'
;; - `pel-join'

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
Use SEP as separator.
Allow up to MAX-PER-LINE elements per line.
For second and following lines, put leading PREFIX string."
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

;; ---------------------------------------------------------------------------
;;
(provide 'pel-list)

;;; pel-list.el ends here
