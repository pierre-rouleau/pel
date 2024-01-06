;;; pel-regexp.el --- Regexp utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2024  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

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


;;; Commentary:
;;
;; Emacs Lisp does not support a concept of raw strings that would greatly
;; simplify writing code that express regular expressions. The function in this
;; file attempt to provide useful tool to help this situation.

;;; Code:

;; * `pel-insert-regexp'
;;   - `pel-insert-in-quotes'

;; -----------------------------------------------------------------------------
;; Inserting a regexp from literal typed at prompt
;; -----------------------------------------------

(defun pel-insert-in-quotes (text)
  "Insert the text string between double quotes."
  (insert "\"")
  (insert text)
  (insert "\""))

;;-pel-autoload
(defun pel-insert-regexp (&optional insert-both)
  "Prompt for a regexp literal, insert corresponding quoted regexp at point.

This converts what Emacs calls the \\='string syntax\\=' into the
Emacs \\='read syntax\\='.

When INSERT-BOTH argument is non-nil, insert both strings.
If INSERT-BOTH is a string, it is inserted between both strings,
otherwise the string-syntax regexp  is inserted, followed by \" --> \"
followed by the read-syntax regexp.

At the prompt enter the literal regexp string, ie. a string with
double quote, capturing group parentheses and the alternative bar
escaped with a single backslash.

Example 1:
- when typing:                 \\(foo\\\\|bar\\)
- this text is inserted:   \"\\\\(foo\\\\|bar\\\\)\"

Example 2:
- when typing:                     \\(foo\\|bar--\\\"--\\)
- this text is inserted:     \"\\\\(foo\\\\|bar--\\\\\\\"--\\\\)\"

Example 3, using a \\[universal-argument]:
- When typing:              abc\\S\"gh
- This text is inserted:  \"abc\\S\"gh\" --> \"abc\\\\S\\\"gh\"

Notice that you must not type the surrounding double quotes."
  (interactive "P")
  (let ((string-syntax-regexp (read-string "regexp: ")))
    (when insert-both
      (pel-insert-in-quotes string-syntax-regexp)
      (insert (if (stringp insert-both) insert-both " --> ")))
    (pel-insert-in-quotes
     (replace-regexp-in-string
      "\""
      "\\\\\""
      (replace-regexp-in-string
       "\\\\"
       "\\\\\\\\"
       string-syntax-regexp)))))

;; -----------------------------------------------------------------------------
(provide 'pel-regexp)

;;; pel-regexp.el ends here
