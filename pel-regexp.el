;;; pel-regexp.el --- Regexp utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Pierre Rouleau

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

;; -----------------------------------------------------------------------------
;; Inserting a regexp from literal typed at prompt
;; -----------------------------------------------

;;-pel-autoload
(defun pel-insert-regexp ()
  "Prompt for a regexp literal, insert corresponding quoted regexp at point.

This converts what Emacs calls the 'string syntax' into the Emacs 'read syntax'.

At the prompt enter the literal regexp string, ie. a string with
double quote, capturing group parentheses and the alternative bar
escaped with a single backslash.

Example 1:
- when typing something like:  \\(foo\\\\|bar\\)
- this string is inserted:   \"\\\\(foo\\\\|bar\\\\)\"

Example 2:
or example:
- when typing something like:  \\(foo\\|bar--\\\"--\\)
- this string is inserted:   \"\\\\(foo\\\\|bar--\\\\\\\"--\\\\)\"

Notice that you must not type the surrounding double quotes."
  (interactive)
  (let ((regexp (read-string "regexp: ")))
    (insert "\"")
    (insert
     (replace-regexp-in-string
      "\""
      "\\\\\\\\\""
      (replace-regexp-in-string
       "\\\\\\([^\"]\\)"
       "\\\\\\\\\\1"
       regexp)))
     (insert "\"")))

;; -----------------------------------------------------------------------------
(provide 'pel-regexp)

;;; pel-regexp.el ends here
