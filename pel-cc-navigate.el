;;; pel-cc-navigate.el --- C/C++ Specialized Navigation Commands.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, January  2 2024.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2024-01-04 23:17:43 EST, updated by Pierre Rouleau>

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
;; This file hold logic for commands that navigate out of C and C++ switch
;; statements as well as to the boundaries of enum, union struct and class
;; definition blocks.
;;
;; With the point inside a switch statement or definition block, 'move to
;; begin' command moves the point to the '{' character of the current switch
;; statement block and the 'move to end' command moves point just after the
;; '}' character at the end of the block.
;;
;; If point is outside a switch statement these commands issue an appropriate
;; user error.
;;
;; The commands are:
;;
;; - `pel-cc-to-switch-begin'
;; - `pel-cc-to-switch-end'
;; - `pel-cc-to-enum-begin'
;; - `pel-cc-to-enum-end'
;; - `pel-cc-to-union-begin'
;; - `pel-cc-to-union-end'
;; - `pel-cc-to-struct-begin', also named `pel-cc-to-class-begin'
;; - `pel-cc-to-struct-end', also named `pel-cc-to-class-end'
;;
;; These commands all use the `pel-cc-elem-boundaries' and `pel--cc-move-to'
;; utility functions to find the correct target position and move point to it
;; if one is found.
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--syntax-macros)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; Navigating to switch statement boundaries
;; -----------------------------------------

(defconst pel-cc--switch-regexp
  "\\(^\\|;\\)\\s-*switch\\s-*("
  "Regex to find start paren of switch statement")

;; [:todo 2024-01-04: add ability to select the regexp via user-options ]
;; [:todo 2024-01-04: add support for C++11 attribute specifier]
(defconst pel-cc--class-regexp
  "\\(^\\|;\\)\\s-*\\(\\(class\\)\\|\\(struct\\)\\)\\s-"
  "Regex to find start of C++ class or struct statement")

(defconst pel-cc--struct-regexp
  "\\(^\\|;\\)\\(\\s-*typedef\\s-*\\)*\\s-*struct\\s-"
  "Regex to find start of C/C++ class or struct statement")

(defconst pel-cc--union-regexp
  "\\(^\\|;\\)\\(\\s-*typedef\\s-*\\)*\\s-*union\\s-"
  "Regex to find start of C/C++ union statement")

(defconst pel-cc--enum-regexp
  "\\(^\\|;\\)\\(\\s-*typedef\\s-*\\)*\\s-*enum\\(\\(\\s-+\\(class\\s-*\\)?\\)\\|$\\)"
  "Regex to find start of C/C++ enum or C++ enum class statement")


(defun pel-cc--inside-function-p ()
  "Return t if point is inside a C/C++ function, nil otherwise.

Limitation: will report valid when inside a structure or class
block (or any block).  Use only when appropriate."
  (save-excursion
    (condition-case nil
        (progn
          (backward-up-list)
          t)
      (error nil))))


(defun pel-cc-elem-boundaries (elem-regexp)
  "Return the position boundaries for element surrounding point.

ELEM-REGEXP is the regexp to search for the element block.
Return a list of (begin, end) position surrounding point if there
is one, nil otherwise."
  (let ((current-position (point))
        (found-boundary nil)
        (begin-pos nil)
        (end-pos nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward elem-regexp nil :noerror)
        (when (pel-inside-code-p)
          (when (search-forward "{" nil :noerror)
            (left-char)
            (setq begin-pos (point))
            (forward-sexp)
            (setq end-pos (point))
            ;; move right after current block start brace, to detect nested
            ;; ones.
            (goto-char (+ begin-pos 1))
            (when (and (< begin-pos current-position)
                       (> end-pos   current-position))
              (setq found-boundary
                    (cons (list begin-pos end-pos) found-boundary))))))
      ;; If the point is inside nested blocks, we want to return info about
      ;; the most nested one: the first one in the list (which might be empty)
      (car-safe found-boundary))))


(defun pel--cc-move-to (positions n elem-str)
  "Utility: move to begin position or report user error.

- POSITIONS is a list of (begin end) positions or nil if none found
  by the caller.
- N must be 0 to move to the beginning of the block, 1 to move to the end.
- The ELEM-STR must be the name of the syntactic
  element (such as 'string' or 'enum')."
  (if positions
      (progn
        (push-mark)
        (goto-char (nth n positions)))
    (user-error "Point is not inside a %s block!" elem-str)))


(defun pel-cc-to-switch-begin ()
  "Move point to the beginning of current switch statement.

If one found, mark position before moving point, allowing moving the point
back by using \\[pel-jump-to-mark].
If no switch statement is found, issue a user error."
  (interactive)
  (if (pel-cc--inside-function-p)
      (pel--cc-move-to
       (pel-cc-elem-boundaries pel-cc--switch-regexp)
       0
       "switch statement")
    (user-error "Point is outside a function block!")))

(defun pel-cc-to-switch-end ()
  "Move point to the end of current switch statement.

If one found, mark position before moving point, allowing moving the point
back by using \\[pel-jump-to-mark].
If no switch statement is found, issue a user error."
  (interactive)
  (if (pel-cc--inside-function-p)
      (pel--cc-move-to
       (pel-cc-elem-boundaries pel-cc--switch-regexp)
       1
       "switch statement")
    (user-error "Point is outside a function block!")))


(defun pel-cc-to-enum-begin ()
  "Move point to beginning of current enum definition.

If point is inside one, mark position before moving point,
allowing moving the point back by using \\[pel-jump-to-mark].
If point is not inside an enum definition block, issue a user
error."
  (interactive)
  (pel--cc-move-to
   (pel-cc-elem-boundaries pel-cc--enum-regexp)
   0
   "enum"))

(defun pel-cc-to-enum-end ()
  "Move point to end of current enum definition.

If point is inside one, mark position before moving point,
allowing moving the point back by using \\[pel-jump-to-mark].
If point is not inside an enum definition block, issue a user
error."
  (interactive)
  (pel--cc-move-to
   (pel-cc-elem-boundaries pel-cc--enum-regexp)
   1
   "enum"))


(defun pel-cc-to-union-begin ()
  "Move point to beginning of current union definition.

If point is inside one, mark position before moving point,
allowing moving the point back by using \\[pel-jump-to-mark].
If point is not inside an union definition block, issue a user
error."
  (interactive)
  (pel--cc-move-to
   (pel-cc-elem-boundaries pel-cc--union-regexp)
   0
   "union"))

(defun pel-cc-to-union-end ()
  "Move point to end of current union definition.

If point is inside one, mark position before moving point,
allowing moving the point back by using \\[pel-jump-to-mark].
If point is not inside an union definition block, issue a user
error."
  (interactive)
  (pel--cc-move-to
   (pel-cc-elem-boundaries pel-cc--union-regexp)
   1
   "union"))

(defun pel-cc-to-struct-begin ()
  "Move point to beginning of current struct (or class) definition.

If point is inside one, mark position before moving point,
allowing moving the point back by using \\[pel-jump-to-mark].
If point is not inside an struct (or class) definition block,
issue a user error."
  (interactive)
  (pel--cc-move-to
   (pel-cc-elem-boundaries (if (eq major-mode 'c++-mode)
                               pel-cc--class-regexp
                             pel-cc--struct-regexp))
   0
   (if (eq major-mode 'c++-mode)
       "struct or class"
     "struct")))

(defun pel-cc-to-class-begin ()
  "Move point to beginning of current struct (or class) definition.

Same as `pel-cc-to-struct-begin'"
  (interactive)
  (pel-cc-to-struct-begin))


(defun pel-cc-to-struct-end ()
  "Move point to end of current struct (or class) definition.

If point is inside one, mark position before moving point,
allowing moving the point back by using \\[pel-jump-to-mark].
If point is not inside an struct (or class) definition block,
issue a user error."
  (interactive)
  (pel--cc-move-to
   (pel-cc-elem-boundaries (if (eq major-mode 'c++-mode)
                               pel-cc--class-regexp
                             pel-cc--struct-regexp))
   1
   (if (eq major-mode 'c++-mode)
       "struct or class"
     "struct")))

(defun pel-cc-to-class-end ()
  "Move point to end of current struct (or class) definition.

Same as `pel-cc-to-struct-end'"
  (interactive)
  (pel-cc-to-struct-end))

;;; --------------------------------------------------------------------------
(provide 'pel-cc-navigate)

;;; pel-cc-navigate.el ends here
