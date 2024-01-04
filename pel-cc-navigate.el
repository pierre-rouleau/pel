;;; pel-cc-navigate.el --- C/C++ Specialized Navigation Commands.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, January  2 2024.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2024-01-04 17:53:19 EST, updated by Pierre Rouleau>

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
;; With the point inside a switch statement the `pel-cc-to-switch-begin'
;; command moves the point to the '{' character of the current switch
;; statement block and the `pel-cc-to-switch-end' moves point just after the
;; '}' character at the end of the switch block.
;;
;; If point is outside a switch statement these commands issue an appropriate
;; user error.
;;
;; The logic uses a set of helper functions.
;; - `pel-cc-list-switch-in-fct' builds and returns a list of position
;;   2-elements list, each identifying the location of the beginning and end
;;   of a switch statement inside the current function. This is then used to
;;   determine the position where point must be moved if any.
;;   - It uses the `pel-cc-inside-function-p' predicate function that returns
;;   t when point is inside a C/C++ function.
;; - The `pel-cc-inside-switch-pos' calls `pel-cc-list-switch-in-fct' and
;;   returns the 2-element list that corresponds to the text-area of the
;;   enclosing switch statement if the point is inside one.
;;   - The two interactive commands call `pel-cc-inside-switch-pos' and
;;   retrieve the appropriate position to jump to, if any is reported.
;;
;; To navigate to the beginning or end of the enum, union, struct or class
;; definition blocks the code provides the following commands:
;;
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

(defconst pel-cc--switch-regexp "switch\\s-*("
  "Regex to find start paren of switch statement")

(defun pel-cc-inside-function-p ()
  "Return t if point is inside a C/C++ function, nil otherwise.

Limitation: will report valid when inside a structure or class
block (or any block).  Use only when appropriate."
  (save-excursion
    (condition-case nil
        (progn
          (backward-up-list)
          t)
      (error nil))))


(defun pel-cc-list-switch-in-fct ()
  "Return list of all switch statements in current function.

Each element of the list is a tuple of 2 points identifying the beginning
and the end of the switch statement.  The list elements are in order of the
switch statements inside the function.

Return nil if the function has no switch statement or point is
outside of a function."
  (let ((switch-pos-list nil))
    (when (pel-cc-inside-function-p)
      (save-excursion
        (beginning-of-defun)
        (when (search-forward "{" nil :noerror)
          (let ((begin-pos (point))
                (end-pos nil))
            (left-char)
            (forward-sexp)
            (left-char)
            (setq end-pos (point))
            (goto-char begin-pos)
            (while (re-search-forward pel-cc--switch-regexp end-pos :noerror)
              (when (pel-inside-code-p)
                (when (search-forward "{" nil :noerror)
                  (let ((sw-begin-pos (point))
                        (sw-end-pos nil))
                    (left-char)
                    (forward-sexp)
                    (left-char)
                    (setq sw-end-pos (point))
                    (goto-char sw-begin-pos)
                    (setq switch-pos-list
                          (cons (list sw-begin-pos sw-end-pos)
                                switch-pos-list))))))))))
    (reverse switch-pos-list)))

(defun pel-cc-inside-switch-pos ()
  "Return switch boundary positions if point is inside a switch statement.

Return nil otherwise."
  (let ((current-pos (point))
        (switch-pos-list (pel-cc-list-switch-in-fct))
        (found-pos nil))
    (when switch-pos-list
      (dolist (positions switch-pos-list)
        (let ((sw-beg-pos (nth 0 positions))
              (sw-end-pos (nth 1 positions)))
          (when (and (< sw-beg-pos current-pos)
                     (> sw-end-pos current-pos))
            (setq found-pos positions)))))
    found-pos))

(defun pel-cc-to-switch-begin ()
  "Move point to the beginning of current switch statement.

If one found, mark position before moving point, allowing moving the point
back by using \\[pel-jump-to-mark].
If no switch statement is found, issue a user error."
  (interactive)
  (let ((enclosing-pos (pel-cc-inside-switch-pos)))
    (if enclosing-pos
        (progn
          (push-mark)
          (goto-char (- (nth 0 enclosing-pos) 1)))
      (user-error "Point is not inside a switch statement!"))))

(defun pel-cc-to-switch-end ()
  "Move point to the end of current switch statement.

If one found, mark position before moving point, allowing moving the point
back by using \\[pel-jump-to-mark].
If no switch statement is found, issue a user error."
  (interactive)
  (let ((enclosing-pos (pel-cc-inside-switch-pos)))
    (if enclosing-pos
        (progn
          (push-mark)
          (goto-char (+ (nth 1 enclosing-pos) 1)))
      (user-error "Point is not inside a switch statement!"))))


;; Navigating to class, enum, struct, union definitions boundaries
;; ---------------------------------------------------------------

;; [:todo 2024-01-04: add ability to select the regexp via user-options ]
;; [:todo 2024-01-04: add support for C++11 attribute specifier]
(defconst pel-cc--class-regexp "^\\s-*\\(\\(class\\)\\|\\(struct\\)\\)\\s-"
  "Regex to find start of C++ class or struct statement")

(defconst pel-cc--struct-regexp "^\\(\\s-*typedef\\s-*\\)*\\s-*struct\\s-"
  "Regex to find start of C/C++ class or struct statement")

(defconst pel-cc--union-regexp "^\\(\\s-*typedef\\s-*\\)*\\s-*union\\s-"
  "Regex to find start of C/C++ union statement")

(defconst pel-cc--enum-regexp
  "^\\(\\s-*typedef\\s-*\\)*\\s-*enum\\(\\(\\s-+\\(class\\s-*\\)?\\)\\|$\\)"
  "Regex to find start of C/C++ enum or C++ enum class statement")


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
      (while (and (not found-boundary)
                  (re-search-forward elem-regexp nil :noerror))
        (when (pel-inside-code-p)
          (when (search-forward "{" nil :noerror)
            (left-char)
            (setq begin-pos (point))
            (forward-sexp)
            (setq end-pos (point))
            (when (and (< begin-pos current-position)
                       (> end-pos   current-position))
              (setq found-boundary (list begin-pos end-pos))))))
      found-boundary)))


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

(defalias 'pel-cc-to-class-begin #'pel-cc-to-struct-begin)


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

(defalias 'pel-cc-to-class-end #'pel-cc-to-struct-end)

;;; --------------------------------------------------------------------------
(provide 'pel-cc-navigate)

;;; pel-cc-navigate.el ends here
