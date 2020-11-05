;;; pel-indent.el --- PEL indentation utilities.  -*- lexical-binding: t; -*-

;; Created   : Saturday, February 29 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2020-11-05 15:11:13, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020  Pierre Rouleau
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
;; This file provides rigid indentation support.  It is meant to be used in
;; files where the super useful automatic indentation of the function
;; `indent-for-tab-command' is used (and assigned to the tab key).
;;
;; The force rigid indentation is needed in several scenarios.  It's not that
;; useful for edit Lisp source code but is useful for other modes such as C
;; and other curly bracket programming languages as well as indentation
;; sensitive programming languages like Python.
;;
;; 3 commands are provided:
;;
;; - Function `pel-indent-lines' and `pel-unindent-lines' rigidly indent and
;;   un-indent the region by a specified number of indentation levels which
;;   defaults to 1.
;; - Function `pel-indent-rigidly' allows interactive indentation by 1
;;   character at a time or indentation level at a time.
;;
;; The number of columns used for the indentation level used by the functions
;; `pel-indent-lines' and 'pel-unindent-lines' is returned by the function
;; `pel-indent-level-colums'.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; --------------------------------------------------------------------------
;;; Code:

;; ---------------------------------------------------------------------------
;; TODO: enhance pel-indent-level-columns to better consider various types of
;; files for various programming and markup languages.

(defun pel-indent-level-columns (&optional n)
  "Return the number of columns corresponding to 1 (or N) indentation levels.
The indentation level depends on the type of buffer."
  (let ((cols-by-indentation-level   (if (and (boundp 'c-basic-offset)
                                              (integerp c-basic-offset))
                                         c-basic-offset
                                       tab-width)))
    (* (or n 1) cols-by-indentation-level)))

(defun pel-indent-marked-lines ()
  "Return information about currently marked lines.
The returned value is a list of 3 elements:
- line number of the first line in the region
- line number of the last line in the region
- symbol 'point-before-mark or 'mark-before-point."
  (list
   (line-number-at-pos (region-beginning))
   (line-number-at-pos (region-end))
   (if (< (point) (mark))
       'point-before-mark
     'mark-before-point)))

(defun pel-indent-line-pos (line at-end)
  "Return position of specified LINE at its first character unless AT-END."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (if at-end
        (move-end-of-line nil)
      (move-beginning-of-line nil))
    (point)))

(defun pel-indent-mark-lines (first-line end-line order)
  "Mark a region of lines identified by the arguments.
FIRST-LINE := line number of the first line in the region
END-LINE   := line number of the last line in the region
ORDER      := symbol.  One of 'point-before-mark or 'mark-before-point.
The ORDER argument identifies the relative position of the point and mark
in the region created by the function."
  (cond ((eq order 'point-before-mark)
         (goto-char (pel-indent-line-pos first-line nil))
         (set-mark  (pel-indent-line-pos end-line t)))
        ((eq order 'mark-before-point)
         (set-mark  (pel-indent-line-pos first-line nil))
         (goto-char (pel-indent-line-pos end-line t)))
        (t
         (error "Invalid order argument value: %s" order))))


(defun pel-indent-mark-lines-by-spec (marked-lines-spec)
  "Mark a region of lines identified by the MARKED-LINES-SPEC argument.
The MARKED-LINES-SPEC argument is a list with the following 3 elements:
- line number of the first line in the region
- line number of the last line in the region
- symbol 'point-before-mark or 'mark-before-point."
  (pel-indent-mark-lines (nth 0 marked-lines-spec)
                         (nth 1 marked-lines-spec)
                         (nth 2 marked-lines-spec)))

(defun pel--insert-c-indent-line (n)
  "Insert N times the indentation level number of space chars on current line."
    (if (< n 0)
        (pel-unindent-lines (abs n))
      (move-beginning-of-line nil)
      (insert (make-string (pel-indent-level-columns n) ?\s))))


;;-pel-autoload
(defun pel-indent-lines (&optional n)
  "Insert current or marked lines by N indentation levels.
A special argument N can specify more than one
indentation level.  It defaults to 1.
If a negative number is specified, `pel-unindent-lines' is used.
If a region was marked, the function does not deactivate it to allow
repeated execution of the command.  It also modifies the region to
include all characters in all affected lines."
  (interactive "*P")
  (let ((n (prefix-numeric-value n)))
    (if (use-region-p)
        (let ((original-region-spec (pel-indent-marked-lines))
              (begin-point (region-beginning))
              (line-count (count-lines (region-beginning) (region-end)))
              ;; Normally, Emacs deactivates the region right after a command
              ;; has executed.  To prevent this, and allow region to stay
              ;; active and visible for further execution of this command,
              ;; set the deactivate-mark variable to nil for this form.
              (deactivate-mark nil))
          (deactivate-mark)
          (goto-char begin-point)
          (dotimes (_i line-count)
            (pel--insert-c-indent-line n)
            (forward-line 1))
          (pel-indent-mark-lines-by-spec original-region-spec))
      (pel--insert-c-indent-line n))))

;; --
(defun pel--line-unindent (&optional n)
  "Un-indent current line by N indentation levels.
Works when point is anywhere on the line.
Limitation: does not handle hard tabs and may move point."
  (back-to-indentation)
  (let ((n (prefix-numeric-value n)))
    (when (> (current-column) 0)
      (left-char (min (current-column) (pel-indent-level-columns n)))
      (if (and (require 'pel-ccp nil :no-error)
               (fboundp 'pel-delete-to-next-visible))
          (pel-delete-to-next-visible)
        (error "Function pel-delete-to-next-visible is not loaded")))))

;;-pel-autoload
(defun pel-unindent-lines (&optional n)
  "Un-indent current line or marked region by N indentation levels.
Works when point is anywhere on the line.
If region was marked, the function does not deactivate it to allow
repeated execution of the command.
If a region was marked, the function does not deactivate it to allow
repeated execution of the command.  It also modifies the region to
include all characters in all affected lines.
Limitation: does not handle hard tabs."
  (interactive "*P")
  (save-excursion
    (if (use-region-p)
        (let ((original-region-spec (pel-indent-marked-lines))
              (begin-point (region-beginning))
              (line-count (count-lines (region-beginning) (region-end)))
              ;; Normally, Emacs deactivates the region right after a command
              ;; has executed.  To prevent this, and allow region to stay
              ;; active and visible for further execution of this command,
              ;; set the deactivate-mark variable to nil for this form.
              (deactivate-mark nil))
          (deactivate-mark)
          (goto-char begin-point)
          (dotimes (_i line-count)
            (pel--line-unindent n)
            (forward-line 1))
          (pel-indent-mark-lines-by-spec original-region-spec))
      (pel--line-unindent n))))

;;-pel-autoload
(defun pel-indent-rigidly (&optional n)
  "Indent rigidly the marked region or current line N times.
If a region is marked, it uses `indent-rigidly' and provides the
same prompts to control indentation changes.
If no region is marked, it operates on current line(s) identified
by the numeric argument N (or if not specified N=1):
- N = [-1, 0, 1]   : operate on current line
- N > 1   : operate on the current line and N-1 lines below.
- N < -1  : operate on the current line and (abs N) -1 lines above."
  (interactive "*P")
  (let ((n (prefix-numeric-value n)))
    (unless (use-region-p)
      (if (= n 0)
          (setq n 1))
      (if (and (require 'pel-mark nil :no-error)
               (fboundp 'pel-mark-line-up)
               (fboundp 'pel-mark-line-down))
          (if (< n 0)
              (pel-mark-line-up n)
            (pel-mark-line-down n))
        (error "The pel-mark functions are not loaded")))
    (indent-rigidly (region-beginning) (region-end) nil t)))

;;; --------------------------------------------------------------------------
(provide 'pel-indent)

;;; pel-indent.el ends here
