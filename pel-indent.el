;;; pel-indent.el --- PEL indentation utilities.  -*- lexical-binding: t; -*-

;; Created   : Saturday, February 29 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-04-30 09:48:35 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2023, 2024, 2025  Pierre Rouleau
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
;;
;; The function `pel-indent-lines' and `pel-unindent-lines' handle hard tabs
;; properly according to the currently active `indent-tabs-mode'.
;;
;; - When hard tabs are not permitted (i.e. `indent-tabs-mode' is nil), the
;;   function replace all hard tabs in the indentation by the appropriate
;;   number of space characters.  The functions do not replace hard tabs that
;;   are somewhere else on the line (e.g. inside a code string).
;; - When hard tabs are permitted (i.e. `indent-tabs-mode' is t), the
;;   functions tabify the line or region marked.

;; TODO: enhance pel-indent-level-columns to better consider various types of
;; files for various programming and markup languages.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;

(require 'pel--base)
;;; --------------------------------------------------------------------------
;;; Code:
;; ---------------------------------------------------------------------------


(defun pel-indent-current-line-positions ()
  "Return a cons with the positions of the beginning and end of current line."
  (let (begin end)
    (save-excursion
      (setq begin (progn
                    (beginning-of-line)
                    (point)))
      (setq end   (progn
                    (end-of-line)
                    (point))))
    (cons begin end)))

(defun pel-indent-tabify-current-line ()
  "Tabify the current line."
    (let ((begin.end (pel-indent-current-line-positions)))
      (tabify (car begin.end) (cdr begin.end))))

(defun pel-indent-untabify-current-line ()
  "Untabify the current line."
    (let ((begin.end (pel-indent-current-line-positions)))
      (untabify (car begin.end) (cdr begin.end))))

;; --

(defun pel-indent-hard-tab-in-region-or-line-p ()
  "Check if any lines is indented with a hard-tab in the marked region.
Return non-nil if there is at least one.  The returned value is the position
of the first hard tab found.
Return nil if there are no hard tab in the indentation."
  (let (begin-pos end-pos)
    (if (use-region-p)
        (progn
          (setq begin-pos (region-beginning))
          (setq end-pos   (region-end)))
      (let ((begin.end (pel-indent-current-line-positions)))
        (setq begin-pos (car begin.end))
        (setq end-pos   (cdr begin.end))))
    (save-excursion
      (goto-char begin-pos)
      (condition-case nil
          (re-search-forward "^ *\t" end-pos)
        (search-failed nil)))))

(defun pel-indent-untabify-region (marked-lines-spec)
  "Replace all hard tabs by spaces in the MARKED-LINES-SPEC region of lines.
FIRST-LINE := line number of the first line in the region
END-LINE   := line number of the last line in the region
ORDER      := symbol.  One of \\='point-before-mark or \\='mark-before-point.
The ORDER argument identifies the relative position of the point and mark
in the region created by the function."
  (let ((first-line (nth 0 marked-lines-spec))
        (end-line   (nth 1 marked-lines-spec)))
    (untabify (pel-indent-line-pos first-line nil)
              (pel-indent-line-pos end-line t))))

(defun pel-indent-tabify-region (marked-lines-spec)
  "Use hard tabs and spaces in the MARKED-LINES-SPEC region of lines.
FIRST-LINE := line number of the first line in the region
END-LINE   := line number of the last line in the region
ORDER      := symbol.  One of \\='point-before-mark or \\='mark-before-point.
The ORDER argument identifies the relative position of the point and mark
in the region created by the function."
  (let ((first-line (nth 0 marked-lines-spec))
        (end-line   (nth 1 marked-lines-spec)))
    (tabify (pel-indent-line-pos first-line nil)
            (pel-indent-line-pos end-line t))))

;; --
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
- symbol \\='point-before-mark or \\='mark-before-point."
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
ORDER      := symbol.  One of \\='point-before-mark or \\='mark-before-point.
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
- symbol \\='point-before-mark or \\='mark-before-point."
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
  "Indent current or marked lines by N indentation levels.
Works with point anywhere on the line.
All lines touched by the region are indented.
A special argument N can specify more than one
indentation level.  It defaults to 1.
If a negative number is specified, `pel-unindent-lines' is used.
If a region was marked, the function does not deactivate it to allow
repeated execution of the command.  It also modifies the region to
include all characters in all affected lines.
Handles presence of hard tabs:
- If `indent-tabs-mode' is non-nil the indentation is created with a mix
  of hard-tabs and space characters.
- If `indent-tabs-mode' is nil, any hard tab in the indentation of the
  marked lines is replaced by the proper number of spaces.
  Hard tabs after first non-whitespace character on the line are left."
  (interactive "*P")
  (let ((n (prefix-numeric-value n)))
    (let ((has-hard-tab (pel-indent-hard-tab-in-region-or-line-p)))
      (if (use-region-p)
          (let ((original-region-spec (pel-indent-marked-lines))
                (begin-point (region-beginning))
                (line-count (count-lines (region-beginning) (region-end)))
                ;; Normally, Emacs deactivates the region right after a
                ;; command has executed.  To prevent this, and allow region to
                ;; stay active and visible for further execution of this
                ;; command, set the deactivate-mark variable to nil for this
                ;; form.
                (deactivate-mark nil))
            (when has-hard-tab
              (pel-indent-untabify-region original-region-spec))
            (deactivate-mark)
            (goto-char begin-point)
            (dotimes (_i line-count)
              (pel--insert-c-indent-line n)
              (forward-line 1))
            (when indent-tabs-mode
              (pel-indent-tabify-region original-region-spec))
            (pel-indent-mark-lines-by-spec original-region-spec))
        ;; handle single line
        (when has-hard-tab
          (pel-indent-untabify-current-line))
        (pel--insert-c-indent-line n)
        (when indent-tabs-mode
          (pel-indent-tabify-current-line))))))

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
  "Un-indent current line or marked lines by N indentation levels.
Works when point is anywhere on the line.
All lines touched by the region are un-indented.
If region was marked, the function does not deactivate it to allow
repeated execution of the command.
If a region was marked, the function does not deactivate it to allow
repeated execution of the command.  It also modifies the region to
include all characters in all affected lines.
Limitation: does not handle hard tabs.
Handles presence of hard tabs:
- If `indent-tabs-mode' is non-nil the indentation is created with a mix
  of hard-tabs and space characters.
- If `indent-tabs-mode' is nil, any hard tab in the indentation of the
  marked lines is replaced by the proper number of spaces.
  Hard tabs after first non-whitespace character on the line are left."
  (interactive "*P")
  (let ((has-hard-tab (pel-indent-hard-tab-in-region-or-line-p)))
    (if (use-region-p)
        (let ((original-region-spec (pel-indent-marked-lines))
              (begin-point (region-beginning))
              (line-count (count-lines (region-beginning) (region-end)))
              ;; Normally, Emacs deactivates the region right after a command
              ;; has executed.  To prevent this, and allow region to stay
              ;; active and visible for further execution of this command,
              ;; set the deactivate-mark variable to nil for this form.
              (deactivate-mark nil))
          (when has-hard-tab
              (pel-indent-untabify-region original-region-spec))
          (deactivate-mark)
          (goto-char begin-point)
          (dotimes (_i line-count)
            (pel--line-unindent n)
            (forward-line 1))
          (when indent-tabs-mode
            (pel-indent-tabify-region original-region-spec))
          (pel-indent-mark-lines-by-spec original-region-spec))
      ;; handle single line
      (when has-hard-tab
          (pel-indent-untabify-current-line))
      (pel--line-unindent n)
      (when indent-tabs-mode
        (pel-indent-tabify-current-line)))))

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


(defconst pel--c-basic-offset-modes '(c-mode
                                      c++-mode
                                      objc-mode
                                      java-mode
                                      idl-mode
                                      pike-mode
                                      awk-mode
                                      d-mode
                                      tcl-mode) ; [:todo 2025-04-30, by Pierre
                                                ; Rouleau: not sure about tcl]
  "Major modes implemented as cc-modes.")

;;-pel-autoload
(defun pel-show-indent (&optional append)
  "Display current buffer's indentation behaviour controlling variable state."
  (interactive "P")
  (let ((pel-insert-symbol-content-context-buffer (current-buffer))
        (isa-cc-mode (derived-mode-p pel--c-basic-offset-modes))
        (isa-sh-mode (derived-mode-p '(sh-mode)))
        (indent-width-control-var pel--indentation-width-control-variable))
    (pel-print-in-buffer
     "*pel-indent-info*"
     "Indentation Control"
     (lambda ()
       (let ((some-major-mode-specific nil))
         (dolist (fmt '("pel-%s-indent-width"
                        "pel-%s-tab-width"
                        "pel-%s-use-tabs"))
           (let ((symbol (pel-major-mode-symbol-value-or fmt 'not-defined)))
             (unless (eq symbol 'not-defined)
               (setq some-major-mode-specific t)
               (pel-insert-symbol-content-line (pel-major-mode-symbol-for
                                                fmt)))))
         (when indent-width-control-var
           (insert "\n---
The following variable control indentation width in this mode:")
           (pel-insert-symbol-content-line indent-width-control-var))
         (when some-major-mode-specific
           (insert "\n----
The above major-mode specific user options take precedence
over the following global ones (unless they are set by
file variables):"))
         (when isa-cc-mode
           (pel-insert-symbol-content-line 'c-basic-offset))
         (when isa-sh-mode
           (pel-insert-symbol-content-line 'sh-basic-offset))
         (pel-insert-symbol-content-line 'tab-width)
         (insert "\n  -> Use ")
         (pel-insert-symbol 'pel-set-tab-width)
         (insert " to change locally and have tabs rendered with a different width.")
         (pel-insert-symbol-content-line 'indent-tabs-mode)
         (pel-insert-symbol-content-line 'standard-indent)
         (pel-insert-symbol-content-line 'tab-always-indent)
         (pel-insert-symbol-content-line 'tab-stop-list)))
     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-indent)

;;; pel-indent.el ends here
