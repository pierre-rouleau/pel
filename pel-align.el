;;; pel-align.el --- Text alignment utilities.  -*- lexical-binding: t; -*-

;; Created   : Saturday, October 24 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2024-12-23 19:40:24 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2022, 2023, 2024  Pierre Rouleau
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
;; This file contains alignment utility functions.
;;
;;  - `pel-newline-and-indent-below' is useful as a variant of the return key.
;;    If buffer local variable `pel-newline-does-align' is set to t, the command
;;    also aligns text based on the previous contiguous lines and then inserts
;;    new line and indents.  If the variable is nil then it only inserts a new line
;;    and indent. Use the function `pel-toggle-newline-indent-align' to toggle
;;    the value of the variable and `pel-align-info' to show its
;;    current state.
;;
;;  The `pel-align-words-vertically' aligns words in columns.  It can be used
;;  to align code in table format, a useful code formatting technique that
;;  helps in creating easy-to-read and understand code.  This technique is
;;  very unfortunately badly regarded by various development groups even if
;;  it helps reduce the mental effort to understand code and reduce bugs. The
;;  main reason for the reluctance of using this technique is the lack of
;;  editing support.  So here it is.  Code is meant to be easy to *read*,
;;  use good editors to help create it.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel-hash)
(require 'pel--base)                     ; used in pel-align-words-vertically
;;; --------------------------------------------------------------------------
;;; Code:
;;
(defvar-local pel-newline-does-align nil
  "When set to t, the function `pel-newline-and-indent-below'
executes the function `align-newline-and-indent' which aligns
the current line with the above one(s) before inserting the new line and
indenting.
Use the function `pel-toggle-newline-indent-align' to change this value.")

;;-pel-autoload
(defun pel-newline-and-indent-below ()
  "Insert an indented line just below current line.
If variable `pel-newline-does-align' is t, also align
the statements on the current line with the above contiguous lines."
  (interactive)
  (move-end-of-line nil)
  (if (and pel-newline-does-align
           (require 'align nil :noerror)
           (fboundp 'align-newline-and-indent))
      (align-newline-and-indent)
  (newline-and-indent)))

;; pel-autoload
(defun pel-align-info (&optional append)
  "Display current buffer's vertical alignment behaviour.

Clear previous buffer content unless optional APPEND argument is non-nil,
in which case it appends to the previous report."
  (interactive "P")
  (let ((pel-insert-symbol-content-context-buffer (current-buffer)))
    (pel-print-in-buffer
     "*pel-align-info*"
     "Vertical Alignment Control"
     (lambda ()
       (pel-insert-symbol-content-line 'pel-newline-does-align)
       (insert
        (format "\n  -> %s"
                (pel-symbol-text
                 'pel-newline-does-align
                 "on : M-RET aligns, adds newline and indents."
                 "off: M-RET does not align, but adds newline and indents.")))
       (insert "\n  -> Use ")
       (pel-insert-symbol 'pel-toggle-newline-indent-align)
       (insert " command to toggle it.")
       (pel-insert-symbol-content-line 'pel-modes-activating-align-on-return))
     (unless append :clear-buffer)
     :use-help-mode)))

;; pel-autoload
(defun pel-toggle-newline-indent-align ()
  "Toggle variable `pel-newline-does-align'.
This toggles the way function `pel-newline-and-indent-below'
operates."
  (interactive "*")
  (pel-toggle-and-show 'pel-newline-does-align))

;; ---------------------------------------------------------------------------
;; pel-autoload
(defun pel-multi-align-regexp ()
  "Align marked area with consecutive regexp.

Use this to align a marked area one several column, each with its own regexp,
starting on the left and going toward the right.

The command prompts for a regexp and then another and it will use as many
regex as provided.  To stop providing regexp, just hit RET without entering
text."
  (interactive "*")
  (unless (use-region-p)
    (user-error "No marked area"))
  (let ((pos-beg (region-beginning))
        (pos-end (region-end))
        (regexps nil)
        (new-regexp nil))
    (while (progn
             (setq new-regexp (read-string
                               (if regexps
                                   "Next align regexp (RET to stop): "
                                 "First align regexp: ")))
             (unless (string= "" new-regexp)
               (push new-regexp regexps))))
    (save-restriction
      (narrow-to-region pos-beg pos-end)
      (dolist (regexp (reverse regexps))
        (align-regexp (point-min)
                      (point-max)
                      (concat "\\(\\s-*\\)" regexp)
                      1 1 nil)))))

;; ---------------------------------------------------------------------------
;; pel-autoload
(defun pel-align-words-vertically ()
  "Align the words of multiple lines in area in vertically aligned columns."
  (interactive "*")
  (unless (use-region-p)
    (user-error "No marked area"))
  (let ((pos-beg (region-beginning))
        (pos-end (region-end))
        (words-per-line   (pel-make-hash-of-lists))
        (words-per-column (pel-make-hash-of-lists))
        (column-widths nil)
        (original-indentation 0))
    (save-restriction
      ;; Build a list of words per columns for all region lines
      (narrow-to-region pos-beg pos-end)
      (untabify (point-min) (point-max))
      (goto-char (point-min))
      (back-to-indentation)
      (setq original-indentation (current-column))
      (let ((line 0))
        (while (progn
                 (let ((words
                        (split-string (buffer-substring-no-properties
                                       (line-beginning-position)
                                       (line-end-position))))
                       (column 0))
                   (dolist (word words)
                     (pel-addto-hash-of-lists words-per-line line word))
                   (dolist (word words)
                     (pel-addto-hash-of-lists words-per-column column word)
                     (setq column (1+ column)))
                   (forward-line 1)
                   (setq line (1+ line)))
                 (not (eobp)))))
      ;; Compute the maximum word length for each column
      (let ((column -1)
            (words-in-column nil))
        (while (progn
                 (setq column (1+ column))
                 (setq words-in-column
                       (pel-get-list-from-hash-of-lists-for
                        words-per-column
                        column))
                 (when words-in-column
                   (setq column-widths
                         (cons (seq-reduce
                                (function max)
                                (mapcar (function length) words-in-column)
                                0)
                               column-widths))))))
      (setq column-widths (reverse column-widths))
      (message "column-widths: %S" column-widths)
      ;; delete current text (it's in the words-per-line) from buffer
      (delete-region (point-min) (point-max))
      ;; Insert back words in aligned columns
      (goto-char 0)
      (let ((words nil)
            (line -1)
            (line-indentation (make-string original-indentation ?\s)))
        (while (progn
                 (setq line (1+ line))
                 (setq words (pel-get-list-from-hash-of-lists-for
                              words-per-line
                              line))
                 (when words
                   (let ((column -1))
                     (insert line-indentation)
                     (dolist (word words)
                       (setq column (1+ column))
                       (insert (format
                                (format "%%-%ds " (nth column column-widths))
                                word)))
                     ;; remove line trailing space just inserted
                     (delete-char -1)
                     (insert "\n")
                     t))))))))


;;; --------------------------------------------------------------------------
(provide 'pel-align)

;;; pel-align.el ends here
