;;; pel-align.el --- Text alignment utilities.  -*- lexical-binding: t; -*-

;; Created   : Saturday, October 24 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2023-10-27 12:14:30 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2022, 2023  Pierre Rouleau
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

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)

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
  "Display current buffer's vertical alignment behaviour."
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
  (pel-toggle 'pel-newline-does-align)
  (pel-align-info))

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

;;; --------------------------------------------------------------------------
(provide 'pel-align)

;;; pel-align.el ends here
