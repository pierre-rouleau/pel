;;; pel-screenlog.el --- GNU Screen Log File Transformation.  -*- lexical-binding: t; -*-

;; Created   : Saturday, December 10 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-12-11 17:44:25 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2022  Pierre Rouleau
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
;; This file provides support for the GNU Screen log file.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--options)
(require 'pel-whitespace)
(require 'ansi-color)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-screen-log-fix-rendering ()
  "Fix rendering of buffer created by the GNU Screen log.

The function converts the marked area of a buffer, if it is
marked, otherwise it processes the entire or the narrowed portion
of the buffer.

It renders the escape codes, converts the line endings to
Unix-style line endings.

In some cases the log file created by GNU Screen injects one
extra line per line.  If the
`pel-screen-log-delete-all-consecutive-blank-lines' user-option
is set, the function removes them.

Some artifact main remain after these transformations.  To fix
them, identify a set of regular expression/replacement string
pairs in the `pel-screen-log-fix-regexp-pairs' user-option.  The
`pel-screen-log-fix-rendering' function performs the replacements."
  (interactive)
  (save-restriction)
  (let ((pos-beg (if (use-region-p) (region-beginning) (point-min)))
        (pos-end (if (use-region-p) (region-end)       (point-max))))
    (save-restriction
      ;; render all escape sequences left verbatim inside the log file created
      ;; by GNU Screen
      (ansi-color-apply-on-region pos-beg pos-end)

      (when (memq (pel-current-buffer-eol-type) '(dos mac))
        (set-buffer-file-coding-system 'unix))

      (when pel-screen-log-delete-all-consecutive-blank-lines
        (pel-delete-all-dual-consecutive-blank-lines))

      (when pel-screen-log-fix-regexp-pairs
        (dolist (regexp-pair pel-screen-log-fix-regexp-pairs)
          (while (progn
                   (goto-char (point-min))
                   (re-search-forward (car regexp-pair) nil :noerror))
            (replace-match (cadr regexp-pair) :fixedcase)))))))

;;; --------------------------------------------------------------------------
(provide 'pel-screen)

;;; pel-screen.el ends here
