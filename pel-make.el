;;; pel-make.el --- Makefile editing support.  -*- lexical-binding: t; -*-

;; Created   : Friday, January 15 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-01-15 16:00:26, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021  Pierre Rouleau
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
;; This file provides commands to navigate inside make files.  These
;; complements what the standard Emacs make-mode.el provides.  The provided
;; commands are:
;;
;;  * `pel-make-next-macro'
;;  * `pel-make-previous-macro'
;;
;; These 2 commands navigate across the macro definition statements. They
;; support shift marking and can be used to count the number of make macro
;; statements.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: pel-count-string
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defconst pel-make-macro-regxp "^[ \t]*\\(\\_<[_[:alpha:]][_[:alnum:]]+\\_>\\) *:*="
  "Regexp used to search make file macro definitions.")

;; --
;;-pel-autoload
(defun pel-make-next-macro (&optional n silent dont-push-mark)
  "Move to the beginning of next N make file macro definition statement.

The function skips over comments.

If no valid form is found, don't move point, issue an error
describing the failure unless SILENT is non-nil, in which case
the function returns nil on error and non-nil on success.
The error message states the number of instanced searched, the
regexp used and the number of instances found.

On success, the function push original position on the mark ring
unless DONT-PUSH-MARK is non-nil.

The command support shift-marking."
  (interactive "^p")
  (if (< n 0)
      (pel-make-previous-macro (abs n))
    (let ((start-pos (point))
          (count 0))
      (condition-case err
          (progn
            (dotimes (_ n)
              (end-of-line) ; don't stay on current definition line if cursor is at bol
              (re-search-forward pel-make-macro-regxp)
              (setq count (1+ count)))
            (back-to-indentation)
            (unless dont-push-mark
                (push-mark start-pos))
            t)
        (search-failed
         ;; restore original position when search failed
         (goto-char start-pos)
         (unless silent
           (user-error "Found only %d of requested %s:\n%s"
                       count
                       (pel-count-string n "macro definition statement")
                       err)))))))

;;-pel-autoload
(defun pel-make-previous-macro (&optional n silent dont-push-mark)
  "Move to the beginning of previous N make file macro definition statement.

The function skips over comments.

If no valid form is found, don't move point, issue an error
describing the failure unless SILENT is non-nil, in which case
the function returns nil on error and non-nil on success.
The error message states the number of instanced searched, the
regexp used and the number of instances found.

On success, the function push original position on the mark ring
unless DONT-PUSH-MARK is non-nil.

The command support shift-marking."
  (interactive "^p")
  (if (< n 0)
      (pel-make-previous-macro (abs n))
    (let ((start-pos (point))
          (count 0))
      (condition-case err
          (progn
            (dotimes (_ n)
              (re-search-backward pel-make-macro-regxp)
              (setq count (1+ count)))
            (back-to-indentation)
            (unless dont-push-mark
                (push-mark start-pos))
            t)
        (search-failed
         ;; restore original position when search failed
         (goto-char start-pos)
         (unless silent
           (user-error "Found only %d of requested %s:\n%s"
                       count
                       (pel-count-string n "macro definition statement")
                       err)))))))

;;; --------------------------------------------------------------------------
(provide 'pel-make)

;;; pel-make.el ends here
