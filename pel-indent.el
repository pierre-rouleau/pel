;;; pel-indent.el --- PEL Indentation utilities

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau.swd@gmail.com>

;; This file is part of the PEL package
;; This file is not part of GNU Emacs.

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

;; -----------------------------------------------------------------------------
;;; Commentary:
;;
;; CAUTION: This file is in very early stage of development.
;; The goal is to provide a consistent handling of indentation that makes
;; sense for all sorts of file types.
;; To do dat, a need to review the way indentation works for a large
;; number of files is required and will take some time.  This meancs that the
;; design of this file and the functions it provides is very likely to change
;; until this analysis is completed.
;;
;; TODO: complete indentation behaviour analysis for at least: C, C++, python,
;;       Emacs Lisp, Erlang, Rust, Haskell, makefile, text modes, Org mode,
;;       etc...

;; -----------------------------------------------------------------------------
;;; Code:
(eval-when-compile
  (require 'cc-vars)                      ; uses: c-basic-offset
  (require 'pel-mark)                     ; uses: pel-mark-line-up
  (require 'pel-ccp))                     ; uses: pel-delete-to-next-visible

;;-pel-autoload
(defun pel-insert-c-indent (&optional n)
  "Insert N times the `c-basic-offset` space characters.
.
A special argument N can specify more than one
indentation level.  It defaults to 1.
If a negative number is specified, `pel-unindent' is used."
  (interactive "*P")
  (let ((n (prefix-numeric-value n)))
    (progn
      (if (< n 0)
          (pel-unindent (abs n))
        (insert (make-string (* c-basic-offset n) ?\s))))))

;;-pel-autoload
(defun pel-unindent (&optional n)
  "Unindent by N times `c-basic-offset' spaces."
  (interactive "*P")
  (let ((n (prefix-numeric-value n)))
    (when (> (current-column) 0)
      (left-char (min (current-column) (* c-basic-offset n)))
      (pel-delete-to-next-visible))))

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
      (if (< n 0)
          (pel-mark-line-up n)
        (pel-mark-line-down n)))
    (indent-rigidly (region-beginning) (region-end) nil t)))

;; -----------------------------------------------------------------------------
(provide 'pel-indent)

;;; pel-indent.el ends here
