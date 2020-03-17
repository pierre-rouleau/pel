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
;; To do that, a need to review the way indentation works for a large
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
  (require 'cc-vars))                      ; uses: c-basic-offset


;; --
(defun pel--insert-c-indent-line (&optional n)
  "Insert N times the `c-basic-offset' space characters on current line."
  (let ((n (prefix-numeric-value n)))
    (if (< n 0)
        (pel-unindent (abs n))
      (insert (make-string (* c-basic-offset n) ?\s)))))

;;-pel-autoload
(defun pel-insert-c-indent (&optional n)
  "Insert N times `c-basic-offset' spaces on current or marked line(s).
A special argument N can specify more than one
indentation level.  It defaults to 1.
If a negative number is specified, `pel-unindent' is used.
If region was marked, the function does not deactivate it to allow
repeated execution of the command."
  (interactive "*P")
  (save-excursion
    (if (use-region-p)
        (let ((begin-point (region-beginning))
              (line-count (count-lines (region-beginning) (region-end)))
              ;; by default all insertion commands set the var deactivate-mark
              ;; to force mark deactivation.  Prevent this by creating a local binding.
              deactivate-mark)
          (goto-char begin-point)
          (dotimes (i line-count)
            (pel--insert-c-indent-line n)
            (forward-line 1))
          (set-mark begin-point))
      (pel--insert-c-indent-line n))))

;; --
(defun pel--line-unindent (&optional n)
  "Un-indent current line by N times `c-basic-offset' spaces.
Works when point is anywhere on the line.
Limitation: does not handle hard tabs and may move point."
  (back-to-indentation)
  (let ((n (prefix-numeric-value n)))
    (when (> (current-column) 0)
      (left-char (min (current-column) (* c-basic-offset n)))
      (if (and (require 'pel-ccp nil :no-error)
               (fboundp 'pel-delete-to-next-visible))
          (pel-delete-to-next-visible)
        (error "Function pel-delete-to-next-visible is not loaded")))))

;;-pel-autoload
(defun pel-unindent (&optional n)
  "Un-indent current line or marked region by N times `c-basic-offset' spaces.
Works when point is anywhere on the line.
If region was marked, the function does not deactivate it to allow
repeated execution of the command.
Limitation: does not handle hard tabs."
  (interactive "*P")
  (save-excursion
    (if (use-region-p)
        (let ((begin-point (region-beginning))
              (line-count (count-lines (region-beginning) (region-end)))
              ;; by default all insertion commands set the var deactivate-mark
              ;; to force mark deactivation.  Prevent this by creating a local binding.
              deactivate-mark)
          (goto-char begin-point)
          (dotimes (i line-count)
            (pel--line-unindent n)
            (forward-line 1))
          (set-mark begin-point))
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

;; -----------------------------------------------------------------------------
(provide 'pel-indent)

;;; pel-indent.el ends here
