;;; pel-diff.el --- File diff utilities.  -*- lexical-binding: t; -*-

;; Created   : Friday, March 12 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-10-21 09:23:50 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2022  Pierre Rouleau
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
;; This file provides utilities that speed up the use of Emacs EDiff library.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: pel-current-buffer-filename
(require 'pel-window)                   ; use: pel-window-direction-for
;;                                      ;      pel-window-select
(require 'pel--keys-macros)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; Key setup - that would normally be placed in pel_keys.el
;; But in case of ediff it must be done here because the hook is
;; passed directly to the ediff commands.


(define-pel-global-prefix pel:for-ediff-mode (kbd "<f11> SPC SPC d e"))

(defun pel--setup-for-ediff-mode ()
  "Activate ediff-mode setup, take local variables into account."
  (pel-local-set-f12-M-f12 'pel:for-ediff-mode))
(declare-function pel--setup-for-ediff-mode "pel_keys")
(pel--mode-hook-maybe-call
 (function pel--setup-for-ediff-mode)
 'ediff-mode 'ediff-mode-hook)

;; ---------------------------------------------------------------------------

;;-pel-autoload
(defun pel-ediff-2files (&optional n)
  "Run ediff-files on the files of current and other window.

If argument N is specified and in the [2,8] range it identifies
the window in the direction corresponding to the cursor numeric keypad:
  -             8 := 'up
  - 4 := 'left  5 := 'current  6 := 'right
  -             2 := 'down "
  ;; Note: ediff-files is available in Emacs -Q
  (interactive "P")
  (let ((fname-a (pel-current-buffer-filename))
        (fname-b (save-excursion
                   (pel-window-select
                    (pel-window-direction-for
                     (abs (prefix-numeric-value n))
                     'other))
                   (pel-current-buffer-filename))))
    (ediff-files fname-a fname-b
                 (list (function pel--setup-for-ediff-mode)))))

;;-pel-autoload
(defun pel-ediff-revision ()
  "Run ediff-revision for current file against its last commit version."
  ;; Note: ediff-revision is available in Emacs -Q
  (interactive)
  (ediff-revision (pel-current-buffer-filename)
                  (list (function pel--setup-for-ediff-mode))))

;;-pel-autoload
(defun pel-diff-show-status ()
  "Display diff-mode status information."
  (interactive)
  (message "diff-jump-to-old-file: %s => operate on %s file."
           (pel-symbol-on-off-string
            'diff-jump-to-old-file nil nil "not loaded")
           (if (bound-and-true-p diff-jump-to-old-file)
               "OLD"
             "new")))


;; ---------------------------------------------------------------------------

;;-pel-autoload
(defun pel-diff-hunk-files-occur (&optional nlines)
  "Show hunk files of current path patch inside an occur buffer.

Each line is displayed with NLINES before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to 0 regardless of the current value of
`list-matching-lines-default-context-lines'.
If a region is defined the search is restricted to the region."
  (interactive "^P")
  (let* ((list-matching-lines-default-context-lines 0)
         (boundary (when (use-region-p)
                     (region-bounds)))
         (has-index (save-excursion
                      (goto-char (point-min))
                      (re-search-forward "^Index: " boundary :noerror))))
    (occur (if has-index
               "^Index: "
             "^\\+\\+\\+ ")
           (or nlines 0)
           boundary)))

;;; --------------------------------------------------------------------------
(provide 'pel-diff)

;;; pel-diff.el ends here
