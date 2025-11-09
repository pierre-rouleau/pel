;;; pel-fill.el --- PEL Source code management -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2021, 2022, 2025  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

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
;; Loose collection of commands and functions that deal with text fill.

(require 'pel--base)                 ; use: pel-major-mode-of, pel-toggle
;;; Code:

;; The following 2 declarations of global variables from other files
;; that are distributed with GNU Emacs prevent lint warnings.
(defvar comment-fill-column)               ; from newcomment.el
(defvar emacs-lisp-docstring-fill-column)  ; from lisp-mode.el

;;-pel-autoload
(defun pel-auto-fill-only-comments ()
  "Toggle the variable `comment-auto-fill-only-comments'.
Activate/de-activate automatic filling in source code comments only."
  (interactive)
  (pel-toggle 'comment-auto-fill-only-comments))


(defun pel-show-fill-columns (&optional append)
  "Display value of relevant fill columns for current buffer.

Clear previous buffer content unless optional APPEND argument is non-nil,
in which case it appends to the previous report."
  (interactive "P")
  (let* ((fill-column-symbol (pel-major-mode-symbol-for "pel-%s-fill-column"))
        (fill-column-symbol-is-bound (boundp fill-column-symbol))
        (text-fill-column (current-fill-column))
        (isa-lisp-mode (memq (pel-major-mode-of)
                             '(emacs-lisp-mode
                               lisp-interaction-mode
                               lisp-mode)))
        (pel-insert-symbol-content-context-buffer (current-buffer)))
    (pel-print-in-buffer
     "*pel-fill-info*"
     "Fill Control"
     (lambda ()
       "Print fill control variable values."
       (pel-insert-symbol-content-line 'auto-fill-function)
       (pel-insert-symbol-content-line 'normal-auto-fill-function)
       (pel-insert-symbol-content-line 'auto-fill-inhibit-regexp)
       (pel-insert-symbol-content-line 'fill-column)
       (insert (format ", active value for text: %S" text-fill-column))
       (when fill-column-symbol-is-bound
         (pel-insert-symbol-content-line fill-column-symbol)
         (insert ", under PEL this controls fill-column."))
       (when isa-lisp-mode
         (pel-insert-symbol-content-line 'emacs-lisp-docstring-fill-column))
       (pel-insert-symbol-content-line 'default-justification)
       (pel-insert-symbol-content-line 'fill-prefix)
       (pel-insert-symbol-content-line 'left-margin)
       (pel-insert-symbol-content-line 'fill-individual-varying-indent)
       (pel-insert-symbol-content-line 'sentence-end-double-space)
       (pel-insert-symbol-content-line 'sentence-end-without-period)
       (pel-insert-symbol-content-line 'comment-fill-column)
       (pel-insert-symbol-content-line 'display-fill-column-indicator)
       (pel-insert-list-content  'global-display-fill-column-indicator-modes))
     (unless append :clear-buffer)
     :use-help-mode)))


;; -----------------------------------------------------------------------------
(provide 'pel-fill)

;;; pel-fill.el ends here
