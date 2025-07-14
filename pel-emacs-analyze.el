;;; pel-emacs-analyze.el --- Analyze various parts of Emacs system.  -*- lexical-binding: t; -*-

;; Created   : Monday, July 14 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-07-14 16:02:13 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2025  Pierre Rouleau
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
;; This file holds a set of functions written to help analyze Emacs bahavior
;; for various mechanisms by printing the values of user-option and variables.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-show-lisp-control-variables (&optional append)
  "Display values of lisp.el control variables."
  (interactive "P")
  (let ((pel-insert-symbol-content-context-buffer (current-buffer)))
    (pel-print-in-buffer
     "pel-lisp.el-info*"
     (format  "lisp.el control variables (Emacs %s)" (emacs-version))
     (lambda ()
       (insert "User options:\n")
       (pel-insert-symbol-content-line 'defun-prompt-regexp)
       (pel-insert-symbol-content-line 'parens-require-spaces)
       (pel-insert-symbol-content-line 'insert-pair-alist)
       (pel-insert-symbol-content-line 'delete-pair-blink-delay)
       (insert "\n\nVariables:")
       (pel-insert-symbol-content-line 'forward-sexp-function)
       (pel-insert-symbol-content-line 'beginning-of-defun-function)
       (pel-insert-symbol-content-line 'end-of-defun-function)
       (pel-insert-symbol-content-line 'end-of-defun-moves-to-eol)
       (pel-insert-symbol-content-line 'narrow-to-defun-include-comments))
     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-emacs-analyze)

;;; pel-emacs-analyze.el ends here
