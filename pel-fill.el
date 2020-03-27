;;; pel-fill.el --- PEL Source code management -*-lexical-binding: t-*-

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
;; Loose collection of commands and functions that deal with text fill.

(require 'pel--base)                 ; use: pel-used-major-mode-of, pel-toggle
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


;;-pel-autoload
(defun pel-show-fill-columns ()
  "Display value of relevant fill columns for current buffer."
  (interactive)
  (if (memq (pel-used-major-mode-of)
            '(emacs-lisp-mode lisp-interaction-mode lisp-mode))
      (message "Lisp fill columns:
- comment  : %S
- docstring: %S
- code     : %S"
               (or comment-fill-column fill-column)
               emacs-lisp-docstring-fill-column
               (current-fill-column))
    (message "Fill column:
- comment  : %S
- text     : %S"
             (or comment-fill-column fill-column)
             (current-fill-column))))

;; -----------------------------------------------------------------------------
(provide 'pel-fill)

;;; pel-fill.el ends here
