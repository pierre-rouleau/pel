;;; pel-python.el --- PEL extra support for Python.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, March 18 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-03-18 07:43:44 EDT, updated by Pierre Rouleau>

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
;; PEL extra support for the Python programming language.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)              ; use: `pel-has-shebang-line'
(require 'pel--options)           ; use: `pel-python-shebang-line'
(require 'pel-ccp)                ; use: `pel-delete-line'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; Python Shebang Line Control
;; ------------------------
;;
;; Use for extension-less Python files that use Python directly.

(defun pel-python-insert-shebang-line ()
  "Insert a shebang line corresponding to user-option choice."
  ;; If the user-option specifies to use Emacs default, assume that
  ;; it was already inserted by the `pel-as' command selecting Python.
  ;; Otherwise assume it's there and must be replaced.
  (save-excursion
    ;; If the file already has a shebang line, replace it by the user-option selected
    (when (pel-has-shebang-line)
      (goto-char (point-min))
      (pel-delete-line))
    (goto-char (point-min))
    (insert pel-python-shebang-line)
    (insert "\n")))

;;; --------------------------------------------------------------------------
(provide 'pel-python)

;;; pel-python.el ends here
