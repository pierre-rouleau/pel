;;; pel-nim.el --- PEL extra support for Nim.  -*- lexical-binding: t; -*-

;; Created   : Monday, March 19 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-03-19 13:47:10 EDT, updated by Pierre Rouleau>

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
;; PEL extra support for the Nim programming language.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)              ; use: `pel-has-shebang-line'
(require 'pel--options)           ; use: `pel-nim-shebang-line'
(require 'pel-ccp)                ; use: `pel-delete-line'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; Nim Shebang Line Control
;; ------------------------
;;
;; Use for extension-less Nim files that use Nim directly.

(defun pel-nim-insert-shebang-line ()
  "Insert a shebang line corresponding to user-option choice."
  ;; If the user-option specifies to use Emacs default, assume that
  ;; it was already inserted by the `pel-as' command selecting Nim.
  ;; Otherwise assume it's there and must be replaced.
  (save-excursion
    ;; If the file already has a shebang line, replace it by the user-option selected
    (when (pel-has-shebang-line)
      (goto-char (point-min))
      (pel-delete-line))
    (goto-char (point-min))
    (insert pel-nim-shebang-line)
    (insert "\n")))

;;; --------------------------------------------------------------------------
(provide 'pel-nim)

;;; pel-nim.el ends here
