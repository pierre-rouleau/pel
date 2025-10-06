;;; pel-gleam.el --- PEL Gleam support extension.  -*- lexical-binding: t; -*-

;; Created   : Monday, October  6 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-06 15:47:42 EDT, updated by Pierre Rouleau>

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
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)  ; use: `pel-toggle-and-show-user-option'
;;                           `pel-symbol-value-or'
;;                           `pel-symbol-on-off-string'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-gleam-toggle-format-on-buffer-save (&optional globally)
  "Toggle automatic execution of gleam format when saving Gleam files.
By default change behaviour for local buffer only.

When GLOBALLY argument is non-nil, change it for all Gleam buffers in
the current Emacs editing session (the change does not persist across
Emacs sessions).  To modify the global state permanently modify the
customized value of the `gleam-ts-format-on-save' user option."
  (interactive "P")
  (pel-toggle-and-show-user-option 'gleam-ts-format-on-save globally))

;;-pel-autoload
(defun pel-gleam-setup-info ()
    "Display Gleam setup information."
(interactive)
  (message "\
Tab-width for this buffer     : %d
Indentation offset this buffer: %s
Runs gleam format on buffer save: %s"
           tab-width
           (pel-symbol-value-or 'gleam-ts-indent-offset)
           (pel-symbol-on-off-string 'gleam-ts-format-on-save "yes" "no")))

;;; --------------------------------------------------------------------------
(provide 'pel-gleam)

;;; pel-gleam.el ends here
