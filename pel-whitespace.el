;;; pel-whitespace.el --- Text whitespace control utilities.  -*- lexical-binding: t; -*-

;; Created   : Monday, May 10 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-05-10 08:56:54, updated by Pierre Rouleau>

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
;; This file holds functions and commands that delete trailing whitepaces.
;;
;; - The `pel-delete-trailing-whitespace-if-activated' function is used as a
;;   hook call-back when the file is saved.
;; - The `pel-toggle-delete-trailing-space-on-save' command toggles the
;;   user-option variable `pel-delete-trailing-whitespace' locally or globally
;;   to change whether the trailing whitespaces are automatically deleted on
;;   file save.

;; Calling hierarchy:
;;
;; * `pel-toggle-delete-trailing-space-on-save'
;; - `pel-delete-trailing-whitespace-if-activated'
;;   - `pel-delete-trailing-whitespace'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)
(require 'pel--options)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-toggle-delete-trailing-space-on-save (&optional globally)
  "Toggle deletion of trailing spaces on file save and display current state.
By default change behaviour for local buffer only.  When GLOBALLY
argument is non-nil, change it for all buffers for the current
Emacs editing session (the change does not persist across Emacs
sessions).

To modify the global state permanently modify the customized
value of the user-option variable `pel-delete-trailing-whitespace'
via the `pel-pkg-for-filemng' group customize buffer."
  (interactive "P")
  (pel-toggle-and-show-user-option 'pel-delete-trailing-whitespace globally))

(defun pel-delete-trailing-whitespace ()
  "Delete trailing whitespace on current line."
  (let ((line-start-pos (progn (forward-line 0) (point)))
        (line-end-pos   (progn (move-end-of-line nil))))
    (delete-trailing-whitespace line-start-pos line-end-pos)))

;; pel-delete-trailing-whitespace-if-activated is used as a hook in pel_keys
;;-pel-autoload
(defun pel-delete-trailing-whitespace-if-activated (&optional start end)
  "Delete trailing whitespace if currently active.
Delete trailing whitespace in complete buffer or in the region identified
by the START and END positions if specified."
  (when pel-delete-trailing-whitespace
    (delete-trailing-whitespace start end)))

;;; --------------------------------------------------------------------------
(provide 'pel-whitespace)

;;; pel-whitespace.el ends here
