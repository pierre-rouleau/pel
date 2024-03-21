;;; pel-comint.el --- PEL comint extension utilities.  -*- lexical-binding: t; -*-

;; Created   : Saturday, June 26 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2024-03-20 12:08:17 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2022, 2024  Pierre Rouleau
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
;;  Utilities to deal with comint buffers: mainly REPLs for various
;;  programming languages and interactive shells.


;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)
(require 'comint)

;;; --------------------------------------------------------------------------
;;; Code:
;;
;;-pel-autoload
(defun pel-comint-clear-buffer (&optional buffer-or-name get-prompt)
  "Clear the content of the comint-compliant BUFFER-OR-NAME.

If BUFFER-OR-NAME is not provided (or nil) the current buffer is
used.  If GET-PROMPT is non-nil send input to force getting a
prompt ready for input."
  (interactive "P")
  (with-current-buffer (or buffer-or-name (current-buffer))
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer))
    (when get-prompt
      (comint-send-input))))

;;-pel-autoload
(defun pel-comint-clear-buffer-and-get-prompt (&optional buffer-or-name)
  "Clear the content of the comint-compliant BUFFER-OR_NAME.

If BUFFER-OR-NAME is not provided (or nil) the current buffer is
used.  Send input to force getting a prompt ready for input."
  (interactive "P")
  (pel-comint-clear-buffer buffer-or-name :get-prompt))


;;-pel-autoload
(defun pel-comint-toggle-shell-echoes ()
  "Toggle the comint-process-echoes for the current buffer.

Use this when you want to prevent shell from echoing the command
in the current buffer and you do not want to modify the `comint-process-echoes'
customized value."
  (interactive)
  (pel-toggle-and-show 'comint-process-echoes nil nil :locally))

;;; --------------------------------------------------------------------------
(provide 'pel-comint)

;;; pel-comint.el ends here
