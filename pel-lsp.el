;;; pel-lsp.el --- Language Server Protocol Utilities.  -*- lexical-binding: t; -*-

;; Created   : Monday, June 21 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-06-21 18:19:05, updated by Pierre Rouleau>

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
;; This file defines a set of utility commands to help manage Language Server
;; support for various programming languages.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: pel-toggle-and-show-user-option
;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-toggle-lsp-log-io (&optional locally)
  "Toggle the logging of LSP I/O.
The initial state is set by the `lsp-log-io' user-option.
By default this command impact is global unless an argument prefix is
specified, in which case it is applied to the current buffer only."
  (interactive "P")
  (pel-toggle-and-show-user-option 'lsp-log-io (not locally)))

;;-pel-autoload
(defun pel-toggle-lsp-ui-sideline (&optional locally)
  "Toggle the display of information of the current line.
The initial state is set by the `lsp-ui-sideline-enable' user-option.
By default this command impact is global unless an argument prefix is
specified, in which case it is applied to the current buffer only."
  (interactive "P")
  (pel-toggle-and-show-user-option 'lsp-ui-sideline-enable (not locally)))

;;-pel-autoload
(defun pel-toggle-lsp-ui-doc (&optional locally)
  "Toggle the display of documentation.
The initial state is set by the `lsp-ui-doc-enable' user-option.
By default this command impact is global unless an argument prefix is
specified, in which case it is applied to the current buffer only."
  (interactive "P")
  (pel-toggle-and-show-user-option 'lsp-ui-doc-enable (not locally)))

;;; --------------------------------------------------------------------------
(provide 'pel-lsp)

;;; pel-lsp.el ends here
