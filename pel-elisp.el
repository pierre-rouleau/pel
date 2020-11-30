;;; pel-elisp.el --- Emacs Lisp support.  -*- lexical-binding: t; -*-

;; Created   : Friday, November 27 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2020-11-30 12:49:59, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020  Pierre Rouleau
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
;; This only contains a function to change the behaviour of the Emacs Lisp
;; defun navigation functions like `beginning-of-defun' and `end-of-defun'.
;;
;; By default these stop at what looks like a defun form located inside a
;; string.  But the behaviour is controlled by the
;; `open-paren-in-column-0-is-defun-start' user option.
;;
;; This file provides the command function
;; `pel-toggle-paren-in-column-0-is-defun-start' to toggle the user option and
;; the behaviour of the navigation functions.

;; Credits:  Thanks to Andreas Röhler to mention the Standard Emacs variable
;;           `open-paren-in-column-0-is-defun-start'! That allowed me to
;;           replace a large amount of code with a simple toggle command and
;;           that handles the problem I had with checkdoc as well!

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: pel-toggle-and-show

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-toggle-paren-in-column-0-is-defun-start ()
  "Toggle interpretation of a paren in column 0 and display new behaviour.
Toggle the value of `open-paren-in-column-0-is-defun-start'."
  (interactive)
  (pel-toggle-and-show
   'open-paren-in-column-0-is-defun-start
   "Now interpret all '(' in column 0 as a start of defun."
   "A '(' in column 0 is no longer always interpreted as a defun start."))

;;; --------------------------------------------------------------------------
(provide 'pel-elisp)

;;; pel-elisp.el ends here
