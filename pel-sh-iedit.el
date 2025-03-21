;;; pel-sh-iedit.el --- Sh-mode iedit support.  -*- lexical-binding: t; -*-

;; Created   : Monday, April 18 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-01-10 10:51:06 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2022, 2024, 2025  Pierre Rouleau
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
;; This file provides convenience code to enhance iedit-mode support inside
;; the buffers using the following major modes:
;;
;; - `cperl-mode'
;; - `sh-mode'
;; - `tcl-mode'
;;
;; It handles problems related to mis-handling of several syntactic elements
;; for the supported major modes, handling the problem described by the
;; following *reference* : https://github.com/victorhge/iedit/issues/148
;;
;;
;; As identified by the issue 148 referenced above, iedit is normally not able
;; to select the instance of someroot or somepath in a `sh-mode' buffer when the
;; variable is referenced with a $ following a slash as in:
;;
;;       # sh-mode buffer:
;;       someroot=abc
;;       somedir=def
;;       somefile=ghi
;;       export DIRPATH=/$someroot/$somedir/$somefile
;;
;; A similar issue exists with the ':' character.  In sh-mode the ':'
;; character is identified as part of the '_' syntax preventing detection of
;; symbols in strings that are prefixed or followed by the ':' character.
;;
;; To correct the issue, call `pel-sh-iedit-enhance' when sh-mode starts when
;; iedit is available.
;;

;; [:todo 2025-01-10, by Pierre Rouleau: Future Refactoring]
;;
;; This file was originally written to support only `sh-mode' but has evolved
;; to support other modes.  Ideally it would be renamed to reflect its true
;; nature but I'm leaving this for the future, if I ever refactor PEL in major
;; ways.
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
;;; --------------------------------------------------------------------------
;;; Code:
;;

;; -- Perl Editing
;;
(defun pel-cperl-iedit-start ()
  "Activate iedit extension for cperl-mode"
  (when (boundp 'cperl-mode-syntax-table)
    (modify-syntax-entry ?: "." cperl-mode-syntax-table)))

(defun pel-cperl-iedit-end ()
  "Deactivate iedit extension for cperl-mode"
  (when (boundp 'cperl-mode-syntax-table)
    (modify-syntax-entry ?: "_" cperl-mode-syntax-table)))

(defun pel-cperl-iedit-enhance ()
  "Enable automatic activation of iedit extension for Perl editing modes."
  (add-hook 'iedit-mode-hook 'pel-cperl-iedit-start)
  (add-hook 'iedit-mode-hook 'pel-cperl-iedit-end))

;; -- Unix Shell Mode editing
;;
(defun pel-sh-iedit-start ()
  "Activate iedit extension for the sh-mode."
  (when (boundp 'sh-mode-syntax-table)
    (modify-syntax-entry ?/ "." sh-mode-syntax-table)
    (modify-syntax-entry ?: "." sh-mode-syntax-table)
    (modify-syntax-entry ?. "." sh-mode-syntax-table)
    (modify-syntax-entry ?+ "." sh-mode-syntax-table)))

(defun pel-sh-iedit-end ()
  "Deactivate iedit extension for the sh-mode."
  (when (boundp 'sh-mode-syntax-table)
    (modify-syntax-entry ?/ "_" sh-mode-syntax-table)
    (modify-syntax-entry ?: "_" sh-mode-syntax-table)
    (modify-syntax-entry ?. "_" sh-mode-syntax-table)
    (modify-syntax-entry ?+ "_" sh-mode-syntax-table)))

(defun pel-sh-iedit-enhance ()
  "Enable automatic activation of iedit extension for sh-mode."
  (add-hook 'iedit-mode-hook 'pel-sh-iedit-start)
  (add-hook 'iedit-mode-hook 'pel-sh-iedit-end))

;; -- TCL Editing
;;
(defun pel-tcl-iedit-start ()
  "Activate iedit extension for the tcl-mode."
  (when (boundp 'tcl-mode-syntax-table)
    (modify-syntax-entry ?$ "." tcl-mode-syntax-table)))

(defun pel-tcl-iedit-end ()
  "Deactivate iedit extension for the tcl-mode."
  (when (boundp 'tcl-mode-syntax-table)
    (modify-syntax-entry ?$ "_" tcl-mode-syntax-table)))

(defun pel-tcl-iedit-enhance ()
  "Enable automatic activation of iedit extension for sh-mode."
  (add-hook 'iedit-mode-hook 'pel-tcl-iedit-start)
  (add-hook 'iedit-mode-hook 'pel-tcl-iedit-end))

;;; --------------------------------------------------------------------------
(provide 'pel-sh-iedit)

;;; pel-sh-iedit.el ends here
