;;; pel--indent.el --- PEL indentation base code.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, November  5 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-11-06 10:31:19 EST, updated by Pierre Rouleau>

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
;; This file defines variables used by indent.el logic.  The variables are
;; also set by the logic in pel_keys.el and pel--keys-macros.el.  These two
;; files cannot request pel-indent.el (to help speed up startup time),
;; therefore they are defined here.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;; None.

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defvar-local pel-indentation-width-control-variables nil
  "Variable(s) used by the current major mode to control indentation width.

Symbol or list of symbols of the variable(s) that are used by the major
mode to control indentation.

If a list is specified, the last symbol is the variable controlling the
indentation, the symbol listed before are values that are stored into
the next one.  These variables are used to set the default.

Set by the major modes that PEL has instrumented.")

(defvar-local pel-indentation-other-control-variables nil
  "List of other indentation control variables used by the major mode.
Identify variables that play a role in the indentation but are not a
width value.")

(defvar-local pel-tab-width-control-variables nil
  "Variable(s) controlling tab width of the current major mode.

This variable can hold the following:
- One symbol,
- A list of symbols,
- A list of (symbol . offset) cons cells.

The symbols are the symbols of variables that must be set to the value
of tab-width.  The offset from that value defaults to 0, but can also be
specified by the offset integer value in cons cells.

The `pel-set-tab-width' command sets each variable identified in this list
to the following value:  (+ tab-width offset)")

;;; --------------------------------------------------------------------------
(provide 'pel--indent)

;;; pel--indent.el ends here
