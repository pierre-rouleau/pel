;;; pel-scheme.el --- PEL support for Scheme and Scheme dialects.  -*- lexical-binding: t; -*-

;; Created   : Saturday, June 26 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-06-26 18:47:58, updated by Pierre Rouleau>

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
;; Utilities to deal with Scheme and Scheme dialects.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel-comint)
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-clear-scheme-repl-buffer ()
  "Erase content of the Scheme REPL running under Emacs."
  (interactive)
  (pel-clear-comint-buffer "*scheme*"))

;;; --------------------------------------------------------------------------
(provide 'pel-scheme)

;;; pel-scheme.el ends here
