;;; pel-fypp.el --- Fortran Python Pre-Processor Support  -*- lexical-binding: t; -*-

;; Created   : Friday, May  8 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-05-08 08:54:54 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2026  Pierre Rouleau
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
;; A minimal implementation of fypp major-mode, a major mode for the Fortran
;; Python Pre-Processor.  It essentially just adds syntax highlighting for
;; fypp expressions.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'f90)
;;; --------------------------------------------------------------------------
;;; Code:
;;

(define-derived-mode fypp-mode f90-mode "Fypp"
  "Major mode for editing Fypp-preprocessed Fortran code."
  (font-lock-add-keywords
   nil
   '(
     ;; Highlights #:if, #:for, etc.
     ("#:[a-z]+" . font-lock-preprocessor-face)

     ;; Enclosed Python expressions
     ;; - Highlights ${expr}$
     ("\\${.*?}\\$"   . font-lock-keyword-face)
     ;;- Highlights line-form $: eval directives
     ("^\\s-*\\$:.*$" . font-lock-keyword-face) )))

;;; -------------------------------------------------------------------------
(provide 'pel-fypp)

;;; pel-fypp.el ends here
