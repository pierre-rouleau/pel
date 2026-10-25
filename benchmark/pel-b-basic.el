;;; pel-b-basic.el --- Benchmark basic low-level function candidate implementations  -*- lexical-binding: t; -*-

;; Created   : Sunday, October 25 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-10-25 12:19:47 EDT, updated by Pierre Rouleau>

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
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)
(require 'pel-benchmark-macros)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-b-major-modes-for ()
  "Benchmark different implementations of the `pel-major-modes-for'."

  (pel-benchmark-compare 2000000
  "Native Concat"      (concat "c++-ts" "-mode")
  "Template Format"    (format "%s-mode" "c++-ts")
  "Substring Slicing"  (substring "c++-ts-mode" 0 -5)))


(defun pel-mode-name-for-2 (mode-symbol)
  "Return the base mode name of MODE-SYMBOL as a string.

Remove the trailing \"-mode\" or \"-ts-mode\" suffix.
Signal an error if MODE-SYMBOL has neither suffix.

Examples:
 (pel-mode-name-for \\='c-mode)       ; => \"c\"
 (pel-mode-name-for \\='c-ts-mode)    ; => \"c\"
 (pel-mode-name-for \\='python-mode)  ; => \"python\""
  (let* ((name-str (symbol-name mode-symbol))
         (case-fold-search nil)
         (suffix-start
          (string-match-p "\\(?:-ts\\)?-mode\\'" name-str)))
    (unless suffix-start
      (error "Mode symbol lacks a -mode suffix: %S" mode-symbol))
    (substring name-str 0 suffix-start)))

(defun pel-b-mode-name-for ()
  "Benchmark different implementations of the `pel-major-modes-for'."

  (let ((count 100000))
    (princ  (pel-benchmark-compare-auto count
              (pel-mode-name-for 'c-mode)
              (pel-mode-name-for-2 'c-mode)))
    (princ "\n")
    (princ (pel-benchmark-compare-auto count
             (pel-mode-name-for 'c-ts-mode)
             (pel-mode-name-for-2 'c-ts-mode)))
    (princ "\n")
    (princ (pel-benchmark-compare-auto count
             (pel-mode-name-for 'c++-mode)
             (pel-mode-name-for-2 'c++-mode)))
    (princ "\n")
    (princ (pel-benchmark-compare-auto count
             (pel-mode-name-for 'c++-ts-mode)
             (pel-mode-name-for-2 'c++-ts-mode))))
  nil)

;;; --------------------------------------------------------------------------
(provide 'pel-b-basic)

;;; pel-b-basic.el ends here
