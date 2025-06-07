;;; pel-elisp-eval.el --- Evaluate Emacs Lisp Code.  -*- lexical-binding: t; -*-

;; Created   : Saturday, June  7 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-06-07 10:43:29 EDT, updated by Pierre Rouleau>

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
;; This file holds Emacs Lisp expansion utility.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'macroexp)                     ; use: `macroexpand-all'
(require 'elisp-mode)                   ; use: `eval-sexp-add-defvars'
;;                                      ;      `elisp--eval-defun-1'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; Credit: Stephen Berman provided the core of the following function to
;; extract the result to copy in the kill-ring.

(defun pel-eval-last-sexp-and-copy ()
  "Evaluate sexp before point; print value in echo area and copy to kill-ring."
  (interactive )
  (let ((result (eval (macroexpand-all
                           (eval-sexp-add-defvars
                            (elisp--eval-defun-1
                             (macroexpand (elisp--preceding-sexp)))))
                      lexical-binding)))
    (message "%s" result)
    (kill-new (format "%s" result))))

;;; --------------------------------------------------------------------------
(provide 'pel-elisp-eval)

;;; pel-elisp-eval.el ends here
