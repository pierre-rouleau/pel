;;; pel-elcode.el --- Emacs Lisp Code Analysis.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, March 17 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-18 00:04:10 EDT, updated by Pierre Rouleau>

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
;; This file defines a the `pel-elcode-print-properties-of-sexp-at-point'
;; command that displays a declare for that identifies whether the sexp at
;; point is pure, side-effect-free and/or error-free.  Use this to improve the
;; declaration of your low-level code to allow the compiler to generate more
;; efficient code.
;;
;; Calling hierarchy:
;;
;;  * `pel-elcode-print-properties-of-sexp-at-point'
;;    - `pel-elcode-properties-of-sexp-at-point'
;;      - `pel-elcode-symbols-of-sexp-at-point'
;;        - `pel-elcode-operators-in'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; `pel-delqs'
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-elcode-operators-in (exp)
  "Recursively extract operator symbols from EXP, ignoring variable names.
Return nil for anything but a list (like numbers, strings or symbols)."
  (let ((symbols '()))
    (cond
     ((and (listp exp) (symbolp (car exp)))
      (let ((head (car exp))
            (body (cdr exp)))
        ;; 1. Add the current function symbol (the head)
        (push head symbols)

        ;; 2. Determine which parts of the body to skip (variable lists)
        (let ((to-process
               (cond
                ;; (defun name (args) body...) -> skip name and (args)
                ;; same for defsubst
                ((memq head '(defun defsubst)) (cddr body))
                ;;
                ;; (let ((var1 (def1 ...)) (var2 (def2 ...)) body...)
                ;;    -> skip var1, var2, process (def1 ...), (def2 ...) and body...
                ((memq head '(let let*))
                 (let* ((bindings (car body))
                        (binding-vals
                         (delq nil (mapcar (lambda (b)
                                             (when (consp b)
                                               (cadr b)))
                                           bindings))))
                   (append binding-vals (cdr body))))
                ;;
                ;; (lambda (args) body...)  -> skip (args), process body...
                ((eq head 'lambda) (cdr body))
                ;;
                ;; Standard call: process everything in the body
                (t body))))

          ;; 3. Recurse into the valid body parts
          (dolist (item to-process)
            (setq symbols
                  (append (reverse (pel-elcode-operators-in item))
                          symbols))))))

     ;; If it's a list but the head isn't a symbol (e.g. ((lambda...) args))
     ((listp exp)
      (dolist (item exp)
        (setq symbols (append (reverse (pel-elcode-operators-in item))
                              symbols)))))

    (reverse                            ; keep original code order
     (seq-filter #'identity             ; remove nil if an empty list is found
                 (delete-dups           ; no duplicates
                  symbols)))))


(defun pel-elcode-symbols-of-sexp-at-point (&optional pos)
  "Return the list of symbols for the defun at point."
  (save-excursion
    (when pos
      (goto-char pos))
    (pel-elcode-operators-in (sexp-at-point))))

;; --

(defconst pel-elcode-non-impacting-operators
  '(and
    or
    if
    when
    unless
    cond
    progn
    prog1
    let
    let*
    while
    dolist
    ;; also ignore current declarations in case code changed
    declare
    pure
    side-effect-free)
  "List of operators that have no impact on purity or side-effect.")

(defun pel-elcode-properties-of-sexp-at-point (&optional pos)
  "Return a property declare form for sexp at POS or at point.
The declare form identifies whether the sexp is pure, side-effect-free and/or
error-free."
  (let ((operators (pel-elcode-symbols-of-sexp-at-point pos)))
    (when operators
      ;; Some flow control/iteration special form/functions have
      ;; no impact on whether the defun is pure or side-effect-free,
      ;; so remove them from the inspected list of operators.
      (setq operators (pel-delqs pel-elcode-non-impacting-operators
                                 operators))
      ;;
      ;; If the first symbol is defun, remove it from the list.
      (when (memq (car-safe operators) '(defun defsubst))
        (setq operators (cdr operators)))
      ;;
      ;; Inspect the remaining operators.
      ;; If one has does not have a property, the defun at point does not
      ;; have that property: so remove it from the defun-props.
      (let ((defun-props '(pure side-effect-free error-free)))
        (catch 'break
          (dolist (op operators)
            (unless (function-get op 'pure)
              (setq defun-props (delq 'pure defun-props)))
            (pcase (function-get op 'side-effect-free)
              ('error-free)
              ('t (setq defun-props (delq 'error-free defun-props)))
              (_  (setq defun-props (pel-delqs '(side-effect-free error-free)
                                               defun-props))))
            ;; Stop once there's no properties left.
            (unless defun-props
              (throw 'break nil))))
        ;; Return the properties that remain for the defun.
        ;; But first reformat it into a proper declare argument.
        (let ((expr ()))
          (if (memq 'error-free defun-props)
              (push '(side-effect-free error-free) expr)
            (when (memq 'side-effect-free defun-props)
              (push '(side-effect-free t) expr)))
          (when (memq 'pure defun-props)
            (push '(pure t) expr))
          (when expr
            (push 'declare expr))
          expr)))))

(defun pel-elcode-print-properties-of-sexp-at-point ()
  "Print whether sexp at point is pure, side-effect-free and/or error-free.
When a pure, side-effect-free or error-free property can be applied to the
sexp the `declare' form is copied in the kill ring for later insertion in code
and also printed in a message.  If no property applied the function just print
a \"nil\" message."
  (interactive)
  (let ((props (pel-elcode-properties-of-sexp-at-point)))
    (when props
      (kill-new (format "%S" props)))
    (message "%S" props)))

;;; --------------------------------------------------------------------------

(provide 'pel-elcode)

;;; pel-elcode.el ends here
