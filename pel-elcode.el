;;; pel-elcode.el --- Emacs Lisp Code Analysis.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, March 17 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-20 12:09:46 EDT, updated by Pierre Rouleau>

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
;; This file defines the `pel-elcode-print-properties-of-sexp-at-point'
;; command that displays a declare for that identifies whether the sexp at
;; point is pure, side-effect-free and/or error-free.  Use this to improve the
;; declaration of your low-level code to allow the compiler to generate more
;; efficient code.
;;
;; Calling hierarchy:
;;
;;  * `pel-elcode-print-properties-of-sexp-at-point'
;;    - `pel-elcode-properties-of-sexp-at-point'
;;      - `pel-elcode-properties-of-sexp'
;;        - `pel-elcode-operators-in'
;;          - `pel-elcode--args-in'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)    ; use: `pel-delqs'
(require 'seq)          ; use: `seq-filter' (not autoloaded in Emacs 26)
;;                             `seq-partition', `seq-every-p'

;;; --------------------------------------------------------------------------
;;; Code:
;;

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
    dotimes
    ;; The following special forms operators have no impact
    quote
    function)
  "List of operators that have no impact on purity or side-effect.")

(defconst pel-elcode-structural-forms
  '(defun defsubst lambda
     dolist dotimes
     declare
     ;; non macros
     let let*
     quote function
     setq)
  "List of structural forms.  First 6 are macros that must not be expanded.")

(defun pel-elcode--args-in (arglist)
  "Return the plain variable symbols from a lambda/defun ARGLIST.
Strips lambda-list keywords:
`&optional', `&rest', `&key', `&allow-other-keys'."
  (seq-filter (lambda (s)
                (and (symbolp s)
                     (not (memq s '(&optional &rest &key
                                              &allow-other-keys)))))
              arglist))

(defmacro pel-elcode--add-ops-from-list (items var new-local-vars)
  "Add operators present in ITEMS list to the VAR accumulator list.
Take NEW-LOCAL-VARS local variables into account."
  `(setq ,var
         (append (reverse (pel-elcode-operators-in ,items ,new-local-vars))
                 ,var)))

(defmacro pel-elcode--add-ops-from-lists (body var new-local-vars)
  "Add operators present in BODY list to the VAR accumulator list.
Take NEW-LOCAL-VARS local variables into account."
  `(dolist (item (,@body))
     (pel-elcode--add-ops-from-list item ,var ,new-local-vars)))

(defun pel-elcode-operators-in (exp &optional local-vars)
  "Recursively extract operator symbols from EXP, ignoring variable names.

LOCAL-VARS is a list of symbols that are locally bound in the current
lexical scope.  A `setq' form whose every target variable is in
LOCAL-VARS is treated as non-impacting: the `setq' symbol is excluded
from the result and only its value sub-expressions are recursed into.

Unknown macro forms, not listed in `pel-elcode-structural-forms' and
`pel-elcode-non-impacting-operators', are expanded once with
`macroexpand-1' before analysis, so the real operators they hide are
made visible.

Return a list of operator symbols found in EXP in the order of their
first appearance, with all duplicates removed.  Return nil if no
operator are found."
  (let ((symbols ()))
    (cond
     ((and (listp exp) (symbolp (car exp)))
      (let ((head (car exp))
            (body (cdr exp)))

        ;; -- Macro expansion for unknown macros
        ;; -------------------------------------------------------------------
        ;; Structural forms and operators in
        ;; `pel-elcode-non-impacting-operators' are handled explicitly below
        ;; and must NOT be expanded: expanding e.g. `dolist' would expose
        ;; internal implementation operators (`car', `cdr', a second `setq')
        ;; that falsely degrade purity.
        (when (and (macrop head)
                   (not (memq head pel-elcode-structural-forms))
                   (not (memq head pel-elcode-non-impacting-operators)))
          (let ((expanded (macroexpand-1 exp)))
            (unless (equal expanded exp)
              (setq exp expanded
                    head (car-safe exp)
                    body (cdr-safe exp)))))

        ;; -- Push operator `head' unless it is declare form -----------------
        (unless (eq head 'declare)
          (push head symbols))

        ;; -- Structural dispatch --------------------------------------------
        ;; Extract operators from each type of form; add them to symbols in
        ;; order of appearance.
        (cond

         ;; (defun    NAME (ARGS) [DOCSTRING] BODY...)
         ;; (defsubst NAME (ARGS) [DOCSTRING] BODY...)
         ;; [-head--][------------ body -------------]
         ;;   Skip NAME; add ARGS to local-vars for BODY.
         ((memq head '(defun defsubst))
          (let ((locvars (append (pel-elcode--args-in (cadr body))
                                 local-vars)))
            (pel-elcode--add-ops-from-lists (cddr body) symbols locvars)))

         ;; (lambda (ARGS) BODY...)
         ;;   Skip ARGS; add them to local-vars for BODY.
         ((eq head 'lambda)
          (let ((locvars (append (pel-elcode--args-in (car body))
                                 local-vars)))
            (pel-elcode--add-ops-from-lists (cdr body) symbols locvars)))

         ;; (let ((VAR VAL) ...) BODY...)
         ;;      [--------- body -------]
         ;;   Parallel binding: VAL forms see current local-vars;
         ;;   BODY sees current local-vars + all VARs.
         ((eq head 'let)
          (let* ((bindings  (car body))
                 (vars      (delq nil (mapcar (lambda (b)
                                                (if (consp b) (car b) b))
                                              bindings)))
                 (vals      (delq nil (mapcar (lambda (b)
                                                (when (consp b) (cadr b)))
                                              bindings)))
                 (locals (append vars local-vars)))
            (pel-elcode--add-ops-from-lists vals       symbols local-vars)
            (pel-elcode--add-ops-from-lists (cdr body) symbols locals)))

         ;; (let* ((VAR VAL) ...) BODY...)
         ;;       [------- body ---------]
         ;;   Sequential binding: each VAR is in scope for subsequent VALs
         ;;   and for BODY.
         ((eq head 'let*)
          (let ((running-locals local-vars))
            (dolist (binding (car body))
              (when (consp binding)
                (setq symbols
                      (append (reverse (pel-elcode-operators-in
                                        (cadr binding) running-locals))
                              symbols))
                (push (car binding) running-locals)))
            (pel-elcode--add-ops-from-lists (cdr body) symbols running-locals)))

         ;; (dolist (VAR LIST [RESULT]) BODY...)
         ;; [-head-][---------- body ----------]
         ;;   VAR is local inside BODY (and RESULT, if present).
         ;;   LIST is evaluated in the outer scope.
         ((eq head 'dolist)
          (let* ((var-spec   (car body)) ; (VAR LIST [RESULT])
                 (var        (car var-spec))
                 (list-form  (cadr var-spec))
                 (result-form (cddr var-spec)) ; nil or (RESULT)
                 (body-forms (cdr body))
                 (locals  (cons var local-vars)))
            ;; LIST is in the outer scope
            (pel-elcode--add-ops-from-list list-form symbols local-vars)
            ;; RESULT and BODY see VAR as local
            (pel-elcode--add-ops-from-lists result-form symbols locals)
            (pel-elcode--add-ops-from-lists body-forms  symbols locals)))

         ;; (dotimes (VAR COUNT [RESULT]) BODY...)
         ;; [-head-] [----------- body ----------]
         ;;   VAR is local inside BODY (and RESULT, if present).
         ;;   COUNT is evaluated in the outer scope.
         ((eq head 'dotimes)
          (let* ((var-spec    (car body))
                 (var         (car var-spec))
                 (count-form  (cadr var-spec))
                 (result-form (cddr var-spec))
                 (body-forms  (cdr body))
                 (locals   (cons var local-vars)))
            (pel-elcode--add-ops-from-list count-form symbols local-vars)
            (pel-elcode--add-ops-from-lists result-form symbols locals)
            (pel-elcode--add-ops-from-lists body-forms symbols locals)))

         ;; (setq VAR1 VAL1 VAR2 VAL2 ...)
         ;;   If EVERY target variable is locally bound, setq is non-impacting:
         ;;   remove the `setq' symbol already pushed and skip it.
         ;;   In either case, recurse into the value sub-expressions.
         ((eq head 'setq)
          (let* ((pairs       (seq-partition body 2))
                 (target-vars (mapcar #'car  pairs))
                 (val-forms   (mapcar #'cadr pairs)))
            ;; remove the `setq' just pushed as head if that setq only sets
            ;; the value of local variables.
            (when (seq-every-p (lambda (v) (memq v local-vars))
                               target-vars)
              (pop symbols))
            ;;
            (pel-elcode--add-ops-from-lists val-forms symbols local-vars)))

         ;; (quote X) / (function X)  → nothing to recurse into
         ((memq head '(quote function)))

         ;; (declare ...) → already excluded from symbols above; skip body
         ((eq head 'declare))

         ;; Standard function/macro call: recurse into all arguments
         (t
          (pel-elcode--add-ops-from-lists body symbols local-vars)))))

     ;; If it's a list but the head isn't a symbol (e.g. ((lambda...) args))
     ((listp exp)
      (pel-elcode--add-ops-from-lists exp symbols local-vars)))

    (reverse                            ; keep original code order
     (seq-filter #'identity             ; remove nil if an empty list is found
                 (delete-dups           ; no duplicates
                  symbols)))))

(defun pel-elcode-operators-in-sexp-at-point (&optional pos)
  "Return operators in the SEXP at POS or at point."
  (save-excursion
    (when pos
      (goto-char pos))
    (pel-elcode-operators-in (sexp-at-point))))

;; --



(defun pel-elcode-properties-of-sexp (sexp)
   "Return a property declare form for specified SEXP.
The declare form identifies whether the sexp is pure, side-effect-free and/or
error-free."
  (let ((operators (pel-elcode-operators-in sexp)))
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
      ;; If one does not have a property, the defun at point does not
      ;; have that property: so remove it from the defun-props.
      (let ((defun-props (list 'pure 'side-effect-free 'error-free)))
        (catch 'pel-elcode-break
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
              (throw 'pel-elcode-break nil))))
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

(defun pel-elcode-properties-of-sexp-at-point (&optional pos)
  "Return a property declare form for sexp at POS or at point.
The declare form identifies whether the sexp is pure, side-effect-free and/or
error-free."
  (save-excursion
    (when pos
      (goto-char pos))
    (pel-elcode-properties-of-sexp (sexp-at-point))))

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
