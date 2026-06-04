;;; pel-elcode.el --- Emacs Lisp Code Analysis.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, March 17 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-06-04 09:34:51 EDT, updated by Pierre Rouleau>

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
;; command that displays a declare form that identifies whether the sexp at
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
;;  * `pel-elcode-print-properties-of-next-defun-with-some'
;;    + `pel-elcode-properties-of-sexp-at-point'
;;      + ...

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--base)    ; use: `pel-delqs'

(require 'pel-elisp)    ; use: `pel-elisp-beginning-of-previous-form'
;;                             `pel-elisp-beginning-of-next-form'

(require 'seq)          ; use: `seq-filter' (not autoloaded in Emacs 26)
;;                             `seq-partition', `seq-every-p'

;; `pel-elcode-properties-of-sexp' infers declaration properties by
;; querying function symbol properties such as `pure' and
;; `side-effect-free' with `function-get'.  For many built-in functions
;; (`not', `eq', `symbolp', `consp', etc.) these properties are installed
;; by `byte-opt.el'.  A fresh interpreted or `emacs -Q --batch' session
;; does not necessarily load the byte compiler machinery first, so require
;; `byte-opt' explicitly to make the inference environment-independent.
(require 'byte-opt)


;;; --------------------------------------------------------------------------
;;; Code:
;;

;; side-effect-free and pure
;; -------------------------
;;
;; For Emacs Lisp declare semantics, side-effect-free means the byte compiler
;; may discard the call when its return value is unused. It does not imply
;; that two calls can be merged, common-subexpression-eliminated, or replaced
;; by one shared result. That stronger substitution property belongs to pure,
;; not side-effect-free.
;;
;; - side-effect-free enables dead-code elimination (discarding unused calls).
;; - pure enables referential transparency/substitution (constant folding and
;;   common-subexpression elimination).
;;
;; To back this up, we can look at how the Emacs source code itself defines
;; these properties in byte-run.el:
;;
;; - side-effect-free: Defined as "the function is useful only for its return
;;                     value".
;;                     If that value isn't used, the compiler throws the call
;;                     away.
;;
;; - pure:             Defined as "the function choice depends only on its
;;                     arguments, and has no side effects".
;;                     This is what signals to the compiler that the function
;;                     will always yield the exact same value for identical
;;                     inputs, allowing it to safely merge or pre-evaluate the
;;                     calls.

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
  "List of operators that have no impact on purity or side effects.")

(defconst pel-elcode-potentially-signaling-operators
  '(format format-message)
  "Operators that are side-effect-free but can signal for invalid arguments.

`format' and `format-message' can signal when the supplied arguments do
not match the conversion specifications in the format string.  Since
`pel-elcode' does not perform type inference, calls to these operators
must not preserve the `error-free' declaration property.")

(defconst pel-elcode-allocating-operators
  '(;; Hash tables
    make-hash-table copy-hash-table
                    ;; Sequences / vectors / lists
    make-vector make-list make-bool-vector make-char-table
    cons list append reverse copy-sequence copy-tree copy-alist
    vector vconcat
    ;; Strings
    make-string concat format format-message propertize
    ;; Symbols
    make-symbol gensym
    ;; Records / byte-code objects
    make-record record make-byte-code
    ;; Keymaps
    make-keymap make-sparse-keymap)
  "Functions that allocate fresh objects.

Allocation does not make a function side-effecting under Emacs
`declare' semantics: a function that calls `list', `cons', `vector',
`concat', etc. may still be `side-effect-free' because it does not mutate
pre-existing state.

However, allocation prevents `pure' inference because repeated calls may
return fresh, non-`eq' objects.  For example, two calls to a function that
returns `(list 1 2 3)' produce equal but not `eq' results.")

(defconst pel-elcode-structural-forms
  '(defun defsubst lambda
     dolist dotimes
     declare
     ;; non macros
     let let*
     quote function)
  "List of structural forms.  First 6 are macros that must not be expanded.")

(defun pel-elcode--args-in (arglist)
  "Return the plain variable symbols from a lambda/defun ARGLIST.
Strips lambda-list keywords:
`&optional', `&rest', `&key', `&allow-other-keys', `&aux'."
  (seq-filter (lambda (s)
                (and (symbolp s)
                     (not (memq s '(&optional &rest &key
                                              &allow-other-keys
                                              &aux)))))
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
  `(dolist (item ,body)
     (pel-elcode--add-ops-from-list item ,var ,new-local-vars)))

(defun pel-elcode--variable-read-symbol-p (symbol local-vars)
  "Return non-nil if SYMBOL denotes a non-local variable read.

Ignore locally bound variables, nil, t and keywords."
  (declare (pure t) (side-effect-free t))
  (and (symbolp symbol)
       (not (memq symbol local-vars))
       (not (memq symbol '(nil t)))
       (not (keywordp symbol))))

(defmacro pel-elcode--add-vars-from-list (items var new-local-vars)
  "Add variable reads present in ITEMS list to VAR.
Take NEW-LOCAL-VARS local variables into account."
  `(setq ,var
         (append (reverse (pel-elcode-variables-read-in
                           ,items ,new-local-vars))
                 ,var)))

(defmacro pel-elcode--add-vars-from-lists (body var new-local-vars)
  "Add variable reads present in BODY list to VAR.
Take NEW-LOCAL-VARS local variables into account."
  `(dolist (item ,body)
     (pel-elcode--add-vars-from-list item ,var ,new-local-vars)))

(defun pel-elcode-variables-read-in (exp &optional local-vars)
  "Return non-local variable symbols read by EXP.

LOCAL-VARS is a list of symbols locally bound in the current lexical
scope.  Variable symbols found in quoted forms, function forms,
lambda/defun arglists, binding positions and `setq' target positions
are ignored.

The returned list preserves first-read order and contains no duplicates."
  (let ((symbols ()))
    (cond

     ;; A plain symbol in value position is a variable read unless it is local
     ;; or self-evaluating.
     ((pel-elcode--variable-read-symbol-p exp local-vars)
      (push exp symbols))

     ((and (consp exp) (symbolp (car exp)))
      (let ((head (car exp))
            (body (cdr exp)))

        ;; Keep macro handling consistent with `pel-elcode-operators-in'.
        (when (and (macrop head)
                   (not (memq head pel-elcode-structural-forms))
                   (not (memq head pel-elcode-non-impacting-operators)))
          (let ((expanded (macroexpand-1 exp)))
            (unless (equal expanded exp)
              (setq exp expanded
                    head (car-safe exp)
                    body (cdr-safe exp)))))

        (cond

         ;; (defun NAME (ARGS) [DOCSTRING] BODY...)
         ;; Skip NAME and ARGS.  BODY sees ARGS as locals.
         ((memq head '(defun defsubst))
          (let ((locvars (append (pel-elcode--args-in (cadr body))
                                 local-vars)))
            (pel-elcode--add-vars-from-lists (cddr body) symbols locvars)))

         ;; (lambda (ARGS) BODY...)
         ;; Skip ARGS.  BODY sees ARGS as locals.
         ((eq head 'lambda)
          (let ((locvars (append (pel-elcode--args-in (car body))
                                 local-vars)))
            (pel-elcode--add-vars-from-lists (cdr body) symbols locvars)))

         ;; (let ((VAR VAL) ...) BODY...)
         ;; VAL forms see the outer scope.  BODY sees all VARs as locals.
         ((eq head 'let)
          (let* ((bindings (car body))
                 (vars     (delq nil
                                 (mapcar (lambda (b)
                                           (cond
                                            ((symbolp b) b)
                                            ((symbolp (car-safe b)) (car b))))
                                         bindings)))
                 (vals     (delq nil
                                 (mapcar (lambda (b)
                                           (when (consp b) (cadr b)))
                                         bindings)))
                 (locals   (append vars local-vars)))
            (pel-elcode--add-vars-from-lists vals symbols local-vars)
            (pel-elcode--add-vars-from-lists (cdr body) symbols locals)))

         ;; (let* ((VAR VAL) ...) BODY...)
         ;; Each VAL sees the variables bound by previous bindings.
         ((eq head 'let*)
          (let ((running-locals local-vars))
            (dolist (binding (car body))
              (if (consp binding)
                  (progn
                    (setq symbols
                          (append (reverse (pel-elcode-variables-read-in
                                            (cadr binding) running-locals))
                                  symbols))
                    (when (symbolp (car binding))
                      (push (car binding) running-locals)))
                (when (symbolp binding)
                  (push binding running-locals))))
            (pel-elcode--add-vars-from-lists (cdr body)
                                             symbols
                                             running-locals)))

         ;; (dolist (VAR LIST [RESULT]) BODY...)
         ;; LIST sees the outer scope.  RESULT and BODY see VAR as local.
         ((eq head 'dolist)
          (let* ((var-spec    (car body))
                 (var         (car var-spec))
                 (list-form   (cadr var-spec))
                 (result-form (cddr var-spec))
                 (body-forms  (cdr body))
                 (locals      (if (symbolp var)
                                  (cons var local-vars)
                                local-vars)))
            (pel-elcode--add-vars-from-list list-form symbols local-vars)
            (pel-elcode--add-vars-from-lists result-form symbols locals)
            (pel-elcode--add-vars-from-lists body-forms symbols locals)))

         ;; (dotimes (VAR COUNT [RESULT]) BODY...)
         ;; COUNT sees the outer scope.  RESULT and BODY see VAR as local.
         ((eq head 'dotimes)
          (let* ((var-spec    (car body))
                 (var         (car var-spec))
                 (count-form  (cadr var-spec))
                 (result-form (cddr var-spec))
                 (body-forms  (cdr body))
                 (locals      (if (symbolp var)
                                  (cons var local-vars)
                                local-vars)))
            (pel-elcode--add-vars-from-list count-form symbols local-vars)
            (pel-elcode--add-vars-from-lists result-form symbols locals)
            (pel-elcode--add-vars-from-lists body-forms symbols locals)))

         ;; (setq VAR1 VAL1 VAR2 VAL2 ...)
         ;; Target variables are writes, not reads.  Only VAL forms are reads.
         ((eq head 'setq)
          (let ((val-forms (mapcar #'cadr (seq-partition body 2))))
            (pel-elcode--add-vars-from-lists val-forms symbols local-vars)))

         ;; (cond (TEST BODY...) ...)
         ;; Each clause is a list of expressions, not a function call.
         ((eq head 'cond)
          (dolist (clause body)
            (pel-elcode--add-vars-from-lists clause symbols local-vars)))

         ;; (quote X) / (function X) / (declare ...) contain no variable reads
         ;; for the purpose of this analysis.
         ((memq head '(quote function declare)))

         ;; Standard function/macro call: only arguments are value positions.
         (t
          (pel-elcode--add-vars-from-lists body symbols local-vars)))))

     ;; If the head is not a symbol, every element is in value position.
     ((consp exp)
      (pel-elcode--add-vars-from-lists exp symbols local-vars)))

    (reverse
     (seq-filter #'identity
                 (delete-dups symbols)))))




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
operators are found."
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
        (unless (or (eq head 'declare)
                    ;; skip non-symbol heads due to bad macro expansion
                    (not (symbolp head)))
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
              (if (consp binding)
                  (progn
                    (setq symbols
                          (append (reverse (pel-elcode-operators-in
                                            (cadr binding) running-locals))
                                  symbols))
                    (push (car binding) running-locals))
                ;; bare symbol binding (that has no explicit initial value)
                (push binding running-locals)))
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
  (let ((operators (pel-elcode-operators-in sexp))
        (vars-read (pel-elcode-variables-read-in sexp)))
    (when (or operators vars-read)
      ;; Some flow control/iteration special form/functions have
      ;; no impact on whether the defun is pure or side-effect-free,
      ;; so remove them from the inspected list of operators.
      (setq operators (pel-delqs pel-elcode-non-impacting-operators
                                 operators))
      ;;
      ;; If the first symbol is defun or defsubst, remove it from the list.
      (when (memq (car-safe operators) '(defun defsubst))
        (setq operators (cdr operators)))
      ;;
      ;; Inspect the remaining operators.
      ;; If one does not have a property, the defun at point does not
      ;; have that property: so remove it from the defun-props.
      (let ((defun-props (list 'pure 'side-effect-free 'error-free)))

        ;; Reading a non-local variable makes the result depend on external
        ;; state.  That includes user-options (`defcustom' variables), globals
        ;; and dynamically scoped variables.  Such reads do not necessarily
        ;; mutate state, so they do not by themselves remove `side-effect-free',
        ;; but they must remove `pure'.
        (when vars-read
          (setq defun-props (delq 'pure defun-props))
          ;; A read of an unbound non-local variable can signal void-variable.
          (unless (seq-every-p #'boundp vars-read)
            (setq defun-props (delq 'error-free defun-props))))

        (catch 'pel-elcode-break
          (dolist (op operators)
            (unless (function-get op 'pure)
              (setq defun-props (delq 'pure defun-props)))
            ;;
            ;; Allocation functions may still be side-effect-free, but they
            ;; are not pure: repeated calls may return fresh, non-`eq' objects.
            (when (memq op pel-elcode-allocating-operators)
              (setq defun-props (delq 'pure defun-props)))
            ;;
            ;; Some operators are side-effect-free but can still signal depending
            ;; on their arguments.  Without type/format-string proof, do not infer
            ;; `error-free' for forms that call them.
            (when (memq op pel-elcode-potentially-signaling-operators)
              (setq defun-props (delq 'error-free defun-props)))
            ;;
            (pcase (function-get op 'side-effect-free)
              ('error-free)
              ('t (setq defun-props (delq 'error-free defun-props)))
              (_
               ;; `function-get' returned nil or an unrecognised value.  Two cases:
               ;;
               ;;  1. The property IS on the symbol plist but is not `t' or
               ;;     `error-free' (the operator is *known* to have side effects):
               ;;     remove both `side-effect-free' and `error-free'.
               ;;
               ;;  2. The property is simply ABSENT from the plist (e.g. a
               ;;     user-defined function/macro not yet loaded in this session.
               ;;     This is common in `emacs -Q --batch' linting runs).
               ;;     - One way to handle this is to warn about unknown
               ;;     operators to get the user to ensure that the macro file.
               ;; [:todo 2026-06-04, by Pierre Rouleau: do the above?]
               (setq defun-props (pel-delqs '(side-effect-free error-free)
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

;;-pel-autoload
(defun pel-elcode-print-properties-of-sexp-at-point ()
  "Print whether defun at point is pure, side-effect-free and/or error-free.

When a pure, side-effect-free or error-free property can be applied to the
defun the `declare' form is copied in the kill ring for later insertion in code
and also printed in a message.  If no property applies the function prints
no message."
  (interactive)
  (save-excursion
    (let ((original-pos (point))
          defun-start-pos defun-end-pos)
      ;; move to indentation otherwise next block of code will move to
      ;; previous form.
      (back-to-indentation)
      (unless (looking-at "(defun ")
        (unless (pel-elisp-beginning-of-previous-form 1 'defun-forms
                                                      :silent :dont-push-mark)
          (user-error "Point is not inside a defun form!")))

      (setq defun-start-pos (point)
            defun-end-pos   (ignore-errors (scan-sexps defun-start-pos 1)))
      (if (and defun-end-pos
               (<= defun-start-pos original-pos)
               (< original-pos defun-end-pos))
          (let ((props (pel-elcode-properties-of-sexp-at-point)))
            (when props
              (kill-new (format "%S" props))
              (message "%S" props)))
        (user-error "Point is not inside a defun form!")))))

;; --
;;-pel-autoload
(defun pel-elcode-print-properties-of-next-defun-with-some ()
  "Move point to beginning of the next defun with properties; print them.
Note that it skips the defsubst forms.
Also store the property form in the kill ring."
  (interactive)
  (let ((one-done nil)
        (original-pos (point))
        found)
    (while (and (not found)
                (not (eobp)))
      (if (pel-elisp-beginning-of-next-form 1 'defun-forms
                                            :silent :dont-push-mark)
          ;; Found a form
          (let ((props (pel-elcode-properties-of-sexp-at-point)))
            (when props
              (setq one-done t)
              (kill-new (format "%S" props))
              (message "%S" props)
              (setq found t)))
        ;; no defun found; stop looping
        (setq found t)))
    (unless one-done
      (message "No defun with applicable properties found below")
      (goto-char original-pos))))

;;; --------------------------------------------------------------------------

(provide 'pel-elcode)

;;; pel-elcode.el ends here
