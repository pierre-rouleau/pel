;;; pel--macros.el --- PEL utility macros -*-lexical-binding: t-*-

;; Created   : Monday, March 23 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2024-01-05 20:21:52 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package
;; This file is not part of GNU Emacs.

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

;;;----------------------------------------------------------------------------
;;; Commentary:
;;
;;  Utility macros used in PEL code.
;;
;;  One aspect of PEL is that it integrates external packages that may or may
;;  not be available in user's system.  Also all of PEL's Emacs Lisp code is
;;  using lexical-binding and is byte compiled.  The goal of PEL is to provide
;;  access to a large amount of Emacs Lisp packages while retaining the
;;  ability to start Emacs quickly.
;;
;;  To achieve the above goals PEL files cannot load or require external
;;  packages directly, they must do it lazily.  The code inside pel_keys.el
;;  take advantage of the use-package macro to do that.  The code inside the
;;  other files must require the external packages lazily inside functions and
;;  must also verify if the external functions and variables they use are
;;  bound, otherwise the compiler generates warnings and the code may fail
;;  inside a PEL function.
;;
;;  This file provides a set of macros that simplify writing the needed symbol
;;  bound check code as well as the lazy library require calls.  These are:
;;
;;   - `pel-when-fbound'
;;   - `pel-when-bound'
;;   - `pel-with-required'
;;     - `pel-make-apply-to-elems'
;;
;;  These macros are used to set the state of minor modes:
;;
;;   - `pel-turn-mode-on-when-off'
;;   - `pel-turn-mode-off-when-on'
;;
;;  The following macro append elements to a named list variable.
;;
;;   - `pel-append-to'
;;
;;  And this one appends text to a string variable.
;;
;;   - `pel-concat-to'
;;
;;  The following 2 macros are used to prevent lint warnings:
;;
;;    - `pel-setq'
;;    - `pel-setq-default'
;;
;;  This last macro is used for writing loops that must not run more than a
;;  specified number of times:
;;
;;   - `while-n'

;;; --------------------------------------------------------------------------
;;; Code:


;; Symbol Bound Checks
;; -------------------

(defmacro pel-when-fbound (fct &rest body)
  "Eval the BODY if FCT is a bound function, otherwise raise a user error.
FCT must be a quoted function symbol."
  (declare (debug (symbolp body))
           (indent 1))
  `(if (fboundp ,fct)
       (progn
         ,@body)
     (user-error "Function not bound: %s" ,fct)))

;; --

(defmacro pel-when-bound (var &rest body)
  "Eval the BODY if VAR is a bound variable, otherwise raise a user error.
VAR must be a quoted variable symbol"
  (declare (debug (symbolp body))
           (indent 1))
  `(if (boundp ,var)
       (progn
         ,@body)
     (user-error "Variable not bound: %s" ,var)))

;; --

(defun pel-make-apply-to-elems (func elems &rest args)
  "Return a list of function call to FUNC with each element of ELEMS.
FUNC  := symbol of a function.
ELEMS := quoted list of elements.
ARGS  := quoted list of extra arguments to the functions.
For example:
  (pel-make-apply-to-elems \\='run \\='(a b c))
returns:
  ((run a)
   (run b)
   (run c))

This is used in code building macros."
  (let (lst)
    (dolist (elem elems (nreverse lst))
      (setq lst (cons (cons func (cons elem args)) lst)))))

(defmacro pel-with-required (features functions variables &rest body)
  "Evaluate BODY only if FEATURES, FUNCTIONS and VARIABLES are available.
If something is not available raise an error.  Otherwise evaluate BODY.

FEATURES  := list of feature symbol(s) or nil
FUNCTIONS := list of function symbol(s) or nil
VARIABLES := list of variable symbol(s) or nil

\(fn (FEATURES) (FUNCS) (VARS) BODY... )"
  (declare (debug ((symbolp) (symbolp) (symbolp) body))
           (indent 3))
  (let ((check-features  (pel-make-apply-to-elems 'require features nil
                                                  :noerror))
        (check-functions (pel-make-apply-to-elems 'fboundp functions))
        (check-variables (pel-make-apply-to-elems 'boundp variables)))
  `(progn
     (if (and ,@check-features
              ,@check-functions
              ,@check-variables)
         (progn
           ,@body)
       (error "Failed loading required resources!")))))

;; ---------------------------------------------------------------------------
;; Set mode state
;; --------------

(defmacro pel-turn-mode-on-when-off (mode &optional feature)
  "Turn MODE on, but only if it is currently off.

MODE is the mode symbol unquoted.
FEATURE is an optional feature name symbol, also unquoted.

Check that the mode feature is active before checking if the mode
is currently off.

The macro assumes that the mode command is either already loaded
or auto-loaded.

Check the feature state using FEATURE, if specified, otherwise
check its feature using the MODE symbol.

Most Emacs Lisp features use a feature name that is the same as
the mode name.  In those cases the FEATURE argument is not
required, it is required when they differ."
  (let ((feature-symbol (or feature mode)))
    `(when (or (not (featurep (quote ,feature-symbol)))
               (not ,mode))
       (,mode 1))))


(defmacro pel-turn-mode-off-when-on (mode &optional feature)
  "Turn MODE off, but only if it is currently off.

MODE is the mode symbol unquoted.
FEATURE is an optional feature name symbol, also unquoted.

See `pel-turn-mode-on-when-off' for more info."
  (let ((feature-symbol (or feature mode)))
    `(when (and (featurep (quote ,feature-symbol))
                ,mode)
       (,mode -1))))

;; ---------------------------------------------------------------------------
;; Appending to a list
;; -------------------

(defmacro pel-append-to (listvar elem)
  "Append ELEM to LISTVAR."
  `(setq ,listvar (append ,listvar ,elem)))

;; ---------------------------------------------------------------------------
;; Appending to a string

(defmacro pel-concat-to  (stringvar text)
  "Append TEXT to STRINGVAR."
  `(setq ,stringvar (concat ,stringvar ,text)))

;; ---------------------------------------------------------------------------
;; Warning Preventing Macros
;; -------------------------

(defmacro pel-setq (sym val)
  "Set a symbol SYM to specified value VAL and prevent warning."
  `(progn
     ;; declare the symbol to prevent lint warning
     (defvar ,sym)
     ;; now set the symbol to the specified value
     (setq ,sym ,val)))

(defmacro pel-setq-local (sym val)
  "Set a symbol SYM to specified value VAL in local buffer and prevent warning."
  `(progn
     ;; declare the symbol to prevent lint warning
     (defvar ,sym)
     ;; now set the symbol to the specified value
     (setq-local ,sym ,val)))

(defmacro pel-setq-default (sym val)
  "Set a symbol SYM to specified default value VAL and prevent warning."
  `(progn
     ;; declare the symbol to prevent lint warning
     (defvar ,sym)
     ;; now set the symbol to the specified value
     (setq-default ,sym ,val)))

;; ---------------------------------------------------------------------------
;; Loop Control Macro
;; ------------------

(defmacro while-n (count cond &rest body)
  "Bounded while: execute BODY a maximum of COUNT times, while COND is true."
  (declare (indent 1))
  (let ((tmpvar (make-symbol "i")))
    `(let ((,tmpvar ,count))
       (while (and (> ,tmpvar 0) ,cond)
         ,@body
         (decf ,tmpvar)))))

;;; --------------------------------------------------------------------------
(provide 'pel--macros)

;;; pel--macros.el ends here
