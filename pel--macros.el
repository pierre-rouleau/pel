;;; pel--macros.el --- PEL utility macros -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2021  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

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
;;  using lexical-binding and is byte compiled. The goal of PEL is to provide
;;  access to a large amount of Emacs Lisp packages while retaining the
;;  ability to start Emacs quickly.
;;
;;  To achieve the above goals PEL files cannot load or require external
;;  packages directly, they must do it lazily. The code inside pel_keys.el
;;  take advantage of the use-package macro to do that.  The code inside the
;;  other files must require the external packages lazily inside functions and
;;  must also verify if the external functions and variables they use are
;;  bound, otherwise the compiler generates warnings and the code may fail
;;  inside a PEL function.
;;
;;  This file provides a set of macros that simplify writing the needed symbol
;;  bound check code as well as the lazy library require calls. These are:
;;
;;   - `pel-when-fbound'
;;   - `pel-when-bound'
;;   - `pel-with-required'
;;     - `pel-make-apply-to-elems'
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
  "Eval the BODY if FCT is a bound function, otherwise raise a user error."
  (declare (indent 1))
  `(if (fboundp ,fct)
       (progn
         ,@body)
     (user-error "Function not bound: %s" ,fct)))

;; --

(defmacro pel-when-bound (var &rest body)
  "Eval the BODY if VAR is a bound variable, otherwise raise a user error."
  (declare (indent 1))
  `(if (boundp ,var)
       (progn
         ,@body)
     (user-error "Variable not bound: %s" ,var)))

;; --

(defun pel-make-apply-to-elems (func elems &rest args)
  "Return a list of calls to FUNC with each element of ELEMS.
FUNC  := symbol of a function.
ELEMS := quoted list of elements
For example:
  (pel-make-apply-to-elems 'run '(a b c))
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
If something is not available raise an error. Otherwise evaluate BODY.

FEATURES  := list of feature symbol(s) or nil
FUNCTIONS := list of function symbol(s) or nil
VARIABLES := list of variable symbol(s) or nil

\(fn (FEATURES) (FUNCS) (VARS) BODY... )"
  (declare (indent 3))
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

;;----------------------------------------------------------------------------
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
