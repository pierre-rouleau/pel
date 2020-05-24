;;; pel--macros.el --- PEL utility macros -*-lexical-binding: t-*-

;; Copyright (C) 2020  Pierre Rouleau

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

;;; Commentary:
;;

;;; Code:

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

;; -----------------------------------------------------------------------------
;; Symbol Bound Checks
;; -------------------

(defmacro pel-when-fbound (fct &rest body)
  "Eval the BODY if FCT is a bound function, otherwise raise a user error."
  (declare (indent 1))
  `(if (fboundp ,fct)
       (progn
         ,@body)
     (user-error "Function not bound: %s" ,fct)))

(defmacro pel-when-bound (var &rest body)
  "Eval the BODY if VAR is a bound variable, otherwise raise a user error."
  (declare (indent 1))
  `(if (boundp ,var)
       (progn
         ,@body)
     (user-error "Variable not bound: %s" ,var)))

;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
(provide 'pel--macros)

;;; pel--macros.el ends here
