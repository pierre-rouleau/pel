;;; pel-elcode-test.el --- Test pel-elcode.el.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, March 17 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-18 09:24:46 EDT, updated by Pierre Rouleau>

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
(require 'pel-elcode)
(require 'pel-ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(ert-deftest ert-test-pel-symbols-in-code ()
  "Test `pel-symbols-in-code'."

  ;; If sexp is empty nothing gets returned
  (should-not (pel-elcode-operators-in '()))

  ;; A simple defun
  (should (equal
           (pel-elcode-operators-in
            '(defun div-by-2 (val) "A division by 2" (/ val 2)))
           '(defun /)))

  (should (equal
           (pel-elcode-operators-in
            '(defun my-func (x) (message "value: %s" (+ x 1))))
           '(defun message +)))


  (should (equal
           (pel-elcode-operators-in
            '(defun pel-inside-code (&optional pos)
               "Return non-nil when point or POS is in code, nil if in comment or string.
Note that this changes the search match data!"
               (let ((syntax (syntax-ppss (or pos (point)))))
                 (and (not (nth 3 syntax))
                      (not (nth 4 syntax))))))
           '(defun let syntax-ppss or point and not nth)))

  (should (equal
           (pel-elcode-operators-in
            '(defun pel-inside-code (&optional pos)
               "Return non-nil when point or POS is in code, nil if in comment or string.
Note that this changes the search match data!"
               (let ((syntax (syntax-ppss (or pos (point))))
                     (just-for-test (* (+ (/ 33 22) 9) pos)))
                 (and (not (nth 3 syntax))
                      (not (nth 4 syntax))))))
           '(defun let syntax-ppss or point * + / and not nth)))


  ;; Test that the declare, pure and side-effect-free are not
  ;; included in the list of symbols extracted: they should not be
  ;; extracted.
  (should (equal
           (pel-elcode-operators-in
            '(defun pel-expression-p (val)
               "Return non-nil if VAL is an expression, nil if it is a value.
Return nil for t and nil.
Return t for \\='some-symbols or \\='(some expressions), nothing else.
Meant to be used to identify code that is quoted (for delayed
code execution)."
               (declare (pure t) (side-effect-free error-free))
               (and (not (eq val t))
                    (not (eq val nil))
                    (or (symbolp val)
                        (consp val)))))
           '(defun and not eq or symbolp consp))))


(ert-deftest ert-test-pel-elcode-properties-of-sexp ()
  "Test `pel-elcode-properties-of-sexp'."

  (should-not (pel-elcode-properties-of-sexp
               '(defun pel-inside-code (&optional pos)
                  "Return non-nil when point or POS is in code, nil if in comment or string.
Note that this changes the search match data!"
                  (let ((syntax (syntax-ppss (or pos (point)))))
                    (and (not (nth 3 syntax))
                         (not (nth 4 syntax)))))))

  (should (equal
           (pel-elcode-properties-of-sexp
            '(defun pel-user-option-p (symbol)
               "Return t when SYMBOL is a valid PEL user-option, nil otherwise."
               (declare (side-effect-free t))
               (and (custom-variable-p symbol)
                    (eq t (compare-strings "pel-use-" nil nil
                                           (symbol-name symbol) 0 8)))))
           '(declare (side-effect-free t))))

  ;; (should (equal
  ;;            (pel-elcode-properties-of-sexp
  ;;             '(defun pel-expression-p (val)
  ;;                "Return non-nil if VAL is an expression, nil if it is a value.
  ;; Return nil for t and nil.
  ;; Return t for \\='some-symbols or \\='(some expressions), nothing else.
  ;; Meant to be used to identify code that is quoted (for delayed
  ;; code execution)."
  ;;                (declare (pure t) (side-effect-free error-free))
  ;;                (and (not (eq val t))
  ;;                     (not (eq val nil))
  ;;                     (or (symbolp val)
  ;;                         (consp val)))))
  ;;            '(declare (pure t) (side-effect-free error-free))))


  )

;;; --------------------------------------------------------------------------
(provide 'pel-elcode-test)

;;; pel-elcode-test.el ends here
