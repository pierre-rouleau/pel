;;; pel-base-tests.el --- Regression tests for pel--base.el.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, February 16 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-04-27 22:13:07, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021  Pierre Rouleau
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
;; Currently very sparse tests of the pel--base functions.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)
(require 'pel-ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(ert-deftest ert-test-pel-base-action-for ()
  "Test pel-action-for."
  ;; test toggle request
  (should (eq (pel-action-for nil nil) 'activate ))
  (should (eq (pel-action-for nil t)   'deactivate ))
  (should (eq (pel-action-for 0   nil) 'activate ))
  (should (eq (pel-action-for 0   t)   'deactivate ))
  ;; test activate request
  (should (eq (pel-action-for 1 nil)   'activate ))
  (should (eq (pel-action-for 1 t)      nil))
  (should (eq (pel-action-for 10 nil)  'activate ))
  (should (eq (pel-action-for 10 t)      nil))
  ;; test activate with a list - as C-u would cause
  (should (eq (pel-action-for '(4) nil)   'activate ))
  (should (eq (pel-action-for '(4) t)      nil))
  (should (eq (pel-action-for '(16) nil)  'activate ))
  (should (eq (pel-action-for '(16) t)     nil))
  ;; test deactivate request
  (should (eq (pel-action-for -1 nil)   nil ))
  (should (eq (pel-action-for -1 t)     'deactivate))
  (should (eq (pel-action-for -10 nil)  nil))
  (should (eq (pel-action-for -10 t)    'deactivate)))

(ert-deftest ert-test-pel-base-expression ()
  "Test pel-expression-p."
  (should (eq nil (pel-expression-p nil)))
  (should (eq nil (pel-expression-p t)))
  (should (eq nil (pel-expression-p 't)))
  (should (eq nil (pel-expression-p 'nil)))
  (should (eq nil (pel-expression-p 22)))
  (should (eq nil (pel-expression-p 22.3)))
  (should (eq nil (pel-expression-p "22.3")))
  (let ((some-val 3))
    (should (eq nil (pel-expression-p some-val))))
  ;;
  (should (eq t (pel-expression-p 'a-symbol)))
  (should (eq t (pel-expression-p '(+ 2 3))))
  (let ((indirect-symbol 'a-symbol)
        (indirect-expr   '(* 3 4)))
    (should (eq t   (pel-expression-p indirect-symbol)))
    (should (eq t   (pel-expression-p indirect-expr)))))

;;; --------------------------------------------------------------------------
(provide 'pel-base-tests)

;;; pel-base-tests.el ends here
