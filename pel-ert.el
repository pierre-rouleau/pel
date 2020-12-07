;;; pel-ert.el --- Emacs Regression Test Utilities.  -*- lexical-binding: t; -*-

;; Created   : Monday, December  7 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2020-12-07 16:42:46, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020  Pierre Rouleau
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
;; This defines a set of comparison and predicate functions meant to be used
;; inside a ERT `ert-deftest' form to help describe the variable environment
;; of a failed test.
;;
;; Background
;; ----------
;;
;; When writing ERT-based test, the code uses the following ert macros to
;; verify the code under test validity:
;; - `should'
;; - `should-not'
;; - `should-error'
;;
;; for instance to verify that (max a b) returns the largest of the 2 values
;; you would write:
;;
;;    (ert-deftest ert-test-max ()
;;      (should (eq (max 2 3) 3)))
;;
;; With the obvious typo in the test, executing ert on the following code
;; would cause a failure:
;;
;;    (ert-deftest ert-test-max ()
;;      (should (eq (min 2 3) 3)))
;;
;; ERT would show:
;;
;;    F ert-test-max
;;        (ert-test-failed
;;         ((should
;;           (eq
;;            (min 2 3)
;;            3))
;;          :form
;;          (eq 2 3)
;;          :value nil))
;;
;;
;; But what if you wanted to execute a more complex test under a large set of
;; conditions like various user-options (variables defined with defcustoms
;; forms) and wanted to avoid having to repeat your test while keeping the
;; ability to quickly identify the invalid conditions?
;;
;; To provide a simple example, lets say we wanted to test the max function
;; with various use cases scenarios for max using the following:
;;
;; (defconst test-scenarios
;; Arguments                   Expected result
;; '(((3 4)                      4)
;;   ((1 2 3 4 5 6)              6)
;;   ((-2 -4 -10 6 3 4)          6)
;;   ((1 2 4.5 6)                60)))  ; <-- error in expected results
;;
;; (ert-deftest ert-test-max ()
;;   (dolist (scenario          test-scenarios)
;;     (let  ((arg              (car scenario))
;;            (expected-result  (cadr scenario)))
;;       (should (eq (apply (function max) arg) expected-result)))))
;;
;;
;; When the test fails the ERT buffer will not show the input arguments, it only
;; shows the following
;;
;; F ert-test-max
;;     (ert-test-failed
;;      ((should
;;        (eq
;;         (apply #'max arg)
;;         expected-result))
;;       :form
;;       (eq 6 60)
;;       :value nil))
;;
;; To provide more information, you can use the pel-eq function instead and
;; pass the scenario as the third argument:
;;
;; (ert-deftest ert-test-max ()
;;   (dolist (scenario          test-scenarios)
;;     (let  ((arg              (car scenario))
;;            (expected-result  (cadr scenario)))
;;       (should (pel-eq (apply (function max) arg) expected-result
;;                       scenario)))))
;;
;; The information available for the failed test in the *ert* buffer now includes
;; the value of the used scenario, providing more information about why the
;; test failed:
;;
;; F ert-test-max
;;     (ert-test-failed
;;      ((should
;;        (pel-eq
;;         (apply #'max arg)
;;         expected-result scenario))
;;       :form
;;       (pel-eq 6 60
;;               ((1 2 4.5 6)
;;                60))
;;       :value nil))
;;
;; You can pass *any* value and and number of values to the functions defined
;; here.  This way you can provide information about a set of global variables
;; that constitute a test jig and print their values in the failure report.
;; These functions are super simple they only accept an extra set of arguments
;; that are all ignored but seen by ert and included in the ert failure report.
;;
;;
;; Provided functions
;; ------------------
;;
;; This file provides the following extended predicates:
;;
;; - Equality predicates:
;;   - `pel-eq'
;;   - `pel-equal'
;;   - `pel-string='
;; - Inequality predicates:
;;   - `pel-neq'
;;   - `pel-nequal'
;;   - `pel-string!='

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'ert)   ; Not required here, but allows requiring only pel-ert in
;;               ; test files.

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-eq (v1 v2 &rest _)
  "Return t when value V1 and V2 are eq, nil otherwise.
Ignore all other arguments that are placed passed for the sole
purpose of showing their values in the ert report when the test fails."
  (eq v1 v2))

(defun pel-equal (v1 v2 &rest _)
  "Return t when value V1 and V2 are equal, nil otherwise.
Ignore all other arguments that are placed passed for the sole
purpose of showing their values in the ert report when the test fails."
  (equal v1 v2))

(defun pel-string= (s1 s2 &rest _)
  "Return t when string S1 and S2 are equal, nil otherwise.
Ignore all other arguments that are placed passed for the sole
purpose of showing their values in the ert report when the test fails."
  (string= s1 s2))

(defun pel-neq (v1 v2 &rest _)
  "Return t when value V1 and V2 are not eq, nil otherwise.
Ignore all other arguments that are placed passed for the sole
purpose of showing their values in the ert report when the test fails."
  (not (eq v1 v2)))

(defun pel-nequal (v1 v2 &rest _)
  "Return t when value V1 and V2 are not equal, nil otherwise.
Ignore all other arguments that are placed passed for the sole
purpose of showing their values in the ert report when the test fails."
  (not (equal v1 v2)))

(defun pel-string!= (s1 s2 &rest _)
  "Return t when string S1 and S2 are not equal, nil otherwise.
Ignore all other arguments that are placed passed for the sole
purpose of showing their values in the ert report when the test fails."
  (not (string= s1 s2)))

;;; --------------------------------------------------------------------------
(provide 'pel-ert)

;;; pel-ert.el ends here
