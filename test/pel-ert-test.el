;;; pel-ert-test.el --- ERT tests for pel-ert.el.  -*- lexical-binding: t; -*-

;; Created   : Friday, March 21 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-21 18:23:15 EDT, updated by Pierre Rouleau>

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
;; Regression tests for the ERT utility predicates provided by pel-ert.el.
;; Each function accepts two primary values and any number of extra arguments
;; that are silently ignored; the ignored arguments are shown by ERT in its
;; failure report, which is their entire purpose.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; ---------------------------------------------------------------------------
;; pel-eq

(ert-deftest ert-test-pel-eq ()
  "Test `pel-eq'."
  ;; Symbols are eq to themselves.
  (should      (pel-eq 'a 'a))
  (should      (pel-eq nil nil))
  (should      (pel-eq t t))
  ;; Small integers are eq (implementation-specific but universally true in
  ;; practice for fixnums).
  (should      (pel-eq 0 0))
  (should      (pel-eq 42 42))
  ;; Different values are not eq.
  (should-not  (pel-eq 'a 'b))
  (should-not  (pel-eq 1 2))
  (should-not  (pel-eq nil t))
  ;; Two freshly consed lists with the same content are not eq (different
  ;; objects) — distinguishes pel-eq from pel-equal.
  (should-not  (pel-eq (list 1 2) (list 1 2)))
  ;; Extra arguments are silently ignored; result is unchanged.
  (should      (pel-eq 'x 'x "scenario-A"))
  (should      (pel-eq 'x 'x "scenario-A" 'extra-info))
  (should-not  (pel-eq 'x 'y "scenario-B" '(some context))))

;; ---------------------------------------------------------------------------
;; pel-equal

(ert-deftest ert-test-pel-equal ()
  "Test `pel-equal'."
  ;; Structural equality.
  (should      (pel-equal '(1 2 3) '(1 2 3)))
  (should      (pel-equal "abc" "abc"))
  (should      (pel-equal [1 2] [1 2]))
  (should      (pel-equal nil nil))
  ;; Different structures.
  (should-not  (pel-equal '(1 2) '(1 3)))
  (should-not  (pel-equal "abc" "ABC"))
  ;; Extra arguments are silently ignored.
  (should      (pel-equal '(a b) '(a b) "scenario-1"))
  (should-not  (pel-equal '(a b) '(a c) "scenario-2" 'more-context)))

;; ---------------------------------------------------------------------------
;; pel-string=

(ert-deftest ert-test-pel-string= ()
  "Test `pel-string='."
  (should      (pel-string= "" ""))
  (should      (pel-string= "hello" "hello"))
  (should      (pel-string= "abc123" "abc123"))
  (should-not  (pel-string= "hello" "world"))
  (should-not  (pel-string= "abc" "ABC"))
  ;; Extra arguments are silently ignored.
  (should      (pel-string= "x" "x" "ctx"))
  (should-not  (pel-string= "x" "y" "ctx" 42)))

;; ---------------------------------------------------------------------------
;; pel-neq

(ert-deftest ert-test-pel-neq ()
  "Test `pel-neq'."
  ;; Inverse of pel-eq.
  (should      (pel-neq 'a 'b))
  (should      (pel-neq 1 2))
  (should      (pel-neq nil t))
  (should-not  (pel-neq 'a 'a))
  (should-not  (pel-neq nil nil))
  (should-not  (pel-neq 0 0))
  ;; Extra arguments are silently ignored.
  (should      (pel-neq 1 2 "scenario"))
  (should-not  (pel-neq 1 1 "scenario" 'info)))

;; ---------------------------------------------------------------------------
;; pel-nequal

(ert-deftest ert-test-pel-nequal ()
  "Test `pel-nequal'."
  ;; Inverse of pel-equal.
  (should      (pel-nequal '(1 2) '(1 3)))
  (should      (pel-nequal "abc" "xyz"))
  (should-not  (pel-nequal '(1 2 3) '(1 2 3)))
  (should-not  (pel-nequal "abc" "abc"))
  ;; Extra arguments are silently ignored.
  (should      (pel-nequal '(a) '(b) "scenario"))
  (should-not  (pel-nequal '(a) '(a) "scenario" 99)))

;; ---------------------------------------------------------------------------
;; pel-string!=

(ert-deftest ert-test-pel-string!= ()
  "Test `pel-string!='."
  ;; Inverse of pel-string=.
  (should      (pel-string!= "foo" "bar"))
  (should      (pel-string!= "" "x"))
  (should      (pel-string!= "abc" "ABC"))
  (should-not  (pel-string!= "foo" "foo"))
  (should-not  (pel-string!= "" ""))
  ;; Extra arguments are silently ignored.
  (should      (pel-string!= "a" "b" "context"))
  (should-not  (pel-string!= "z" "z" "context" 'data)))

;; ---------------------------------------------------------------------------
;; Demonstrate the intended usage: extra args surface in the ERT failure report.
;; The tests below intentionally use extra args to show the pattern.

(defconst pel--ert-test-scenarios
  ;; (arg-list  expected)
  '(((2 3)     3)
    ((1 2 3)   3)
    ((-1 0 5)  5)
    ((4 4)     4))
  "Scenarios used by `ert-test-pel-ert-scenario-pattern'.")

(ert-deftest ert-test-pel-ert-scenario-pattern ()
  "Demonstrate passing a scenario as context arg to pel-eq / pel-equal.
When a test fails the scenario appears verbatim in the ERT report."
  (dolist (scenario pel--ert-test-scenarios)
    (let ((args            (car scenario))
          (expected-result (cadr scenario)))
      (should (pel-eq     (apply #'max args) expected-result scenario))
      (should (pel-equal  (apply #'max args) expected-result scenario)))))

;;; --------------------------------------------------------------------------
(provide 'pel-ert-test)

;;; pel-ert-test.el ends here
