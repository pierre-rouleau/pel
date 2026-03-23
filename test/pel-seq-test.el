;;; pel-seq-test.el --- ERT tests for pel-seq.el  -*- lexical-binding: t; -*-

;; Created   : Monday, March 23 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-23 11:00:01 EDT, updated by Pierre Rouleau>

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
;; ERT tests for pel-seq.el.
;;
;; The file defines one function: `pel-all-fboundp'.
;;
;; `pel-all-fboundp' returns t if every symbol in its argument list is
;; `fboundp', and nil otherwise.  Key behavioural properties tested here:
;;
;;   1. No arguments  → t  (vacuous truth: no symbols to check)
;;   2. All bound     → t
;;   3. Any unbound   → nil
;;   4. Return value is exactly t / nil (not merely truthy / falsy)
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-seq)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:

;; ===========================================================================
;; pel-all-fboundp
;; ===========================================================================

;; ---------------------------------------------------------------------------
;; Vacuous / empty case
;; ---------------------------------------------------------------------------

(ert-deftest pel-seq-test/all-fboundp/no-args ()
  "With no arguments `pel-all-fboundp' returns t.
There are no symbols to check, so the result is vacuously true."
  (should (eq t (pel-all-fboundp))))

;; ---------------------------------------------------------------------------
;; All-bound cases
;; ---------------------------------------------------------------------------

(ert-deftest pel-seq-test/all-fboundp/single-bound ()
  "Returns t when given a single fbound symbol."
  (should (eq t (pel-all-fboundp 'car))))

(ert-deftest pel-seq-test/all-fboundp/multiple-bound ()
  "Returns t when every supplied symbol is fbound."
  (should (eq t (pel-all-fboundp 'car 'cdr 'cons 'mapcar 'fboundp))))

;; ---------------------------------------------------------------------------
;; Unbound cases
;; ---------------------------------------------------------------------------

(ert-deftest pel-seq-test/all-fboundp/single-unbound ()
  "Returns nil for a single unbound symbol."
  (let ((unbound (make-symbol "pel--seq-test-unbound")))
    (should (null (pel-all-fboundp unbound)))))

(ert-deftest pel-seq-test/all-fboundp/all-unbound ()
  "Returns nil when every supplied symbol is unbound."
  (let ((u1 (make-symbol "pel--seq-test-unbound-1"))
        (u2 (make-symbol "pel--seq-test-unbound-2")))
    (should (null (pel-all-fboundp u1 u2)))))

;; ---------------------------------------------------------------------------
;; Mixed cases (some bound, some unbound)
;; ---------------------------------------------------------------------------

(ert-deftest pel-seq-test/all-fboundp/unbound-first ()
  "Returns nil when the first symbol is unbound and the rest are bound."
  (let ((unbound (make-symbol "pel--seq-test-unbound")))
    (should (null (pel-all-fboundp unbound 'car 'cdr)))))

(ert-deftest pel-seq-test/all-fboundp/unbound-last ()
  "Returns nil when the last symbol is unbound and the rest are bound."
  (let ((unbound (make-symbol "pel--seq-test-unbound")))
    (should (null (pel-all-fboundp 'car 'cdr unbound)))))

(ert-deftest pel-seq-test/all-fboundp/unbound-middle ()
  "Returns nil when an unbound symbol appears in the middle of the list."
  (let ((unbound (make-symbol "pel--seq-test-unbound")))
    (should (null (pel-all-fboundp 'car unbound 'cdr)))))

;; ---------------------------------------------------------------------------
;; Return-value exactness
;; ---------------------------------------------------------------------------

(ert-deftest pel-seq-test/all-fboundp/returns-exactly-t ()
  "The truthy return value is exactly the symbol t, not merely non-nil."
  (should (eq t (pel-all-fboundp 'car 'cdr))))

(ert-deftest pel-seq-test/all-fboundp/returns-exactly-nil ()
  "The falsy return value is exactly nil."
  (let ((unbound (make-symbol "pel--seq-test-unbound")))
    (should (null (pel-all-fboundp 'car unbound)))))

;;; --------------------------------------------------------------------------
(provide 'pel-seq-test)

;;; pel-seq-test.el ends here
