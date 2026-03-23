;;; pel-hash-test.el --- ERT tests for pel-hash.el.  -*- lexical-binding: t; -*-

;; Created   : Monday, March 23 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-23 09:28:04 EDT, updated by Pierre Rouleau>

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
;; Regression tests for the hash-of-lists utilities provided by pel-hash.el.
;;
;; Tested functions:
;;  - `pel-make-hash-of-lists'
;;  - `pel-addto-hash-of-lists'
;;  - `pel-get-list-from-hash-of-lists-for'
;;
;; Design notes:
;;
;; Internally, `pel-addto-hash-of-lists' prepends elements via `cons', so the
;; raw hash value is stored in reverse insertion order (last-in, first).
;; `pel-get-list-from-hash-of-lists-for' calls `reverse' before returning, so
;; the caller always sees elements in the original insertion order
;; (first-in, first).  Tests that inspect the raw hash value via `gethash'
;; verify the internal (reversed) ordering; tests that use
;; `pel-get-list-from-hash-of-lists-for' verify the public (insertion) order.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-hash)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; ---------------------------------------------------------------------------
;; pel-make-hash-of-lists

(ert-deftest test-pel-make-hash-of-lists ()
  "Test `pel-make-hash-of-lists'."
  ;; Returns a hash table.
  (should      (hash-table-p (pel-make-hash-of-lists)))
  ;; Default test predicate is eq.
  (should      (eq (hash-table-test (pel-make-hash-of-lists)) 'eq))
  ;; Explicit test predicates are honoured.
  (should      (eq (hash-table-test (pel-make-hash-of-lists 'eq))    'eq))
  (should      (eq (hash-table-test (pel-make-hash-of-lists 'eql))   'eql))
  (should      (eq (hash-table-test (pel-make-hash-of-lists 'equal)) 'equal))
  ;; Each call returns a fresh, independent hash table.
  (let ((h1 (pel-make-hash-of-lists))
        (h2 (pel-make-hash-of-lists)))
    (should-not (eq h1 h2)))
  ;; Freshly created table is empty.
  (should      (zerop (hash-table-count (pel-make-hash-of-lists)))))

(ert-deftest test-pel-make-hash-of-lists/nil-arg ()
  "Passing nil explicitly still defaults to the eq predicate."
  (should (eq (hash-table-test (pel-make-hash-of-lists nil)) 'eq)))

;; ---------------------------------------------------------------------------
;; pel-addto-hash-of-lists

(ert-deftest test-pel-addto-hash-of-lists/nil-element ()
  "nil is a valid element value."
  (let ((h (pel-make-hash-of-lists)))
    (pel-addto-hash-of-lists h 'k nil)
    (should (equal (gethash 'k h) '(nil)))
    (should (equal (pel-get-list-from-hash-of-lists-for h 'k) '(nil)))))

(ert-deftest test-pel-addto-hash-of-lists/single-element ()
  "Adding a single element creates a one-element list under the key."
  (let ((h (pel-make-hash-of-lists)))
    ;; Return value is the updated (internal) list: one element.
    (should (equal (pel-addto-hash-of-lists h 'key 'a) '(a)))
    ;; The hash table now has exactly one key.
    (should (= (hash-table-count h) 1))))

(ert-deftest test-pel-addto-hash-of-lists/duplicate-elements ()
  "The same element can be added multiple times; each occurrence is retained."
  (let ((h (pel-make-hash-of-lists)))
    (pel-addto-hash-of-lists h 'k 'x)
    (pel-addto-hash-of-lists h 'k 'x)
    (should (equal (pel-get-list-from-hash-of-lists-for h 'k) '(x x)))))

(ert-deftest test-pel-addto-hash-of-lists/multiple-elements ()
  "Adding multiple elements under the same key accumulates them.
Internally the list is stored in reverse insertion order (cons prepends)."
  (let ((h (pel-make-hash-of-lists)))
    (pel-addto-hash-of-lists h 'key 1)
    (pel-addto-hash-of-lists h 'key 2)
    ;; After adding 1 then 2 then 3, internal order is (3 2 1).
    (should (equal (pel-addto-hash-of-lists h 'key 3) '(3 2 1)))))

(ert-deftest test-pel-addto-hash-of-lists/multiple-keys ()
  "Elements stored under different keys are independent."
  (let ((h (pel-make-hash-of-lists)))
    (pel-addto-hash-of-lists h 'k1 'a)
    (pel-addto-hash-of-lists h 'k1 'b)
    (pel-addto-hash-of-lists h 'k2 'x)
    (pel-addto-hash-of-lists h 'k2 'y)
    (pel-addto-hash-of-lists h 'k2 'z)
    ;; Two distinct keys in the table.
    (should (= (hash-table-count h) 2))
    ;; Each key holds only its own elements (raw, in reverse insertion order).
    (should (equal (gethash 'k1 h) '(b a)))
    (should (equal (gethash 'k2 h) '(z y x)))))

(ert-deftest test-pel-addto-hash-of-lists/integer-keys ()
  "Integer keys work with the default eq test."
  (let ((h (pel-make-hash-of-lists)))
    (pel-addto-hash-of-lists h 1 'one)
    (pel-addto-hash-of-lists h 2 'two)
    (pel-addto-hash-of-lists h 1 'ONE)
    ;; Key 1 has two elements stored in reverse insertion order.
    (should (equal (gethash 1 h) '(ONE one)))
    ;; Key 2 has one element.
    (should (equal (gethash 2 h) '(two)))))

(ert-deftest test-pel-addto-hash-of-lists/float-keys ()
  "Float keys require the eql test predicate."
  (let ((h (pel-make-hash-of-lists 'eql)))
    (pel-addto-hash-of-lists h 1.5 'a)
    (pel-addto-hash-of-lists h 2.5 'b)
    (pel-addto-hash-of-lists h 1.5 'c)
    (should (= (hash-table-count h) 2))
    (should (equal (gethash 1.5 h) '(c a)))
    (should (equal (gethash 2.5 h) '(b)))))

(ert-deftest test-pel-addto-hash-of-lists/string-keys ()
  "String keys require the equal test predicate."
  (let ((h (pel-make-hash-of-lists 'equal)))
    (pel-addto-hash-of-lists h "alpha" 1)
    (pel-addto-hash-of-lists h "alpha" 2)
    (pel-addto-hash-of-lists h "beta"  10)
    (should (= (hash-table-count h) 2))
    (should (equal (gethash "alpha" h) '(2 1)))
    (should (equal (gethash "beta"  h) '(10)))))

;; ---------------------------------------------------------------------------
;; pel-get-list-from-hash-of-lists-for

(ert-deftest test-pel-get-list-from-hash-of-lists-for/missing-key ()
  "Querying a key that was never inserted returns nil."
  (let ((h (pel-make-hash-of-lists)))
    (should (null (pel-get-list-from-hash-of-lists-for h 'absent)))
    (should (null (pel-get-list-from-hash-of-lists-for h 42)))))

(ert-deftest test-pel-get-list-from-hash-of-lists-for/single-element ()
  "A list with one element is returned correctly."
  (let ((h (pel-make-hash-of-lists)))
    (pel-addto-hash-of-lists h 'k 'only)
    (should (equal (pel-get-list-from-hash-of-lists-for h 'k) '(only)))))

(ert-deftest test-pel-get-list-from-hash-of-lists-for/insertion-order ()
  "Retrieved list reflects original insertion order (first-in, first)."
  (let ((h (pel-make-hash-of-lists)))
    (pel-addto-hash-of-lists h 'seq 'first)
    (pel-addto-hash-of-lists h 'seq 'second)
    (pel-addto-hash-of-lists h 'seq 'third)
    (should (equal (pel-get-list-from-hash-of-lists-for h 'seq)
                   '(first second third)))))

(ert-deftest test-pel-get-list-from-hash-of-lists-for/multiple-keys ()
  "Each key returns only its own elements in insertion order."
  (let ((h (pel-make-hash-of-lists)))
    (pel-addto-hash-of-lists h 'fruit  'apple)
    (pel-addto-hash-of-lists h 'veggie 'carrot)
    (pel-addto-hash-of-lists h 'fruit  'banana)
    (pel-addto-hash-of-lists h 'veggie 'broccoli)
    (pel-addto-hash-of-lists h 'fruit  'cherry)
    (should (equal (pel-get-list-from-hash-of-lists-for h 'fruit)
                   '(apple banana cherry)))
    (should (equal (pel-get-list-from-hash-of-lists-for h 'veggie)
                   '(carrot broccoli)))))

(ert-deftest test-pel-get-list-from-hash-of-lists-for/integer-keys ()
  "Integer keys: insertion order is preserved in the returned list."
  (let ((h (pel-make-hash-of-lists)))
    (pel-addto-hash-of-lists h 0 'zero)
    (pel-addto-hash-of-lists h 0 'nil-like)
    (pel-addto-hash-of-lists h 1 'one)
    (should (equal (pel-get-list-from-hash-of-lists-for h 0) '(zero nil-like)))
    (should (equal (pel-get-list-from-hash-of-lists-for h 1) '(one)))))

(ert-deftest test-pel-get-list-from-hash-of-lists-for/string-keys ()
  "String keys with equal test: insertion order is preserved."
  (let ((h (pel-make-hash-of-lists 'equal)))
    (pel-addto-hash-of-lists h "lang" 'c)
    (pel-addto-hash-of-lists h "lang" 'lisp)
    (pel-addto-hash-of-lists h "lang" 'python)
    (should (equal (pel-get-list-from-hash-of-lists-for h "lang")
                   '(c lisp python)))))

;; ---------------------------------------------------------------------------
;; Round-trip integration: add then get

(ert-deftest test-pel-hash-round-trip ()
  "Round-trip: elements added with `pel-addto-hash-of-lists' are retrieved
in insertion order by `pel-get-list-from-hash-of-lists-for'."
  (let ((h (pel-make-hash-of-lists 'equal)))
    ;; Build a list of numbers under one key.
    (dolist (item '(10 20 30 40 50))
      (pel-addto-hash-of-lists h "numbers" item))
    (should (equal (pel-get-list-from-hash-of-lists-for h "numbers")
                   '(10 20 30 40 50)))
    ;; Build a list of symbols under a separate key.
    (dolist (item '(a b c))
      (pel-addto-hash-of-lists h "letters" item))
    (should (equal (pel-get-list-from-hash-of-lists-for h "letters")
                   '(a b c)))
    ;; Confirm the first key's list is unchanged.
    (should (equal (pel-get-list-from-hash-of-lists-for h "numbers")
                   '(10 20 30 40 50)))))

;;; --------------------------------------------------------------------------
(provide 'pel-hash-test)

;;; pel-hash-test.el ends here
