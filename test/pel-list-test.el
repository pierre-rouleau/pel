;;; pel-list-test.el --- Pel-list regression tests.  -*- lexical-binding: t; -*-

;; Created   : Thursday, May 27 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-22 16:15:52 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2026  Pierre Rouleau
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
;; ERT tests for all functions defined in pel-list.el:
;;
;;   pel-insert-in-list
;;   pel-insert-list-in-list
;;   pel-join
;;   pel-list-index
;;   pel-list-split
;;   pel-mapmapcar

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-list)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; ---------------------------------------------------------------------------
;; pel-insert-in-list

(ert-deftest ert-test-insert-in-list ()
  (should (equal (pel-insert-in-list "abc" 0 '(0 1 2 3 4))
                 '("abc" 0 1 2 3 4)))

  (should (equal (pel-insert-in-list '("abc") 0 '(0 1 2 3 4))
                 '(("abc")
                   0 1 2 3 4)))

  (should (equal (pel-insert-in-list '("abc") 4 '(0 1 2 3 4))
                 '(0 1 2 3
                     ("abc")
                     4)))

  (should (equal (pel-insert-in-list '("abc") 7 '(0 1 2 3 4))
                 '(0 1 2 3 4
                     ("abc"))))

  (should (equal (pel-insert-in-list '("abc" "def" 22 les poulets) 4 '(0 1 2 3 4))
                 '(0 1 2 3
                     ("abc" "def" 22 les poulets)
                     4))))

(ert-deftest pel-insert-in-list-edge-cases-test ()
  "Edge cases: empty list, insert exactly at length, idempotent on original."
  ;; Insert into an empty list — result is a one-element list.
  (should (equal (pel-insert-in-list 'x 0 '())
                 '(x)))

  ;; Insert at N = length of list appends the element.
  (should (equal (pel-insert-in-list 'z 5 '(a b c d e))
                 '(a b c d e z)))

  ;; Insert at the last valid index (4) pushes before the last element.
  (should (equal (pel-insert-in-list 'z 4 '(a b c d e))
                 '(a b c d z e)))

  ;; The original list is NOT modified.
  (let ((orig '(1 2 3)))
    (pel-insert-in-list 99 1 orig)
    (should (equal orig '(1 2 3))))

  ;; Insert a nil element.
  (should (equal (pel-insert-in-list nil 1 '(a b c))
                 '(a nil b c))))

;; ---------------------------------------------------------------------------
;; pel-insert-list-in-list

(ert-deftest ert-test-insert-list-in-list ()
  (should (equal (pel-insert-list-in-list "abc" 0 '(0 1 2 3 4))
                 '(97 98 99 0 1 2 3 4)))

  (should (equal (pel-insert-list-in-list '("abc") 0 '(0 1 2 3 4))
                 '("abc" 0 1 2 3 4)))

  (should (equal (pel-insert-list-in-list '("abc") 0 '(0 1 2 3 4))
                 '("abc" 0 1 2 3 4)))

  (should (equal (pel-insert-list-in-list '("abc") 7 '(0 1 2 3 4))
                 '(0 1 2 3 4 "abc")))

  (should (equal (pel-insert-list-in-list '("abc") 4 '(0 1 2 3 4))
                 '(0 1 2 3 "abc" 4)))

  (should (equal (pel-insert-list-in-list '("abc" "def" 22 les poulets) 4 '(0 1 2 3 4))
                 '(0 1 2 3 "abc" "def" 22 les poulets 4))))

(ert-deftest pel-insert-list-in-list-edge-cases-test ()
  "Edge cases: empty SEQ, empty LIST, insert at end, original unchanged."
  ;; Insert an empty sequence — list is unchanged.
  (should (equal (pel-insert-list-in-list '() 2 '(a b c))
                 '(a b c)))

  ;; Insert into an empty list at position 0 — result equals SEQ.
  (should (equal (pel-insert-list-in-list '(x y) 0 '())
                 '(x y)))

  ;; Insert at N = length of list appends SEQ.
  (should (equal (pel-insert-list-in-list '(p q) 3 '(a b c))
                 '(a b c p q)))

  ;; The original list is NOT modified.
  (let ((orig '(1 2 3)))
    (pel-insert-list-in-list '(9 9) 1 orig)
    (should (equal orig '(1 2 3))))

  ;; Insert a multi-element sequence at position 0.
  (should (equal (pel-insert-list-in-list '(x y z) 0 '(1 2 3))
                 '(x y z 1 2 3))))

;; ---------------------------------------------------------------------------
;; pel-join

(ert-deftest ert-test-list-join ()
  (should (string= (pel-join '("hello" "you") "-")
                   "hello-you"))

  (should (string= (pel-join '("hello" "you" "too") "-" 5)
                   "hello-you-too"))

  (should (string= (pel-join '("hello" "you" "too") "-" 2)
                   "hello-you-\ntoo"))

  (should (string= (pel-join '("hello" "you" "too" "and" "you" "and" "you") "--" 2)
                   "hello--you--\ntoo--and--\nyou--and--\nyou"))

  (should (string= (pel-join '("hello" "you" "too" "and" "you" "and" "you") "--" 2 "==>")
                   "hello--you--\n==>too--and--\n==>you--and--\n==>you"))

  (should (string= (pel-join '("hello" "you") "-" 10 "==>")
                   "hello-you")))

(ert-deftest pel-join-edge-cases-test ()
  "Edge cases: empty list, single element, max-per-line=0 error, max-per-line=1."
  ;; Empty element list returns the empty string.
  (should (string= (pel-join '() "-") ""))

  ;; Single element — no separator emitted.
  (should (string= (pel-join '("only") "-") "only"))

  ;; max-per-line = 0 must signal a plain error.
  (should-error (pel-join '("a" "b") "-" 0))

  ;; max-per-line = 1 places every element on its own line.
  (should (string= (pel-join '("a" "b" "c") "," 1)
                   "a,\nb,\nc"))

  ;; max-per-line = 1 with a prefix on continuation lines.
  (should (string= (pel-join '("a" "b" "c") "," 1 "  ")
                   "a,\n  b,\n  c"))

  ;; max-per-line = nil (unlimited) — same as omitting it.
  (should (string= (pel-join '("x" "y" "z") "+" nil)
                   "x+y+z"))

  ;; Separator is the empty string.
  (should (string= (pel-join '("foo" "bar") "")
                   "foobar"))

  ;; Two elements, max-per-line = 2 — both on the same line (no newline).
  (should (string= (pel-join '("a" "b") "-" 2)
                   "a-b"))

  ;; Three elements, max-per-line = 3 — all on the same line.
  (should (string= (pel-join '("a" "b" "c") "-" 3)
                   "a-b-c")))

;; ---------------------------------------------------------------------------
;; pel-list-index

(ert-deftest ert-test-list-index ()
  (should (eq nil (pel-list-index 10 '(1 2 3 4))))
  (should (eq 2   (pel-list-index 10 '(1 2 10 3 4))))
  (should (eq 3   (pel-list-index '(1 2) '("a 1 2" 2 1 (1 2) "1 2 b" 55))))
  (should (eq 0   (pel-list-index '("nut" . 55) '(("nut" . 55)
                                                  ("nut" . 22)
                                                  ("fruit" . "apple")
                                                  ("nut" . 55))))))

(ert-deftest pel-list-index-edge-cases-test ()
  "Edge cases: empty list, object at last position, multiple occurrences."
  ;; Empty list always returns nil.
  (should (eq nil (pel-list-index 'x '())))

  ;; Object at position 0.
  (should (eq 0 (pel-list-index 'a '(a b c))))

  ;; Object at the last position.
  (should (eq 4 (pel-list-index 'e '(a b c d e))))

  ;; Multiple occurrences — first index is returned.
  (should (eq 1 (pel-list-index 'x '(a x b x c x))))

  ;; nil as object.
  (should (eq 2 (pel-list-index nil '(a b nil c))))

  ;; String equality (equal, not eq).
  (should (eq 1 (pel-list-index "hello" '("world" "hello" "there"))))

  ;; Object not present in a non-empty list.
  (should (eq nil (pel-list-index 99 '(1 2 3 4 5)))))

;; ---------------------------------------------------------------------------
;; pel-list-split

(ert-deftest ert-test-list-split ()
  (should (equal (pel-list-split 4 '(0 1 2 3))
                 '((0 1 2 3)
                   nil)))

  (should (equal (pel-list-split 4 '(0 1 2 3) :in-last)
                 '((0 1 2 3)
                   nil)))

  (should (equal (pel-list-split 4 '(0 1 2 3) :in-first)
                 '((0 1 2 3)
                   nil)))

  (should (equal (pel-list-split 0 '(0 1 2 3))
                 '(nil
                   (1 2 3))))

  (should (equal (pel-list-split 1 '(0 1 2 3))
                 '((0)
                   (2 3))))

  (should (equal (pel-list-split 1 '(0 1 2 3) :in-first)
                 `((0 1)
                   (2 3))))

  (should (equal (pel-list-split 1 '(0 1 2 3) :in-last)
                 '((0)
                   (1 2 3))))

  (should (equal (pel-list-split 0 '(0 1 2 3) :in-first)
                 '((0)
                   (1 2 3))))

  (should (equal (pel-list-split 0 '(0 1 2 3) :in-last)
                 '(nil
                   (0 1 2 3)))))

(ert-deftest pel-list-split-edge-cases-test ()
  "Edge cases: empty list, split at last element, multiple occurrences."
  ;; Empty list — object not found: first half is the list, second is nil.
  (should (equal (pel-list-split 'x '())
                 '(nil nil)))

  ;; Single-element list, object present, plain split.
  (should (equal (pel-list-split 'a '(a))
                 '(nil nil)))

  ;; Single-element list, object present, :in-first.
  (should (equal (pel-list-split 'a '(a) :in-first)
                 '((a) nil)))

  ;; Single-element list, object present, :in-last.
  (should (equal (pel-list-split 'a '(a) :in-last)
                 '(nil (a))))

  ;; Split at last element (plain).
  (should (equal (pel-list-split 'd '(a b c d))
                 '((a b c) nil)))

  ;; Split at last element (:in-first).
  (should (equal (pel-list-split 'd '(a b c d) :in-first)
                 '((a b c d) nil)))

  ;; Split at last element (:in-last).
  (should (equal (pel-list-split 'd '(a b c d) :in-last)
                 '((a b c) (d))))

  ;; Multiple occurrences — split on the FIRST occurrence only.
  (should (equal (pel-list-split 'x '(a x b x c))
                 '((a) (b x c))))

  ;; Multiple occurrences with :in-first.
  (should (equal (pel-list-split 'x '(a x b x c) :in-first)
                 '((a x) (b x c))))

  ;; Multiple occurrences with :in-last.
  (should (equal (pel-list-split 'x '(a x b x c) :in-last)
                 '((a) (x b x c))))

  ;; Compound objects (equal comparison).
  (should (equal (pel-list-split '(1 2) '((0) (1 2) (3)))
                 '(((0)) ((3))))))

;; ---------------------------------------------------------------------------
;; pel-mapmapcar

(ert-deftest pel-mapmapcar-basic-test ()
  "Apply a numeric function to each element of a list-of-lists."
  (should (equal (pel-mapmapcar #'1+ '((1 2 3) (4 5 6)))
                 '((2 3 4) (5 6 7)))))

(ert-deftest pel-mapmapcar-string-function-test ()
  "Apply a string function to a list of string lists."
  (should (equal (pel-mapmapcar #'upcase '(("hello" "world") ("foo" "bar")))
                 '(("HELLO" "WORLD") ("FOO" "BAR")))))

(ert-deftest pel-mapmapcar-empty-outer-list-test ()
  "An empty list-of-lists yields an empty list."
  (should (equal (pel-mapmapcar #'1+ '())
                 '())))

(ert-deftest pel-mapmapcar-inner-empty-lists-test ()
  "Inner empty lists are mapped to empty lists."
  (should (equal (pel-mapmapcar #'1+ '(() ()))
                 '(() ())))

  ;; Mix of empty and non-empty inner lists.
  (should (equal (pel-mapmapcar #'1+ '(() (1 2) ()))
                 '(() (2 3) ()))))

(ert-deftest pel-mapmapcar-single-element-inner-lists-test ()
  "Single-element inner lists produce single-element result lists."
  (should (equal (pel-mapmapcar #'number-to-string '((1) (2) (3)))
                 '(("1") ("2") ("3")))))

(ert-deftest pel-mapmapcar-single-outer-list-test ()
  "A list-of-lists with one inner list works correctly."
  (should (equal (pel-mapmapcar #'1+ '((10 20 30)))
                 '((11 21 31)))))

(ert-deftest pel-mapmapcar-identity-test ()
  "Applying `identity' returns the original structure unchanged."
  (let ((input '((a b c) (1 2 3) ("x" "y"))))
    (should (equal (pel-mapmapcar #'identity input)
                   input))))

(ert-deftest pel-mapmapcar-lambda-test ()
  "Works correctly with a lambda function."
  (should (equal (pel-mapmapcar (lambda (x) (* x x))
                                '((1 2 3) (4 5)))
                 '((1 4 9) (16 25)))))

(ert-deftest pel-mapmapcar-does-not-modify-input-test ()
  "pel-mapmapcar must not modify the original list-of-lists."
  (let ((orig '((1 2) (3 4))))
    (pel-mapmapcar #'1+ orig)
    (should (equal orig '((1 2) (3 4))))))

;;; --------------------------------------------------------------------------
(provide 'pel-list-test)

;;; pel-list-test.el ends here
