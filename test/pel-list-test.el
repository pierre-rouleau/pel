;;; pel-list-test.el --- Pel-list regression tests.  -*- lexical-binding: t; -*-

;; Created   : Thursday, May 27 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-05-27 13:53:09, updated by Pierre Rouleau>

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
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel-list)
(require 'pel-ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;

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


(ert-deftest ert-test-list-index ()
  (should (eq nil (pel-list-index 10 '(1 2 3 4))))
  (should (eq 2   (pel-list-index 10 '(1 2 10 3 4))))
  (should (eq 3   (pel-list-index '(1 2) '("a 1 2" 2 1 (1 2) "1 2 b" 55))))
  (should (eq 0   (pel-list-index '("nut" . 55) '(("nut" . 55)
                                                  ("nut" . 22)
                                                  ("fruit" . "apple")
                                                  ("nut" . 55))))))

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
;;; --------------------------------------------------------------------------
(provide 'pel-list-test)

;;; pel-list-test.el ends here
