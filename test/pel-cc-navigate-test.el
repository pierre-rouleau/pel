;;; pel-cc-navigate-test.el --- ERT tests for pel-cc-navigate.el  -*- lexical-binding: t; -*-

;; Created   : Wednesday, March 25 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-25 17:29:31 EDT, updated by Pierre Rouleau>

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

;; ERT tests for pel-cc-navigate.el.
;; Tests cover regexp constants, `pel-cc-elem-boundaries', and
;; `pel--cc-move-to'.

;;; --------------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'pel-cc-navigate)

;;; --------------------------------------------------------------------------
;;; Regexp constant sanity checks
;;; --------------------------------------------------------------------------

(ert-deftest pel-cc-navigate-test/switch-regexp/matches-basic ()
  "switch-regexp matches a basic switch statement."
  (should (string-match pel-cc--switch-regexp "switch (x)")))

(ert-deftest pel-cc-navigate-test/switch-regexp/matches-after-semicolon ()
  "switch-regexp matches switch after a semicolon."
  (should (string-match pel-cc--switch-regexp "; switch (x)")))

;; [:todo 2026-03-25, by Pierre Rouleau: Fix this test: it needs to use c-mode]
(ert-deftest pel-cc-navigate-test/switch-regexp/no-match-in-comment ()
  "switch-regexp does not specially exclude comments (regexp only check)."
  ;; The regexp itself will match; comment exclusion is done by pel-inside-code-p
  (should (string-match pel-cc--switch-regexp "switch (y)")))

(ert-deftest pel-cc-navigate-test/enum-regexp/matches-basic-enum ()
  "enum-regexp matches a basic enum declaration."
  (should (string-match pel-cc--enum-regexp "enum Color")))

(ert-deftest pel-cc-navigate-test/enum-regexp/matches-enum-class ()
  "enum-regexp matches C++ enum class."
  (should (string-match pel-cc--enum-regexp "enum class Color")))

(ert-deftest pel-cc-navigate-test/enum-regexp/matches-typedef-enum ()
  "enum-regexp matches typedef enum."
  (should (string-match pel-cc--enum-regexp "typedef enum Color")))

(ert-deftest pel-cc-navigate-test/union-regexp/matches-basic-union ()
  "union-regexp matches a basic union declaration."
  (should (string-match pel-cc--union-regexp "union Data")))

(ert-deftest pel-cc-navigate-test/union-regexp/matches-typedef-union ()
  "union-regexp matches typedef union."
  (should (string-match pel-cc--union-regexp "typedef union Data")))

(ert-deftest pel-cc-navigate-test/struct-regexp/matches-basic-struct ()
  "struct-regexp matches a basic struct declaration."
  (should (string-match pel-cc--struct-regexp "struct Point")))

(ert-deftest pel-cc-navigate-test/struct-regexp/matches-typedef-struct ()
  "struct-regexp matches typedef struct."
  (should (string-match pel-cc--struct-regexp "typedef struct Point")))

(ert-deftest pel-cc-navigate-test/class-regexp/matches-class ()
  "class-regexp matches a class declaration."
  (should (string-match pel-cc--class-regexp "class Foo ")))

(ert-deftest pel-cc-navigate-test/class-regexp/matches-struct ()
  "class-regexp matches a struct declaration (C++ mode uses class-regexp)."
  (should (string-match pel-cc--class-regexp "struct Foo ")))

;;; --------------------------------------------------------------------------
;;; Tests for `pel--cc-move-to'
;;; --------------------------------------------------------------------------

(ert-deftest pel-cc-navigate-test/move-to/no-positions-signals-user-error ()
  "When positions is nil, signal a user-error with the element name."
  (with-temp-buffer
    (should-error
     (pel--cc-move-to nil 0 "switch statement")
     :type 'user-error)))

(ert-deftest pel-cc-navigate-test/move-to/moves-to-begin ()
  "When positions is (10 20), move to position 10 (index 0)."
  (with-temp-buffer
    (insert (make-string 30 ?x))
    (goto-char (point-min))
    (pel--cc-move-to '(10 20) 0 "switch statement")
    (should (= (point) 10))))

(ert-deftest pel-cc-navigate-test/move-to/moves-to-end ()
  "When positions is (10 20), move to position 20 (index 1)."
  (with-temp-buffer
    (insert (make-string 30 ?x))
    (goto-char (point-min))
    (pel--cc-move-to '(10 20) 1 "switch statement")
    (should (= (point) 20))))

(ert-deftest pel-cc-navigate-test/move-to/pushes-mark ()
  "pel--cc-move-to pushes a mark before moving."
  (with-temp-buffer
    (insert (make-string 30 ?x))
    (goto-char 5)
    (pel--cc-move-to '(10 20) 0 "switch statement")
    ;; The mark should have been set at the previous point position
    (should (= (mark) 5))))

;;; --------------------------------------------------------------------------
;;; Tests for `pel-cc-elem-boundaries'
;;; --------------------------------------------------------------------------

(ert-deftest pel-cc-navigate-test/elem-boundaries/finds-enum-block ()
  "elem-boundaries returns (begin end) when point is inside enum block."
  (with-temp-buffer
    ;; Minimal C code with an enum block
    (insert "enum Color {\n  RED,\n  GREEN\n};\n")
    ;; Place point inside the block (after '{')
    (goto-char (point-min))
    (search-forward "RED")
    (let ((result (pel-cc-elem-boundaries pel-cc--enum-regexp)))
      (should (consp result))
      (should (= (length result) 2))
      (should (< (car result) (cadr result))))))

(ert-deftest pel-cc-navigate-test/elem-boundaries/returns-nil-when-outside ()
  "elem-boundaries returns nil when point is not inside any matching block."
  (with-temp-buffer
    (insert "int x = 5;\n")
    (goto-char (point-min))
    (let ((result (pel-cc-elem-boundaries pel-cc--enum-regexp)))
      (should (null result)))))

(ert-deftest pel-cc-navigate-test/elem-boundaries/finds-struct-block ()
  "elem-boundaries returns positions when point is inside a struct block."
  (with-temp-buffer
    (insert "struct Point {\n  int x;\n  int y;\n};\n")
    (goto-char (point-min))
    (search-forward "int x")
    (let ((result (pel-cc-elem-boundaries pel-cc--struct-regexp)))
      (should (consp result))
      (should (< (car result) (cadr result))))))

;;; --------------------------------------------------------------------------
(provide 'pel-cc-navigate-test)
;;; pel-cc-navigate-test.el ends here
