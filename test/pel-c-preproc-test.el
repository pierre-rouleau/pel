;;; pel-c-preproc-test.el --- ERT tests for pel-c-preproc.el  -*- lexical-binding: t; -*-

;; Created   : Monday, March 23 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-24 16:40:22 EDT, updated by Pierre Rouleau>

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
;; ERT tests for pel-c-preproc.el.
;;
;; Covered items:
;;
;;   pel--c-preproc-conditional-regexp    - regexp constant for C preprocessor
;;                                          conditionals
;;   pel--c-preproc-token-from-match      - maps match-data to 'if / 'else / 'end
;;   pel--c-preproc-token-pos-from-match  - extracts token position from match-data
;;
;; Items intentionally NOT covered:
;;   - `pel-c-preproc-forward-conditional'          (navigation; requires live point)
;;   - `pel-c-preproc-backward-conditional'         (same)
;;   - `pel-c-preproc-outward-forward-conditional'  (same)
;;   - `pel-c-preproc-outward-backward-conditional' (same)
;;   - `pel-c-preproc-conditionals-occur'           (drives occur; interactive)
;;   - `pel-c-preproc-conditionals-multi-occur'     (same, + projectile)
;;   - `pel--projectile-multi-occur'                (requires projectile runtime)
;;
;; Notes on test strategy:
;;   - `pel--c-preproc-token-from-match' and `pel--c-preproc-token-pos-from-match'
;;     are exercised by running actual `re-search-forward' calls against
;;     `pel--c-preproc-conditional-regexp' in temp buffers; this guarantees
;;     the match-data lists are genuine rather than hand-crafted mocks, and
;;     also validates the regexp at the same time.
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-c-preproc)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:

;; ===========================================================================
;; Helpers
;; ===========================================================================

(defmacro pel-c-preproc-test--with-code (code &rest body)
  "Execute BODY in a temp buffer containing CODE with point at `point-min'."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,code)
     (goto-char (point-min))
     ,@body))

(defun pel-c-preproc-test--search (code)
  "Search CODE for `pel--c-preproc-conditional-regexp'; return match-data or nil."
  (with-temp-buffer
    (insert code)
    (goto-char (point-min))
    (when (re-search-forward pel--c-preproc-conditional-regexp nil t)
      (match-data t))))

;; ===========================================================================
;; pel--c-preproc-conditional-regexp  —  string properties
;; ===========================================================================

(ert-deftest pel-c-preproc-test/regexp/is-non-empty-string ()
  "`pel--c-preproc-conditional-regexp' must be a non-empty string."
  (should (stringp pel--c-preproc-conditional-regexp))
  (should (> (length pel--c-preproc-conditional-regexp) 0)))

;; ===========================================================================
;; pel--c-preproc-conditional-regexp  —  #if family matches
;; ===========================================================================

(ert-deftest pel-c-preproc-test/regexp/matches-hash-if ()
  "`#if EXPR' is matched."
  (should (string-match pel--c-preproc-conditional-regexp "#if FOO")))

(ert-deftest pel-c-preproc-test/regexp/matches-hash-ifdef ()
  "`#ifdef SYM' is matched."
  (should (string-match pel--c-preproc-conditional-regexp "#ifdef MY_SYM")))

(ert-deftest pel-c-preproc-test/regexp/matches-hash-ifndef ()
  "`#ifndef SYM' is matched."
  (should (string-match pel--c-preproc-conditional-regexp "#ifndef MY_SYM")))

(ert-deftest pel-c-preproc-test/regexp/matches-indented-if ()
  "Leading whitespace before #if is accepted."
  (should (string-match pel--c-preproc-conditional-regexp "   #if FOO")))

(ert-deftest pel-c-preproc-test/regexp/matches-hash-space-if ()
  "A space between `#' and `if' is accepted."
  (should (string-match pel--c-preproc-conditional-regexp "#  if FOO")))

;; ===========================================================================
;; pel--c-preproc-conditional-regexp  —  `#else` / `#elif` matches
;; ===========================================================================

(ert-deftest pel-c-preproc-test/regexp/matches-hash-else ()
  "#else is matched."
  (should (string-match pel--c-preproc-conditional-regexp "#else")))

(ert-deftest pel-c-preproc-test/regexp/matches-hash-elif ()
  "`#elif EXPR' is matched."
  (should (string-match pel--c-preproc-conditional-regexp "#elif BAR")))

;; ===========================================================================
;; pel--c-preproc-conditional-regexp  —  #endif match
;; ===========================================================================

(ert-deftest pel-c-preproc-test/regexp/matches-hash-endif ()
  "#endif is matched."
  (should (string-match pel--c-preproc-conditional-regexp "#endif")))

(ert-deftest pel-c-preproc-test/regexp/matches-indented-endif ()
  "Leading whitespace before #endif is accepted."
  (should (string-match pel--c-preproc-conditional-regexp "  #endif")))

;; ===========================================================================
;; pel--c-preproc-conditional-regexp  —  non-matches
;; ===========================================================================

(ert-deftest pel-c-preproc-test/regexp/rejects-include ()
  "`#include' is not a conditional and must not be matched."
  (should-not (string-match pel--c-preproc-conditional-regexp "#include <stdio.h>")))

(ert-deftest pel-c-preproc-test/regexp/rejects-define ()
  "`#define' must not be matched."
  (should-not (string-match pel--c-preproc-conditional-regexp "#define FOO 1")))

(ert-deftest pel-c-preproc-test/regexp/rejects-plain-code ()
  "Plain C code without a preprocessor directive must not be matched."
  (should-not (string-match pel--c-preproc-conditional-regexp "int x = 0;")))

(ert-deftest pel-c-preproc-test/regexp/rejects-mid-line-hash ()
  "A `#' that does not start the logical line must not be matched."
  (should-not (string-match pel--c-preproc-conditional-regexp "x = a #if b")))

;; ===========================================================================
;; pel--c-preproc-token-from-match
;; Each test runs a real search and passes the genuine match-data to the
;; function under test.
;; ===========================================================================

(ert-deftest pel-c-preproc-test/token-from-match/if-for-hash-if ()
  "#if produces the symbol `if'."
  (let ((md (pel-c-preproc-test--search "#if FOO\n")))
    (should md)
    (should (eq 'if (pel--c-preproc-token-from-match md)))))

(ert-deftest pel-c-preproc-test/token-from-match/if-for-ifdef ()
  "`#ifdef' produces the symbol `if'."
  (let ((md (pel-c-preproc-test--search "#ifdef MY_SYM\n")))
    (should md)
    (should (eq 'if (pel--c-preproc-token-from-match md)))))

(ert-deftest pel-c-preproc-test/token-from-match/if-for-ifndef ()
  "`#ifndef' produces the symbol `if'."
  (let ((md (pel-c-preproc-test--search "#ifndef MY_SYM\n")))
    (should md)
    (should (eq 'if (pel--c-preproc-token-from-match md)))))

(ert-deftest pel-c-preproc-test/token-from-match/else-for-hash-else ()
  "#else produces the symbol `else'."
  (let ((md (pel-c-preproc-test--search "#else\n")))
    (should md)
    (should (eq 'else (pel--c-preproc-token-from-match md)))))

(ert-deftest pel-c-preproc-test/token-from-match/else-for-elif ()
  "`#elif' produces the symbol `else'."
  (let ((md (pel-c-preproc-test--search "#elif EXPR\n")))
    (should md)
    (should (eq 'else (pel--c-preproc-token-from-match md)))))

(ert-deftest pel-c-preproc-test/token-from-match/end-for-endif ()
  "#endif produces the symbol `end'."
  (let ((md (pel-c-preproc-test--search "#endif\n")))
    (should md)
    (should (eq 'end (pel--c-preproc-token-from-match md)))))

;; ===========================================================================
;; pel--c-preproc-token-pos-from-match
;; ===========================================================================

(ert-deftest pel-c-preproc-test/token-pos-from-match/returns-integer-for-if ()
  "`pel--c-preproc-token-pos-from-match' returns an integer position for #if."
  (let ((md (pel-c-preproc-test--search "#if FOO\n")))
    (should md)
    (let ((pos (pel--c-preproc-token-pos-from-match md)))
      (should (integerp pos))
      (should (> pos 0)))))

(ert-deftest pel-c-preproc-test/token-pos-from-match/returns-integer-for-else ()
  "`pel--c-preproc-token-pos-from-match' returns an integer position for #else."
  (let ((md (pel-c-preproc-test--search "#else\n")))
    (should md)
    (let ((pos (pel--c-preproc-token-pos-from-match md)))
      (should (integerp pos))
      (should (> pos 0)))))

(ert-deftest pel-c-preproc-test/token-pos-from-match/returns-integer-for-endif ()
  "`pel--c-preproc-token-pos-from-match' returns an integer position for #endif."
  (let ((md (pel-c-preproc-test--search "#endif\n")))
    (should md)
    (let ((pos (pel--c-preproc-token-pos-from-match md)))
      (should (integerp pos))
      (should (> pos 0)))))

(ert-deftest pel-c-preproc-test/token-pos-from-match/if-uses-match-start ()
  "For #if, the returned position equals the start of the whole match."
  (pel-c-preproc-test--with-code "#if FOO\n"
    (re-search-forward pel--c-preproc-conditional-regexp nil t)
    (let* ((md (match-data t))
           (pos (pel--c-preproc-token-pos-from-match md)))
      ;; The whole-match start is (nth 0 md); for #if the function returns that.
      (should (= pos (nth 0 md))))))

;; ===========================================================================
;; Round-trip: regexp → token → position across a multi-directive buffer
;; ===========================================================================

(ert-deftest pel-c-preproc-test/round-trip/sequence-of-directives ()
  "Successive searches in a realistic snippet yield the correct token sequence."
  (pel-c-preproc-test--with-code
      "#ifdef PLATFORM_A\n#elif defined PLATFORM_B\n#else\n#endif\n"
    (let ((expected '(if else else end))
          (tokens   '()))
      (while (re-search-forward pel--c-preproc-conditional-regexp nil t)
        (push (pel--c-preproc-token-from-match (match-data t)) tokens))
      (should (equal (nreverse tokens) expected)))))

;;; --------------------------------------------------------------------------
(provide 'pel-c-preproc-test)

;;; pel-c-preproc-test.el ends here
