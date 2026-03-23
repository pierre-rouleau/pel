;;; pel-c-utils-test.el --- ERT tests for pel-c-utils.el  -*- lexical-binding: t; -*-

;; Created   : Monday, March 23 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-23 16:39:17 EDT, updated by Pierre Rouleau>

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
;; ERT tests for pel-c-utils.el.
;;
;; Covered items:
;;
;;   pel-preproc-if-regexp            - constant regexp value
;;   pel-preproc-if-eq-regexp-format  - constant format string
;;   pel-c-search-equal-NULL          - search for == NULL / NULL ==
;;   pel-c-search-not-equal-NULL      - search for != NULL / NULL !=
;;   pel-c-search-equal-true          - search for == true/TRUE
;;   pel-c-search-not-equal-true      - search for != true/TRUE
;;   pel-c-search-equal-false         - search for == false/FALSE
;;   pel-c-search-not-equal-false     - search for != false/FALSE
;;   pel-c-search-any-comparison-problem - search for any of the above
;;   pel-c-search-preproc-if          - search for bare '#if VAR'
;;   pel-c-search-preproc-if-set      - search for '#if VAR == 0/1'
;;   pel-c-fix-comparison-problems    - fix all comparison anti-patterns
;;     (incl. verifying that text inside C comments and string literals
;;      is left untouched — these tests activate `c-mode' to provide a
;;      proper C syntax table to `syntax-ppss')
;;   pel-c-fix-preproc-if-problems    - fix preprocessor #if anti-patterns
;;
;; Items intentionally NOT covered:
;;   - C++ class-qualifier path (requires c++-mode)
;;   - Complex nested pointer/function-call expressions
;;   - `pel--c-adjusted' (private; tested indirectly via fix functions)
;;   - `pel--c-reformat-cond' (private; tested indirectly)
;;
;; Notes on test isolation:
;;   - Tests run in `fundamental-mode' (the default for temp buffers).
;;   - In that mode `pel--c-adjusted' substitutes "" for the C++ class
;;     qualifier placeholder, which is the same behaviour as `c-mode'.
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-c-utils)
(require 'cl-lib)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:

;; ===========================================================================
;; Helpers
;; ===========================================================================

(defmacro pel-c-utils-test--with-code (code &rest body)
  "Execute BODY inside a writable temp buffer pre-loaded with CODE (a string).
Point is left at the beginning of the buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,code)
     (goto-char (point-min))
     ,@body))

;; ===========================================================================
;; Constants
;; ===========================================================================

(ert-deftest pel-c-utils-test/constants/preproc-if-regexp-is-string ()
  "`pel-preproc-if-regexp' must be a non-empty string."
  (should (stringp pel-preproc-if-regexp))
  (should (> (length pel-preproc-if-regexp) 0)))

(ert-deftest pel-c-utils-test/constants/preproc-if-regexp-matches-bare-if ()
  "`pel-preproc-if-regexp' matches a bare `#if VAR' line."
  (should (string-match pel-preproc-if-regexp "#if MY_FLAG"))
  (should (string-match pel-preproc-if-regexp "  #if MY_FLAG"))
  (should (string-match pel-preproc-if-regexp "#  if MY_FLAG")))

(ert-deftest pel-c-utils-test/constants/preproc-if-regexp-rejects-if-eq ()
  "`pel-preproc-if-regexp' does NOT match `#if VAR == 0' (has trailing == 0)."
  (should-not (string-match pel-preproc-if-regexp "#if MY_FLAG == 0"))
  (should-not (string-match pel-preproc-if-regexp "#if MY_FLAG == 1")))

(ert-deftest pel-c-utils-test/constants/preproc-if-eq-regexp-format-is-string ()
  "`pel-preproc-if-eq-regexp-format' must be a non-empty string."
  (should (stringp pel-preproc-if-eq-regexp-format))
  (should (> (length pel-preproc-if-eq-regexp-format) 0)))

(ert-deftest pel-c-utils-test/constants/preproc-if-eq-regexp-matches-eq-0 ()
  "Formatted with '0', the regexp matches `#if VAR == 0'."
  (let ((re (format pel-preproc-if-eq-regexp-format "0")))
    (should (string-match re "#if MY_FLAG == 0"))
    (should (string-match re "#if MY_FLAG==0"))))

(ert-deftest pel-c-utils-test/constants/preproc-if-eq-regexp-matches-eq-1 ()
  "Formatted with '1', the regexp matches `#if VAR == 1'."
  (let ((re (format pel-preproc-if-eq-regexp-format "1")))
    (should (string-match re "#if MY_FLAG == 1"))
    (should (string-match re "#if MY_FLAG==1"))))

(ert-deftest pel-c-utils-test/constants/preproc-if-eq-regexp-rejects-bare-if ()
  "Formatted with '0', the regexp does NOT match a bare `#if VAR'."
  (let ((re (format pel-preproc-if-eq-regexp-format "0")))
    (should-not (string-match re "#if MY_FLAG"))))

;; ===========================================================================
;; Search functions — pel-c-search-equal-NULL
;; ===========================================================================

(ert-deftest pel-c-utils-test/search-equal-NULL/finds-variable-eq-NULL ()
  "`pel-c-search-equal-NULL' finds `ptr == NULL'."
  (pel-c-utils-test--with-code "if (ptr == NULL)"
    (should (pel-c-search-equal-NULL))
    (should (> (point) 1))))

(ert-deftest pel-c-utils-test/search-equal-NULL/finds-NULL-eq-variable ()
  "`pel-c-search-equal-NULL' finds `NULL == ptr' (Yoda condition)."
  (pel-c-utils-test--with-code "if (NULL == ptr)"
    (should (pel-c-search-equal-NULL))))

(ert-deftest pel-c-utils-test/search-equal-NULL/not-found-signals-error ()
  "`pel-c-search-equal-NULL' signals an error when pattern is absent."
  (pel-c-utils-test--with-code "if (ptr != NULL)"
    (should-error (pel-c-search-equal-NULL))))

;; ===========================================================================
;; Search functions — pel-c-search-not-equal-NULL
;; ===========================================================================

(ert-deftest pel-c-utils-test/search-not-equal-NULL/finds-ne-NULL ()
  "`pel-c-search-not-equal-NULL' finds `ptr != NULL'."
  (pel-c-utils-test--with-code "if (ptr != NULL)"
    (should (pel-c-search-not-equal-NULL))))

(ert-deftest pel-c-utils-test/search-not-equal-NULL/finds-NULL-ne-variable ()
  "`pel-c-search-not-equal-NULL' finds `NULL != ptr'."
  (pel-c-utils-test--with-code "if (NULL != ptr)"
    (should (pel-c-search-not-equal-NULL))))

(ert-deftest pel-c-utils-test/search-not-equal-NULL/not-found-signals-error ()
  "`pel-c-search-not-equal-NULL' signals an error when pattern is absent."
  (pel-c-utils-test--with-code "if (ptr == NULL)"
    (should-error (pel-c-search-not-equal-NULL))))

;; ===========================================================================
;; Search functions — true/false
;; ===========================================================================

(ert-deftest pel-c-utils-test/search-equal-true/finds-lowercase ()
  "`pel-c-search-equal-true' finds `flag == true'."
  (pel-c-utils-test--with-code "if (flag == true)"
    (should (pel-c-search-equal-true))))

(ert-deftest pel-c-utils-test/search-equal-true/finds-uppercase ()
  "`pel-c-search-equal-true' finds `flag == TRUE'."
  (pel-c-utils-test--with-code "if (flag == TRUE)"
    (should (pel-c-search-equal-true))))

(ert-deftest pel-c-utils-test/search-equal-true/not-found-signals-error ()
  "`pel-c-search-equal-true' signals an error when pattern is absent."
  (pel-c-utils-test--with-code "if (flag == false)"
    (should-error (pel-c-search-equal-true))))

(ert-deftest pel-c-utils-test/search-not-equal-true/finds-ne-true ()
  "`pel-c-search-not-equal-true' finds `flag != true'."
  (pel-c-utils-test--with-code "if (flag != true)"
    (should (pel-c-search-not-equal-true))))

(ert-deftest pel-c-utils-test/search-not-equal-true/finds-ne-TRUE ()
  "`pel-c-search-not-equal-true' finds `flag != TRUE'."
  (pel-c-utils-test--with-code "if (flag != TRUE)"
    (should (pel-c-search-not-equal-true))))

(ert-deftest pel-c-utils-test/search-not-equal-true/not-found-signals-error ()
  "`pel-c-search-not-equal-true' signals an error when pattern is absent."
  (pel-c-utils-test--with-code "if (flag == true)"
    (should-error (pel-c-search-not-equal-true))))

(ert-deftest pel-c-utils-test/search-equal-false/finds-eq-false ()
  "`pel-c-search-equal-false' finds `flag == false'."
  (pel-c-utils-test--with-code "if (flag == false)"
    (should (pel-c-search-equal-false))))

(ert-deftest pel-c-utils-test/search-equal-false/finds-eq-FALSE ()
  "`pel-c-search-equal-false' finds `flag == FALSE'."
  (pel-c-utils-test--with-code "if (flag == FALSE)"
    (should (pel-c-search-equal-false))))

(ert-deftest pel-c-utils-test/search-equal-false/not-found-signals-error ()
  "`pel-c-search-equal-false' signals an error when pattern is absent."
  (pel-c-utils-test--with-code "if (flag != false)"
    (should-error (pel-c-search-equal-false))))

(ert-deftest pel-c-utils-test/search-not-equal-false/finds-ne-false ()
  "`pel-c-search-not-equal-false' finds `flag != false'."
  (pel-c-utils-test--with-code "if (flag != false)"
    (should (pel-c-search-not-equal-false))))

(ert-deftest pel-c-utils-test/search-not-equal-false/finds-ne-FALSE ()
  "`pel-c-search-not-equal-false' finds `flag != FALSE'."
  (pel-c-utils-test--with-code "if (flag != FALSE)"
    (should (pel-c-search-not-equal-false))))

(ert-deftest pel-c-utils-test/search-not-equal-false/not-found-signals-error ()
  "`pel-c-search-not-equal-false' signals an error when pattern is absent."
  (pel-c-utils-test--with-code "if (flag == false)"
    (should-error (pel-c-search-not-equal-false))))

;; ===========================================================================
;; Search functions — pel-c-search-any-comparison-problem
;; ===========================================================================

(ert-deftest pel-c-utils-test/search-any/finds-eq-NULL ()
  "`pel-c-search-any-comparison-problem' finds == NULL."
  (pel-c-utils-test--with-code "if (ptr == NULL)"
    (should (pel-c-search-any-comparison-problem))))

(ert-deftest pel-c-utils-test/search-any/finds-ne-NULL ()
  "`pel-c-search-any-comparison-problem' finds != NULL."
  (pel-c-utils-test--with-code "if (ptr != NULL)"
    (should (pel-c-search-any-comparison-problem))))

(ert-deftest pel-c-utils-test/search-any/finds-eq-true ()
  "`pel-c-search-any-comparison-problem' finds == true."
  (pel-c-utils-test--with-code "if (flag == true)"
    (should (pel-c-search-any-comparison-problem))))

(ert-deftest pel-c-utils-test/search-any/finds-ne-false ()
  "`pel-c-search-any-comparison-problem' finds != false."
  (pel-c-utils-test--with-code "if (flag != false)"
    (should (pel-c-search-any-comparison-problem))))

(ert-deftest pel-c-utils-test/search-any/not-found-signals-error ()
  "`pel-c-search-any-comparison-problem' signals an error when buffer has no
anti-pattern comparisons."
  (pel-c-utils-test--with-code "if (ptr)\nif (flag)\n"
    (should-error (pel-c-search-any-comparison-problem))))

;; ===========================================================================
;; Search functions — preprocessor
;; ===========================================================================

(ert-deftest pel-c-utils-test/search-preproc-if/finds-bare-if ()
  "`pel-c-search-preproc-if' finds a bare `#if VAR' directive."
  (pel-c-utils-test--with-code "#if MY_FLAG\n"
    (should (pel-c-search-preproc-if))))

(ert-deftest pel-c-utils-test/search-preproc-if/not-found-in-defined-form ()
  "`pel-c-search-preproc-if' does NOT find `#if defined(VAR)'."
  (pel-c-utils-test--with-code "#if defined(MY_FLAG)\n"
    (should-error (pel-c-search-preproc-if))))

(ert-deftest pel-c-utils-test/search-preproc-if-set/finds-eq-zero ()
  "`pel-c-search-preproc-if-set' finds `#if VAR == 0'."
  (pel-c-utils-test--with-code "#if MY_FLAG == 0\n"
    (should (pel-c-search-preproc-if-set))))

(ert-deftest pel-c-utils-test/search-preproc-if-set/finds-eq-one ()
  "`pel-c-search-preproc-if-set' finds `#if VAR == 1'."
  (pel-c-utils-test--with-code "#if MY_FLAG == 1\n"
    (should (pel-c-search-preproc-if-set))))

(ert-deftest pel-c-utils-test/search-preproc-if-set/not-found-in-bare-if ()
  "`pel-c-search-preproc-if-set' does NOT find a bare `#if VAR'."
  (pel-c-utils-test--with-code "#if MY_FLAG\n"
    (should-error (pel-c-search-preproc-if-set))))

;; ===========================================================================
;; pel-c-fix-comparison-problems
;;
;; Return value layout:
;;   (equal-NULL-count
;;    not-equal-NULL-count
;;    equal-false-count
;;    not-equal-false-count
;;    equal-true-count
;;    not-equal-true-count)
;;
;; Each test uses a buffer containing exactly one anti-pattern, verifies
;; the transformed buffer content AND the returned counts.
;; ===========================================================================

(defmacro pel-c-utils-test--fix-comparison (input
                                            expected-text
                                            expected-counts)
  "Run `pel-c-fix-comparison-problems' on INPUT.
Verify EXPECTED-TEXT and COUNTS."
  `(with-temp-buffer
     (insert ,input)
     (let ((counts (pel-c-fix-comparison-problems)))
       (should (string= (buffer-string) ,expected-text))
       (should (equal counts ,expected-counts)))))

;; ---------------------------------------------------------------------------
;; != NULL  →  remove the comparison, keeping the pointer/variable
;; ---------------------------------------------------------------------------

(ert-deftest pel-c-utils-test/fix-comparison/remove-ne-NULL ()
  "`!= NULL' is removed, leaving the tested pointer alone."
  (pel-c-utils-test--fix-comparison
   "if (ptr != NULL)\n"
   "if (ptr)\n"
   '(0 1 0 0 0 0)))

(ert-deftest pel-c-utils-test/fix-comparison/remove-NULL-ne ()
  "`NULL !=' (Yoda) is removed."
  (pel-c-utils-test--fix-comparison
   "if (NULL != ptr)\n"
   "if (ptr)\n"
   '(0 1 0 0 0 0)))

;; ---------------------------------------------------------------------------
;; == NULL  →  negate the pointer/variable
;; ---------------------------------------------------------------------------

(ert-deftest pel-c-utils-test/fix-comparison/negate-eq-NULL ()
  "`ptr == NULL' is replaced by `!ptr'."
  (pel-c-utils-test--fix-comparison
   "if (ptr == NULL)\n"
   "if (!ptr)\n"
   '(1 0 0 0 0 0)))

;; test the Yoda condition form - sometimes used for defensive programming
;; See; https://en.wikipedia.org/wiki/Yoda_conditions
(ert-deftest pel-c-utils-test/fix-comparison/negate-NULL-eq ()
  "`NULL == ptr' (Yoda form) is replaced by `!ptr'."
  (pel-c-utils-test--fix-comparison
   "if (NULL == ptr)\n"
   "if (!ptr)\n"
   '(1 0 0 0 0 0)))

;; ---------------------------------------------------------------------------
;; != false  →  keep the boolean, drop the comparison
;; ---------------------------------------------------------------------------

(ert-deftest pel-c-utils-test/fix-comparison/remove-ne-false ()
  "`flag != false' is replaced by `flag'."
  (pel-c-utils-test--fix-comparison
   "if (flag != false)\n"
   "if (flag)\n"
   '(0 0 0 1 0 0)))

(ert-deftest pel-c-utils-test/fix-comparison/remove-ne-FALSE ()
  "`flag != FALSE' is replaced by `flag'."
  (pel-c-utils-test--fix-comparison
   "if (flag != FALSE)\n"
   "if (flag)\n"
   '(0 0 0 1 0 0)))

;; ---------------------------------------------------------------------------
;; == false  →  negate the boolean
;; ---------------------------------------------------------------------------

(ert-deftest pel-c-utils-test/fix-comparison/negate-eq-false ()
  "`flag == false' is replaced by `!flag'."
  (pel-c-utils-test--fix-comparison
   "if (flag == false)\n"
   "if (!flag)\n"
   '(0 0 1 0 0 0)))

(ert-deftest pel-c-utils-test/fix-comparison/negate-eq-FALSE ()
  "`flag == FALSE' is replaced by `!flag'."
  (pel-c-utils-test--fix-comparison
   "if (flag == FALSE)\n"
   "if (!flag)\n"
   '(0 0 1 0 0 0)))

;; ---------------------------------------------------------------------------
;; != true  →  negate the boolean
;; ---------------------------------------------------------------------------

(ert-deftest pel-c-utils-test/fix-comparison/negate-ne-true ()
  "`flag != true' is replaced by `!flag'."
  (pel-c-utils-test--fix-comparison
   "if (flag != true)\n"
   "if (!flag)\n"
   '(0 0 0 0 0 1)))

(ert-deftest pel-c-utils-test/fix-comparison/negate-ne-TRUE ()
  "`flag != TRUE' is replaced by `!flag'."
  (pel-c-utils-test--fix-comparison
   "if (flag != TRUE)\n"
   "if (!flag)\n"
   '(0 0 0 0 0 1)))

;; ---------------------------------------------------------------------------
;; == true  →  keep the boolean, drop the comparison
;; ---------------------------------------------------------------------------

(ert-deftest pel-c-utils-test/fix-comparison/remove-eq-true ()
  "`flag == true' is replaced by `flag'."
  (pel-c-utils-test--fix-comparison
   "if (flag == true)\n"
   "if (flag)\n"
   '(0 0 0 0 1 0)))

(ert-deftest pel-c-utils-test/fix-comparison/remove-eq-TRUE ()
  "`flag == TRUE' is replaced by `flag'."
  (pel-c-utils-test--fix-comparison
   "if (flag == TRUE)\n"
   "if (flag)\n"
   '(0 0 0 0 1 0)))

;; ---------------------------------------------------------------------------
;; Helper for tests that need a real C syntax table.
;;
;; `syntax-ppss' classifies /* */, //, and "..." correctly only when the
;; buffer's syntax table knows about C comment/string syntax.  All tests
;; in this subsection therefore activate `c-mode'.
;; ---------------------------------------------------------------------------

(defmacro pel-c-utils-test--with-c-code (code &rest body)
  "Execute BODY in a `c-mode' temp buffer pre-loaded with CODE.
Point is left at the beginning of the buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (c-mode)
     (insert ,code)
     (goto-char (point-min))
     ,@body))

;; ---------------------------------------------------------------------------
;; Step 1 (!=NULL / NULL!=): text inside C block comments must NOT be mutated
;; ---------------------------------------------------------------------------

(ert-deftest pel-c-utils-test/fix-comparison/ne-NULL-in-block-comment-unchanged ()
  "`!= NULL' inside a C block comment is left untouched."
  (pel-c-utils-test--with-c-code
      "/* if (ptr != NULL) do something */\n"
    (let ((counts (pel-c-fix-comparison-problems)))
      (should (string= (buffer-string)
                       "/* if (ptr != NULL) do something */\n"))
      (should (equal counts '(0 0 0 0 0 0))))))

(ert-deftest pel-c-utils-test/fix-comparison/NULL-ne-in-block-comment-unchanged ()
  "`NULL !=' (Yoda) inside a C block comment is left untouched."
  (pel-c-utils-test--with-c-code
      "/* if (NULL != ptr) do something */\n"
    (let ((counts (pel-c-fix-comparison-problems)))
      (should (string= (buffer-string)
                       "/* if (NULL != ptr) do something */\n"))
      (should (equal counts '(0 0 0 0 0 0))))))

(ert-deftest pel-c-utils-test/fix-comparison/ne-NULL-in-line-comment-unchanged ()
  "`!= NULL' inside a C++ line comment is left untouched."
  (pel-c-utils-test--with-c-code
      "// if (ptr != NULL) then handle it\n"
    (let ((counts (pel-c-fix-comparison-problems)))
      (should (string= (buffer-string)
                       "// if (ptr != NULL) then handle it\n"))
      (should (equal counts '(0 0 0 0 0 0))))))

;; ---------------------------------------------------------------------------
;; Step 1: text inside C string literals must NOT be mutated
;; ---------------------------------------------------------------------------

(ert-deftest pel-c-utils-test/fix-comparison/ne-NULL-in-string-unchanged ()
  "`!= NULL' inside a C string literal is left untouched."
  (pel-c-utils-test--with-c-code
      "const char *msg = \"ptr != NULL means valid\";\n"
    (let ((counts (pel-c-fix-comparison-problems)))
      (should (string= (buffer-string)
                       "const char *msg = \"ptr != NULL means valid\";\n"))
      (should (equal counts '(0 0 0 0 0 0))))))

(ert-deftest pel-c-utils-test/fix-comparison/NULL-ne-in-string-unchanged ()
  "`NULL !=' inside a C string literal is left untouched."
  (pel-c-utils-test--with-c-code
      "const char *msg = \"NULL != ptr check\";\n"
    (let ((counts (pel-c-fix-comparison-problems)))
      (should (string= (buffer-string)
                       "const char *msg = \"NULL != ptr check\";\n"))
      (should (equal counts '(0 0 0 0 0 0))))))

;; ---------------------------------------------------------------------------
;; Step 1: mixed — real anti-pattern in code AND identical text in a comment.
;; Only the code occurrence must be fixed; the comment must be preserved.
;; This is the key regression test for the syntax-ppss guard.
;; ---------------------------------------------------------------------------

(ert-deftest pel-c-utils-test/fix-comparison/ne-NULL-code-fixed-comment-preserved ()
  "Real `!= NULL' in code is fixed; identical text in a comment is preserved."
  (pel-c-utils-test--with-c-code
      "if (ptr != NULL)  /* ptr != NULL should stay */\n"
    (let ((counts (pel-c-fix-comparison-problems)))
      (should (string= (buffer-string)
                       "if (ptr)  /* ptr != NULL should stay */\n"))
      (should (equal counts '(0 1 0 0 0 0))))))

(ert-deftest pel-c-utils-test/fix-comparison/ne-NULL-code-fixed-string-preserved ()
  "Real `!= NULL' in code is fixed; identical text in a string is preserved."
  (pel-c-utils-test--with-c-code
      "if (ptr != NULL) log(\"ptr != NULL\");\n"
    (let ((counts (pel-c-fix-comparison-problems)))
      (should (string= (buffer-string)
                       "if (ptr) log(\"ptr != NULL\");\n"))
      (should (equal counts '(0 1 0 0 0 0))))))

;; ---------------------------------------------------------------------------
;; Steps 2–11 (== NULL, == false, etc.): uniform comment/string protection.
;; These steps were already guarded before the current fix; the tests below
;; document the expected invariant across all fix steps.
;; ---------------------------------------------------------------------------

(ert-deftest pel-c-utils-test/fix-comparison/eq-NULL-in-comment-unchanged ()
  "`== NULL' inside a C block comment is left untouched."
  (pel-c-utils-test--with-c-code
      "/* ptr == NULL in old code */\n"
    (let ((counts (pel-c-fix-comparison-problems)))
      (should (string= (buffer-string)
                       "/* ptr == NULL in old code */\n"))
      (should (equal counts '(0 0 0 0 0 0))))))

(ert-deftest pel-c-utils-test/fix-comparison/eq-false-in-string-unchanged ()
  "`== false' inside a C string literal is left untouched."
  (pel-c-utils-test--with-c-code
      "puts(\"flag == false means off\");\n"
    (let ((counts (pel-c-fix-comparison-problems)))
      (should (string= (buffer-string)
                       "puts(\"flag == false means off\");\n"))
      (should (equal counts '(0 0 0 0 0 0))))))

(ert-deftest pel-c-utils-test/fix-comparison/ne-true-in-comment-unchanged ()
  "`!= true' inside a C line comment is left untouched."
  (pel-c-utils-test--with-c-code
      "// if (flag != true) then ...\n"
    (let ((counts (pel-c-fix-comparison-problems)))
      (should (string= (buffer-string)
                       "// if (flag != true) then ...\n"))
      (should (equal counts '(0 0 0 0 0 0))))))

;; ---------------------------------------------------------------------------
;; Clean buffer: no changes expected
;; ---------------------------------------------------------------------------

(ert-deftest pel-c-utils-test/fix-comparison/clean-code-unchanged ()
  "A buffer with no comparison anti-patterns is left untouched."
  (pel-c-utils-test--fix-comparison
   "if (ptr)\nif (!ptr)\nif (flag)\nif (!flag)\n"
   "if (ptr)\nif (!ptr)\nif (flag)\nif (!flag)\n"
   '(0 0 0 0 0 0)))

;; ===========================================================================
;; pel-c-fix-preproc-if-problems
;;
;; Return value layout:  (changed-count-if  changed-count-if-set)
;;   changed-count-if     : replacements of bare `#if VAR'
;;   changed-count-if-set : replacements of `#if VAR == 0' or `#if VAR == 1'
;; ===========================================================================

(defmacro pel-c-utils-test--fix-preproc (input expected-text expected-counts)
  "Run `pel-c-fix-preproc-if-problems' on INPUT, verify EXPECTED-TEXT and COUNTS."
  `(with-temp-buffer
     (insert ,input)
     (let ((counts (pel-c-fix-preproc-if-problems)))
       (should (string= (buffer-string) ,expected-text))
       (should (equal counts ,expected-counts)))))

;; ---------------------------------------------------------------------------
;; #if VAR  →  #if (defined(VAR) && (VAR != 0))
;; ---------------------------------------------------------------------------

(ert-deftest pel-c-utils-test/fix-preproc-if/bare-if-var ()
  "A bare `#if VAR' is rewritten to check defined and non-zero."
  (pel-c-utils-test--fix-preproc
   "#if MY_FLAG\n"
   "#if (defined(MY_FLAG) && (MY_FLAG != 0))\n"
   '(1 0)))

(ert-deftest pel-c-utils-test/fix-preproc-if/bare-if-var-with-indent ()
  "A bare `#if VAR' with leading blanks preserves indentation."
  (pel-c-utils-test--fix-preproc
   "  #if MY_FLAG\n"
   "  #if (defined(MY_FLAG) && (MY_FLAG != 0))\n"
   '(1 0)))

(ert-deftest pel-c-utils-test/fix-preproc-if/bare-if-var-hash-space ()
  "A bare `# if VAR' (space after #) preserves that style."
  (pel-c-utils-test--fix-preproc
   "# if MY_FLAG\n"
   "# if (defined(MY_FLAG) && (MY_FLAG != 0))\n"
   '(1 0)))

;; ---------------------------------------------------------------------------
;; #if VAR == 0  →  #if (!defined(VAR) || (VAR == 0))
;; ---------------------------------------------------------------------------

(ert-deftest pel-c-utils-test/fix-preproc-if/if-var-eq-zero ()
  "`#if VAR == 0' is rewritten to check not-defined or equal-to-zero."
  (pel-c-utils-test--fix-preproc
   "#if MY_FLAG == 0\n"
   "#if (!defined(MY_FLAG) || (MY_FLAG == 0))\n"
   '(0 1)))

(ert-deftest pel-c-utils-test/fix-preproc-if/if-var-eq-zero-no-spaces ()
  "`#if VAR==0' (no spaces around ==) is also rewritten."
  (pel-c-utils-test--fix-preproc
   "#if MY_FLAG==0\n"
   "#if (!defined(MY_FLAG) || (MY_FLAG == 0))\n"
   '(0 1)))

;; ---------------------------------------------------------------------------
;; #if VAR == 1  →  #if (defined(VAR) && (VAR == 1))
;; ---------------------------------------------------------------------------

(ert-deftest pel-c-utils-test/fix-preproc-if/if-var-eq-one ()
  "`#if VAR == 1' is rewritten to check defined and equal-to-one."
  (pel-c-utils-test--fix-preproc
   "#if MY_FLAG == 1\n"
   "#if (defined(MY_FLAG) && (MY_FLAG == 1))\n"
   '(0 1)))

(ert-deftest pel-c-utils-test/fix-preproc-if/if-var-eq-one-no-spaces ()
  "`#if VAR==1' (no spaces around ==) is also rewritten."
  (pel-c-utils-test--fix-preproc
   "#if MY_FLAG==1\n"
   "#if (defined(MY_FLAG) && (MY_FLAG == 1))\n"
   '(0 1)))

;; ---------------------------------------------------------------------------
;; Already-correct form: no changes expected
;; ---------------------------------------------------------------------------

(ert-deftest pel-c-utils-test/fix-preproc-if/already-correct-defined-form ()
  "A `#if (defined(VAR) && ...)' directive is left untouched."
  (pel-c-utils-test--fix-preproc
   "#if (defined(MY_FLAG) && (MY_FLAG != 0))\n"
   "#if (defined(MY_FLAG) && (MY_FLAG != 0))\n"
   '(0 0)))

(ert-deftest pel-c-utils-test/fix-preproc-if/multiple-directives ()
  "Multiple anti-pattern directives on separate lines are all fixed."
  (pel-c-utils-test--fix-preproc
   "#if FLAG_A\n#if FLAG_B\n"
   "#if (defined(FLAG_A) && (FLAG_A != 0))\n#if (defined(FLAG_B) && (FLAG_B != 0))\n"
   '(2 0)))

;;; --------------------------------------------------------------------------
(provide 'pel-c-utils-test)

;;; pel-c-utils-test.el ends here
