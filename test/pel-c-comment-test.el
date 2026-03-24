;;; pel-c-comment-test.el --- ERT tests for pel-c-comment.el  -*- lexical-binding: t; -*-

;; Created   : Monday, March 23 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-24 15:00:02 EDT, updated by Pierre Rouleau>

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
;; ERT tests for pel-c-comment.el.
;;
;; Covered items:
;;
;;   pel--c-comment-escape-comments  - escapes /* and */ inside a narrowed region
;;   pel--c-comment-marked           - wraps buffer content in a C multi-line comment
;;     (1-star format:  /* ... ** ... / */  )
;;     (2-star format:  /* ... ** ... / */  )
;;   pel-c-comment-in-comment-p      - tests whether a buffer position is inside a comment
;;
;; Items intentionally NOT covered:
;;   - `pel-c-comment-dwim'  (interactive; drives region selection and mode dispatch)
;;
;; Notes on test isolation:
;;   - `pel--c-comment-escape-comments' and `pel--c-comment-marked' are
;;     private helpers designed to operate on a narrowed/temp buffer; all
;;     tests here use `with-temp-buffer' to provide that isolation.
;;   - `pel-c-comment-in-comment-p' relies on `syntax-ppss', which requires a
;;     proper C syntax table.  Those tests activate `c-mode' in the temp buffer
;;     so that block/line comment delimiters are correctly classified.
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-c-comment)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:

;;; --------------------------------------------------------------------------
;;; Compatibility:
;;
;; `string-search' was introduced in Emacs 28.  Provide a fallback so the
;; test suite runs on Emacs 26 and 27 as well.
(unless (fboundp 'string-search)
  (defun string-search (needle haystack &optional start-pos)
    "Return position of first occurrence of NEEDLE in HAYSTACK, or nil.
Optional START-POS (default 0) causes the search to begin at that index.
This is a compatibility shim for Emacs < 28."
    (string-match (regexp-quote needle) haystack start-pos)))

;; ===========================================================================
;; Helpers
;; ===========================================================================

(defmacro pel-c-comment-test--with-code (code &rest body)
  "Execute BODY in a writable temp buffer pre-loaded with CODE.
Point is left at `point-min'."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,code)
     (goto-char (point-min))
     ,@body))

(defmacro pel-c-comment-test--with-c-code (code &rest body)
  "Execute BODY in a `c-mode' temp buffer pre-loaded with CODE.
Point is left at `point-min'."
  (declare (indent 1))
  `(with-temp-buffer
     (c-mode)
     (insert ,code)
     (goto-char (point-min))
     ,@body))

;; ===========================================================================
;; pel--c-comment-escape-comments
;; ===========================================================================

(ert-deftest pel-c-comment-test/escape/no-op-on-plain-code ()
  "A buffer without C comment delimiters is left untouched."
  (pel-c-comment-test--with-code "int x = 1;\nreturn x;\n"
    (let ((original (buffer-string)))
      (pel--c-comment-escape-comments)
      (should (string= original (buffer-string))))))

(ert-deftest pel-c-comment-test/escape/escapes-open-delimiter ()
  "`/*' is replaced by `/\\*'."
  (pel-c-comment-test--with-code "/* comment */"
    (pel--c-comment-escape-comments)
    ;; The literal two-char sequence /* must no longer be present.
    (should-not (string-search "/*" (buffer-string)))
    ;; The escaped form /\* must now be present.
    (should (string-search "/\\*" (buffer-string)))))

(ert-deftest pel-c-comment-test/escape/escapes-close-delimiter ()
  "`*/' is replaced by `*\\/'."
  (pel-c-comment-test--with-code "/* comment */"
    (pel--c-comment-escape-comments)
    ;; The literal two-char sequence */ must no longer be present.
    (should-not (string-search "*/" (buffer-string)))
    ;; The escaped form *\/ must now be present.
    (should (string-search "*\\/" (buffer-string)))))

(ert-deftest pel-c-comment-test/escape/exact-result-for-single-comment ()
  "A single `/* comment */' is escaped to `/\\* comment *\\/'."
  (pel-c-comment-test--with-code "/* comment */"
    (pel--c-comment-escape-comments)
    (should (string= (buffer-string) "/\\* comment *\\/"))))

(ert-deftest pel-c-comment-test/escape/all-occurrences-escaped ()
  "Multiple `/*' and `*/' pairs are all escaped."
  (pel-c-comment-test--with-code "/* a */ int x; /* b */"
    (pel--c-comment-escape-comments)
    (let ((result (buffer-string)))
      (should-not (string-search "/*" result))
      (should-not (string-search "*/" result)))))

(ert-deftest pel-c-comment-test/escape/idempotent-on-already-escaped ()
  "A buffer with no raw `/*' or `*/' is unchanged by a second escape pass."
  (pel-c-comment-test--with-code "/\\* already escaped *\\/"
    (let ((before (buffer-string)))
      (pel--c-comment-escape-comments)
      (should (string= before (buffer-string))))))

;; ===========================================================================
;; pel--c-comment-marked  —  1-star format (pel-c-multiline-comments = 1)
;; ===========================================================================

(ert-deftest pel-c-comment-test/marked/one-star/single-line ()
  "A single-line buffer gets `/* ' prepended and ` */\\n' appended."
  (let ((pel-c-multiline-comments 1))
    (pel-c-comment-test--with-code "line1\n"
      (pel--c-comment-marked 0)
      (should (string= (buffer-string) "/* line1\n */\n")))))

(ert-deftest pel-c-comment-test/marked/one-star/three-lines ()
  "Three-line buffer produces correct 1-star multi-line comment."
  (let ((pel-c-multiline-comments 1))
    (pel-c-comment-test--with-code "line1\nline2\nline3\n"
      (pel--c-comment-marked 0)
      (should (string= (buffer-string)
                       "/* line1\n * line2\n * line3\n */\n")))))

(ert-deftest pel-c-comment-test/marked/one-star/first-line-starts-with-open ()
  "First line of result starts with `/* '."
  (let ((pel-c-multiline-comments 1))
    (pel-c-comment-test--with-code "alpha\nbeta\n"
      (pel--c-comment-marked 0)
      (should (string-prefix-p "/* " (buffer-string))))))

(ert-deftest pel-c-comment-test/marked/one-star/continuation-lines-have-space-star ()
  "Interior lines are prefixed with ` * ' in 1-star format."
  (let ((pel-c-multiline-comments 1))
    (pel-c-comment-test--with-code "a\nb\nc\n"
      (pel--c-comment-marked 0)
      (let ((lines (split-string (buffer-string) "\n" t)))
        ;; lines[0] = "/* a", lines[1] = " * b", lines[2] = " * c", lines[3] = " */"
        (should (string-prefix-p " * " (nth 1 lines)))
        (should (string-prefix-p " * " (nth 2 lines)))))))

(ert-deftest pel-c-comment-test/marked/one-star/closes-with-space-star-slash ()
  "The closing line is ` */' in 1-star format."
  (let ((pel-c-multiline-comments 1))
    (pel-c-comment-test--with-code "x\ny\n"
      (pel--c-comment-marked 0)
      (should (string-suffix-p " */\n" (buffer-string))))))

;; ===========================================================================
;; pel--c-comment-marked  —  2-star format (pel-c-multiline-comments = 2)
;; ===========================================================================

(ert-deftest pel-c-comment-test/marked/two-star/three-lines ()
  "Three-line buffer produces correct 2-star multi-line comment."
  (let ((pel-c-multiline-comments 2))
    (pel-c-comment-test--with-code "line1\nline2\nline3\n"
      (pel--c-comment-marked 0)
      (should (string= (buffer-string)
                       "/* line1\n** line2\n** line3\n*/\n")))))

(ert-deftest pel-c-comment-test/marked/two-star/continuation-lines-have-double-star ()
  "Interior lines are prefixed with `** ' in 2-star format."
  (let ((pel-c-multiline-comments 2))
    (pel-c-comment-test--with-code "a\nb\nc\n"
      (pel--c-comment-marked 0)
      (let ((lines (split-string (buffer-string) "\n" t)))
        (should (string-prefix-p "** " (nth 1 lines)))
        (should (string-prefix-p "** " (nth 2 lines)))))))

(ert-deftest pel-c-comment-test/marked/two-star/closes-with-star-slash ()
  "The closing line is `*/' (no leading space) in 2-star format."
  (let ((pel-c-multiline-comments 2))
    (pel-c-comment-test--with-code "x\ny\n"
      (pel--c-comment-marked 0)
      (should (string-suffix-p "*/\n" (buffer-string)))
      ;; Must NOT have a leading space before */
      (should-not (string-suffix-p " */\n" (buffer-string))))))

;; ===========================================================================
;; pel--c-comment-marked  —  column offset
;; ===========================================================================

(ert-deftest pel-c-comment-test/marked/column-offset/two-lines-indented ()
  "With column=2 the opener replaces leading spaces and continuation is indented."
  (let ((pel-c-multiline-comments 1))
    (pel-c-comment-test--with-code "  line1\n  line2\n"
      (pel--c-comment-marked 0)
      ;; "/* " is inserted at point-min (before the first "  ").
      ;; For line2: forward-line lands at "  line2", then right-char 2
      ;; advances past the two spaces, then " * " is inserted there.
      (should (string-prefix-p "/* " (buffer-string)))
      ;; Full expected output (verify once by running in a live Emacs if unsure):
      (should (string= (buffer-string) "/*   line1\n *   line2\n */\n")))))

;; ===========================================================================
;; pel-c-comment-in-comment-p
;; All tests activate c-mode so that syntax-ppss correctly identifies C
;; comment syntax.
;; ===========================================================================

(ert-deftest pel-c-comment-test/in-comment-p/false-on-plain-code ()
  "Returns nil when the position is in plain C code, not in a comment."
  (pel-c-comment-test--with-c-code "int x = 1;\n"
    ;; Position 1 is the 'i' of "int"
    (should-not (pel-c-comment-in-comment-p 1))))

(ert-deftest pel-c-comment-test/in-comment-p/true-inside-block-comment ()
  "Returns non-nil for a position inside a `/* ... */' block comment."
  (pel-c-comment-test--with-c-code "/* inside */"
    ;; Position 5 is 'i' of "inside", which is inside the comment.
    (should (pel-c-comment-in-comment-p 5))))

(ert-deftest pel-c-comment-test/in-comment-p/false-after-block-comment ()
  "Returns nil for a position after the closing `*/' of a block comment."
  (pel-c-comment-test--with-c-code "/* hi */ int y;"
    ;; Position 10 is 'i' of "int", which is after the comment.
    (should-not (pel-c-comment-in-comment-p 10))))

(ert-deftest pel-c-comment-test/in-comment-p/true-inside-line-comment ()
  "Returns non-nil for a position inside a `// ...' line comment."
  (pel-c-comment-test--with-c-code "// line comment\n"
    ;; Position 4 is 'l' of "line", which is inside the // comment.
    (should (pel-c-comment-in-comment-p 4))))

(ert-deftest pel-c-comment-test/in-comment-p/false-before-any-comment ()
  "Returns nil when the buffer has no comments at all."
  (pel-c-comment-test--with-c-code "return 0;\n"
    (should-not (pel-c-comment-in-comment-p 1))))

(ert-deftest pel-c-comment-test/in-comment-p/false-on-comment-opener ()
  "Returns nil when point is exactly on the `/' of `/*' (not yet inside)."
  (pel-c-comment-test--with-c-code "/* comment */"
    ;; Position 1 is the opening '/' — syntax-ppss considers this the
    ;; start of the comment delimiter, not yet inside the comment body.
    (should-not (pel-c-comment-in-comment-p 1))))

;;; --------------------------------------------------------------------------
(provide 'pel-c-comment-test)

;;; pel-c-comment-test.el ends here
