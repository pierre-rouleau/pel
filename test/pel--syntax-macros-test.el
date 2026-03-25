;;; pel--syntax-macros-test.el --- ERT tests for pel--syntax-macros.el  -*- lexical-binding: t; -*-

;; Created   : Tuesday, March 24 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-25 11:39:13 EDT, updated by Pierre Rouleau>

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
;; ERT tests for pel--syntax-macros.el.
;;
;; Covered items:
;;
;;   Internal macros (accept a raw syntax-ppss result list):
;;     pel--inside-string-p   - non-nil when nth 3 of syntax list is non-nil
;;     pel--inside-block-p    - non-nil when nth 0 of syntax list is > 0
;;     pel--inside-comment-p  - non-nil when nth 4 of syntax list is non-nil
;;     pel--open-parens-pos   - returns nth 9 of syntax list
;;
;;   Public predicate functions (call syntax-ppss internally):
;;     pel-inside-string-p    - non-nil if POS or point is inside a string
;;     pel-inside-comment-p   - non-nil if POS or point is inside a comment
;;     pel-inside-block-p     - non-nil if POS or point is inside a code block;
;;                              returns nil inside strings even when depth > 0
;;     pel-inside-code-p      - t if POS or point is in plain code (not
;;                              inside a string or comment)
;;
;; Items intentionally NOT covered:
;;   - There are no interactive commands or file-I/O functions in this module.
;;     All testable items are covered above.
;;
;; Notes on test isolation:
;;   - Internal macro tests use hand-crafted syntax-ppss-shaped lists; no
;;     real buffers are required, and no modes need to be activated.
;;   - Public function tests use real `emacs-lisp-mode' temp buffers so that
;;     `syntax-ppss' correctly classifies Emacs Lisp string and comment
;;     syntax.  `c-mode' is used for the block-comment `pel-inside-comment-p'
;;     test.
;;   - `pel-inside-block-p' has special behavior: it returns nil when point
;;     is inside a string even if the paren depth is > 0.  A dedicated test
;;     verifies this.
;;
;; One subtlety worth calling out: pel-inside-block-p explicitly returns nil when
;; inside a string — even when the paren depth is > 0 e.g., position inside
;; "(foo)" that is itself inside (setq x "(foo)")). That special case gets its
;; own dedicated test.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--syntax-macros)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:

;; ===========================================================================
;; Helpers
;; ===========================================================================

;; ---------------------------------------------------------------------------
;; Synthetic syntax-ppss result lists
;;
;; syntax-ppss returns a list whose relevant indexed elements are:
;;   nth 0  - paren depth (integer)
;;   nth 3  - inside-string: nil or the string-delimiter character
;;   nth 4  - inside-comment: nil, t (non-nestable), or integer (nestable)
;;   nth 9  - list of open-paren positions (outermost first)
;; All other indices are set to nil in the synthetic lists below.

(defconst pel--syntax-macros-test--plain-code
  '(0 nil nil nil nil nil nil nil nil nil nil)
  "Synthetic syntax-ppss result: plain code, depth 0, no string/comment.")

(defconst pel--syntax-macros-test--in-string
  '(0 nil nil ?\" nil nil nil nil nil nil nil)
  "Synthetic syntax-ppss result: inside a double-quoted string.")

(defconst pel--syntax-macros-test--in-comment
  '(0 nil nil nil t nil nil nil nil nil nil)
  "Synthetic syntax-ppss result: inside a non-nestable comment.")

(defconst pel--syntax-macros-test--in-nested-comment
  '(0 nil nil nil 2 nil nil nil nil nil nil)
  "Synthetic syntax-ppss result: inside a comment nested 2 levels deep.")

(defconst pel--syntax-macros-test--in-block-depth1
  '(1 nil nil nil nil nil nil nil nil (10) nil)
  "Synthetic syntax-ppss result: depth 1, open paren at position 10.")

(defconst pel--syntax-macros-test--in-block-depth2
  '(2 nil nil nil nil nil nil nil nil (5 10) nil)
  "Synthetic syntax-ppss result: depth 2, open parens at positions 5 and 10.")

;; ---------------------------------------------------------------------------
;; Buffer helpers

(defmacro pel--syntax-macros-test--with-elisp (code &rest body)
  "Execute BODY in an `emacs-lisp-mode' temp buffer pre-loaded with CODE.
Point is at `point-min' on entry."
  (declare (indent 1))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,code)
     (goto-char (point-min))
     ,@body))

(defmacro pel--syntax-macros-test--with-c (code &rest body)
  "Execute BODY in a `c-mode' temp buffer pre-loaded with CODE.
Point is at `point-min' on entry."
  (declare (indent 1))
  `(with-temp-buffer
     (c-mode)
     (insert ,code)
     (goto-char (point-min))
     ,@body))

;; ===========================================================================
;; pel--inside-string-p  (internal macro)
;; ===========================================================================

(ert-deftest pel--syntax-macros-test/inside-string-p/nil-in-plain-code ()
  "`pel--inside-string-p' returns nil for plain-code syntax."
  (should-not (pel--inside-string-p pel--syntax-macros-test--plain-code)))

(ert-deftest pel--syntax-macros-test/inside-string-p/nil-in-comment ()
  "`pel--inside-string-p' returns nil when inside a comment."
  (should-not (pel--inside-string-p pel--syntax-macros-test--in-comment)))

(ert-deftest pel--syntax-macros-test/inside-string-p/non-nil-in-string ()
  "`pel--inside-string-p' returns non-nil when inside a string."
  (should (pel--inside-string-p pel--syntax-macros-test--in-string)))

(ert-deftest pel--syntax-macros-test/inside-string-p/returns-delimiter-char ()
  "`pel--inside-string-p' returns the string delimiter character (not just t)."
  (should (eq ?\" (pel--inside-string-p pel--syntax-macros-test--in-string))))

(ert-deftest pel--syntax-macros-test/inside-string-p/nil-in-block ()
  "`pel--inside-string-p' returns nil when inside a block (no string)."
  (should-not (pel--inside-string-p pel--syntax-macros-test--in-block-depth1)))

;; ===========================================================================
;; pel--inside-block-p  (internal macro)
;; ===========================================================================

(ert-deftest pel--syntax-macros-test/inside-block-p/nil-at-depth-zero ()
  "`pel--inside-block-p' returns nil when paren depth is 0."
  (should-not (pel--inside-block-p pel--syntax-macros-test--plain-code)))

(ert-deftest pel--syntax-macros-test/inside-block-p/nil-in-comment ()
  "`pel--inside-block-p' returns nil at depth 0 even when in a comment."
  (should-not (pel--inside-block-p pel--syntax-macros-test--in-comment)))

(ert-deftest pel--syntax-macros-test/inside-block-p/non-nil-at-depth-one ()
  "`pel--inside-block-p' returns non-nil when paren depth is 1."
  (should (pel--inside-block-p pel--syntax-macros-test--in-block-depth1)))

(ert-deftest pel--syntax-macros-test/inside-block-p/non-nil-at-depth-two ()
  "`pel--inside-block-p' returns non-nil when paren depth is 2."
  (should (pel--inside-block-p pel--syntax-macros-test--in-block-depth2)))

;; ===========================================================================
;; pel--inside-comment-p  (internal macro)
;; ===========================================================================

(ert-deftest pel--syntax-macros-test/inside-comment-p/nil-in-plain-code ()
  "`pel--inside-comment-p' returns nil for plain-code syntax."
  (should-not (pel--inside-comment-p pel--syntax-macros-test--plain-code)))

(ert-deftest pel--syntax-macros-test/inside-comment-p/nil-in-string ()
  "`pel--inside-comment-p' returns nil when inside a string."
  (should-not (pel--inside-comment-p pel--syntax-macros-test--in-string)))

(ert-deftest pel--syntax-macros-test/inside-comment-p/non-nil-in-comment ()
  "`pel--inside-comment-p' returns non-nil for a non-nestable comment."
  (should (pel--inside-comment-p pel--syntax-macros-test--in-comment)))

(ert-deftest pel--syntax-macros-test/inside-comment-p/t-for-non-nestable ()
  "`pel--inside-comment-p' returns t (not an integer) for a non-nestable comment."
  (should (eq t (pel--inside-comment-p pel--syntax-macros-test--in-comment))))

(ert-deftest pel--syntax-macros-test/inside-comment-p/integer-for-nested ()
  "`pel--inside-comment-p' returns an integer for a nestable (depth > 1) comment."
  (let ((result (pel--inside-comment-p pel--syntax-macros-test--in-nested-comment)))
    (should result)
    (should (integerp result))))

(ert-deftest pel--syntax-macros-test/inside-comment-p/nil-in-block ()
  "`pel--inside-comment-p' returns nil when inside a paren block (no comment)."
  (should-not (pel--inside-comment-p pel--syntax-macros-test--in-block-depth1)))

;; ===========================================================================
;; pel--open-parens-pos  (internal macro)
;; ===========================================================================

(ert-deftest pel--syntax-macros-test/open-parens-pos/nil-at-depth-zero ()
  "`pel--open-parens-pos' returns nil when not inside any paren block."
  (should (eq nil (pel--open-parens-pos pel--syntax-macros-test--plain-code))))

(ert-deftest pel--syntax-macros-test/open-parens-pos/single-paren ()
  "`pel--open-parens-pos' returns a one-element list at depth 1."
  (let ((result (pel--open-parens-pos pel--syntax-macros-test--in-block-depth1)))
    (should (listp result))
    (should (= 1 (length result)))
    (should (= 10 (car result)))))

(ert-deftest pel--syntax-macros-test/open-parens-pos/two-parens ()
  "`pel--open-parens-pos' returns a two-element list at depth 2, outermost first."
  (let ((result (pel--open-parens-pos pel--syntax-macros-test--in-block-depth2)))
    (should (listp result))
    (should (= 2 (length result)))
    ;; Outermost open paren first (position 5), innermost second (position 10).
    (should (= 5  (nth 0 result)))
    (should (= 10 (nth 1 result)))))

(ert-deftest pel--syntax-macros-test/open-parens-pos/nil-in-string ()
  "`pel--open-parens-pos' returns nil when inside a string (depth 0)."
  (should (eq nil (pel--open-parens-pos pel--syntax-macros-test--in-string))))

;; ===========================================================================
;; pel-inside-string-p  (public function)
;; ===========================================================================

(ert-deftest pel--syntax-macros-test/public/inside-string-p/nil-in-code ()
  "`pel-inside-string-p' returns nil for a position in plain code."
  (pel--syntax-macros-test--with-elisp "(setq x 1)\n"
    ;; Position 2 = 's' of setq — plain code.
    (should-not (pel-inside-string-p 2))))

(ert-deftest pel--syntax-macros-test/public/inside-string-p/non-nil-in-string ()
  "`pel-inside-string-p' returns non-nil for a position inside a string literal."
  (pel--syntax-macros-test--with-elisp "(setq x \"hello\")\n"
    ;; Position 10 = 'h' of "hello" — inside the string.
    (should (pel-inside-string-p 10))))

(ert-deftest pel--syntax-macros-test/public/inside-string-p/nil-at-opening-quote ()
  "`pel-inside-string-p' returns nil on the opening quote character itself."
  (pel--syntax-macros-test--with-elisp "(setq x \"hello\")\n"
    ;; Position 9 = the opening `"' — syntax-ppss classifies this as the
    ;; start, not yet inside.
    (should-not (pel-inside-string-p 9))))

(ert-deftest pel--syntax-macros-test/public/inside-string-p/nil-in-comment ()
  "`pel-inside-string-p' returns nil for a position inside a comment."
  (pel--syntax-macros-test--with-elisp "; a comment\n"
    ;; Position 4 = 'c' of "comment" — inside the ; comment.
    (should-not (pel-inside-string-p 4))))

(ert-deftest pel--syntax-macros-test/public/inside-string-p/defaults-to-point ()
  "`pel-inside-string-p' uses point when POS is omitted."
  (pel--syntax-macros-test--with-elisp "(setq x \"hello\")\n"
    (goto-char 10)
    (should (eq (pel-inside-string-p)
                (pel-inside-string-p 10)))))

;; ===========================================================================
;; pel-inside-comment-p  (public function)
;; ===========================================================================

(ert-deftest pel--syntax-macros-test/public/inside-comment-p/nil-in-code ()
  "`pel-inside-comment-p' returns nil for a position in plain code."
  (pel--syntax-macros-test--with-elisp "(+ 1 2)\n"
    (should-not (pel-inside-comment-p 2))))

(ert-deftest pel--syntax-macros-test/public/inside-comment-p/nil-in-string ()
  "`pel-inside-comment-p' returns nil when inside a string."
  (pel--syntax-macros-test--with-elisp "\"no; comment here\"\n"
    ;; Position 5 = 'c' after ';' inside the string.
    (should-not (pel-inside-comment-p 5))))

(ert-deftest pel--syntax-macros-test/public/inside-comment-p/non-nil-in-line-comment ()
  "`pel-inside-comment-p' returns non-nil for a position inside a ; comment."
  (pel--syntax-macros-test--with-elisp "; a comment\n"
    ;; Position 3 = 'a' inside the comment.
    (should (pel-inside-comment-p 3))))

(ert-deftest pel--syntax-macros-test/public/inside-comment-p/t-for-line-comment ()
  "`pel-inside-comment-p' returns t (not an integer) for a Lisp line comment."
  (pel--syntax-macros-test--with-elisp "; comment\n"
    (should (eq t (pel-inside-comment-p 3)))))

(ert-deftest pel--syntax-macros-test/public/inside-comment-p/non-nil-in-c-block-comment ()
  "`pel-inside-comment-p' returns non-nil inside a C /* ... */ block comment."
  (pel--syntax-macros-test--with-c "/* inside */\n"
    ;; Position 5 = 'i' of "inside" — inside the block comment.
    (should (pel-inside-comment-p 5))))

(ert-deftest pel--syntax-macros-test/public/inside-comment-p/nil-after-c-block-comment ()
  "`pel-inside-comment-p' returns nil for a position after a C block comment."
  (pel--syntax-macros-test--with-c "/* hi */ int x;\n"
    ;; Position 10 = 'i' of "int" — after the comment.
    (should-not (pel-inside-comment-p 10))))

(ert-deftest pel--syntax-macros-test/public/inside-comment-p/defaults-to-point ()
  "`pel-inside-comment-p' uses point when POS is omitted."
  (pel--syntax-macros-test--with-elisp "; comment\n"
    (goto-char 3)
    (should (eq (pel-inside-comment-p)
                (pel-inside-comment-p 3)))))

;; ===========================================================================
;; pel-inside-block-p  (public function)
;; ===========================================================================

(ert-deftest pel--syntax-macros-test/public/inside-block-p/nil-in-top-level-code ()
  "`pel-inside-block-p' returns nil when not inside any paren block."
  (pel--syntax-macros-test--with-elisp "setq x 1\n"
    (should-not (pel-inside-block-p 2))))

(ert-deftest pel--syntax-macros-test/public/inside-block-p/non-nil-inside-parens ()
  "`pel-inside-block-p' returns non-nil when inside a paren-delimited form."
  (pel--syntax-macros-test--with-elisp "(setq x 1)\n"
    ;; Position 2 = 's' of setq — inside the outer paren.
    (should (pel-inside-block-p 2))))

(ert-deftest pel--syntax-macros-test/public/inside-block-p/nil-inside-string-even-at-depth ()
  "`pel-inside-block-p' returns nil when inside a string, even when paren depth > 0.
This is the documented special case: string takes precedence over block depth."
  (pel--syntax-macros-test--with-elisp "(setq x \"(embedded)\")\n"
    ;; The 'e' of "(embedded)" is inside a string; the depth from the outer
    ;; (setq ...) is 1, but pel-inside-block-p must still return nil because
    ;; we're inside the string.
    ;;
    ;; Buffer layout (1-indexed):
    ;;   (  s  e  t  q     x     "  (  e  m ...
    ;;   1  2  3  4  5  6  7  8  9  10 11 ...
    ;; Position 11 = 'e' of "(embedded)" inside the string.
    (should-not (pel-inside-block-p 11))))

(ert-deftest pel--syntax-macros-test/public/inside-block-p/inside-comment ()
  "`pel-inside-block-p' return nil when inside a Lisp comment by default.
It can check outside code when the second argument is non-nil."
  (pel--syntax-macros-test--with-elisp "(+ 1 ; comment\n   2)\n"
    ;; The '; comment' text is at position 7.  Even though we are still
    ;; syntactically inside the '(+ ...)' form, being in a comment means
    ;; pel-inside-block-p should not flag it as being in a code block.
    ;; Actually, pel-inside-block-p only checks string, not comment.
    ;; Verify actual behavior: inside a comment, the depth may still be 1
    ;; and pel--inside-string-p is nil, so pel-inside-block-p returns t
    ;; (the block depth still counts).
    ;; This test documents the actual behavior.
    ;; '7' = ';' of the comment, still inside the paren block.

    ;; By default it returns nil inside comment
    (should-not (pel-inside-block-p 9))
    ;; But returns t if we allow searching outside code
    (should     (pel-inside-block-p 9 t))))

(ert-deftest pel--syntax-macros-test/public/inside-block-p/non-nil-nested-block ()
  "`pel-inside-block-p' returns non-nil when inside nested parens."
  (pel--syntax-macros-test--with-elisp "(foo (bar x))\n"
    ;; Position 7 = 'b' of bar — inside the inner (bar ...) form, depth 2.
    (should (pel-inside-block-p 7))))

(ert-deftest pel--syntax-macros-test/public/inside-block-p/defaults-to-point ()
  "`pel-inside-block-p' uses point when POS is omitted."
  (pel--syntax-macros-test--with-elisp "(setq x 1)\n"
    (goto-char 2)
    (should (eq (pel-inside-block-p)
                (pel-inside-block-p 2)))))

;; ===========================================================================
;; pel-inside-block-p  — CAN-BE-OUTSIDE-CODE  argument (new capability)
;; ===========================================================================
;;
;; Five scenarios are tested, each with and without check-in-string:
;;
;;   A. Plain code inside a block
;;   B. Inside a string that is itself inside a block (the main new case)
;;   C. Comment-like text (";") inside a string inside a block
;;   D. String-like text ('"') inside a real comment inside a block
;;   E. Inside a string at top-level (depth 0) — check-in-string has no effect
;;
;; Buffer layouts used (all positions are 1-indexed):
;;
;;   A/B/C  "(setq x \"(foo)\")\n"
;;          (  s  e  t  q     x     "  (  f  o  o  )  "  )  \n
;;          1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
;;          pos 11 = 'f' inside embedded string; outer depth = 1.
;;
;;   C      "(setq x \"; not a comment\")\n"
;;          (  s  e  t  q     x     "  ;     n  o  t ...
;;          1  2  3  4  5  6  7  8  9 10 11 12 13 14 ...
;;          pos 12 = 'n' of "not"; inside-string=t, inside-comment=nil, depth=1.
;;
;;   D      "(foo ; \"bar\"\n  x)\n"
;;          (  f  o  o     ;     "  b  a  r  "  \n     x  )  \n
;;          1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
;;          pos 9 = 'b'; inside-comment=t, inside-string=nil, depth=1.
;;          The '"' after ';' does NOT open a real string in Emacs Lisp.
;;
;;   E      "\"(foo)\"\n"
;;          "  (  f  o  o  )  "  \n
;;          1  2  3  4  5  6  7  8
;;          pos 3 = 'f'; inside-string=t, depth=0.

;; ---------------------------------------------------------------------------
;; A. Plain code — CAN-BE-OUTSIDE-CODE does not change behavior

(ert-deftest pel--syntax-macros-test/public/inside-block-p/plain-code-unaffected-by-check-nil ()
  "`pel-inside-block-p' returns non-nil in plain code inside a block with CAN-BE-OUTSIDE-CODE nil."
  (pel--syntax-macros-test--with-elisp "(setq x 1)\n"
    ;; pos 2 = 's' of setq — plain code, depth = 1.
    (should (pel-inside-block-p 2 nil))))

(ert-deftest pel--syntax-macros-test/public/inside-block-p/plain-code-unaffected-by-check-t ()
  "`pel-inside-block-p' returns non-nil in plain code inside a block with CAN-BE-OUTSIDE-CODE t.
CAN-BE-OUTSIDE-CODE has no effect when not inside a string."
  (pel--syntax-macros-test--with-elisp "(setq x 1)\n"
    (should (pel-inside-block-p 2 t))))

;; ---------------------------------------------------------------------------
;; B. Inside a string inside a block — the core new capability

(ert-deftest pel--syntax-macros-test/public/inside-block-p/in-string-in-block-default-nil ()
  "`pel-inside-block-p' returns nil inside a string by default (CAN-BE-OUTSIDE-CODE omitted).
The outer (setq …) block has depth 1, but string classification takes precedence."
  (pel--syntax-macros-test--with-elisp "(setq x \"(foo)\")\n"
    ;; pos 11 = 'f' inside \"(foo)\"; inside-string=t, depth=1.
    (should-not (pel-inside-block-p 11))))

(ert-deftest pel--syntax-macros-test/public/inside-block-p/in-string-in-block-explicit-nil ()
  "`pel-inside-block-p' returns nil inside a string when CAN-BE-OUTSIDE-CODE is explicitly nil."
  (pel--syntax-macros-test--with-elisp "(setq x \"(foo)\")\n"
    (should-not (pel-inside-block-p 11 nil))))

(ert-deftest pel--syntax-macros-test/public/inside-block-p/in-string-in-block-check-t ()
  "`pel-inside-block-p' returns non-nil inside a string when CAN-BE-OUTSIDE-CODE is non-nil.
This is the new capability: the string guard is bypassed, exposing the outer paren depth."
  (pel--syntax-macros-test--with-elisp "(setq x \"(foo)\")\n"
    ;; pos 11 = 'f' inside \"(foo)\"; outer depth=1.  check-in-string bypasses the guard.
    (should (pel-inside-block-p 11 t))))

;; ---------------------------------------------------------------------------
;; C. Comment-like text (semicolon) inside a string inside a block

(ert-deftest pel--syntax-macros-test/public/inside-block-p/comment-in-string-default-nil ()
  "`pel-inside-block-p' returns nil inside a string containing a semicolon (default).
The semicolon is NOT a real comment; the position is still inside-string."
  (pel--syntax-macros-test--with-elisp "(setq x \"; not a comment\")\n"
    ;; pos 12 = 'n' of \"not\"; inside-string=t, inside-comment=nil, depth=1.
    (should-not (pel-inside-block-p 12))))

(ert-deftest pel--syntax-macros-test/public/inside-block-p/comment-in-string-check-t ()
  "`pel-inside-block-p' returns non-nil inside a string with a semicolon when CAN-BE-OUTSIDE-CODE is t.
Confirms that with CAN-BE-OUTSIDE-CODE the outer block depth is respected."
  (pel--syntax-macros-test--with-elisp "(setq x \"; not a comment\")\n"
    ;; pos 12 = 'n'; inside-string=t, depth=1.  check-in-string bypasses the guard.
    (should (pel-inside-block-p 12 t))))

;; ---------------------------------------------------------------------------
;; D. String-like text (double-quote) inside a real comment inside a block

(ert-deftest pel--syntax-macros-test/public/inside-block-p/string-in-comment-default ()
  "`pel-inside-block-p' returns nil inside a comment by default.
It can also check in a comment when the second argument is non-nil."
  (pel--syntax-macros-test--with-elisp "(foo ; \"bar\"\n  x)\n"
    ;; pos 9 = 'b' of \"bar\" text; inside-comment=t, inside-string=nil, depth=1.
    (should-not (pel-inside-block-p 9))
    (should     (pel-inside-block-p 9 t))))

(ert-deftest pel--syntax-macros-test/public/inside-block-p/string-in-comment-check-t ()
  "`pel-inside-block-p' behaves identically with CAN-BE-OUTSIDE-CODE t inside a comment.
Since inside-string is nil (the '\"' is in a comment, not a real string),
CAN-BE-OUTSIDE-CODE has no additional effect."
  (pel--syntax-macros-test--with-elisp "(foo ; \"bar\"\n  x)\n"
    ;; pos 9 = 'b'; same result as without check-in-string.
    (should (pel-inside-block-p 9 t))))

;; ---------------------------------------------------------------------------
;; E. Inside a top-level string (depth 0) — check-in-string cannot manufacture depth

(ert-deftest pel--syntax-macros-test/public/inside-block-p/top-level-string-default-nil ()
  "`pel-inside-block-p' returns nil inside a top-level string (depth 0, default)."
  (pel--syntax-macros-test--with-elisp "\"(foo)\"\n"
    ;; pos 3 = 'f'; inside-string=t, depth=0.
    (should-not (pel-inside-block-p 3))))

(ert-deftest pel--syntax-macros-test/public/inside-block-p/top-level-string-check-t-nil ()
  "`pel-inside-block-p' returns nil inside a top-level string even with CAN-BE-OUTSIDE-CODE t.
No enclosing paren block exists; depth is 0 regardless of the flag."
  (pel--syntax-macros-test--with-elisp "\"(foo)\"\n"
    ;; pos 3 = 'f'; depth=0 — check-in-string cannot create depth.
    (should-not (pel-inside-block-p 3 t))))

;; ===========================================================================
;; pel-inside-code-p  (public function)
;; ===========================================================================

(ert-deftest pel--syntax-macros-test/public/inside-code-p/t-in-plain-code ()
  "`pel-inside-code-p' returns t for a position in plain code."
  (pel--syntax-macros-test--with-elisp "(+ 1 2)\n"
    (should (pel-inside-code-p 2))))

(ert-deftest pel--syntax-macros-test/public/inside-code-p/nil-in-string ()
  "`pel-inside-code-p' returns nil when inside a string literal."
  (pel--syntax-macros-test--with-elisp "(setq x \"hello\")\n"
    ;; Position 10 = 'h' of "hello".
    (should-not (pel-inside-code-p 10))))

(ert-deftest pel--syntax-macros-test/public/inside-code-p/nil-in-comment ()
  "`pel-inside-code-p' returns nil when inside a ; comment."
  (pel--syntax-macros-test--with-elisp "; a comment\n"
    ;; Position 3 = 'a' inside the comment.
    (should-not (pel-inside-code-p 3))))

(ert-deftest pel--syntax-macros-test/public/inside-code-p/nil-in-c-block-comment ()
  "`pel-inside-code-p' returns nil inside a C /* ... */ block comment."
  (pel--syntax-macros-test--with-c "/* inside */\nint x;\n"
    ;; Position 5 = 'i' of "inside".
    (should-not (pel-inside-code-p 5))))

(ert-deftest pel--syntax-macros-test/public/inside-code-p/t-after-c-block-comment ()
  "`pel-inside-code-p' returns t for a position after a C block comment."
  (pel--syntax-macros-test--with-c "/* hi */ int x;\n"
    ;; Position 10 = 'i' of "int" — after the comment.
    (should (pel-inside-code-p 10))))

(ert-deftest pel--syntax-macros-test/public/inside-code-p/t-inside-paren-block ()
  "`pel-inside-code-p' returns t when inside a paren block in plain code."
  (pel--syntax-macros-test--with-elisp "(+ 1 2)\n"
    ;; Position 3 = '1' — inside the paren form, but plain code.
    (should (pel-inside-code-p 3))))

(ert-deftest pel--syntax-macros-test/public/inside-code-p/defaults-to-point ()
  "`pel-inside-code-p' uses point when POS is omitted."
  (pel--syntax-macros-test--with-elisp "(+ 1 2)\n"
    (goto-char 2)
    (should (eq (pel-inside-code-p)
                (pel-inside-code-p 2)))))

;;; --------------------------------------------------------------------------
(provide 'pel--syntax-macros-test)

;;; pel--syntax-macros-test.el ends here
