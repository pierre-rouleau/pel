;;; pel-syntax-test.el --- ERT tests for pel-syntax.el  -*- lexical-binding: t; -*-

;; Created   : Tuesday, March 24 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-24 13:58:01 EDT, updated by Pierre Rouleau>

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
;; ERT tests for pel-syntax.el.
;;
;; Covered items:
;;
;;   Development tools:
;;     pel-get-text-property        - reads arbitrary text property at pos
;;     pel-get-syntax-prop          - reads the syntax-table text property at pos
;;     pel-get-face                 - reads the face text property at pos
;;
;;   Syntax utilities:
;;     pel-syntax-at                - returns (class-string . syntax-descriptor) list
;;     pel-syntax-matching-parens-position - finds matching paren via sexp motion
;;
;;   Syntax skipping utilities:
;;     pel-syntax-skip-string-forward            - advance past end of string
;;     pel-syntax-skip-string-backward           - retreat before start of string
;;     pel-syntax-skip-string-and-comment-forward  - advance past string or comment
;;     pel-syntax-skip-string-and-comment-backward - retreat before string or comment
;;
;;   Block syntax fixer:
;;     pel-syntax-block-text-at     - returns (open close text) for block at point
;;     pel-syntax-fix-block-content - comma-separates expressions inside a block
;;
;;   Conditional navigation:
;;     pel-syntax-conditional-forward  - navigate to matching end-of-conditional
;;     pel-syntax-conditional-backward - navigate to matching start-of-conditional
;;
;; Items intentionally NOT covered:
;;   - `pel-syntax-at-point'               (interactive; calls `what-cursor-position')
;;   - `pel-insert-space-in-enclosing-block' (depends on `pel-inside-block-p' macro
;;                                            which requires a live block context)
;;   - `pel---replace-with'                (private; tested indirectly via
;;                                          `pel-syntax-fix-block-content')
;;   - `pel-replace'                       (macro wrapper over `pel---replace-with';
;;                                          tested indirectly)
;;
;; Notes on test isolation:
;;   - Tests that require string/comment syntax-ppss classification use
;;     `emacs-lisp-mode' (always available) or `c-mode'.
;;   - Conditional navigation tests use a self-contained, minimal
;;     IF/ELSE/END keyword regexp defined in this file so the test suite
;;     does not depend on pel-c-preproc.
;;   - `(match-data t)' is used throughout to ensure integer positions
;;     are stored in match-data lists; this avoids stale-marker issues
;;     after temporary buffers are destroyed.
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-syntax)
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
;; Test helpers / macros
;; ===========================================================================

(defmacro pel-syntax-test--with-code (code &rest body)
  "Run BODY in a temp buffer (fundamental-mode) pre-loaded with CODE.
Point is at `point-min' on entry."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,code)
     (goto-char (point-min))
     ,@body))

(defmacro pel-syntax-test--with-elisp-code (code &rest body)
  "Run BODY in a temp buffer in `emacs-lisp-mode' pre-loaded with CODE.
Point is at `point-min' on entry."
  (declare (indent 1))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,code)
     (goto-char (point-min))
     ,@body))

(defmacro pel-syntax-test--with-c-code (code &rest body)
  "Run BODY in a temp buffer in `c-mode' pre-loaded with CODE.
Point is at `point-min' on entry."
  (declare (indent 1))
  `(with-temp-buffer
     (c-mode)
     (insert ,code)
     (goto-char (point-min))
     ,@body))

;; ---------------------------------------------------------------------------
;; Helpers for conditional navigation tests
;; A self-contained IF / ELSE / END keyword regexp (all upper-case) avoids
;; any dependency on pel-c-preproc.

(defconst pel-syntax-test--cond-re
  "\\(IF\\)\\|\\(ELSE\\)\\|\\(END\\)"
  "Simple regexp for conditional navigation tests.
Group 1 = IF, group 2 = ELSE, group 3 = END.")

(defun pel-syntax-test--cond-token (mdata)
  "Return token symbol from match-data MDATA for `pel-syntax-test--cond-re'."
  (cond ((nth 2 mdata) 'if)
        ((nth 4 mdata) 'else)
        ((nth 6 mdata) 'end)))

(defun pel-syntax-test--cond-pos (mdata)
  "Return start position of token from match-data MDATA."
  (nth 0 mdata))

;; ===========================================================================
;; pel-get-text-property
;; ===========================================================================

(ert-deftest pel-syntax-test/get-text-property/returns-nil-when-absent ()
  "`pel-get-text-property' returns nil when property is not present."
  (pel-syntax-test--with-code "hello"
    (should (eq nil (pel-get-text-property 'my-prop 1)))))

(ert-deftest pel-syntax-test/get-text-property/returns-value-when-set ()
  "`pel-get-text-property' returns the value of an explicit text property."
  (pel-syntax-test--with-code "hello"
    (put-text-property 1 6 'my-prop 'test-value)
    (should (eq 'test-value (pel-get-text-property 'my-prop 1)))))

(ert-deftest pel-syntax-test/get-text-property/uses-point-when-no-pos ()
  "`pel-get-text-property' defaults to point when POS is omitted."
  (pel-syntax-test--with-code "hello"
    (put-text-property 1 6 'my-prop 42)
    (goto-char 3)
    (should (= 42 (pel-get-text-property 'my-prop)))))

;; ===========================================================================
;; pel-get-syntax-prop
;; ===========================================================================

(ert-deftest pel-syntax-test/get-syntax-prop/returns-nil-when-absent ()
  "`pel-get-syntax-prop' returns nil when no syntax-table property is set."
  (pel-syntax-test--with-code "abc"
    (should (eq nil (pel-get-syntax-prop 1)))))

(ert-deftest pel-syntax-test/get-syntax-prop/returns-value-when-set ()
  "`pel-get-syntax-prop' returns an explicitly placed syntax-table property."
  (pel-syntax-test--with-code "abc"
    (let ((marker-syntax (string-to-syntax ".")))
      (put-text-property 1 2 'syntax-table marker-syntax)
      (should (equal marker-syntax (pel-get-syntax-prop 1))))))

;; ===========================================================================
;; pel-get-face
;; ===========================================================================

(ert-deftest pel-syntax-test/get-face/returns-nil-when-absent ()
  "`pel-get-face' returns nil when no face property is present."
  (pel-syntax-test--with-code "abc"
    (should (eq nil (pel-get-face 1)))))

(ert-deftest pel-syntax-test/get-face/returns-face-value-when-set ()
  "`pel-get-face' returns the face set via `put-text-property'."
  (pel-syntax-test--with-code "abc"
    (put-text-property 1 4 'face 'font-lock-keyword-face)
    (should (eq 'font-lock-keyword-face (pel-get-face 1)))))

;; ===========================================================================
;; pel-syntax-at
;; ===========================================================================

(ert-deftest pel-syntax-test/syntax-at/returns-two-element-list ()
  "`pel-syntax-at' returns a list of exactly 2 elements."
  (pel-syntax-test--with-elisp-code "(hello)"
    (should (= 2 (length (pel-syntax-at 1))))))

(ert-deftest pel-syntax-test/syntax-at/first-element-is-string ()
  "The first element returned by `pel-syntax-at' is a one-char string."
  (pel-syntax-test--with-elisp-code "(hello)"
    (let ((info (pel-syntax-at 1)))
      (should (stringp (nth 0 info)))
      (should (= 1 (length (nth 0 info)))))))

(ert-deftest pel-syntax-test/syntax-at/open-paren-class ()
  "An open paren has syntax class `(' in `emacs-lisp-mode'."
  (pel-syntax-test--with-elisp-code "(hello)"
    ;; Position 1 is '('
    (should (string= "(" (nth 0 (pel-syntax-at 1))))))

(ert-deftest pel-syntax-test/syntax-at/word-char-class ()
  "A word constituent has syntax class `w' in `emacs-lisp-mode'."
  (pel-syntax-test--with-elisp-code "(hello)"
    ;; Position 2 is 'h'
    (should (string= "w" (nth 0 (pel-syntax-at 2))))))

(ert-deftest pel-syntax-test/syntax-at/defaults-to-point ()
  "`pel-syntax-at' uses point when POS is omitted."
  (pel-syntax-test--with-elisp-code "(hello)"
    (goto-char 1)
    (let ((at-point (pel-syntax-at))
          (at-1     (pel-syntax-at 1)))
      (should (equal at-point at-1)))))

;; ===========================================================================
;; pel-syntax-matching-parens-position
;; ===========================================================================

(ert-deftest pel-syntax-test/matching-parens/forward-round-paren ()
  "From `(' in `(abc)', `pel-syntax-matching-parens-position' returns position of `)'."
  (pel-syntax-test--with-elisp-code "(abc)"
    ;; '(' is at 1, ')' is at 5
    (should (= 5 (pel-syntax-matching-parens-position 1)))))

(ert-deftest pel-syntax-test/matching-parens/backward-round-paren ()
  "From `)' in `(abc)', the function returns position of `('."
  (pel-syntax-test--with-elisp-code "(abc)"
    ;; ')' is at 5, '(' is at 1
    (should (= 1 (pel-syntax-matching-parens-position 5)))))

(ert-deftest pel-syntax-test/matching-parens/nested-parens ()
  "For outer `(' in `(a (b) c)', the function returns the outer `)'."
  (pel-syntax-test--with-elisp-code "(a (b) c)"
    ;; outer '(' at 1, outer ')' at 9
    (should (= 9 (pel-syntax-matching-parens-position 1)))))

(ert-deftest pel-syntax-test/matching-parens/signals-on-non-paren ()
  "Passing a non-paren character position signals an error."
  (pel-syntax-test--with-elisp-code "(abc)"
    ;; 'a' at 2 is not a paren
    (should-error (pel-syntax-matching-parens-position 2))))

(ert-deftest pel-syntax-test/matching-parens/does-not-move-point ()
  "`pel-syntax-matching-parens-position' must not change point."
  (pel-syntax-test--with-elisp-code "(abc)"
    (let ((saved (point)))
      (pel-syntax-matching-parens-position 1)
      (should (= saved (point))))))

;; ===========================================================================
;; pel-syntax-skip-string-forward
;; In `emacs-lisp-mode', a "..." string is recognised by syntax-ppss.
;; Buffer: "hello" rest   (1-indexed: " at 1, h at 2, ..., " at 7, space at 8)
;; Starting at pos 2 (inside the string) the function should land at 8.
;; ===========================================================================

(ert-deftest pel-syntax-test/skip-string-forward/exits-string ()
  "Starting inside a string, point lands just after the closing quote."
  (pel-syntax-test--with-elisp-code "\"hello\" rest"
    ;; go to 'h', which is position 2 — inside the string
    (goto-char 2)
    (pel-syntax-skip-string-forward)
    ;; position 8 is the space after the closing quote
    (should (= 8 (point)))))

(ert-deftest pel-syntax-test/skip-string-forward/no-move-outside-string ()
  "When point is already outside a string, `pel-syntax-skip-string-forward'
does not move it."
  (pel-syntax-test--with-elisp-code "\"hello\" rest"
    ;; go to 'r' of "rest", which is at position 9
    (goto-char 9)
    (let ((saved (point)))
      (pel-syntax-skip-string-forward)
      (should (= saved (point))))))

;; ===========================================================================
;; pel-syntax-skip-string-backward
;; Buffer: prefix "hello"   (p=1, r=2, ..., space=7, "=8, h=9,..., "=14)
;; Starting at position 11 (inside string), should land at 8 (opening ").
;; ===========================================================================

(ert-deftest pel-syntax-test/skip-string-backward/exits-string ()
  "Starting inside a string, point retreats to the opening quote."
  (pel-syntax-test--with-elisp-code "prefix \"hello\""
    ;; "prefix " = 7 chars; " at 8, h at 9
    ;; go to 'h' at position 9 — inside the string
    (goto-char 9)
    (pel-syntax-skip-string-backward)
    ;; should land at position 8 (the opening quote)
    (should (= 8 (point)))))

(ert-deftest pel-syntax-test/skip-string-backward/no-move-outside-string ()
  "When point is outside a string, `pel-syntax-skip-string-backward' is a no-op."
  (pel-syntax-test--with-elisp-code "prefix \"hello\""
    (goto-char 3)
    (let ((saved (point)))
      (pel-syntax-skip-string-backward)
      (should (= saved (point))))))

;; ===========================================================================
;; pel-syntax-skip-string-and-comment-forward
;; In emacs-lisp-mode, '; comment\n' is a line comment.
;; Buffer:  ; comment\ncode   (';' at 1, space at 10, '\n' at 10, 'c' at 11)
;; Starting at position 3 (inside comment), should exit to position 11.
;; ===========================================================================

(ert-deftest pel-syntax-test/skip-str-comment-forward/exits-comment ()
  "Starting inside a line comment, point advances to just after the newline."
  (pel-syntax-test--with-elisp-code "; comment\ncode"
    ;; ';' at 1, ' ' at 2, 'c' of "comment" at 3 — inside the comment
    (goto-char 3)
    (let ((end-pos (pel-syntax-skip-string-and-comment-forward)))
      ;; '\n' is at 10, so just after it is 11
      (should (= 11 end-pos))
      (should (= 11 (point))))))

(ert-deftest pel-syntax-test/skip-str-comment-forward/exits-string ()
  "Starting inside a string, point advances to just after the closing quote."
  (pel-syntax-test--with-elisp-code "\"hi\" rest"
    (goto-char 2)
    (let ((end-pos (pel-syntax-skip-string-and-comment-forward)))
      ;; '"' at 1, 'h' at 2, 'i' at 3, '"' at 4, ' ' at 5
      (should (= 5 end-pos)))))

(ert-deftest pel-syntax-test/skip-str-comment-forward/no-move-outside ()
  "When point is already in plain code, the function returns point unchanged."
  (pel-syntax-test--with-elisp-code "; comment\ncode"
    (goto-char 12)
    (let ((saved (point)))
      (should (= saved (pel-syntax-skip-string-and-comment-forward))))))

;; ===========================================================================
;; pel-syntax-skip-string-and-comment-backward
;; ===========================================================================

(ert-deftest pel-syntax-test/skip-str-comment-backward/exits-string ()
  "Starting inside a string, point retreats to the opening quote."
  (pel-syntax-test--with-elisp-code "code \"hi\" more"
    ;; 'c'=1 'o'=2 'd'=3 'e'=4 ' '=5 '"'=6 'h'=7 'i'=8 '"'=9 ' '=10
    ;; position 7 ('h') is inside the string
    (goto-char 7)
    (let ((start-pos (pel-syntax-skip-string-and-comment-backward)))
      (should (= 6 start-pos))
      (should (= 6 (point))))))

(ert-deftest pel-syntax-test/skip-str-comment-backward/no-move-outside ()
  "When point is in plain code, `pel-syntax-skip-string-and-comment-backward'
does not move it."
  (pel-syntax-test--with-elisp-code "code \"hi\" more"
    (goto-char 2)
    (let ((saved (point)))
      (should (= saved (pel-syntax-skip-string-and-comment-backward))))))

;; ===========================================================================
;; pel-syntax-block-text-at
;; ===========================================================================

(ert-deftest pel-syntax-test/block-text-at/returns-three-elements ()
  "`pel-syntax-block-text-at' returns a 3-element list."
  (pel-syntax-test--with-elisp-code "(a b c)"
    ;; point inside the block — go to 'a' at position 2
    (goto-char 2)
    (let ((result (pel-syntax-block-text-at)))
      (should (= 3 (length result))))))

(ert-deftest pel-syntax-test/block-text-at/open-pos-is-integer ()
  "The first element (open-pos) is an integer."
  (pel-syntax-test--with-elisp-code "(a b c)"
    (goto-char 2)
    (should (integerp (nth 0 (pel-syntax-block-text-at))))))

(ert-deftest pel-syntax-test/block-text-at/close-pos-is-integer ()
  "The second element (close-pos) is an integer."
  (pel-syntax-test--with-elisp-code "(a b c)"
    (goto-char 2)
    (should (integerp (nth 1 (pel-syntax-block-text-at))))))

(ert-deftest pel-syntax-test/block-text-at/text-is-string ()
  "The third element (text) is a string containing the whole block."
  (pel-syntax-test--with-elisp-code "(a b c)"
    (goto-char 2)
    (should (stringp (nth 2 (pel-syntax-block-text-at))))))

(ert-deftest pel-syntax-test/block-text-at/text-includes-parens ()
  "The text returned includes the surrounding delimiter characters."
  (pel-syntax-test--with-elisp-code "(a b c)"
    (goto-char 2)
    (let ((text (nth 2 (pel-syntax-block-text-at))))
      (should (string-prefix-p "(" text))
      (should (string-suffix-p ")" text)))))

(ert-deftest pel-syntax-test/block-text-at/open-close-positions ()
  "Open position is 1 and close position is 7 for `(a b c)'."
  (pel-syntax-test--with-elisp-code "(a b c)"
    (goto-char 2)
    (let ((result (pel-syntax-block-text-at)))
      (should (= 1 (nth 0 result)))
      (should (= 7 (nth 1 result))))))

;; ===========================================================================
;; pel-syntax-fix-block-content
;; Tests run in emacs-lisp-mode so syntax-ppss can classify strings.
;; ===========================================================================

(ert-deftest pel-syntax-test/fix-block/adds-commas-between-words ()
  "Bare space-separated words inside a block get commas inserted."
  (pel-syntax-test--with-elisp-code "(a b c)"
    (goto-char 2)
    (pel-syntax-fix-block-content)
    (should (string= "(a, b, c)" (buffer-string)))))

(ert-deftest pel-syntax-test/fix-block/does-not-double-existing-commas ()
  "Already comma-separated content is left structurally correct."
  (pel-syntax-test--with-elisp-code "(a, b, c)"
    (goto-char 2)
    (pel-syntax-fix-block-content)
    ;; No extra commas should appear.
    (should-not (string-search ",," (buffer-string)))))

(ert-deftest pel-syntax-test/fix-block/returns-integer-change-count ()
  "`pel-syntax-fix-block-content' returns an integer count of modifications."
  (pel-syntax-test--with-elisp-code "(a b c)"
    (goto-char 2)
    (should (integerp (pel-syntax-fix-block-content)))))

(ert-deftest pel-syntax-test/fix-block/leaves-string-content-untouched ()
  "Content inside a string literal inside the block is not modified."
  (pel-syntax-test--with-elisp-code "(\"a b\" x)"
    (goto-char 2)
    (pel-syntax-fix-block-content)
    ;; The space inside \"a b\" must still be a space, not replaced by \", \"
    (should (string-search "\"a b\"" (buffer-string)))))

;; ===========================================================================
;; pel-syntax-conditional-forward
;; Uses the self-contained IF/ELSE/END regexp defined above.
;; ===========================================================================

(ert-deftest pel-syntax-test/cond-forward/finds-matching-end ()
  "With nesting=0, forward search lands at END of a single IF…END block."
  (pel-syntax-test--with-code "IF x\nEND\n"
    ;; Start at point-min (before IF), nesting=0.
    (let ((pos (pel-syntax-conditional-forward
                pel-syntax-test--cond-re
                #'pel-syntax-test--cond-token
                #'pel-syntax-test--cond-pos
                0 nil "END")))
      (should pos)
      ;; After the call, point should be on or just after the END token.
      (should (> (point) 5)))))

(ert-deftest pel-syntax-test/cond-forward/skips-nested-block ()
  "A nested IF…END does not prematurely stop the outer search."
  (pel-syntax-test--with-code "IF outer\nIF inner\nEND\nEND\n"
    (let ((pos (pel-syntax-conditional-forward
                pel-syntax-test--cond-re
                #'pel-syntax-test--cond-token
                #'pel-syntax-test--cond-pos
                0 nil "END")))
      (should pos)
      ;; Outer END is after the inner END — point must be past the inner END.
      (should (> (point) (+ (length "IF outer\nIF inner\nEND\n") 0))))))

(ert-deftest pel-syntax-test/cond-forward/finds-else ()
  "With to-else=t, forward search stops at ELSE."
  (pel-syntax-test--with-code "IF x\nELSE\nEND\n"
    (let ((pos (pel-syntax-conditional-forward
                pel-syntax-test--cond-re
                #'pel-syntax-test--cond-token
                #'pel-syntax-test--cond-pos
                0 t "ELSE")))
      (should pos)
      ;; ELSE appears before END; point must be before END.
      (let ((end-pos (save-excursion
                       (goto-char (point-min))
                       (search-forward "END"))))
        (should (< (point) end-pos))))))

(ert-deftest pel-syntax-test/cond-forward/returns-nil-on-no-match ()
  "Returns nil (and issues user-error) when no matching END exists."
  (pel-syntax-test--with-code "IF x\n"
    ;; No END is present; should signal a user-error.
    (should-error
     (pel-syntax-conditional-forward
      pel-syntax-test--cond-re
      #'pel-syntax-test--cond-token
      #'pel-syntax-test--cond-pos
      0 nil "END")
     :type 'user-error)))

;; ===========================================================================
;; pel-syntax-conditional-backward
;; ===========================================================================

(ert-deftest pel-syntax-test/cond-backward/finds-matching-if ()
  "With nesting=0, backward search from END lands at IF."
  ;; A leading newline ensures IF is at position 2 (not 1), so the
  ;; unconditional (left-char 1) inside the search loop never hits bobp.
  (pel-syntax-test--with-code "\nIF x\nEND\n"
    (goto-char (point-max))
    (let ((pos (pel-syntax-conditional-backward
                pel-syntax-test--cond-re
                #'pel-syntax-test--cond-token
                #'pel-syntax-test--cond-pos
                0 nil "IF")))
      (should pos)
      ;; IF is at position 2; point ends up at 2 or just after.
      (should (<= (point) 4)))))

(ert-deftest pel-syntax-test/cond-backward/skips-nested-block ()
  "A nested IF…END does not prematurely stop the outer backward search."
  ;; Leading newline prevents beginning-of-buffer when outer IF is matched.
  (pel-syntax-test--with-code "\nIF outer\nIF inner\nEND\nEND\n"
    (goto-char (point-max))
    (let ((pos (pel-syntax-conditional-backward
                pel-syntax-test--cond-re
                #'pel-syntax-test--cond-token
                #'pel-syntax-test--cond-pos
                0 nil "IF")))
      (should pos)
      ;; Outer IF is at position 2; point ends up at 2 or just after.
      (should (<= (point) 4)))))

(ert-deftest pel-syntax-test/cond-backward/returns-nil-on-no-match ()
  "Returns nil (and issues user-error) when no matching IF exists."
  ;; Leading newline: END is at pos 2, left-char moves to pos 1 (valid),
  ;; then re-search-backward finds nothing → user-error fires correctly.
  (pel-syntax-test--with-code "\nEND\n"
    (goto-char (point-max))
    (should-error
     (pel-syntax-conditional-backward
      pel-syntax-test--cond-re
      #'pel-syntax-test--cond-token
      #'pel-syntax-test--cond-pos
      0 nil "IF")
     :type 'user-error)))

;; ---------------------------------------------------------------------------
;; ===========================================================================
;; pel-syntax-conditional-backward — edge cases: token at (point-min)
;;
;; These tests have NO leading newline so the relevant token is at buffer
;; position 1 = (point-min).  They validate the (unless (bobp) (left-char 1))
;; guard introduced at line 522 of pel-syntax.el; without the guard each test
;; would raise (beginning-of-buffer).
;; ===========================================================================

;; The fourth test is especially important: it proves the guard doesn't silently
;; swallow a legitimate user-error — it only suppresses the spurious
;; beginning-of-buffer signal.

(ert-deftest pel-syntax-test/cond-backward/no-error-when-if-at-bobp ()
  "No `beginning-of-buffer' when the matching IF is at `point-min'."
  ;; IF is at position 1.  The bobp guard must prevent the signal.
  (pel-syntax-test--with-code "IF x\nEND\n"
    (goto-char (point-max))
    (let (got-bobp-error result)
      (condition-case _
          (setq result (pel-syntax-conditional-backward
                        pel-syntax-test--cond-re
                        #'pel-syntax-test--cond-token
                        #'pel-syntax-test--cond-pos
                        0 nil "IF"))
        (beginning-of-buffer (setq got-bobp-error t)))
      ;; The guard must suppress the beginning-of-buffer signal.
      (should-not got-bobp-error)
      ;; The function must have returned a valid (integer) position.
      (should result)
      (should (integerp result)))))

(ert-deftest pel-syntax-test/cond-backward/point-near-bobp-after-match ()
  "Point lands at position 2 (just after IF at position 1) after the search."
  ;; After finding IF at 1, the epilogue `(right-char 1)' moves point to 2.
  (pel-syntax-test--with-code "IF x\nEND\n"
    (goto-char (point-max))
    (pel-syntax-conditional-backward
     pel-syntax-test--cond-re
     #'pel-syntax-test--cond-token
     #'pel-syntax-test--cond-pos
     0 nil "IF")
    (should (<= (point) 3))))

(ert-deftest pel-syntax-test/cond-backward/no-error-nested-if-at-bobp ()
  "No `beginning-of-buffer' when outer IF of a nested block is at `point-min'."
  ;; outer IF at position 1; inner IF/END pair is processed first (nesting++),
  ;; then outer IF is found at bobp — guard fires on the final iteration.
  (pel-syntax-test--with-code "IF outer\nIF inner\nEND\nEND\n"
    (goto-char (point-max))
    (let (got-bobp-error result)
      (condition-case _
          (setq result (pel-syntax-conditional-backward
                        pel-syntax-test--cond-re
                        #'pel-syntax-test--cond-token
                        #'pel-syntax-test--cond-pos
                        0 nil "IF"))
        (beginning-of-buffer (setq got-bobp-error t)))
      (should-not got-bobp-error)
      (should result)
      ;; Point should have retreated to near the outer IF (position 1).
      (should (<= (point) 3)))))

(ert-deftest pel-syntax-test/cond-backward/user-error-not-bobp-for-unmatched-end-at-bobp ()
  "Unmatched END at `point-min' raises `user-error', not `beginning-of-buffer'."
  ;; END is at position 1.  After matching it (nesting → 1), the bobp guard
  ;; skips left-char; re-search-backward from bobp returns nil; nesting > 0
  ;; so the function signals user-error — not beginning-of-buffer.
  (pel-syntax-test--with-code "END\n"
    (goto-char (point-max))
    (should-error
     (pel-syntax-conditional-backward
      pel-syntax-test--cond-re
      #'pel-syntax-test--cond-token
      #'pel-syntax-test--cond-pos
      0 nil "IF")
     :type 'user-error)))

;;; --------------------------------------------------------------------------
(provide 'pel-syntax-test)

;;; pel-syntax-test.el ends here
