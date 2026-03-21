;;; pel-erlang-test.el --- ERT tests for pel-erlang.el.  -*- lexical-binding: t; -*-

;; Created   : Friday, March 21 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-21 19:48:38 EDT, updated by Pierre Rouleau>

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
;; Tests for the pure and buffer-position functions defined in pel-erlang.el.
;; These tests do not require an Erlang installation; they exercise only
;; functions whose logic is implemented entirely in Emacs Lisp.
;;
;; Functions under test
;; --------------------
;;  Pure:
;;   - `pel-erlang-mode-used-text'
;;
;;  Buffer-position:
;;   - `pel-erlang-before-binary'
;;   - `pel-erlang-after-binary'
;;   - `pel--after-dash'
;;   - `pel--erlang-line-3%-comment-p'
;;
;;  Path extraction:
;;   - `pel--erlang-dirpath'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-erlang)
(require 'pel-ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; ===========================================================================
;; pel-erlang-mode-used-text
;; ===========================================================================

(defconst pel--erlang-mode-text-scenarios
  ;; (use-erlang-value  expected-result-substring)
  ;;
  ;; We test for substrings so the tests remain valid even if cosmetic wording
  ;; in the messages changes.
  '((t                 "use erlang-mode")
    (with-tree-sitter  "use erlang-ts-mode"))
  "Scenarios for `ert-test-pel-erlang-mode-used-text'.")

(ert-deftest ert-test-pel-erlang-mode-used-text ()
  "Test `pel-erlang-mode-used-text'."
  ;; Known valid values.
  (should (pel-string= (pel-erlang-mode-used-text t)
                       "use erlang-mode from erlang-mode.el."
                       't))
  (should (pel-string= (pel-erlang-mode-used-text 'with-tree-sitter)
                       "use erlang-ts-mode tree-sitter aware mode."
                       'with-tree-sitter))
  ;; Any other value falls through to the default branch.
  (should (pel-string= (pel-erlang-mode-used-text nil)
                       "Invalid! Use t or with-tree-sitter"
                       nil))
  (should (pel-string= (pel-erlang-mode-used-text 'something-else)
                       "Invalid! Use t or with-tree-sitter"
                       'something-else))
  (should (pel-string= (pel-erlang-mode-used-text 42)
                       "Invalid! Use t or with-tree-sitter"
                       42))
  ;; Scenario-table driven: each entry supplies its own context arg.
  (dolist (scenario pel--erlang-mode-text-scenarios)
    (let ((arg      (car scenario))
          (expected (cadr scenario)))
      (should (pel-equal
               (string-match-p (regexp-quote expected)
                               (pel-erlang-mode-used-text arg))
               0                 ; match at position 0 = starts with substring
               scenario)))))

;; ===========================================================================
;; pel-erlang-before-binary
;; ===========================================================================

(ert-deftest ert-test-pel-erlang-before-binary ()
  "Test `pel-erlang-before-binary': non-nil iff point is just before <<."
  ;; Point at the very start of "<<".
  (with-temp-buffer
    (insert "<<")
    (goto-char (point-min))
    (should (pel-erlang-before-binary)))

  ;; Point between the two angle brackets — no longer "before <<".
  (with-temp-buffer
    (insert "<<")
    (goto-char (1+ (point-min)))
    (should-not (pel-erlang-before-binary)))

  ;; Point after "<<".
  (with-temp-buffer
    (insert "<<foo>>")
    (goto-char (+ (point-min) 2))
    (should-not (pel-erlang-before-binary)))

  ;; "<<" embedded in text; point placed right before it.
  (with-temp-buffer
    (insert "foo <<bar>>")
    (goto-char (+ (point-min) 4))       ; just before the <<
    (should (pel-erlang-before-binary)))

  ;; Single "<" is not a binary open.
  (with-temp-buffer
    (insert "<foo")
    (goto-char (point-min))
    (should-not (pel-erlang-before-binary)))

  ;; Empty buffer.
  (with-temp-buffer
    (should-not (pel-erlang-before-binary))))

;; ===========================================================================
;; pel-erlang-after-binary
;; ===========================================================================

(ert-deftest ert-test-pel-erlang-after-binary ()
  "Test `pel-erlang-after-binary': non-nil iff point is just after >>."
  ;; Point immediately after ">>".
  ;; (ert-skip "Temporary skip test under development.")
  (with-temp-buffer
    (insert ">>")
    (goto-char (point-max))
    (should (pel-erlang-after-binary)))

  ;; Point between the two ">".
  (with-temp-buffer
    (insert ">>")
    (goto-char (1+ (point-min)))
    (should-not (pel-erlang-after-binary)))

  ;; Point before ">>".
  (with-temp-buffer
    (insert ">>")
    (goto-char (point-min))
    (should-not (pel-erlang-after-binary)))

  ;; ">>" embedded; point placed right after it.
  (with-temp-buffer
    (insert "<<foo>>bar")
    (goto-char (+ (point-min) 7))       ; just after >>
    (should (pel-erlang-after-binary)))

  ;; Single ">" is not a binary close.
  (with-temp-buffer
    (insert "foo>")
    (goto-char (point-max))
    (should-not (pel-erlang-after-binary)))

  ;; before-binary and after-binary are consistent: after forward-binary,
  ;; after-binary must be true.
  (with-temp-buffer
    (insert "<<hello>>")
    (goto-char (point-min))
    (should (pel-erlang-before-binary))
    (pel-erlang-forward-binary)
    (should (pel-erlang-after-binary))))

;; ===========================================================================
;; pel--after-dash
;; ===========================================================================
;;
;; Implementation note: pel--after-dash contains the guard (>= (point) 3).
;; In Emacs, (point-min) == 1 in a normal buffer, so point-max of a 1-char
;; buffer is 2 and the guard fails.  A two-character string places point-max
;; at 3, satisfying the guard.

(ert-deftest ert-test-pel--after-dash ()
  "Test `pel--after-dash': t when point is after - but not after $-."
  ;; Two chars ending in -: satisfies (>= (point) 3) and char-before is -.
  (with-temp-buffer
    (insert "a-")
    (goto-char (point-max))             ; point = 3
    (should (pel--after-dash)))

  ;; After $-: the $ makes it nil.
  (with-temp-buffer
    (insert "$-")
    (goto-char (point-max))             ; point = 3
    (should-not (pel--after-dash)))

  ;; Longer string ending in -: still t.
  (with-temp-buffer
    (insert "foo-")
    (goto-char (point-max))
    (should (pel--after-dash)))

  ;; Longer string ending in $-: nil.
  (with-temp-buffer
    (insert "foo$-")
    (goto-char (point-max))
    (should-not (pel--after-dash)))

  ;; Point guard: single-char buffer, point = 2 < 3 → nil.
  (with-temp-buffer
    (insert "-")
    (goto-char (point-max))             ; point = 2
    (should-not (pel--after-dash)))

  ;; No dash at all.
  (with-temp-buffer
    (insert "ab")
    (goto-char (point-max))
    (should-not (pel--after-dash)))

  ;; After -> (point right after >): char-before is >, not -.
  (with-temp-buffer
    (insert "->")
    (goto-char (point-max))
    (should-not (pel--after-dash)))

  ;; Point right after - inside a longer string.
  (with-temp-buffer
    (insert "foo-bar")
    (goto-char (+ (point-min) 4))       ; after -
    (should (pel--after-dash)))

  ;; Empty buffer.
  (with-temp-buffer
    (should-not (pel--after-dash))))

;; ===========================================================================
;; pel--erlang-line-3%-comment-p
;; ===========================================================================

(ert-deftest ert-test-pel--erlang-line-3%-comment-p ()
  "Test `pel--erlang-line-3%-comment-p'."
  ;; At beginning of buffer (bobp) → always t.
  (with-temp-buffer
    (should (pel--erlang-line-3%-comment-p)))

  ;; On an empty first line (still bobp) → t.
  (with-temp-buffer
    (insert "")
    (goto-char (point-min))
    (should (pel--erlang-line-3%-comment-p)))

  ;; Line 2 col 0, preceded by a %%% line → t.
  (with-temp-buffer
    (insert "%%% Module header\n")
    (goto-char (point-max))             ; beginning of line 2, col 0
    (should (pel--erlang-line-3%-comment-p)))

  ;; Line 2 col 0, preceded by a %% line → nil.
  (with-temp-buffer
    (insert "%% Module header\n")
    (goto-char (point-max))
    (should-not (pel--erlang-line-3%-comment-p)))

  ;; Line 2 col 0, preceded by a % line → nil.
  (with-temp-buffer
    (insert "% comment\n")
    (goto-char (point-max))
    (should-not (pel--erlang-line-3%-comment-p)))

  ;; Line 2 col 0, preceded by code → nil.
  (with-temp-buffer
    (insert "-module(foo).\n")
    (goto-char (point-max))
    (should-not (pel--erlang-line-3%-comment-p)))

  ;; Not at column 0 → nil, even when previous line is %%%
  (with-temp-buffer
    (insert "%%% header\n  ")           ; point ends at column 2
    (goto-char (point-max))
    (should-not (pel--erlang-line-3%-comment-p)))

  ;; Third line: second line is %%%, third line col 0 → t.
  (with-temp-buffer
    (insert "%%% first\n%%% second\n")
    (goto-char (point-max))
    (should (pel--erlang-line-3%-comment-p)))

  ;; Third line: second line is %%, third line col 0 → nil.
  (with-temp-buffer
    (insert "%%% first\n%% second\n")
    (goto-char (point-max))
    (should-not (pel--erlang-line-3%-comment-p))))

;; ===========================================================================
;; pel--erlang-dirpath
;; ===========================================================================

(defvar pel--erlang-test-tmp-dir nil
  "Temporary directory created for `ert-test-pel--erlang-dirpath'.
Cleaned up in the test's unwind form.")

(ert-deftest ert-test-pel--erlang-dirpath ()
  "Test `pel--erlang-dirpath'."

  ;; nil input → nil output (the `when' guard short-circuits).
  (should (eq nil (pel--erlang-dirpath nil)))

  ;; String that points to a non-existent directory:
  ;; result is a cons (dirpath . error-message).
  (let* ((bad-dir (make-temp-name
                   (expand-file-name "pel-erlang-missing-dir-"
                                     temporary-file-directory)))
         (result (pel--erlang-dirpath bad-dir)))
    (should-not (file-exists-p bad-dir))
    (should (consp result))
    (should (pel-string= (car result) bad-dir bad-dir))
    (should (stringp (cdr result)))
    (should (> (length (cdr result)) 0)))

  ;; String that points to an existing directory:
  ;; result is a list (dirpath) — a single-element list, no error.
  (let* ((good-dir (expand-file-name temporary-file-directory))
         (good-dir (directory-file-name good-dir))
         (pel-erlang-man-parent-rootdir good-dir)
         (result (pel--erlang-dirpath good-dir)))
    (should (listp result))
    (should (= (length result) 1))
    (should (pel-string= (car result) good-dir good-dir)))

  ;; cons-based input: (ignored . ENVVAR-NAME)
  ;; When the environment variable is absent the function returns
  ;;   (cons nil "Defined by absent environment variable VARNAME").
  (let* ((fake-envvar "PEL_TEST_ERLANG_NONEXISTENT_VAR_XYZ")
         (previous-value (getenv fake-envvar)))
    (unwind-protect
        (progn
          (setenv fake-envvar nil)
          (let* ((result (pel--erlang-dirpath (cons nil fake-envvar))))
            (should (consp result))
            (should (eq nil (car result)))
            (should (stringp (cdr result)))
            (should (string-match-p (regexp-quote fake-envvar) (cdr result)))))
      (setenv fake-envvar previous-value)))))

;;; --------------------------------------------------------------------------
(provide 'pel-erlang-test)

;;; pel-erlang-test.el ends here
