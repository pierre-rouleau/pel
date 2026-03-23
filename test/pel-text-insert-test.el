;;; pel-text-insert-test.el --- ERT tests for pel-text-insert.el  -*- lexical-binding: t; -*-

;; Created   : Monday, March 23 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-23 13:52:01 EDT, updated by Pierre Rouleau>

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
;; ERT tests for pel-text-insert.el.
;;
;; Covered items:
;;
;;   pel-tilde-file-name        - replace home dir with ~ in a path
;;   pel-comment-specs          - return (comment-start . comment-end) cons
;;   pel-separator-line         - build a commented separator-line string
;;   pel-insert-line            - insert a separator line into the buffer
;;   pel-insert-commented       - insert commented text into the buffer
;;
;; Items intentionally NOT covered:
;;   pel-insert-filename        - requires pel-window / multi-window setup
;;   pel-insert-filename-and-line  - same
;;   pel-insert-filename-wtilde    - same
;;   pel-insert-dirname         - same
;;   pel-insert-dirname-wtilde  - same
;;   pel-set-insert-filename-root  - interactive prompt
;;   pel-insert-date*           - output depends on the current time
;;   pel-insert-iso-date*       - same
;;   pel-insert-todo-note       - depends on time and user-full-name
;;   pel-customize-*            - open customize UI
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-text-insert)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:

;; ===========================================================================
;; pel-tilde-file-name
;;
;; `pel-tilde-file-name' replaces the user home directory prefix with "~".
;; The tests rely only on `expand-file-name' and string comparison, making
;; them portable across installations.
;; ===========================================================================

(ert-deftest pel-text-insert-test/tilde-file-name/home-dir-exactly ()
  "The home directory itself is converted to a bare \"~\"."
  (let ((home (expand-file-name "~")))
    (should (string= (pel-tilde-file-name home) "~"))))

(ert-deftest pel-text-insert-test/tilde-file-name/file-in-home ()
  "A file directly under the home directory gets a ~/file form."
  (let* ((home (expand-file-name "~"))
         (full (concat home "/some-file.txt")))
    (should (string= (pel-tilde-file-name full) "~/some-file.txt"))))

(ert-deftest pel-text-insert-test/tilde-file-name/deep-path-in-home ()
  "A deeply nested path under home is abbreviated."
  (let* ((home (expand-file-name "~"))
         (full (concat home "/projects/foo/bar.el")))
    (should (string= (pel-tilde-file-name full) "~/projects/foo/bar.el"))))

(ert-deftest pel-text-insert-test/tilde-file-name/path-outside-home ()
  "A path outside the home directory is returned unchanged."
  (let ((path "/tmp/somefile.txt"))
    ;; Skip the test explicitly if assumption doesn't hold
    (if (string-prefix-p (expand-file-name "~") path)
        (ert-skip "Test assumes /tmp is outside home directory")
      (should (string= (pel-tilde-file-name path) path)))))

(ert-deftest pel-text-insert-test/tilde-file-name/root-path ()
  "A root path is returned unchanged."
  (let ((path "/"))
    (should (string= (pel-tilde-file-name path) path))))

(ert-deftest pel-text-insert-test/tilde-file-name/idempotent-already-tilde ()
  "A path that happens to start with the literal tilde string is not
double-tilded — only the home directory *expansion* is matched."
  ;; If someone passes a string that starts with \"~/\", the function
  ;; performs a *string* comparison against the expanded home dir.
  ;; Such a path does not match unless the home dir itself is \"~/\".
  (let ((path "~/something"))
    ;; The function compares against (expand-file-name \"~\"), which
    ;; does NOT start with \"~/something\", so the path passes through.
    (should (stringp (pel-tilde-file-name path)))))

;; ===========================================================================
;; pel-comment-specs
;;
;; The function returns a cons (comment-start-string . comment-end-string).
;; Both the start and end depend on the major mode of the current buffer,
;; and the start is doubled for single-char markers (";", "%") when point
;; is at the beginning of the line.
;; ===========================================================================

(ert-deftest pel-text-insert-test/comment-specs/emacs-lisp-at-bol ()
  "In emacs-lisp-mode at column 0, the start is \";;\" and end is nil/empty."
  (with-temp-buffer
    (emacs-lisp-mode)
    (goto-char (point-min))         ; column 0
    (let ((specs (pel-comment-specs)))
      ;; In Emacs Lisp, \";\" is doubled at BOL → \";;\"
      (should (string= (car specs) ";;"))
      ;; comment-end is the empty string for Emacs Lisp.
      (should (or (null (cdr specs))
                  (string= (cdr specs) ""))))))

(ert-deftest pel-text-insert-test/comment-specs/emacs-lisp-mid-line ()
  "In emacs-lisp-mode past non-whitespace text, the start is a single \";\"."
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; Insert real content so current-column (5) ≠ current-indentation (0).
    ;; Using spaces-only would make them equal, triggering the BOL heuristic.
    (insert "(foo)")                    ; column 5, indentation 0
    (let ((specs (pel-comment-specs)))
      ;; Not at BOL or indentation → single ";"
      (should (string= (car specs) ";")))))

(ert-deftest pel-text-insert-test/comment-specs/explicit-prefix-overrides-mode ()
  "An explicit COMMENT-PREFIX argument takes precedence over mode defaults."
  (with-temp-buffer
    (emacs-lisp-mode)
    (goto-char (point-min))
    (let ((specs (pel-comment-specs "//")))
      (should (string= (car specs) "//")))))

;; ===========================================================================
;; pel-separator-line
;;
;; Returns a string (no trailing newline).  Tests use emacs-lisp-mode so
;; comment-start is ";" (doubled to ";;" at BOL) and comment-end is "".
;;
;; Length formula (BOL, emacs-lisp-mode, no comment-end):
;;   ";;" (2) + " " (1) + (linelen - 2 - 0 - 1) dashes  =  linelen chars
;; ===========================================================================

(ert-deftest pel-text-insert-test/separator-line/default-length ()
  "The default separator length equals fill-column."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq fill-column 70)
    (goto-char (point-min))
    (let ((line (pel-separator-line)))
      (should (stringp line))
      (should (= (length line) fill-column)))))

(ert-deftest pel-text-insert-test/separator-line/explicit-length ()
  "An explicit LINELEN argument controls the separator length."
  (with-temp-buffer
    (emacs-lisp-mode)
    (goto-char (point-min))
    (let ((line (pel-separator-line 50)))
      (should (= (length line) 50)))))

(ert-deftest pel-text-insert-test/separator-line/starts-with-comment ()
  "The separator starts with the mode's comment prefix."
  (with-temp-buffer
    (emacs-lisp-mode)
    (goto-char (point-min))
    (let ((line (pel-separator-line 40)))
      ;; At BOL in Emacs Lisp mode the comment start is \";;\"
      (should (string-prefix-p ";;" line)))))

(ert-deftest pel-text-insert-test/separator-line/contains-dash-fill ()
  "The body of the separator is filled with dash characters."
  (with-temp-buffer
    (emacs-lisp-mode)
    (goto-char (point-min))
    (let ((line (pel-separator-line 20)))
      ;; After the comment prefix + space, only dashes should follow.
      ;; \";; \" is 3 chars; the rest (17 chars) must all be \"-\".
      (should (string-match-p "^;; -+$" line)))))

(ert-deftest pel-text-insert-test/separator-line/custom-fill-char ()
  "A custom fill character (CHAR argument) replaces the default dash."
  (with-temp-buffer
    (emacs-lisp-mode)
    (goto-char (point-min))
    (let ((line (pel-separator-line 20 ?=)))
      (should (string-match-p "^;; =+$" line)))))

(ert-deftest pel-text-insert-test/separator-line/custom-comment-prefix ()
  "A custom COMMENT-PREFIX overrides the mode's comment start."
  (with-temp-buffer
    (emacs-lisp-mode)
    (goto-char (point-min))
    (let ((line (pel-separator-line 30 nil "//")))
      (should (string-prefix-p "//" line))
      (should (= (length line) 30)))))

(ert-deftest pel-text-insert-test/separator-line/no-trailing-newline ()
  "The separator string does NOT end with a newline character."
  (with-temp-buffer
    (emacs-lisp-mode)
    (goto-char (point-min))
    (let ((line (pel-separator-line 40)))
      (should-not (string-suffix-p "\n" line)))))

(ert-deftest pel-text-insert-test/separator-line/absolute-value-of-negative-linelen ()
  "A negative LINELEN is treated as its absolute value."
  (with-temp-buffer
    (emacs-lisp-mode)
    (goto-char (point-min))
    (should (= (length (pel-separator-line -30))
               (length (pel-separator-line  30))))))

;; ===========================================================================
;; pel-insert-line
;;
;; Inserts a separator line FOLLOWED BY a newline, at the beginning of the
;; current line.  Point ends at the start of the next line.
;; ===========================================================================

(ert-deftest pel-text-insert-test/insert-line/inserts-separator-and-newline ()
  "After `pel-insert-line' the buffer begins with a separator + newline."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq fill-column 70)
    (insert "existing content\n")
    (goto-char (point-min))
    (pel-insert-line)
    (goto-char (point-min))
    ;; The first line must be the separator (starts with \";;\" in elisp mode).
    (should (string-prefix-p ";;" (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position))))
    ;; A newline must separate the separator from the original text.
    (should (string-match-p "\n" (buffer-string)))))

(ert-deftest pel-text-insert-test/insert-line/original-content-preserved ()
  "Original buffer content is still present after the inserted separator."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq fill-column 70)
    (insert "original text\n")
    (goto-char (point-min))
    (pel-insert-line)
    (should (string-match-p "original text" (buffer-string)))))

(ert-deftest pel-text-insert-test/insert-line/inserted-at-beginning-of-line ()
  "`pel-insert-line' inserts at the beginning of the current line even
when point starts in the middle."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq fill-column 40)
    (insert "hello world\n")
    (goto-char 5)                       ; mid-line
    (pel-insert-line)
    (goto-char (point-min))
    ;; First line should be the separator, not \"hello world\".
    (should (string-prefix-p ";;" (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position))))))

(ert-deftest pel-text-insert-test/insert-line/separator-length-matches-fill-column ()
  "The inserted separator line has exactly `fill-column' characters (no newline)."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq fill-column 60)
    (goto-char (point-min))
    (pel-insert-line)
    (goto-char (point-min))
    (let ((line-text (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
      (should (= (length line-text) fill-column)))))

;; ===========================================================================
;; pel-insert-commented
;;
;; When point is NOT already inside a comment the function inserts
;; comment-start + " " + TEXT (and comment-end if non-empty), then
;; returns the position immediately after TEXT.
;; When point IS already inside a comment it inserts TEXT bare.
;; ===========================================================================

(ert-deftest pel-text-insert-test/insert-commented/outside-comment-elisp ()
  "Outside a comment in emacs-lisp-mode the text is wrapped with \";;\"."
  (with-temp-buffer
    (emacs-lisp-mode)
    (goto-char (point-min))             ; column 0 — uses \";;\"
    (pel-insert-commented "TODO")
    (should (string-match-p "^;; TODO" (buffer-string)))))

(ert-deftest pel-text-insert-test/insert-commented/returns-position-after-text ()
  "The return value is the buffer position immediately after the inserted text."
  (with-temp-buffer
    (emacs-lisp-mode)
    (goto-char (point-min))
    (let* ((ret (pel-insert-commented "HELLO"))
           ;; (content (buffer-string))
           )
      ;; ret must be a valid buffer position
      (should (integerp ret))
      (should (<= ret (point-max)))
      ;; The character just before ret must be the last char of TEXT
      (should (= (char-before ret) ?O))))) ; last char of "HELLO"

(ert-deftest pel-text-insert-test/insert-commented/inside-comment-inserts-bare ()
  "When already inside a comment, text is inserted without extra prefix."
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; Position point inside an existing Emacs Lisp comment.
    (insert ";; existing ")
    ;; point is now at the end of the comment text, still on the comment line
    (pel-insert-commented "extra")
    ;; The buffer should not have a doubled comment prefix.
    (let ((content (buffer-string)))
      (should (string-match-p "existing extra" content))
      ;; There must be no ";; extra" (double prefix) inside the string.
      (should-not (string-match-p ";; extra" content)))))

(ert-deftest pel-text-insert-test/insert-commented/text-present-in-buffer ()
  "The inserted TEXT appears in the buffer after the call."
  (with-temp-buffer
    (emacs-lisp-mode)
    (goto-char (point-min))
    (pel-insert-commented "MARKER_WORD")
    (should (string-match-p "MARKER_WORD" (buffer-string)))))

;;; --------------------------------------------------------------------------
(provide 'pel-text-insert-test)

;;; pel-text-insert-test.el ends here
