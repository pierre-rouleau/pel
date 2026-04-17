;;; pel-read-test.el --- Test pel-read.el  -*- lexical-binding: t; -*-

;; Created   : Thursday, April 16 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-04-16 22:35:21 EDT, updated by Pierre Rouleau>

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
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:

(require 'pel-read)
(require 'ert)
(require 'cl-lib)   ; use: `cl-letf'


;;; --------------------------------------------------------------------------
;;; Code:
;;

;; ---------------------------------------------------------------------------
;; Helpers

(defmacro pel-read-test--with-buffer (content &rest body)
  "Execute BODY in a temp buffer pre-filled with CONTENT, point at start."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;; ---------------------------------------------------------------------------
;; pel-thing-at-point

(ert-deftest pel-read/thing-at-point/returns-word ()
  "Returns the word text when point is on a word."
  (pel-read-test--with-buffer "hello world"
    (should (string= "hello" (pel-thing-at-point 'word)))))

(ert-deftest pel-read/thing-at-point/moves-to-end-of-thing ()
  "Moves point to the end of the matched thing (cdr of bounds)."
  (pel-read-test--with-buffer "hello world"
    (pel-thing-at-point 'word)
    (should (= (point) 6))))          ; after "hello" (1-based, 6 = past 'o')

(ert-deftest pel-read/thing-at-point/signals-user-error-when-absent ()
  "Signals `user-error' when no word thing is at point (whitespace or empty)."
  ;; Use a buffer with only whitespace: Emacs 26.1 guards are in pel-thing-at-point.
  (pel-read-test--with-buffer "   "
    (should-error (pel-thing-at-point 'word) :type 'user-error))
  ;; Also verify with an empty buffer.
  (with-temp-buffer
    (should-error (pel-thing-at-point 'word) :type 'user-error)))

(ert-deftest pel-read/thing-at-point/returns-sentence ()
  "Returns the sentence text at point."
  (pel-read-test--with-buffer "Hello world.  Goodbye."
    (should (string= "Hello world." (pel-thing-at-point 'sentence)))))

(ert-deftest pel-read/thing-at-point/empty-buffer-signals-user-error ()
  "Signals `user-error' in an empty buffer."
  (with-temp-buffer
    (should-error (pel-thing-at-point 'word) :type 'user-error)))

;; ---------------------------------------------------------------------------
;; pel-word-at-point

(ert-deftest pel-read/word-at-point/returns-word ()
  "Returns the word string at point."
  (pel-read-test--with-buffer "emacs lisp"
    (should (string= "emacs" (pel-word-at-point t)))))

(ert-deftest pel-read/word-at-point/dont-move-suppresses-forward-call ()
  "When DONT-MOVE is non-nil, pel-forward-word-start is NOT called."
  (pel-read-test--with-buffer "emacs lisp"
    (let ((called nil))
      (cl-letf (((symbol-function 'pel-forward-word-start)
                 (lambda () (setq called t))))
        (pel-word-at-point t)
        (should-not called)))))

(ert-deftest pel-read/word-at-point/no-dont-move-calls-forward ()
  "When DONT-MOVE is nil, pel-forward-word-start IS called."
  (pel-read-test--with-buffer "emacs lisp"
    (let ((called nil))
      (cl-letf (((symbol-function 'pel-forward-word-start)
                 (lambda () (setq called t))))
        (pel-word-at-point)
        (should called)))))

(ert-deftest pel-read/word-at-point/no-word-signals-user-error ()
  "Signals `user-error' when no word is at point (whitespace or empty)."
  (pel-read-test--with-buffer "   "
    (should-error (pel-word-at-point t) :type 'user-error))
  ;; test empty buffer
  (with-temp-buffer
    (should-error (pel-word-at-point t) :type 'user-error)))

(ert-deftest pel-read/word-at-point/return-type-is-string ()
  "Return value is always a string."
  (pel-read-test--with-buffer "test"
    (should (stringp (pel-word-at-point t)))))

;; ---------------------------------------------------------------------------
;; pel-sentence-at-point

(ert-deftest pel-read/sentence-at-point/returns-sentence ()
  "Returns the sentence text at point."
  (pel-read-test--with-buffer "Hello world.  Goodbye."
    (should (string= "Hello world." (pel-sentence-at-point t)))))

(ert-deftest pel-read/sentence-at-point/dont-move-suppresses-forward-call ()
  "When DONT-MOVE is non-nil, pel-forward-word-start is NOT called."
  (pel-read-test--with-buffer "Hello world.  Goodbye."
    (let ((called nil))
      (cl-letf (((symbol-function 'pel-forward-word-start)
                 (lambda () (setq called t))))
        (pel-sentence-at-point t)
        (should-not called)))))

(ert-deftest pel-read/sentence-at-point/no-dont-move-calls-forward ()
  "When DONT-MOVE is nil, pel-forward-word-start IS called."
  (pel-read-test--with-buffer "Hello world.  Goodbye."
    (let ((called nil))
      (cl-letf (((symbol-function 'pel-forward-word-start)
                 (lambda () (setq called t))))
        (pel-sentence-at-point)
        (should called)))))

(ert-deftest pel-read/sentence-at-point/no-sentence-signals-user-error ()
  "Signals `user-error' in an empty buffer (no sentence)."
  (with-temp-buffer
    (should-error (pel-sentence-at-point t) :type 'user-error)))

;; ---------------------------------------------------------------------------
;; pel-paragraph-at-point

(ert-deftest pel-read/paragraph-at-point/returns-paragraph ()
  "Returns a string containing paragraph text at point."
  (pel-read-test--with-buffer "First paragraph.\n\nSecond paragraph."
    (let ((para (pel-paragraph-at-point t)))
      (should (stringp para))
      (should (string-match-p "First paragraph" para)))))

(ert-deftest pel-read/paragraph-at-point/dont-move-suppresses-forward-call ()
  "When DONT-MOVE is non-nil, pel-forward-word-start is NOT called."
  (pel-read-test--with-buffer "First paragraph.\n\nSecond paragraph."
    (let ((called nil))
      (cl-letf (((symbol-function 'pel-forward-word-start)
                 (lambda () (setq called t))))
        (pel-paragraph-at-point t)
        (should-not called)))))

(ert-deftest pel-read/paragraph-at-point/no-dont-move-calls-forward ()
  "When DONT-MOVE is nil, pel-forward-word-start IS called."
  (pel-read-test--with-buffer "First paragraph.\n\nSecond paragraph."
    (let ((called nil))
      (cl-letf (((symbol-function 'pel-forward-word-start)
                 (lambda () (setq called t))))
        (pel-paragraph-at-point)
        (should called)))))

(ert-deftest pel-read/paragraph-at-point/no-paragraph-signals-user-error ()
  "Signals `user-error' when no paragraph exists at point."
  (with-temp-buffer
    (should-error (pel-paragraph-at-point t) :type 'user-error)))

;; ---------------------------------------------------------------------------
;; pel-string-at-point

(ert-deftest pel-read/string-at-point/point-inside-quoted-string ()
  "Extracts the substring between quote delimiters when point is inside."
  (pel-read-test--with-buffer "\"hello\""
                              (goto-char 2) ; inside the quotes
                              (should (string= "hello" (pel-string-at-point "\"")))))

(ert-deftest pel-read/string-at-point/point-at-opening-delimiter ()
  "Extracts substring correctly when point is at the opening delimiter."
  (pel-read-test--with-buffer "\"hello\""
    (goto-char 1)                         ; at opening quote
    (should (string= "hello" (pel-string-at-point "\"")))))

(ert-deftest pel-read/string-at-point/empty-buffer-returns-empty-string ()
  "Returns empty string when buffer is empty (no next char)."
  (with-temp-buffer
    (should (string= "" (pel-string-at-point "\"")))))

(ert-deftest pel-read/string-at-point/space-auto-added-as-delimiter ()
  "Space is auto-added as delimiter when point is not at a delimiter char."
  (pel-read-test--with-buffer "hello world foo"
    (goto-char 3)                         ; inside "hello", not at delimiter
    (should (string= "hello" (pel-string-at-point "\"")))))

(ert-deftest pel-read/string-at-point/allow-space-includes-spaces ()
  "With ALLOW-SPACE non-nil, spaces are included in the extracted string."
  (pel-read-test--with-buffer "\"hello world\""
    (goto-char 2)
    (should (string= "hello world" (pel-string-at-point "\"" t)))))

(ert-deftest pel-read/string-at-point/multiple-delimiter-chars ()
  "Supports a multi-character DELIMITERS string."
  (pel-read-test--with-buffer "(hello)"
    (goto-char 2)                         ; inside parens
    (should (string= "hello" (pel-string-at-point "()")))))

(ert-deftest pel-read/string-at-point/does-not-move-point ()
  "Does not move point (wrapped in save-excursion)."
  (pel-read-test--with-buffer "\"hello\""
    (goto-char 2)
    (let ((pos (point)))
      (pel-string-at-point "\"")
      (should (= pos (point))))))

;; ---------------------------------------------------------------------------
;; pel-move-to-face

(ert-deftest pel-read/move-to-face/finds-face-and-returns-point ()
  "Moves to the first char with the given face and returns that position."
  (with-temp-buffer
    (insert "abcdef")
    ;; chars at positions 3-5 ("cde") get the test face
    (put-text-property 3 6 'face 'pel-test-face)
    (goto-char (point-min))
    (let ((result (pel-move-to-face 'pel-test-face (point-max))))
      (should (= result 3))
      (should (= (point) 3)))))

(ert-deftest pel-read/move-to-face/returns-nil-when-face-absent ()
  "Returns nil when the face is never found before limit."
  (with-temp-buffer
    (insert "abcdef")
    (goto-char (point-min))
    (should (null (pel-move-to-face 'nonexistent-face (point-max))))))

(ert-deftest pel-read/move-to-face/respects-limit ()
  "Does not cross the limit even if the face appears beyond it."
  (with-temp-buffer
    (insert "abcdef")
    (put-text-property 5 7 'face 'pel-test-face)  ; "ef" has the face
    (goto-char (point-min))
    ;; limit is 4, face starts at 5 → not found
    (should (null (pel-move-to-face 'pel-test-face 4)))))

;; ---------------------------------------------------------------------------
;; pel-move-past-face

(ert-deftest pel-read/move-past-face/advances-past-face-region ()
  "Moves point past the contiguous region carrying the specified face."
  (with-temp-buffer
    (insert "abcdef")
    ;; positions 1-4 ("abcd") have the face
    (put-text-property 1 5 'face 'pel-test-face)
    (goto-char (point-min))
    (let ((result (pel-move-past-face 'pel-test-face (point-max))))
      (should (= result 5))
      (should (= (point) 5)))))

(ert-deftest pel-read/move-past-face/returns-nil-when-face-fills-to-limit ()
  "Returns nil when the face region extends all the way to limit."
  (with-temp-buffer
    (insert "abcdef")
    (put-text-property 1 7 'face 'pel-test-face)  ; entire content
    (goto-char (point-min))
    (should (null (pel-move-past-face 'pel-test-face (point-max))))))

(ert-deftest pel-read/move-past-face/no-face-at-start-returns-current-point ()
  "Returns current point immediately when point is not on the given face."
  (with-temp-buffer
    (insert "abcdef")
    (put-text-property 3 6 'face 'pel-test-face)  ; only "cde"
    (goto-char (point-min))                        ; position 1 — no face here
    (let ((result (pel-move-past-face 'pel-test-face (point-max))))
      (should (= result 1)))))

;; ---------------------------------------------------------------------------
;; pel-customize-symbol-at-line

(ert-deftest pel-read/customize-symbol-at-line/returns-nil-without-face ()
  "Returns nil when no custom-variable-tag face is present on the line."
  (with-temp-buffer
    (insert "Some plain text line")
    (goto-char (point-min))
    (should (null (pel-customize-symbol-at-line)))))

(ert-deftest pel-read/customize-symbol-at-line/returns-tagged-text ()
  "Returns the text carrying the custom-variable-tag face."
  (with-temp-buffer
    (insert "   My Symbol   rest of line")
    ;;       123456789012345
    ;; "My Symbol" occupies positions 4-12 (9 chars, exclusive end = 13)
    (put-text-property 4 13 'face 'custom-variable-tag)
    (goto-char (point-min))
    (should (string= "My Symbol" (pel-customize-symbol-at-line)))))

(ert-deftest pel-read/customize-symbol-at-line/does-not-move-point ()
  "Does not move point (wrapped in save-excursion)."
  (with-temp-buffer
    (insert "   My Symbol   rest of line")
    (put-text-property 4 13 'face 'custom-variable-tag)
    (goto-char (point-min))
    (let ((pos (point)))
      (pel-customize-symbol-at-line)
      (should (= pos (point))))))

(ert-deftest pel-read/customize-symbol-at-line/ignores-other-lines ()
  "Does not see a tagged symbol that is on a different line."
  (with-temp-buffer
    (insert "plain line\nTagged Symbol more text")
    ;;                    ^pos 12
    (put-text-property 12 25 'face 'custom-variable-tag)
    (goto-char (point-min))            ; still on line 1
    (should (null (pel-customize-symbol-at-line)))))

(ert-deftest pel-read/customize-symbol-at-line/empty-buffer-returns-nil ()
  "Returns nil in an empty buffer."
  (with-temp-buffer
    (should (null (pel-customize-symbol-at-line)))))

(ert-deftest pel-read/customize-symbol-at-line/symbol-with-spaces ()
  "Correctly extracts a symbol whose text contains spaces."
  (with-temp-buffer
    (insert "Foo Bar Baz end")
    ;;       123456789012
    ;; "Foo Bar Baz" at 1-11 (exclusive end 12)
    (put-text-property 1 12 'face 'custom-variable-tag)
    (goto-char (point-min))
    (should (string= "Foo Bar Baz" (pel-customize-symbol-at-line)))))

;;; --------------------------------------------------------------------------
(provide 'pel-read-test)

;;; pel-read-test.el ends here
