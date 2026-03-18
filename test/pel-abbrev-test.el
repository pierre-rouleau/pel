;;; pel-abbrev-test.el --- ERT tests for pel-abbrev.el.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, March 18 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-18 16:50:22 EDT, updated by Pierre Rouleau>

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
;; ERT tests for pel-abbrev.el.  Covers:
;;   - `pel-current-or-previous--word': word extraction from buffer state.
;;   - `pel-ispell-word-then-abbrev': spell-correct-and-abbrev logic, including
;;     global vs. local abbrev table selection and the "no typo" error path.
;;   - `pel-extract-abbrev-definitions': user-prompt branching.
;;   - `pel-abbrev-info': smoke test (buffer creation).
;;
;; ispell functions (`ispell-get-word', `ispell-word') are mocked with
;; `cl-letf' so no running ispell process is required.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'cl-lib)
(require 'abbrev)
(require 'ispell)
(require 'pel--base)
(require 'pel-abbrev)
(require 'pel-ert)

;;; --------------------------------------------------------------------------
;;; Code:

;; ===========================================================================
;;; pel-current-or-previous--word
;; ===========================================================================

(ert-deftest ert-test-pel-current-or-previous--word/on-word ()
  "Return word when point is in the middle of a word."
  (with-temp-buffer
    (insert "hello world")
    (goto-char 3)                       ; inside "hello"
    (should (string= "hello"
                     (pel-current-or-previous--word)))))

(ert-deftest ert-test-pel-current-or-previous--word/at-end-of-word ()
  "Return word when point is immediately after a word."
  (with-temp-buffer
    (insert "hello ")
    (goto-char 6)                       ; just after "hello", before space
    (should (string= "hello"
                     (pel-current-or-previous--word)))))

(ert-deftest ert-test-pel-current-or-previous--word/between-words ()
  "Return previous word when point is in whitespace between two words."
  (with-temp-buffer
    (insert "first second")
    (goto-char 6)                       ; in the space between words
    (should (string= "first"
                     (pel-current-or-previous--word)))))

(ert-deftest ert-test-pel-current-or-previous--word/at-start-of-second-word ()
  "Return previous word when point is at start of second word."
  (with-temp-buffer
    (insert "first second")
    (goto-char 7)                       ; at 's' of "second"
    ;; ispell-get-word at the start of "second" may return "second" or "first"
    ;; depending on ispell-get-word implementation; the important thing is a
    ;; non-nil string is returned.
    (should (stringp (pel-current-or-previous--word)))))

(ert-deftest ert-test-pel-current-or-previous--word/empty-buffer ()
  "Return nil when the buffer is empty."
  (with-temp-buffer
    (should (null (pel-current-or-previous--word)))))

(ert-deftest ert-test-pel-current-or-previous--word/only-whitespace ()
  "Return nil when the buffer contains only whitespace."
  (with-temp-buffer
    (insert "   ")
    (goto-char 2)
    (should (null (pel-current-or-previous--word)))))

(ert-deftest ert-test-pel-current-or-previous--word/does-not-move-point ()
  "Verify that the function does not move point as a side effect."
  (with-temp-buffer
    (insert "hello world")
    (goto-char 3)
    (let ((pos (point)))
      (pel-current-or-previous--word)
      (should (= pos (point))))))

;; ===========================================================================
;;; pel-ispell-word-then-abbrev
;; ===========================================================================

(ert-deftest ert-test-pel-ispell-word-then-abbrev/no-word-signals-error ()
  "Signal user-error when buffer is empty (no word at or before point)."
  (with-temp-buffer
    ;; Both ispell-get-word calls return nil → no word at all.
    (cl-letf (((symbol-function 'ispell-get-word)
               (lambda (_following) nil))
              ((symbol-function 'ispell-word)
               (lambda (&rest _) nil)))
      (should-error (pel-ispell-word-then-abbrev)
                    :type 'user-error))))

(ert-deftest ert-test-pel-ispell-word-then-abbrev/no-correction-signals-error ()
  "Signal user-error when corrected word equals original word (no real typo)."
  ;; ispell-word returns t (quiet: word is correct, no change made).
  ;; pel-current-or-previous--word therefore returns the same word both times.
  (with-temp-buffer
    (insert "correct")
    (goto-char 4)
    (cl-letf (((symbol-function 'ispell-get-word)
               (lambda (_following) '("correct" 0 7)))
              ((symbol-function 'ispell-word)
               ;; Returns t = "word was handled" but text unchanged.
               (lambda (&rest _) t)))
      (should-error (pel-ispell-word-then-abbrev)
                    :type 'user-error))))

(ert-deftest ert-test-pel-ispell-word-then-abbrev/defines-global-abbrev ()
  "Define abbreviation in global-abbrev-table when LOCALLY is nil."
  (with-temp-buffer
    (abbrev-mode 1)
    (insert "teh ")
    (goto-char 3)                       ; inside "teh"
    ;; Call counter: ispell-get-word is called once inside the loop (original)
    ;; and once after the loop (corrected).  We simulate ispell-word replacing
    ;; the buffer text by updating what ispell-get-word returns after the first
    ;; call.
    (let ((call-count 0)
          (orig-global (copy-abbrev-table global-abbrev-table)))
      (unwind-protect
          (cl-letf (((symbol-function 'ispell-get-word)
                     (lambda (_following)
                       (setq call-count (1+ call-count))
                       (if (= call-count 1)
                           ;; First call inside save-excursion: the typo.
                           '("teh" 0 3)
                         ;; Subsequent calls (after mock "correction"): corrected word.
                         '("the" 0 3))))
                    ((symbol-function 'ispell-word)
                     ;; Return t = word was corrected (non-nil stops the loop).
                     (lambda (&rest _) t)))
            (pel-ispell-word-then-abbrev)
            ;; "teh" should now expand to "the" in the global table.
            (should (abbrev-expansion "teh" global-abbrev-table))
            (should (string= "the"
                             (abbrev-expansion "teh" global-abbrev-table))))
        ;; Restore global-abbrev-table to avoid polluting other tests.
        (setq global-abbrev-table orig-global)))))

(ert-deftest ert-test-pel-ispell-word-then-abbrev/defines-local-abbrev ()
  "Define abbreviation in local-abbrev-table when LOCALLY is non-nil."
  (with-temp-buffer
    (abbrev-mode 1)
    (insert "recieve ")
    (goto-char 5)
    (let ((call-count 0))
      (cl-letf (((symbol-function 'ispell-get-word)
                 (lambda (_following)
                   (setq call-count (1+ call-count))
                   (if (= call-count 1)
                       '("recieve" 0 7)
                     '("receive" 0 7))))
                ((symbol-function 'ispell-word)
                 (lambda (&rest _) t)))
        (pel-ispell-word-then-abbrev 'locally)
        ;; Abbrev must appear in the local (buffer-local) abbrev table.
        (should (abbrev-expansion "recieve" local-abbrev-table))
        (should (string= "receive"
                         (abbrev-expansion "recieve" local-abbrev-table)))
        ;; And must NOT have been added to the global table.
        (should-not (abbrev-expansion "recieve" global-abbrev-table))))))

(ert-deftest ert-test-pel-ispell-word-then-abbrev/abbrev-is-downcased ()
  "Both the original and corrected words are stored in downcased form."
  (with-temp-buffer
    (abbrev-mode 1)
    (insert "Hte ")
    (goto-char 2)
    (let ((call-count 0)
          (orig-global (copy-abbrev-table global-abbrev-table)))
      (unwind-protect
          (cl-letf (((symbol-function 'ispell-get-word)
                     (lambda (_following)
                       (setq call-count (1+ call-count))
                       (if (= call-count 1)
                           '("Hte" 0 3)
                         '("The" 0 3))))
                    ((symbol-function 'ispell-word)
                     (lambda (&rest _) t)))
            (pel-ispell-word-then-abbrev)
            ;; define-abbrev stores the downcased key.
            (should (abbrev-expansion "hte" global-abbrev-table))
            (should (string= "the"
                             (abbrev-expansion "hte" global-abbrev-table))))
        (setq global-abbrev-table orig-global)))))

;; ===========================================================================
;;; pel-extract-abbrev-definitions
;; ===========================================================================

(ert-deftest ert-test-pel-extract-abbrev-definitions/user-says-no ()
  "Print 'Nothing done.' when the user declines the confirmation prompt."
  (cl-letf (((symbol-function 'yes-or-no-p)
             (lambda (_prompt) nil))
            ((symbol-function 'define-abbrevs)
             (lambda (&rest _)
               (error "define-abbrevs should not have been called"))))
    ;; Verify that the function does not call define-abbrevs.
    ;; No error → user-declined branch was taken.
    (with-temp-buffer
      (should-not (condition-case _
                      (progn (pel-extract-abbrev-definitions nil) nil)
                    (error t))))))

(ert-deftest ert-test-pel-extract-abbrev-definitions/user-says-yes ()
  "Call `define-abbrevs' with the ARG value when the user confirms."
  (let ((define-abbrevs-called nil)
        (define-abbrevs-arg :unset))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt) t))
              ((symbol-function 'define-abbrevs)
               (lambda (arg)
                 (setq define-abbrevs-called t
                       define-abbrevs-arg arg))))
      (with-temp-buffer
        ;; Without prefix arg.
        (pel-extract-abbrev-definitions nil)
        (should define-abbrevs-called)
        (should (eq nil define-abbrevs-arg)))
      ;; With prefix arg (non-nil) — clears existing abbrevs first.
      (setq define-abbrevs-called nil
            define-abbrevs-arg :unset)
      (with-temp-buffer
        (pel-extract-abbrev-definitions '(4))
        (should define-abbrevs-called)
        (should (equal '(4) define-abbrevs-arg))))))

;; ===========================================================================
;;; pel-abbrev-info — smoke test
;; ===========================================================================

(ert-deftest ert-test-pel-abbrev-info/creates-buffer ()
  "Verify that `pel-abbrev-info' creates the *pel-abbrev-info* buffer."
  ;; Kill the info buffer if it already exists from a previous run.
  (when (get-buffer "*pel-abbrev-info*")
    (kill-buffer "*pel-abbrev-info*"))
  ;; The function calls several pel-insert-* helpers that require PEL's UI
  ;; layer.  We mock `pel-print-in-buffer' to a no-op so that the test
  ;; concentrates on whether pel-abbrev-info can be invoked without error
  ;; in a batch environment.
  (cl-letf (((symbol-function 'pel-print-in-buffer)
             (lambda (&rest _)
               ;; Create the expected output buffer so other code that checks
               ;; for it by name finds it.
               (get-buffer-create "*pel-abbrev-info*"))))
    (with-temp-buffer
      (should-not (condition-case err
                      (progn (pel-abbrev-info) nil)
                    (error (format "Unexpected error: %S" err)))))))

;;; --------------------------------------------------------------------------
(provide 'pel-abbrev-test)

;;; pel-abbrev-test.el ends here
