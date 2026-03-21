;;; pel-mark-test.el --- ERT tests for pel-mark.el.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, March 18 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-21 10:12:48 EDT, updated by Pierre Rouleau>

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
;; ERT tests for pel-mark.el.  Covers:
;;   - `pel-global-mark-buffer-positions': global-mark-ring mapping.
;;   - `pel-mark-ring-positions': mark-ring position extraction.
;;   - `pel-popoff-mark-ring': destructive removal of the top mark-ring entry.
;;   - `pel-mark-line-up': region marking upward (no-mark and extend cases).
;;   - `pel-mark-line-down': region marking downward (no-mark and extend cases).
;;   - `pel-push-mark-no-activate': push mark without activating the region.
;;   - `pel-exchange-point-and-mark-no-activate': swap point/mark, no region.
;;   - `pel-mark-ring-stats' and `pel-jump-to-mark': smoke tests.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'cl-lib)
(require 'pel--base)
(require 'pel-mark)
(require 'pel-ert)

;;; --------------------------------------------------------------------------
;;; Code:

;; ---------------------------------------------------------------------------
;;; Helpers
;; ---------------------------------------------------------------------------

(defmacro pel-mark-test--with-3-line-buffer (&rest body)
  "Run BODY in a temporary buffer containing three numbered lines.
The buffer text is:
  line one\\n
  line two\\n
  line three\\n
Point starts at the beginning of line two (position 10)."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert "line one\nline two\nline three\n")
     ;; Move to the beginning of \"line two\".
     (goto-char (point-min))
     (forward-line 1)
     (setq mark-active nil)
     ,@body))

(defmacro pel-mark-test--with-transient-mark-mode-on (&rest body)
  "Run BODY with deterministic mark-related state."
  (declare (indent 0) (debug t))
  `(let ((saved-transient-mark-mode transient-mark-mode)
         (inhibit-message t))
     (unwind-protect
         (progn
           (transient-mark-mode 1)
           ,@body)
       (transient-mark-mode (if saved-transient-mark-mode 1 -1)))))

;; ---------------------------------------------------------------------------
;;; pel-global-mark-buffer-positions
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-global-mark-buffer-positions/empty ()
  "Return nil when `global-mark-ring' is empty."
  (let ((global-mark-ring nil))
    (should (null (pel-global-mark-buffer-positions)))))

(ert-deftest ert-test-pel-global-mark-buffer-positions/single-entry ()
  "Return a one-element list with the correct cons cell for one marker."
  (with-temp-buffer
    (insert "abc")
    (goto-char 2)
    (let* ((m (point-marker))
           (global-mark-ring (list m)))
      (let ((result (pel-global-mark-buffer-positions)))
        (should (= 1 (length result)))
        (should (equal (buffer-name) (car (car result))))
        (should (= (marker-position m) (cdr (car result))))))))

(ert-deftest ert-test-pel-global-mark-buffer-positions/multiple-entries ()
  "Return a cons cell per marker, preserving order."
  (let* ((buf1 (generate-new-buffer " *pel-gmbp-1*"))
         (buf2 (generate-new-buffer " *pel-gmbp-2*")))
    (unwind-protect
        (let (m1 m2)
          (with-current-buffer buf1
            (erase-buffer)
            (insert "buffer one text")
            (goto-char 5)
            (setq m1 (point-marker)))
          (with-current-buffer buf2
            (erase-buffer)
            (insert "buffer two text")
            (goto-char 8)
            (setq m2 (point-marker)))
          (let* ((global-mark-ring (list m1 m2))
                 (result (pel-global-mark-buffer-positions)))
            (should (= 2 (length result)))
            ;; First element corresponds to m1.
            (should (equal (buffer-name buf1) (car (nth 0 result))))
            (should (= (marker-position m1)   (cdr (nth 0 result))))
            ;; Second element corresponds to m2.
            (should (equal (buffer-name buf2) (car (nth 1 result))))
            (should (= (marker-position m2)   (cdr (nth 1 result))))))
      (kill-buffer buf1)
      (kill-buffer buf2))))

(ert-deftest ert-test-pel-global-mark-buffer-positions/returns-list-of-cons ()
  "Every element of the result is a cons cell (string . integer)."
  (with-temp-buffer
    (insert "hello")
    (let* ((m1 (progn (goto-char 1) (point-marker)))
           (m2 (progn (goto-char 3) (point-marker)))
           (global-mark-ring (list m1 m2))
           (result (pel-global-mark-buffer-positions)))
      (dolist (cell result)
        (should (consp cell))
        (should (stringp (car cell)))
        (should (integerp (cdr cell)))))))

;; ---------------------------------------------------------------------------
;;; pel-mark-ring-positions
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-mark-ring-positions/empty ()
  "Return nil when `mark-ring' is empty."
  (with-temp-buffer
    (setq mark-ring nil)
    (should (null (pel-mark-ring-positions)))))

(ert-deftest ert-test-pel-mark-ring-positions/single-marker ()
  "Return a one-element list with the marker's position."
  (with-temp-buffer
    (insert "hello world")
    (goto-char 6)
    (let* ((m (point-marker))
           (mark-ring (list m)))
      (let ((result (pel-mark-ring-positions)))
        (should (= 1 (length result)))
        (should (= 6 (car result)))))))

(ert-deftest ert-test-pel-mark-ring-positions/multiple-markers ()
  "Return integer positions in the same order as `mark-ring'."
  (with-temp-buffer
    (insert "abcdefghij")
    (let* ((m1 (progn (goto-char 2) (point-marker)))
           (m2 (progn (goto-char 5) (point-marker)))
           (m3 (progn (goto-char 9) (point-marker)))
           (mark-ring (list m1 m2 m3)))
      (let ((result (pel-mark-ring-positions)))
        (should (equal (list 2 5 9) result))))))

(ert-deftest ert-test-pel-mark-ring-positions/returns-integers ()
  "Every element of the result is an integer."
  (with-temp-buffer
    (insert "test")
    (let* ((m (progn (goto-char 3) (point-marker)))
           (mark-ring (list m))
           (result (pel-mark-ring-positions)))
      (dolist (pos result)
        (should (integerp pos))))))

;; ---------------------------------------------------------------------------
;;; pel-popoff-mark-ring
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-popoff-mark-ring/empty-ring-no-error ()
  "Calling `pel-popoff-mark-ring' on an empty ring is a no-op."
  (with-temp-buffer
    (setq mark-ring nil)
    ;; Should not signal any error.
    (should-not (condition-case err
                    (progn (pel-popoff-mark-ring) nil)
                  (error (format "%S" err))))
    (should (null mark-ring))))

(ert-deftest ert-test-pel-popoff-mark-ring/removes-top-entry ()
  "Remove exactly one entry from the top of `mark-ring'."
  (with-temp-buffer
    (insert "abcdefghij")
    (let* ((m1 (progn (goto-char 2) (point-marker)))
           (m2 (progn (goto-char 5) (point-marker)))
           (m3 (progn (goto-char 8) (point-marker))))
      (setq mark-ring (list m1 m2 m3))
      (should (= 3 (length mark-ring)))
      ;; Pop off the top (m1).
      (pel-popoff-mark-ring)
      (should (= 2 (length mark-ring)))
      ;; The remaining ring starts with m2 at position 5.
      (should (= 5 (marker-position (car mark-ring))))
      ;; Pop off m2.
      (pel-popoff-mark-ring)
      (should (= 1 (length mark-ring)))
      (should (= 8 (marker-position (car mark-ring))))
      ;; Pop off m3 — ring becomes empty.
      (pel-popoff-mark-ring)
      (should (null mark-ring)))))

;; ---------------------------------------------------------------------------
;;; pel-mark-line-up
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-mark-line-up/marks-current-line ()
  "Without active mark: mark set at end of line, point moved to line start."
  (pel-mark-test--with-transient-mark-mode-on
    (pel-mark-test--with-3-line-buffer
      ;; Point is at beginning of \"line two\" (column 0).
      (let ((bol (line-beginning-position))
            (eol (line-end-position)))
        (pel-mark-line-up)
        ;; mark at end of line
        (should (= eol (mark)))
        ;; point at beginning of line (forward-line 0)
        (should (= bol (point)))
        ;; mark must be active
        (should mark-active)))))

(ert-deftest ert-test-pel-mark-line-up/extends-region-when-active ()
  "With active mark: point moves one additional line upward."
  (pel-mark-test--with-3-line-buffer
    ;; Simulate an already-active region spanning \"line two\".
    (set-mark (line-end-position))
    (setq mark-active t)
    (let ((mark-before (mark))
          (point-before (point)))
      (pel-mark-line-up)
      ;; mark stays where it was
      (should (= mark-before (mark)))
      ;; point moved one line up (forward-line -1 from bol of \"line two\")
      (should (< (point) point-before)))))

(ert-deftest ert-test-pel-mark-line-up/with-prefix-2 ()
  "With n=2 and no active mark: point moves to the line two above."
  (pel-mark-test--with-3-line-buffer
    ;; Point is at beginning of \"line two\".
    ;; With n=2: set-mark at eol, forward-line (- 1 2) = forward-line -1.
    ;; Advance to line three first so we have room to move up two.
    (forward-line 1)                  ; now on \"line three\"
    (let ((eol3 (line-end-position)))
      (pel-mark-line-up 2)
      (should (= eol3 (mark)))
      ;; forward-line (- 1 2) = -1 → back one line from \"line three\"
      ;; → point is at beginning of \"line two\"
      (should (= 10 (point))))))     ; \"line two\" starts at position 10

(ert-deftest ert-test-pel-mark-line-up/does-activate-mark ()
  "pel-mark-line-up activates the mark."
  (pel-mark-test--with-3-line-buffer
    (setq mark-active nil)
    (pel-mark-line-up)
    (should mark-active)))


(ert-deftest ert-test-pel-mark-line-up/extends-region-with-prefix-2 ()
  "With active mark and n=2: point moves two lines upward."
  (pel-mark-test--with-3-line-buffer
    (forward-line 1)                        ; on \"line three\"
    (set-mark (line-end-position))
    (setq mark-active t)
    (let ((point-before (point)))
      (pel-mark-line-up 2)
      ;; n=2 + 1 = 3, forward-line (- 1 3) = -2 → two lines up
      (should (< (point) point-before))
      (should (= (point) (point-min))))))   ; back to start of \"line one\"

;; ---------------------------------------------------------------------------
;;; pel-mark-line-down
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-mark-line-down/marks-current-line ()
  "Without active mark: mark set at beginning of line, point at line end."
  (pel-mark-test--with-transient-mark-mode-on
    (pel-mark-test--with-3-line-buffer
      ;; Point is at beginning of \"line two\".
      (let ((bol (line-beginning-position))
            (eol (line-end-position)))
        (pel-mark-line-down)
        ;; mark at beginning of line
        (should (= bol (mark)))
        ;; point at end of line (end-of-line 1)
        (should (= eol (point)))
        ;; mark must be active
        (should mark-active)))))

(ert-deftest ert-test-pel-mark-line-down/extends-region-when-active ()
  "With active mark: point moves to end of the next line."
  (pel-mark-test--with-3-line-buffer
    ;; Simulate an already-active region.
    (set-mark (line-beginning-position))
    (setq mark-active t)
    (let ((mark-before (mark))
          (point-before (point)))
      (pel-mark-line-down)
      ;; mark stays where it was
      (should (= mark-before (mark)))
      ;; point is now at end of \"line three\" (end-of-line 2 from \"line two\")
      (should (> (point) point-before)))))

(ert-deftest ert-test-pel-mark-line-down/with-prefix-2 ()
  "With n=2 and no active mark: point moves to end of the second line below."
  (pel-mark-test--with-3-line-buffer
    ;; Point is at beginning of \"line two\".
    (let ((bol (line-beginning-position)))
      (pel-mark-line-down 2)
      ;; mark at beginning of \"line two\"
      (should (= bol (mark)))
      ;; (end-of-line 2) from \"line two\" → end of \"line three\"
      ;; \"line three\" ends just before the final newline
      (should (= (- (point-max) 1) (point))))))

(ert-deftest ert-test-pel-mark-line-down/does-activate-mark ()
  "pel-mark-line-down activates the mark."
  (pel-mark-test--with-3-line-buffer
    (setq mark-active nil)
    (pel-mark-line-down)
    (should mark-active)))

;; ---------------------------------------------------------------------------
;;; pel-push-mark-no-activate
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-push-mark-no-activate/pushes-point ()
  "Current point position is pushed onto `mark-ring'."
  (with-temp-buffer
    (insert "hello world")
    (goto-char 6)
    (setq mark-ring nil)
    (pel-push-mark-no-activate)
    ;; First push: no old mark → ring stays empty; mark is now at 6
    (should (= 0 (length mark-ring)))
    (should (= 6 (marker-position (mark-marker))))))

(ert-deftest ert-test-pel-push-mark-no-activate/does-not-activate-region ()
  "Pushing the mark must not activate the region."
  (pel-mark-test--with-transient-mark-mode-on
   (with-temp-buffer
     (insert "hello")
     (goto-char 3)
     (setq mark-active nil)
     (pel-push-mark-no-activate)
     (should-not mark-active))))

(ert-deftest ert-test-pel-push-mark-no-activate/preserves-point ()
  "Point must not move after pushing the mark."
  (with-temp-buffer
    (insert "hello world")
    (goto-char 7)
    (pel-push-mark-no-activate)
    (should (= 7 (point)))))

(ert-deftest ert-test-pel-push-mark-no-activate/multiple-pushes ()
  "Each push adds one entry to `mark-ring'; order is newest first."
  (with-temp-buffer
    (insert "abcdefghij")
    (setq mark-ring nil)
    (goto-char 2) (pel-push-mark-no-activate) ; mark=2,  ring=()
    (goto-char 5) (pel-push-mark-no-activate) ; mark=5,  ring=(2)
    (goto-char 9) (pel-push-mark-no-activate) ; mark=9,  ring=(5 2)
    ;; Current mark is 9 (not in mark-ring); ring holds the previous marks
    (should (= 9 (marker-position (mark-marker))))
    (should (= 2 (length mark-ring)))
    (should (= 5 (marker-position (nth 0 mark-ring))))
    (should (= 2 (marker-position (nth 1 mark-ring))))))

;; ---------------------------------------------------------------------------
;;; pel-exchange-point-and-mark-no-activate
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-exchange-point-and-mark-no-activate/swaps-positions ()
  "Point and mark swap after the call."
  (with-temp-buffer
    (insert "hello world")
    (goto-char 3)
    (set-mark 8)
    (setq mark-active t)
    (pel-exchange-point-and-mark-no-activate)
    ;; New point is old mark position.
    (should (= 8 (point)))
    ;; New mark is old point position.
    (should (= 3 (mark)))))

(ert-deftest ert-test-pel-exchange-point-and-mark-no-activate/deactivates-mark ()
  "The region must not be active after the swap."
  (pel-mark-test--with-transient-mark-mode-on
    (with-temp-buffer
      (insert "hello world")
      (goto-char 2)
      (set-mark 9)
      (setq mark-active t)
      (pel-exchange-point-and-mark-no-activate)
      (should-not mark-active))))

(ert-deftest ert-test-pel-exchange-point-and-mark-no-activate/idempotent ()
  "Calling the function twice restores the original point and mark."
  (with-temp-buffer
    (insert "hello world")
    (goto-char 4)
    (set-mark 10)
    (setq mark-active t)
    (pel-exchange-point-and-mark-no-activate)
    (pel-exchange-point-and-mark-no-activate)
    (should (= 4  (point)))
    (should (= 10 (mark)))))

;; ---------------------------------------------------------------------------
;;; pel-jump-to-mark — smoke test
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-jump-to-mark/empty-ring-signals-error ()
  "Calling on an empty mark-ring signals a user-error."
  (with-temp-buffer
    (insert "abc")
    (setq mark-ring nil)
    (should-error (pel-jump-to-mark))))

(ert-deftest ert-test-pel-jump-to-mark/cycles-mark-ring ()
  "Jump to the top mark and rotate the ring; verify point moves."
  (with-temp-buffer
    (insert "abcdefghij")
    (setq mark-ring nil)
    ;; Push two marks so the ring is non-empty.
    (goto-char 3) (push-mark (point) t nil)
    (goto-char 7) (push-mark (point) t nil)
    ;; Move point away from any mark.
    (goto-char 1)
    (let ((point-before (point)))
      ;; pel-jump-to-mark calls (set-mark-command 1) which pops the ring.
      (should-not (condition-case err
                      (progn (pel-jump-to-mark) nil)
                    (error (format "%S" err))))
      ;; Point should have moved.
      (should (/= point-before (point))))))

;; ---------------------------------------------------------------------------
;;; pel-mark-ring-stats — smoke test
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-mark-ring-stats/no-error ()
  "Verify that `pel-mark-ring-stats' can be called without signalling an error."
  (with-temp-buffer
    (insert "some content")
    (goto-char 6)
    ;; Silence message output during the test.
    (cl-letf (((symbol-function 'message) #'ignore))
      (should-not (condition-case err
                      (progn (pel-mark-ring-stats) nil)
                    (error (format "%S" err)))))))

;;; --------------------------------------------------------------------------
(provide 'pel-mark-test)

;;; pel-mark-test.el ends here
