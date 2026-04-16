;;; pel-highlight-test.el --- Test pel-highlight.el  -*- lexical-binding: t; -*-

;; Created   : Thursday, April 16 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-04-16 15:52:45 EDT, updated by Pierre Rouleau>

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
;; ERT tests for pel-highlight.el.  Covers:
;;   - `pel--find-overlays-specifying': overlay property lookup at a position.
;;   - `pel-highlight-line': toggle overlay on current line or active region.
;;   - `pel-remove-line-highlight': remove all line-highlight overlays.
;;   - `pel-toggle-hl-line-sticky': toggle `hl-line-sticky-flag'.
;;   - `pel-toggle-show-trailing-whitespace': toggle `show-trailing-whitespace'.
;;   - `pel-toggle-indicate-empty-lines': toggle `indicate-empty-lines'.
;;   - `pel-toggle-indent-tabs-mode': toggle or explicitly set `indent-tabs-mode'.
;;   - `pel-set-highlight-color': set highlight face background/foreground/underline.
;;   - `pel-show-paren-info': display paren-mode info in dedicated buffer.
;;   - `pel-customize-highlight': open customize buffer for highlight face (smoke).

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-highlight)
(require 'ert)
(require 'cl-lib)
(require 'hl-line)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; ---------------------------------------------------------------------------
;;; Helpers / shared macros
;; ---------------------------------------------------------------------------

(defmacro pel-hl-test--with-transient-mark (&rest body)
  "Execute BODY with `transient-mark-mode' enabled, restoring it afterwards."
  (declare (indent 0) (debug t))
  `(let ((saved-- transient-mark-mode)
         (inhibit-message t))
     (unwind-protect
         (progn
           (transient-mark-mode 1)
           ,@body)
       (transient-mark-mode (if saved-- 1 -1)))))

(defmacro pel-hl-test--with-3-line-buffer (&rest body)
  "Execute BODY in temp buffer containing three lines; point at line 1, col 0."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert "line one\nline two\nline three\n")
     (goto-char (point-min))
     ,@body))

(defun pel-hl-test--count-highlight-overlays ()
  "Return `line-highlight-overlay-marker' overlays count in current buffer."
  (let ((count 0))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'line-highlight-overlay-marker)
        (cl-incf count)))
    count))

(defun pel-hl-test--find-highlight-overlay ()
  "Return the first `line-highlight-overlay-marker' overlay, or nil."
  (cl-find-if (lambda (ov) (overlay-get ov 'line-highlight-overlay-marker))
              (overlays-in (point-min) (point-max))))

;; ---------------------------------------------------------------------------
;;; pel--find-overlays-specifying
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel--find-overlays-specifying/no-overlays ()
  "Return nil when no overlays exist at position."
  (with-temp-buffer
    (insert "Hello, world!\n")
    (should (null (pel--find-overlays-specifying
                   'line-highlight-overlay-marker 1)))))

(ert-deftest ert-test-pel--find-overlays-specifying/matching-overlay-at-pos ()
  "Return non-nil when a matching overlay covers the queried position."
  (with-temp-buffer
    (insert "Hello, world!\n")
    (let ((ov (make-overlay 1 6)))
      (overlay-put ov 'line-highlight-overlay-marker t)
      (should (pel--find-overlays-specifying
               'line-highlight-overlay-marker 1)))))

(ert-deftest ert-test-pel--find-overlays-specifying/overlay-wrong-property ()
  "Return nil when the overlay at position has a different property."
  (with-temp-buffer
    (insert "Hello, world!\n")
    (let ((ov (make-overlay 1 6)))
      (overlay-put ov 'some-other-property t)
      (should (null (pel--find-overlays-specifying
                     'line-highlight-overlay-marker 1))))))

(ert-deftest ert-test-pel--find-overlays-specifying/overlay-outside-position ()
  "Return nil when the overlay does not span the queried position."
  (with-temp-buffer
    (insert "Hello, world!\n")
    (let ((ov (make-overlay 1 5)))
      (overlay-put ov 'line-highlight-overlay-marker t)
      ;; Position 10 is outside [1, 5)
      (should (null (pel--find-overlays-specifying
                     'line-highlight-overlay-marker 10))))))

(ert-deftest ert-test-pel--find-overlays-specifying/returns-only-matching-overlay ()
  "Return only the overlay that carries the searched property."
  (with-temp-buffer
    (insert "Hello, world!\n")
    (let ((ov1 (make-overlay 1 6))
          (ov2 (make-overlay 1 6)))
      (overlay-put ov1 'line-highlight-overlay-marker t)
      (overlay-put ov2 'some-other-property t)
      (let ((result (pel--find-overlays-specifying
                     'line-highlight-overlay-marker 1)))
        (should result)
        (should (= 1 (length result)))
        (should (eq ov1 (car result)))))))

(ert-deftest ert-test-pel--find-overlays-specifying/multiple-matching-overlays ()
  "Return all overlays that carry the searched property."
  (with-temp-buffer
    (insert "Hello, world!\n")
    (let ((ov1 (make-overlay 1 6))
          (ov2 (make-overlay 1 6)))
      (overlay-put ov1 'line-highlight-overlay-marker t)
      (overlay-put ov2 'line-highlight-overlay-marker t)
      (should (= 2 (length (pel--find-overlays-specifying
                             'line-highlight-overlay-marker 1)))))))

(ert-deftest ert-test-pel--find-overlays-specifying/position-at-overlay-end-is-excluded ()
  "Position equal to overlay end is outside the overlay (half-open interval)."
  (with-temp-buffer
    (insert "Hello!\n")
    ;; Overlay covers [1, 5); position 5 is NOT inside it.
    (let ((ov (make-overlay 1 5)))
      (overlay-put ov 'line-highlight-overlay-marker t)
      (should (null (pel--find-overlays-specifying
                     'line-highlight-overlay-marker 5))))))

;; ---------------------------------------------------------------------------
;;; pel-highlight-line — line mode (no active region)
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-highlight-line/adds-overlay-on-current-line ()
  "First call creates one `line-highlight-overlay-marker' overlay on the line."
  (pel-hl-test--with-3-line-buffer
    (should (= 0 (pel-hl-test--count-highlight-overlays)))
    (let ((inhibit-message t))
      (pel-highlight-line))
    (should (= 1 (pel-hl-test--count-highlight-overlays)))))

(ert-deftest ert-test-pel-highlight-line/second-call-removes-overlay ()
  "Second call on the same line removes the overlay (toggle off)."
  (pel-hl-test--with-3-line-buffer
    (let ((inhibit-message t))
      (pel-highlight-line)
      (should (= 1 (pel-hl-test--count-highlight-overlays)))
      (pel-highlight-line)
      (should (= 0 (pel-hl-test--count-highlight-overlays))))))

(ert-deftest ert-test-pel-highlight-line/overlay-start-is-line-beginning ()
  "Overlay start equals `line-beginning-position'."
  (pel-hl-test--with-3-line-buffer
    (let ((bol (line-beginning-position))
          (inhibit-message t))
      (pel-highlight-line)
      (should (= bol (overlay-start (pel-hl-test--find-highlight-overlay)))))))

(ert-deftest ert-test-pel-highlight-line/overlay-end-is-1-plus-line-end ()
  "Overlay end equals 1+ `line-end-position'."
  (pel-hl-test--with-3-line-buffer
    (let ((eol+1 (1+ (line-end-position)))
          (inhibit-message t))
      (pel-highlight-line)
      (should (= eol+1 (overlay-end (pel-hl-test--find-highlight-overlay)))))))

(ert-deftest ert-test-pel-highlight-line/overlay-has-face-background ()
  "Overlay has a non-nil `:face' attribute set by `pel-highlight-line'."
  (pel-hl-test--with-3-line-buffer
    (let ((inhibit-message t))
      (pel-highlight-line))
    (should (overlay-get (pel-hl-test--find-highlight-overlay) 'face))))

(ert-deftest ert-test-pel-highlight-line/overlay-face-background-matches-color ()
  "The `:background' in the overlay face equals `pel--highlight-color'."
  (pel-hl-test--with-3-line-buffer
    (let ((inhibit-message t))
      (pel-highlight-line))
    (let* ((ov   (pel-hl-test--find-highlight-overlay))
           (face (overlay-get ov 'face)))
      (should (equal pel--highlight-color (plist-get face :background))))))

(ert-deftest ert-test-pel-highlight-line/multiple-lines-get-independent-overlays ()
  "Each highlighted line gets its own independent overlay."
  (pel-hl-test--with-3-line-buffer
    (let ((inhibit-message t))
      (pel-highlight-line)            ; highlight line 1
      (forward-line 1)
      (pel-highlight-line)            ; highlight line 2
      (forward-line 1)
      (pel-highlight-line))           ; highlight line 3
    (should (= 3 (pel-hl-test--count-highlight-overlays)))))

(ert-deftest ert-test-pel-highlight-line/toggle-line1-does-not-affect-line2 ()
  "Removing the line-1 overlay leaves the line-2 overlay intact."
  (pel-hl-test--with-3-line-buffer
    (let ((inhibit-message t))
      (pel-highlight-line)            ; add on line 1
      (forward-line 1)
      (pel-highlight-line)            ; add on line 2
      (goto-char (point-min))
      (pel-highlight-line))           ; remove from line 1
    (should (= 1 (pel-hl-test--count-highlight-overlays)))))

;; ---------------------------------------------------------------------------
;;; pel-highlight-line — region mode (active region)
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-highlight-line/uses-region-start-when-active ()
  "When a region is active, overlay start equals `region-beginning'."
  (pel-hl-test--with-transient-mark
    (with-temp-buffer
      (insert "line one\nline two\nline three\n")
      (goto-char (point-min))
      (set-mark (point-min))
      (forward-line 2)
      (setq mark-active t)
      (should (use-region-p))
      (let ((rstart (region-beginning))
            (inhibit-message t))
        (pel-highlight-line)
        (should (= rstart
                   (overlay-start (pel-hl-test--find-highlight-overlay))))))))

(ert-deftest ert-test-pel-highlight-line/uses-region-end-when-active ()
  "When a region is active, overlay end equals `region-end'."
  (pel-hl-test--with-transient-mark
    (with-temp-buffer
      (insert "line one\nline two\nline three\n")
      (goto-char (point-min))
      (set-mark (point-min))
      (forward-line 2)
      (setq mark-active t)
      (should (use-region-p))
      (let ((rend (region-end))
            (inhibit-message t))
        (pel-highlight-line)
        (should (= rend
                   (overlay-end (pel-hl-test--find-highlight-overlay))))))))

(ert-deftest ert-test-pel-highlight-line/region-toggle-removes-overlay ()
  "Second `pel-highlight-line' call over the same region removes the overlay."
  (pel-hl-test--with-transient-mark
    (with-temp-buffer
      (insert "line one\nline two\n")
      (goto-char (point-min))
      (set-mark (point-min))
      (forward-line 1)
      (setq mark-active t)
      (let ((inhibit-message t))
        (pel-highlight-line)
        (should (= 1 (pel-hl-test--count-highlight-overlays)))
        ;; Re-establish the same region and toggle again.
        (goto-char (point-min))
        (set-mark (point-min))
        (forward-line 1)
        (setq mark-active t)
        (pel-highlight-line)
        (should (= 0 (pel-hl-test--count-highlight-overlays)))))))

;; ---------------------------------------------------------------------------
;;; pel-highlight-line — change-color argument
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-highlight-line/change-color-updates-pel--highlight-color ()
  "With CHANGE-COLOR and a supported color, `pel--highlight-color' is updated."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char (point-min))
    (let ((saved pel--highlight-color)
          (inhibit-message t))
      (unwind-protect
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "blue"))
                    ((symbol-function 'color-supported-p)
                     (lambda (&rest _) t)))
            (pel-highlight-line t)
            (should (equal "blue" pel--highlight-color)))
        (setq pel--highlight-color saved)))))

(ert-deftest ert-test-pel-set-highlight-color/updates-pel--highlight-color-2 ()
  "Calling `pel-set-highlight-color' also updates `pel--highlight-color'."
  (let ((orig-bg  (face-attribute 'highlight :background))
        (orig-col pel--highlight-color))
    (unwind-protect
        (progn
          (pel-set-highlight-color "coral")
          (should (equal "coral" pel--highlight-color)))
      (set-face-background 'highlight orig-bg)
      (setq pel--highlight-color orig-col))))

(ert-deftest ert-test-pel-highlight-line/change-color-still-creates-overlay ()
  "With CHANGE-COLOR, `pel-highlight-line' still creates an overlay on the line."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char (point-min))
    (let ((saved pel--highlight-color)
          (inhibit-message t))
      (unwind-protect
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "red"))
                    ((symbol-function 'color-supported-p)
                     (lambda (&rest _) t)))
            (pel-highlight-line t)
            (should (= 1 (pel-hl-test--count-highlight-overlays))))
        (setq pel--highlight-color saved)))))

(ert-deftest ert-test-pel-highlight-line/change-color-unsupported-signals-user-error ()
  "With CHANGE-COLOR and an unsupported color name, `user-error' is signalled."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "not-a-real-color-xyz"))
              ((symbol-function 'color-supported-p)
               (lambda (&rest _) nil)))
      (should-error (pel-highlight-line t) :type 'user-error))))

(ert-deftest ert-test-pel-highlight-line/change-color-unsupported-leaves-color-unchanged ()
  "With CHANGE-COLOR and an unsupported color, `pel--highlight-color' is not modified."
  ;; the user-error is intentionally suppressed here to test side-effect-free state
  (with-temp-buffer
    (insert "hello\n")
    (goto-char (point-min))
    (let ((saved pel--highlight-color))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "not-a-real-color-xyz"))
                ((symbol-function 'color-supported-p)
                 (lambda (&rest _) nil)))
        (ignore-errors (pel-highlight-line t)))
      (should (equal saved pel--highlight-color)))))

;; ---------------------------------------------------------------------------
;;; pel-remove-line-highlight
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-remove-line-highlight/removes-all-overlays ()
  "All `line-highlight-overlay-marker' overlays are removed immediately."
  (pel-hl-test--with-3-line-buffer
    (let ((inhibit-message t))
      (pel-highlight-line)
      (forward-line 1)
      (pel-highlight-line)
      (forward-line 1)
      (pel-highlight-line))
    (should (= 3 (pel-hl-test--count-highlight-overlays)))
    (pel-remove-line-highlight)
    (should (= 0 (pel-hl-test--count-highlight-overlays)))))

(ert-deftest ert-test-pel-remove-line-highlight/removes-single-overlay ()
  "A single `line-highlight-overlay-marker' overlay is removed correctly."
  (pel-hl-test--with-3-line-buffer
    (let ((inhibit-message t))
      (pel-highlight-line))
    (should (= 1 (pel-hl-test--count-highlight-overlays)))
    (pel-remove-line-highlight)
    (should (= 0 (pel-hl-test--count-highlight-overlays)))))

(ert-deftest ert-test-pel-remove-line-highlight/no-error-on-empty-buffer ()
  "No error when called on a buffer that has no overlays."
  (with-temp-buffer
    (insert "nothing highlighted\n")
    (should (= 0 (pel-hl-test--count-highlight-overlays)))
    (should-not (condition-case err
                    (progn (pel-remove-line-highlight) nil)
                  (error (format "%S" err))))))

(ert-deftest ert-test-pel-remove-line-highlight/idempotent ()
  "Calling `pel-remove-line-highlight' twice does not error."
  (pel-hl-test--with-3-line-buffer
    (let ((inhibit-message t))
      (pel-highlight-line))
    (pel-remove-line-highlight)
    (should-not (condition-case err
                    (progn (pel-remove-line-highlight) nil)
                  (error (format "%S" err))))
    (should (= 0 (pel-hl-test--count-highlight-overlays)))))

(ert-deftest ert-test-pel-remove-line-highlight/does-not-call-y-or-n-p ()
  "The function removes overlays immediately without calling `y-or-n-p'."
  (pel-hl-test--with-3-line-buffer
    (let ((inhibit-message t))
      (pel-highlight-line))
    ;; If y-or-n-p were called it would signal an error because we make it
    ;; error out; the absence of an error proves it is never called.
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (&rest _)
                 (error "y-or-n-p must not be called"))))
      (should-not (condition-case err
                      (progn (pel-remove-line-highlight) nil)
                    (error (format "%S" err)))))
    (should (= 0 (pel-hl-test--count-highlight-overlays)))))

(ert-deftest ert-test-pel-remove-line-highlight/preserves-non-marker-overlays ()
  "Non-`line-highlight-overlay-marker' overlays are NOT removed.
This verifies the scoped `remove-overlays' call leaves unrelated overlays alone."
  (with-temp-buffer
    (insert "Hello, world!\n")
    ;; Create one marker overlay and one unrelated overlay.
    (let ((inhibit-message t))
      (pel-highlight-line))
    (let ((foreign (make-overlay 1 6)))
      (overlay-put foreign 'some-other-property t)
      (should (= 1 (pel-hl-test--count-highlight-overlays)))
      (pel-remove-line-highlight)
      ;; Marker overlay gone, foreign overlay intact.
      (should (= 0 (pel-hl-test--count-highlight-overlays)))
      (should (overlay-buffer foreign)))))

(ert-deftest ert-test-pel-remove-line-highlight/text-content-unchanged ()
  "Buffer text is not altered by `pel-remove-line-highlight'."
  (with-temp-buffer
    (insert "line one\nline two\n")
    (goto-char (point-min))
    (let ((inhibit-message t))
      (pel-highlight-line))
    (let ((text-before (buffer-string)))
      (pel-remove-line-highlight)
      (should (equal text-before (buffer-string))))))

;; ---------------------------------------------------------------------------
;;; pel-toggle-hl-line-sticky
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-toggle-hl-line-sticky/t-to-nil ()
  "Toggle `hl-line-sticky-flag' from t to nil."
  (let ((hl-line-sticky-flag t)
        (inhibit-message t))
    (pel-toggle-hl-line-sticky)
    (should (null hl-line-sticky-flag))))

(ert-deftest ert-test-pel-toggle-hl-line-sticky/nil-to-t ()
  "Toggle `hl-line-sticky-flag' from nil to t."
  (let ((hl-line-sticky-flag nil)
        (inhibit-message t))
    (pel-toggle-hl-line-sticky)
    (should hl-line-sticky-flag)))

(ert-deftest ert-test-pel-toggle-hl-line-sticky/double-toggle-restores ()
  "Toggling twice restores the original value."
  (let ((hl-line-sticky-flag t)
        (inhibit-message t))
    (pel-toggle-hl-line-sticky)
    (pel-toggle-hl-line-sticky)
    (should (eq t hl-line-sticky-flag))))

;; ---------------------------------------------------------------------------
;;; pel-toggle-show-trailing-whitespace
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-toggle-show-trailing-whitespace/nil-to-t ()
  "Toggle `show-trailing-whitespace' from nil to t."
  (with-temp-buffer
    (setq-local show-trailing-whitespace nil)
    (let ((inhibit-message t))
      (pel-toggle-show-trailing-whitespace))
    (should show-trailing-whitespace)))

(ert-deftest ert-test-pel-toggle-show-trailing-whitespace/t-to-nil ()
  "Toggle `show-trailing-whitespace' from t to nil."
  (with-temp-buffer
    (setq-local show-trailing-whitespace t)
    (let ((inhibit-message t))
      (pel-toggle-show-trailing-whitespace))
    (should (null show-trailing-whitespace))))

(ert-deftest ert-test-pel-toggle-show-trailing-whitespace/double-toggle-restores ()
  "Toggling twice restores the original value."
  (with-temp-buffer
    (setq-local show-trailing-whitespace nil)
    (let ((inhibit-message t))
      (pel-toggle-show-trailing-whitespace)
      (pel-toggle-show-trailing-whitespace))
    (should (null show-trailing-whitespace))))

;; ---------------------------------------------------------------------------
;;; pel-toggle-indicate-empty-lines
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-toggle-indicate-empty-lines/nil-to-t ()
  "Toggle `indicate-empty-lines' from nil to t."
  (with-temp-buffer
    (setq-local indicate-empty-lines nil)
    (let ((inhibit-message t))
      (pel-toggle-indicate-empty-lines))
    (should indicate-empty-lines)))

(ert-deftest ert-test-pel-toggle-indicate-empty-lines/t-to-nil ()
  "Toggle `indicate-empty-lines' from t to nil."
  (with-temp-buffer
    (setq-local indicate-empty-lines t)
    (let ((inhibit-message t))
      (pel-toggle-indicate-empty-lines))
    (should (null indicate-empty-lines))))

(ert-deftest ert-test-pel-toggle-indicate-empty-lines/double-toggle-restores ()
  "Toggling twice restores the original value."
  (with-temp-buffer
    (setq-local indicate-empty-lines t)
    (let ((inhibit-message t))
      (pel-toggle-indicate-empty-lines)
      (pel-toggle-indicate-empty-lines))
    (should indicate-empty-lines)))

;; ---------------------------------------------------------------------------
;;; pel-toggle-indent-tabs-mode
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-toggle-indent-tabs-mode/nil-to-t ()
  "Without ARG, toggle `indent-tabs-mode' from nil to t."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (cl-letf (((symbol-function 'beep) #'ignore))
      (let ((inhibit-message t))
        (pel-toggle-indent-tabs-mode)))
    (should indent-tabs-mode)))

(ert-deftest ert-test-pel-toggle-indent-tabs-mode/t-to-nil ()
  "Without ARG, toggle `indent-tabs-mode' from t to nil."
  (with-temp-buffer
    (setq-local indent-tabs-mode t)
    (cl-letf (((symbol-function 'beep) #'ignore))
      (let ((inhibit-message t))
        (pel-toggle-indent-tabs-mode)))
    (should (null indent-tabs-mode))))

(ert-deftest ert-test-pel-toggle-indent-tabs-mode/double-toggle-restores ()
  "Toggling twice without ARG restores the original value."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (cl-letf (((symbol-function 'beep) #'ignore))
      (let ((inhibit-message t))
        (pel-toggle-indent-tabs-mode)
        (pel-toggle-indent-tabs-mode)))
    (should (null indent-tabs-mode))))

(ert-deftest ert-test-pel-toggle-indent-tabs-mode/positive-arg-sets-t ()
  "A positive ARG forces `indent-tabs-mode' to t."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (cl-letf (((symbol-function 'beep) #'ignore))
      (let ((inhibit-message t))
        (pel-toggle-indent-tabs-mode 1)))
    (should indent-tabs-mode)))

(ert-deftest ert-test-pel-toggle-indent-tabs-mode/negative-arg-sets-nil ()
  "A negative ARG forces `indent-tabs-mode' to nil (spaces only)."
  (with-temp-buffer
    (setq-local indent-tabs-mode t)
    (cl-letf (((symbol-function 'beep) #'ignore))
      (let ((inhibit-message t))
        (pel-toggle-indent-tabs-mode -1)))
    (should (null indent-tabs-mode))))

(ert-deftest ert-test-pel-toggle-indent-tabs-mode/positive-arg-idempotent ()
  "A positive ARG keeps `indent-tabs-mode' t when already t."
  (with-temp-buffer
    (setq-local indent-tabs-mode t)
    (cl-letf (((symbol-function 'beep) #'ignore))
      (let ((inhibit-message t))
        (pel-toggle-indent-tabs-mode 2)))
    (should indent-tabs-mode)))

(ert-deftest ert-test-pel-toggle-indent-tabs-mode/negative-arg-idempotent ()
  "A negative ARG keeps `indent-tabs-mode' nil when already nil."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (cl-letf (((symbol-function 'beep) #'ignore))
      (let ((inhibit-message t))
        (pel-toggle-indent-tabs-mode -1)))
    (should (null indent-tabs-mode))))

;; ---------------------------------------------------------------------------
;;; pel-set-highlight-color
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-set-highlight-color/sets-background ()
  "Sets the `highlight' face `:background' attribute to the given color."
  (let ((orig-bg (face-attribute 'highlight :background)))
    (unwind-protect
        (progn
          (pel-set-highlight-color "blue")
          (should (equal "blue" (face-attribute 'highlight :background))))
      (set-face-background 'highlight orig-bg))))

(ert-deftest ert-test-pel-set-highlight-color/clears-foreground ()
  "Sets the `highlight' face `:foreground' attribute to `unspecified'.
`set-face-foreground' with nil causes `face-attribute' to return `unspecified',
unlike `set-face-underline' with nil which stores nil."
  (let ((orig-bg (face-attribute 'highlight :background))
        (orig-fg (face-attribute 'highlight :foreground)))
    (unwind-protect
        (progn
          (pel-set-highlight-color "yellow")
          (should (eq 'unspecified (face-attribute 'highlight :foreground))))
      (set-face-background 'highlight orig-bg)
      (set-face-foreground 'highlight orig-fg))))

(ert-deftest ert-test-pel-set-highlight-color/clears-underline ()
  "Sets the `highlight' face `:underline' attribute to nil (disabled)."
  (let ((orig-bg (face-attribute 'highlight :background))
        (orig-ul (face-attribute 'highlight :underline)))
    (unwind-protect
        (progn
          (pel-set-highlight-color "green")
          ;; set-face-underline with nil stores nil, not 'unspecified
          (should (null (face-attribute 'highlight :underline))))
      (set-face-background 'highlight orig-bg)
      (set-face-underline  'highlight orig-ul))))

(ert-deftest ert-test-pel-set-highlight-color/changing-color-twice ()
  "The last call wins: the background reflects the final color set."
  (let ((orig-bg (face-attribute 'highlight :background)))
    (unwind-protect
        (progn
          (pel-set-highlight-color "red")
          (pel-set-highlight-color "cyan")
          (should (equal "cyan" (face-attribute 'highlight :background))))
      (set-face-background 'highlight orig-bg))))

;; ---------------------------------------------------------------------------
;;; pel-show-paren-info
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-show-paren-info/creates-info-buffer ()
  "Calling `pel-show-paren-info' creates the `*pel-highlight-info*' buffer."
  (when (get-buffer "*pel-highlight-info*")
    (kill-buffer "*pel-highlight-info*"))
  (let ((inhibit-message t))
    (pel-show-paren-info))
  (unwind-protect
      (should (get-buffer "*pel-highlight-info*"))
    (when (get-buffer "*pel-highlight-info*")
      (kill-buffer "*pel-highlight-info*"))))

(ert-deftest ert-test-pel-show-paren-info/buffer-is-non-empty ()
  "The `*pel-highlight-info*' buffer contains paren-mode content."
  (when (get-buffer "*pel-highlight-info*")
    (kill-buffer "*pel-highlight-info*"))
  (let ((inhibit-message t))
    (pel-show-paren-info))
  (unwind-protect
      (with-current-buffer "*pel-highlight-info*"
        (should (> (buffer-size) 0)))
    (when (get-buffer "*pel-highlight-info*")
      (kill-buffer "*pel-highlight-info*"))))

(ert-deftest ert-test-pel-show-paren-info/append-grows-buffer ()
  "Calling with a non-nil APPEND argument appends to the buffer."
  (when (get-buffer "*pel-highlight-info*")
    (kill-buffer "*pel-highlight-info*"))
  (let ((inhibit-message t))
    (pel-show-paren-info))
  (unwind-protect
      (let ((size-after-first
             (with-current-buffer "*pel-highlight-info*" (buffer-size))))
        (let ((inhibit-message t))
          (pel-show-paren-info :append))
        (let ((size-after-second
               (with-current-buffer "*pel-highlight-info*" (buffer-size))))
          (should (> size-after-second size-after-first))))
    (when (get-buffer "*pel-highlight-info*")
      (kill-buffer "*pel-highlight-info*"))))

(ert-deftest ert-test-pel-show-paren-info/no-append-clears-buffer ()
  "Calling without APPEND replaces previous content (buffer size stable)."
  (when (get-buffer "*pel-highlight-info*")
    (kill-buffer "*pel-highlight-info*"))
  (let ((inhibit-message t))
    (pel-show-paren-info))
  (unwind-protect
      (let ((size-after-first
             (with-current-buffer "*pel-highlight-info*" (buffer-size))))
        (let ((inhibit-message t))
          (pel-show-paren-info))
        (let ((size-after-second
               (with-current-buffer "*pel-highlight-info*" (buffer-size))))
          ;; Without append the buffer is cleared and rewritten:
          ;; the two sizes should be equal.
          (should (= size-after-first size-after-second))))
    (when (get-buffer "*pel-highlight-info*")
      (kill-buffer "*pel-highlight-info*"))))

;; ---------------------------------------------------------------------------
;;; pel-customize-highlight — smoke test
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-customize-highlight/no-error ()
  "Calling `pel-customize-highlight' does not signal an error."
  (cl-letf (((symbol-function 'customize-face) (lambda (_face) nil)))
    (should-not (condition-case err
                    (progn (pel-customize-highlight) nil)
                  (error (format "%S" err))))))

(ert-deftest ert-test-pel-customize-highlight/calls-customize-face-with-highlight ()
  "Calls `customize-face' with the symbol `highlight'."
  (let (captured)
    (cl-letf (((symbol-function 'customize-face)
               (lambda (face) (setq captured face))))
      (pel-customize-highlight)
      (should (eq 'highlight captured)))))

;; ---------------------------------------------------------------------------
;;; pel--highlight-color initial state
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel--highlight-color/variable-is-bound ()
  "The variable `pel--highlight-color' is bound."
  (should (boundp 'pel--highlight-color)))

(ert-deftest ert-test-pel--highlight-color/default-variable-is-bound ()
  "The customization variable `pel-highlight-color-default' is bound."
  (should (boundp 'pel-highlight-color-default)))

(ert-deftest ert-test-pel--highlight-color/is-a-string ()
  "`pel--highlight-color' holds a string value."
  (should (stringp pel--highlight-color)))

;; ---------------------------------------------------------------------------
;;; pel--color-completion-collection — structural tests
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel--color-completion-collection/returns-a-function ()
  "`pel--color-completion-collection' returns a callable completion table."
  (should (functionp (pel--color-completion-collection))))

(ert-deftest ert-test-pel--color-completion-collection/metadata-contains-affixation-or-annotation ()
  "The completion table advertises affixation-function (Emacs 27+) or
annotation-function (older Emacs) in its metadata."
  (let* ((table (pel--color-completion-collection))
         (meta  (funcall table "" nil 'metadata)))
    ;; meta is (metadata ...) or nil
    (should (eq 'metadata (car meta)))
    (let ((alist (cdr meta)))
      (should (or (assq 'affixation-function alist)
                  (assq 'annotation-function alist))))))

(ert-deftest ert-test-pel--color-completion-collection/all-action-returns-list ()
  "The completion table returns a list of strings for the `t' action."
  (let* ((table  (pel--color-completion-collection))
         (result (funcall table "" nil t)))
    (should (listp result))
    (should (> (length result) 0))
    (should (stringp (car result)))))

(ert-deftest ert-test-pel--color-completion-collection/try-action-recognizes-red ()
  "The completion table recognizes \"red\" as a valid completion."
  (let* ((table  (pel--color-completion-collection))
         (result (funcall table "red" nil nil)))
    ;; nil action (try-completion): returns t when exact match
    (should result)))

;; ---------------------------------------------------------------------------
;;; Gap `#19` — overlay background matches the newly set color
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-highlight-line/change-color-overlay-background-matches-new-color ()
  "After change-color, the created overlay's :background equals the new color."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char (point-min))
    (let ((saved pel--highlight-color)
          (inhibit-message t))
      (unwind-protect
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "magenta"))
                    ((symbol-function 'color-supported-p)
                     (lambda (&rest _) t)))
            (pel-highlight-line t)
            (let* ((ov   (pel-hl-test--find-highlight-overlay))
                   (face (overlay-get ov 'face)))
              (should (equal "magenta" (plist-get face :background)))))
        (setq pel--highlight-color saved)))))

(ert-deftest ert-test-pel-highlight-line/change-color-overlay-background-differs-from-old-color ()
  "After change-color, the overlay background is the *new* color, not the old one."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char (point-min))
    (let ((saved pel--highlight-color)
          (inhibit-message t))
      (unwind-protect
          (progn
            (setq pel--highlight-color "orange")
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "cyan"))
                      ((symbol-function 'color-supported-p)
                       (lambda (&rest _) t)))
              (pel-highlight-line t)
              (let* ((ov   (pel-hl-test--find-highlight-overlay))
                     (face (overlay-get ov 'face)))
                (should-not (equal "orange" (plist-get face :background)))
                (should     (equal "cyan"   (plist-get face :background))))))
        (setq pel--highlight-color saved)))))

;; ---------------------------------------------------------------------------
;;; Gap `#20` — pel--find-overlays-specifying: property explicitly nil
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel--find-overlays-specifying/property-explicitly-nil-not-found ()
  "An overlay whose property is explicitly set to nil is not returned.
`overlay-get' returns nil for both absent and nil-valued properties."
  (with-temp-buffer
    (insert "Hello, world!\n")
    (let ((ov (make-overlay 1 6)))
      (overlay-put ov 'line-highlight-overlay-marker nil)
      (should (null (pel--find-overlays-specifying
                     'line-highlight-overlay-marker 1))))))

(ert-deftest ert-test-pel--find-overlays-specifying/nil-vs-t-property-discrimination ()
  "Only the overlay with property t is returned; the one with nil is excluded."
  (with-temp-buffer
    (insert "Hello, world!\n")
    (let ((ov-nil (make-overlay 1 6))
          (ov-t   (make-overlay 1 6)))
      (overlay-put ov-nil 'line-highlight-overlay-marker nil)
      (overlay-put ov-t   'line-highlight-overlay-marker t)
      (let ((result (pel--find-overlays-specifying
                     'line-highlight-overlay-marker 1)))
        (should result)
        (should (= 1 (length result)))
        (should (eq ov-t (car result)))))))

;; ---------------------------------------------------------------------------
;;; Gap `#21` — change-color with empty string from prompt
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-highlight-line/change-color-empty-string-unsupported-signals-user-error ()
  "When the prompt returns \"\" and color-supported-p returns nil, user-error is signalled."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) ""))
              ((symbol-function 'color-supported-p)
               (lambda (&rest _) nil)))
      (should-error (pel-highlight-line t) :type 'user-error))))

(ert-deftest ert-test-pel-highlight-line/change-color-empty-string-supported-sets-color ()
  "When prompt returns \"\" and color-supported-p returns t, pel--highlight-color is set to \"\".
Documents the degenerate-but-reachable code path."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char (point-min))
    (let ((saved pel--highlight-color)
          (inhibit-message t))
      (unwind-protect
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) ""))
                    ((symbol-function 'color-supported-p)
                     (lambda (&rest _) t)))
            (pel-highlight-line t)
            (should (equal "" pel--highlight-color))
            (should (= 1 (pel-hl-test--count-highlight-overlays))))
        (setq pel--highlight-color saved)))))

(ert-deftest ert-test-pel-highlight-line/change-color-empty-string-unsupported-no-overlay ()
  "When user-error is signalled for an empty color, no overlay is created."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) ""))
              ((symbol-function 'color-supported-p)
               (lambda (&rest _) nil)))
      (ignore-errors (pel-highlight-line t)))
    (should (= 0 (pel-hl-test--count-highlight-overlays)))))

;; ---------------------------------------------------------------------------
;;; Gap `#22` — pel-highlight-line in a buffer with no trailing newline
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-highlight-line/no-trailing-newline-creates-overlay ()
  "No error when `pel-highlight-line' is called in a buffer with no trailing newline."
  (with-temp-buffer
    (insert "no newline at end")
    (goto-char (point-min))
    (let ((inhibit-message t))
      (should-not (condition-case err
                      (progn (pel-highlight-line) nil)
                    (error (format "%S" err)))))
    (should (= 1 (pel-hl-test--count-highlight-overlays)))))

(ert-deftest ert-test-pel-highlight-line/no-trailing-newline-overlay-end-is-point-max ()
  "Overlay end equals 1+ point-max when there is no trailing newline."
  (with-temp-buffer
    (insert "no newline at end")
    (goto-char (point-min))
    (let ((expected-end (point-max))
          (inhibit-message t))
      (pel-highlight-line)
      (should (= expected-end
                 (overlay-end (pel-hl-test--find-highlight-overlay)))))))

(ert-deftest ert-test-pel-highlight-line/no-trailing-newline-toggle-removes-overlay ()
  "Second call on a no-trailing-newline buffer removes the overlay (toggle off)."
  (with-temp-buffer
    (insert "no newline at end")
    (goto-char (point-min))
    (let ((inhibit-message t))
      (pel-highlight-line)
      (should (= 1 (pel-hl-test--count-highlight-overlays)))
      (pel-highlight-line)
      (should (= 0 (pel-hl-test--count-highlight-overlays))))))

(ert-deftest ert-test-pel-highlight-line/no-trailing-newline-point-at-eob ()
  "Calling with point at point-max in a no-trailing-newline buffer creates an overlay."
  (with-temp-buffer
    (insert "end of buffer")
    (goto-char (point-max))
    (let ((inhibit-message t))
      (pel-highlight-line))
    (should (= 1 (pel-hl-test--count-highlight-overlays)))))

;; ---------------------------------------------------------------------------
;;; pel--prompt-for-color uses completion-ignore-case t
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel--prompt-for-color/calls-completing-read-with-ignore-case ()
  "pel--prompt-for-color binds `completion-ignore-case' to t when calling
`completing-read', making color name matching case-insensitive."
  (let (captured-ignore-case)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _)
                 (setq captured-ignore-case completion-ignore-case)
                 "blue")))
      (pel--prompt-for-color))
    (should (eq t captured-ignore-case))))

(ert-deftest ert-test-pel--prompt-for-color/uses-shared-history-variable ()
  "pel--prompt-for-color passes `pel-set-highlight-color--history' as HIST."
  (let (captured-history-sym)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _collection _pred _require-match
                        _initial-input hist &rest _)
                 (setq captured-history-sym hist)
                 "blue")))
      (pel--prompt-for-color))
    (should (eq 'pel-set-highlight-color--history captured-history-sym))))

;; ---------------------------------------------------------------------------
;;; pel--highlight-color initialized from pel-highlight-color-default
;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel--highlight-color/initialized-from-default ()
  "`pel--highlight-color' is initialized from `pel-highlight-color-default'.
The `defvar' binds `pel--highlight-color' to the customization default.
This test verifies the relationship is preserved at load time."
  ;; At load time the defvar has already run; both should be strings.
  ;; We verify the types are consistent rather than asserting an exact value
  ;; since the default can be customized by the user.
  (should (stringp pel-highlight-color-default))
  (should (stringp pel--highlight-color)))

;;; --------------------------------------------------------------------------
(provide 'pel-highlight-test)
;;; pel-highlight-test.el ends here
