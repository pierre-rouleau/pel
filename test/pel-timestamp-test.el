;;; pel-timestamp-test.el --- Test the pel-timestamp code.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, February 25 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-22 15:44:05 EDT, updated by Pierre Rouleau>

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
;; ERT tests for all functions in pel-timestamp.el.  `cl-letf' is used
;; throughout to mock external side-effectful calls (time-stamp, message,
;; copyright-update, etc.) so the tests are hermetic and require no file I/O.
;;
;; Functions covered:
;;   pel-update-time-stamp-patterns      (buffer content replacement)
;;   pel--update-time-stamp              (internal before-save hook body)
;;   pel-toggle-update-time-stamp-on-save (interactive toggle + hook wiring)
;;   pel-toggle-update-copyright-on-save  (interactive toggle)
;;   pel--update-copyright               (conditional copyright-update dispatch)
;;   pel-time-stamp-control-show-info    (smoke test only: display command)
;;   pel-file-in                         (helper from pel--base, used here)



;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel-timestamp)
(require 'pel--base)    ; use `pel-string-starts-with-p'
;; (require 'pel-ert)
(require 'cl-lib)       ; use `cl-letf' used for mocking
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:

;; ---------------------------------------------------------------------------
;; Helper: silence message calls produced by pel-toggle-and-show-user-option

(defmacro pel-ts-test--silent (&rest body)
  "Evaluate BODY with `message' suppressed."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
     ,@body))

;; ---------------------------------------------------------------------------

(defun pel-mocked-file-directory-p  (path)
  "Mocked `file-directory-p' for PATH."
  (cond
   ((string= path "/a/") t)
   ((string= path "/b/") t)
   ((pel-string-starts-with-p path "/a/") nil)
   ((pel-string-starts-with-p path "/b/") nil)
   (t (error "Invalid %s" path))))

(ert-deftest pel-timestamp-test-skip-copyright-for-1 ()
  "Test `pel-file-in'."
  (let ((skip-copyright-in '("/a/" "/b/b.c" "/b/b1.c")))
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (fp) (pel-mocked-file-directory-p fp))))
      ;; Directory
      ;; anything in the directory /a should be skipped
      (should (string=  (pel-file-in "/a/" skip-copyright-in) "/a/"))
      (should (string=  (pel-file-in "/a"  skip-copyright-in) "/a/"))
      (should (string=  (pel-file-in "/a/f.c" skip-copyright-in) "/a/"))
      (should (string=  (pel-file-in "/a/b/f.c" skip-copyright-in) "/a/"))

      ;; everything else should not be skipped
      (should-not (pel-file-in "/ab" skip-copyright-in))
      (should-not (pel-file-in "/ab/f.c" skip-copyright-in))
      (should-not (pel-file-in "/b/f.c" skip-copyright-in))
      (should-not (pel-file-in "~/my/project/file.el" skip-copyright-in))
      ;;
      ;; File
      (should (pel-file-in "/a/b.c" skip-copyright-in)) ; file inside /a
      (should-not (pel-file-in "/abc" skip-copyright-in))
      (should (pel-file-in "/b/b.c" skip-copyright-in))
      (should (string=  (pel-file-in "/b/b.c" skip-copyright-in) "/b/b.c"))
      (should (pel-file-in "/b/b1.c" skip-copyright-in))
      (should-not (pel-file-in "/b/b1.c.txt" skip-copyright-in)))))


;; ---------------------------------------------------------------------------
;; pel-update-time-stamp-patterns

(ert-deftest pel-update-time-stamp-patterns-empty-list-test ()
  "With an empty pattern list, buffer content must be unchanged."
  (let ((pel-update-time-stamp-pattern-regexps nil))
    (with-temp-buffer
      (insert "Time-stamp: 2020-01-01\n")
      (let ((before (buffer-string)))
        (pel-update-time-stamp-patterns)
        (should (string= (buffer-string) before))))))

(ert-deftest pel-update-time-stamp-patterns-single-match-test ()
  "A matching regexp causes the capture group to be replaced with the formatted time."
  (let ((pel-update-time-stamp-pattern-regexps
         '("Time-stamp: \\([-0-9T:+Z ]+\\)"))
        (pel-date-wkd-time-iso-format "%Y-%m-%d"))
    (cl-letf (((symbol-function 'format-time-string)
               (lambda (_fmt &rest _) "2026-03-22")))
      (with-temp-buffer
        (insert "Time-stamp: 2020-01-01\n")
        (pel-update-time-stamp-patterns)
        (should (string= (buffer-string) "Time-stamp: 2026-03-22\n"))))))

(ert-deftest pel-update-time-stamp-patterns-multiple-occurrences-test ()
  "Every occurrence in the buffer that matches is replaced."
  (let ((pel-update-time-stamp-pattern-regexps
         '("TS:\\([-0-9]+\\)"))
        (pel-date-wkd-time-iso-format "%Y-%m-%d"))
    (cl-letf (((symbol-function 'format-time-string)
               (lambda (_fmt &rest _) "2026-01-01")))
      (with-temp-buffer
        (insert "line 1 TS:2000-01-01\nline 2 TS:1999-12-31\n")
        (pel-update-time-stamp-patterns)
        (should (string= (buffer-string)
                         "line 1 TS:2026-01-01\nline 2 TS:2026-01-01\n"))))))

(ert-deftest pel-update-time-stamp-patterns-multiple-regexps-test ()
  "When the list has two regexps, both matching lines are updated independently."
  (let ((pel-update-time-stamp-pattern-regexps
         '("Created: \\([0-9-]+\\)"
           "Modified: \\([0-9-]+\\)"))
        (pel-date-wkd-time-iso-format "%Y-%m-%d"))
    (cl-letf (((symbol-function 'format-time-string)
               (lambda (_fmt &rest _) "2026-03-22")))
      (with-temp-buffer
        (insert "Created: 2020-01-01\nModified: 2020-06-15\n")
        (pel-update-time-stamp-patterns)
        (should (string= (buffer-string)
                         "Created: 2026-03-22\nModified: 2026-03-22\n"))))))

(ert-deftest pel-update-time-stamp-patterns-no-match-test ()
  "When a regexp has no match in the buffer, the buffer is left unchanged."
  (let ((pel-update-time-stamp-pattern-regexps
         '("NOTPRESENT:\\([0-9]+\\)"))
        (pel-date-wkd-time-iso-format "%Y-%m-%d"))
    (cl-letf (((symbol-function 'format-time-string)
               (lambda (_fmt &rest _) "2026-03-22")))
      (with-temp-buffer
        (insert "Nothing to replace here.\n")
        (let ((before (buffer-string)))
          (pel-update-time-stamp-patterns)
          (should (string= (buffer-string) before)))))))

;; ---------------------------------------------------------------------------
;; pel--update-time-stamp

(ert-deftest pel--update-time-stamp-no-file-test ()
  "When buffer-file-name is nil, neither time-stamp nor patterns are called."
  (let ((ts-called  nil)
        (pat-called nil))
    (cl-letf (((symbol-function 'time-stamp)
               (lambda () (setq ts-called t)))
              ((symbol-function 'pel-update-time-stamp-patterns)
               (lambda () (setq pat-called t))))
      (let ((buffer-file-name   nil)
            (pel-update-time-stamp t))
        (pel--update-time-stamp)
        (should-not ts-called)
        (should-not pat-called)))))

(ert-deftest pel--update-time-stamp-disabled-test ()
  "When pel-update-time-stamp is nil, neither time-stamp nor patterns are called."
  (let ((ts-called  nil)
        (pat-called nil))
    (cl-letf (((symbol-function 'time-stamp)
               (lambda () (setq ts-called t)))
              ((symbol-function 'pel-update-time-stamp-patterns)
               (lambda () (setq pat-called t))))
      (let ((buffer-file-name   "/some/file.el")
            (pel-update-time-stamp nil))
        (pel--update-time-stamp)
        (should-not ts-called)
        (should-not pat-called)))))

(ert-deftest pel--update-time-stamp-active-test ()
  "When buffer-file-name is non-nil and pel-update-time-stamp is non-nil,
both time-stamp and pel-update-time-stamp-patterns must be called."
  (let ((ts-called  nil)
        (pat-called nil))
    (cl-letf (((symbol-function 'time-stamp)
               (lambda () (setq ts-called t)))
              ((symbol-function 'pel-update-time-stamp-patterns)
               (lambda () (setq pat-called t))))
      (let ((buffer-file-name   "/some/file.el")
            (pel-update-time-stamp t))
        (pel--update-time-stamp)
        (should ts-called)
        (should pat-called)))))

(ert-deftest pel--update-time-stamp-call-order-test ()
  "time-stamp must be called before pel-update-time-stamp-patterns."
  (let (call-log)
    (cl-letf (((symbol-function 'time-stamp)
               (lambda () (push 'time-stamp call-log)))
              ((symbol-function 'pel-update-time-stamp-patterns)
               (lambda () (push 'patterns call-log))))
      (let ((buffer-file-name   "/some/file.el")
            (pel-update-time-stamp t))
        (pel--update-time-stamp)
        ;; call-log is built with push so it is in reverse order.
        (should (equal (nreverse call-log) '(time-stamp patterns)))))))

;; ---------------------------------------------------------------------------
;; pel-toggle-update-time-stamp-on-save

(ert-deftest pel-toggle-update-time-stamp-on-save-local-off-to-on-test ()
  "Local toggle from nil → non-nil enables pel-update-time-stamp and
time-stamp-active locally, and registers pel--update-time-stamp in
before-save-hook."
  (pel-ts-test--silent
    (with-temp-buffer
      ;; Isolate the global hook from the test.
      (let ((before-save-hook (remq 'pel--update-time-stamp before-save-hook)))
        (setq-local pel-update-time-stamp nil)
        (setq-local time-stamp-active     nil)
        (pel-toggle-update-time-stamp-on-save)
        (should pel-update-time-stamp)
        (should time-stamp-active)
        (should (memq 'pel--update-time-stamp before-save-hook))))))

(ert-deftest pel-toggle-update-time-stamp-on-save-local-on-to-off-test ()
  "Local toggle from non-nil → nil disables pel-update-time-stamp and
time-stamp-active locally."
  (pel-ts-test--silent
    (with-temp-buffer
      (let ((before-save-hook before-save-hook))
        (setq-local pel-update-time-stamp t)
        (setq-local time-stamp-active     t)
        (pel-toggle-update-time-stamp-on-save)
        (should-not pel-update-time-stamp)
        (should-not time-stamp-active)))))

(ert-deftest pel-toggle-update-time-stamp-on-save-hook-not-duplicated-test ()
  "Toggling on when the hook is already present must not duplicate it."
  (pel-ts-test--silent
    (with-temp-buffer
      (let ((before-save-hook (list 'pel--update-time-stamp)))
        (setq-local pel-update-time-stamp nil)
        (pel-toggle-update-time-stamp-on-save)
        (should (= 1 (length (remq nil
                                   (mapcar (lambda (f)
                                             (eq f 'pel--update-time-stamp))
                                           before-save-hook)))))))))

(ert-deftest pel-toggle-update-time-stamp-on-save-global-test ()
  "Global toggle (GLOBALLY non-nil) changes the global values of
pel-update-time-stamp and time-stamp-active without making them buffer-local."
  (pel-ts-test--silent
    (let ((saved-ts  pel-update-time-stamp)
          (saved-tsa time-stamp-active)
          (before-save-hook before-save-hook))
      (unwind-protect
          (progn
            (setq pel-update-time-stamp nil)
            (setq time-stamp-active     nil)
            (pel-toggle-update-time-stamp-on-save :globally)
            ;; Global variables must be updated.
            (should pel-update-time-stamp)
            (should time-stamp-active)
            ;; Must NOT have become buffer-local.
            (should-not (local-variable-p 'pel-update-time-stamp)))
        ;; Restore global state even if the test fails.
        (setq pel-update-time-stamp saved-ts)
        (setq time-stamp-active     saved-tsa)))))

;; ---------------------------------------------------------------------------
;; pel-toggle-update-copyright-on-save

(ert-deftest pel-toggle-update-copyright-on-save-local-off-to-on-test ()
  "Local toggle from nil → non-nil enables pel-update-copyright locally."
  (pel-ts-test--silent
    (with-temp-buffer
      (setq-local pel-update-copyright nil)
      (pel-toggle-update-copyright-on-save)
      (should pel-update-copyright))))

(ert-deftest pel-toggle-update-copyright-on-save-local-on-to-off-test ()
  "Local toggle from non-nil → nil disables pel-update-copyright locally."
  (pel-ts-test--silent
    (with-temp-buffer
      (setq-local pel-update-copyright t)
      (pel-toggle-update-copyright-on-save)
      (should-not pel-update-copyright))))

(ert-deftest pel-toggle-update-copyright-on-save-global-test ()
  "Global toggle changes pel-update-copyright globally without making it
buffer-local."
  (pel-ts-test--silent
    (let ((saved pel-update-copyright))
      (unwind-protect
          (progn
            (setq pel-update-copyright nil)
            (pel-toggle-update-copyright-on-save :globally)
            (should pel-update-copyright)
            (should-not (local-variable-p 'pel-update-copyright)))
        (setq pel-update-copyright saved)))))

;; ---------------------------------------------------------------------------
;; pel--update-copyright

(ert-deftest pel--update-copyright-no-filename-test ()
  "When pel-current-buffer-filename returns nil, copyright-update is not called."
  (let ((cu-called nil))
    (cl-letf (((symbol-function 'pel-current-buffer-filename)
               (lambda (&rest _) nil))
              ((symbol-function 'copyright-update)
               (lambda (&rest _) (setq cu-called t))))
      (let ((pel-update-copyright t)
            (pel-skip-copyright-in nil)
            (pel-copyright-noprompt-in nil))
        (pel--update-copyright)
        (should-not cu-called)))))

(ert-deftest pel--update-copyright-disabled-test ()
  "When pel-update-copyright is nil, copyright-update is not called."
  (let ((cu-called nil))
    (cl-letf (((symbol-function 'pel-current-buffer-filename)
               (lambda (&rest _) "/project/file.el"))
              ((symbol-function 'copyright-update)
               (lambda (&rest _) (setq cu-called t))))
      (let ((pel-update-copyright nil)
            (pel-skip-copyright-in nil)
            (pel-copyright-noprompt-in nil))
        (pel--update-copyright)
        (should-not cu-called)))))

(ert-deftest pel--update-copyright-skipped-file-test ()
  "When the file matches pel-skip-copyright-in, copyright-update is not called."
  (let ((cu-called nil))
    (cl-letf (((symbol-function 'pel-current-buffer-filename)
               (lambda (&rest _) "/vendor/file.el"))
              ;; Simulate pel-file-in finding the file in the skip list.
              ((symbol-function 'pel-file-in)
               (lambda (fn lst)
                 (when (member fn lst) fn)))
              ((symbol-function 'copyright-update)
               (lambda (&rest _) (setq cu-called t))))
      (let ((pel-update-copyright t)
            (pel-skip-copyright-in '("/vendor/file.el"))
            (pel-copyright-noprompt-in nil))
        (pel--update-copyright)
        (should-not cu-called)))))

(ert-deftest pel--update-copyright-noprompt-file-test ()
  "A file in pel-copyright-noprompt-in calls copyright-update silently:
copyright-query must be nil and interactivep must be t."
  (let (cu-args)
    (cl-letf (((symbol-function 'pel-current-buffer-filename)
               (lambda (&rest _) "/auto/file.el"))
              ;; Not in skip list; is in noprompt list.
              ((symbol-function 'pel-file-in)
               (lambda (fn lst)
                 (cond
                  ;; skip list: empty → nil
                  ((null lst) nil)
                  ;; noprompt list: fn found
                  ((member fn lst) fn)
                  (t nil))))
              ((symbol-function 'copyright-update)
               (lambda (arg interactivep)
                 (setq cu-args (list :arg arg
                                     :query copyright-query
                                     :interactive interactivep)))))
      (let ((pel-update-copyright t)
            (pel-skip-copyright-in nil)
            (pel-copyright-noprompt-in '("/auto/file.el"))
            (copyright-query 'function))  ; default global value
        (pel--update-copyright)
        (should cu-args)
        ;; copyright-query must have been overridden to nil (silent).
        (should (eq  nil       (plist-get cu-args :query)))
        ;; interactivep must be t (call as-if interactive).
        (should (eq  t         (plist-get cu-args :interactive)))))))

(ert-deftest pel--update-copyright-prompt-file-test ()
  "A file not in any list calls copyright-update with copyright-query = function
and interactivep = nil (user prompt path)."
  (let (cu-args)
    (cl-letf (((symbol-function 'pel-current-buffer-filename)
               (lambda (&rest _) "/project/file.el"))
              ;; Not in either list.
              ((symbol-function 'pel-file-in)
               (lambda (_fn _lst) nil))
              ((symbol-function 'copyright-update)
               (lambda (arg interactivep)
                 (setq cu-args (list :arg arg
                                     :query copyright-query
                                     :interactive interactivep)))))
      (let ((pel-update-copyright t)
            (pel-skip-copyright-in nil)
            (pel-copyright-noprompt-in nil)
            (copyright-query 'function))
        (pel--update-copyright)
        (should cu-args)
        ;; copyright-query is NOT overridden → remains 'function.
        (should (eq 'function (plist-get cu-args :query)))
        ;; interactivep is nil (non-silent, non-interactive path).
        (should (eq nil (plist-get cu-args :interactive)))))))

(ert-deftest pel--update-copyright-arg-forwarded-test ()
  "The ARG argument to pel--update-copyright is forwarded to copyright-update."
  (let (received-arg)
    (cl-letf (((symbol-function 'pel-current-buffer-filename)
               (lambda (&rest _) "/project/file.el"))
              ((symbol-function 'pel-file-in)
               (lambda (_fn _lst) nil))
              ((symbol-function 'copyright-update)
               (lambda (arg _interactivep)
                 (setq received-arg arg))))
      (let ((pel-update-copyright t)
            (pel-skip-copyright-in nil)
            (pel-copyright-noprompt-in nil)
            (copyright-query nil))
        ;; Pass a non-nil ARG (replace years rather than append current year).
        (pel--update-copyright :replace)
        (should (eq :replace received-arg))))))

;; ---------------------------------------------------------------------------
;; pel-time-stamp-control-show-info — smoke test
;;
;; This interactive command creates a *pel-time-stamp-info* help buffer.
;; Full output validation is fragile; we only verify that the buffer
;; is created and contains the expected heading string.
;;
;; pel-time-stamp-control-show-info is primarily a display command that
;; calls pel-print-in-buffer / with-help-window. The smoke test mocks
;; with-help-window to avoid a live UI dependency while still verifying the key
;; variable names appear in the output. The four interactive toggle commands
;; (pel-toggle-update-time-stamp-on-save, pel-toggle-update-copyright-on-save)
;; are tested by calling them non-interactively inside with-temp-buffer and using
;; pel-ts-test--silent to suppress message output.

(ert-deftest pel-time-stamp-control-show-info-smoke-test ()
  "Smoke test: pel-time-stamp-control-show-info creates the info buffer."
  ;; Kill the buffer first so we start clean.
  (when (get-buffer "*pel-time-stamp-info*")
    (kill-buffer "*pel-time-stamp-info*"))
  ;; The command internally calls pel-print-in-buffer which uses
  ;; with-help-window; suppress UI side-effects with cl-letf.
  (cl-letf (((symbol-function 'with-help-window)
             (lambda (buf-name fn)
               (with-current-buffer (get-buffer-create buf-name)
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (funcall fn))))))
    (pel-time-stamp-control-show-info))
  (let ((buf (get-buffer "*pel-time-stamp-info*")))
    (should buf)
    (with-current-buffer buf
      ;; The buffer should mention the key variables.
      (should (string-match-p "pel-update-time-stamp" (buffer-string)))
      (should (string-match-p "time-stamp-active"     (buffer-string)))))
  ;; Clean up.
  (when (get-buffer "*pel-time-stamp-info*")
    (kill-buffer "*pel-time-stamp-info*")))

;;; --------------------------------------------------------------------------
(provide 'pel-timestamp-test)

;;; pel-timestamp-test.el ends here
