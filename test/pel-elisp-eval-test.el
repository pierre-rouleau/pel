;;; pel-elisp-eval-test.el --- Test pel-elisp-eval.el  -*- lexical-binding: t; -*-

;; Created   : Thursday, April 30 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-04-30 10:02:20 EDT, updated by Pierre Rouleau>

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
;; ERT regression tests for pel-elisp-eval.el.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-elisp-eval)
(require 'ert)
(require 'pel-ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;
;;* Test Helpers
;; -------------

(defmacro pel-test--with-elisp-buffer (content &rest body)
  "Run BODY inside a temporary `emacs-lisp-mode' buffer containing CONTENT.
Point is placed at `point-min' before BODY runs."
  (declare (indent 1))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (setq-local lexical-binding t)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(defmacro pel-test--capture-message (&rest body)
  "Execute BODY; return the last string delivered to `message'."
  `(let (pel--test-last-message)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (setq pel--test-last-message (apply #'format fmt args)))))
       ,@body)
     pel--test-last-message))

;; ---------------------------------------------------------------------------
;;* pel--eval-target-buffer — initial state
;; -----------------------------------------

(ert-deftest pel-test/eval-target-buffer/initially-nil ()
  "pel--eval-target-buffer is nil at load time."
  (should (null pel--eval-target-buffer)))

;; ---------------------------------------------------------------------------
;;* pel--eval-buffer-binding-type
;; --------------------------------

(ert-deftest pel-test/eval-buffer-binding-type/lexical ()
  "Returns \"lexical\" when the current buffer has lexical-binding."
  (with-temp-buffer
    (setq-local lexical-binding t)
    (should (pel-equal (pel--eval-buffer-binding-type) "lexical"
                       "lexical buffer"))))

(ert-deftest pel-test/eval-buffer-binding-type/dynamic ()
  "Returns \"dynamic\" when the current buffer does not have lexical-binding."
  (with-temp-buffer
    (setq-local lexical-binding nil)
    (should (pel-equal (pel--eval-buffer-binding-type) "dynamic"
                       "dynamic buffer"))))

;; ---------------------------------------------------------------------------
;;* pel-eval-info
;; --------------

(ert-deftest pel-test/pel-eval-info/inactive ()
  "Reports 'inactive' when pel--eval-target-buffer is nil."
  (let ((pel--eval-target-buffer nil))
    (let ((msg (pel-test--capture-message (pel-eval-info))))
      (should (pel-equal (null (string-match "inactive" msg)) nil
                         "message should contain 'inactive'")))))

(ert-deftest pel-test/pel-eval-info/active-live-buffer ()
  "Reports target buffer name when target is alive."
  (let* ((buf (get-buffer-create "*pel-test/eval-info/live*"))
         (pel--eval-target-buffer buf))
    (unwind-protect
        (let ((msg (pel-test--capture-message (pel-eval-info))))
          (should (pel-equal
                   (null (string-match (regexp-quote "*pel-test/eval-info/live*") msg))
                   nil
                   "message should contain buffer name")))
      (kill-buffer buf))))

(ert-deftest pel-test/pel-eval-info/killed-buffer-resets-variable ()
  "When target buffer is killed, pel-eval-info resets pel--eval-target-buffer to nil."
  (let* ((buf (generate-new-buffer "*pel-test/eval-info/dead*"))
         (pel--eval-target-buffer buf))
    (kill-buffer buf)
    (pel-test--capture-message (pel-eval-info))
    (should (null pel--eval-target-buffer))))

(ert-deftest pel-test/pel-eval-info/killed-buffer-message ()
  "When target buffer is killed, pel-eval-info prints an informative message."
  (let* ((buf (generate-new-buffer "*pel-test/eval-info/dead2*"))
         (pel--eval-target-buffer buf))
    (kill-buffer buf)
    (let ((msg (pel-test--capture-message (pel-eval-info))))
      ;; Should mention something about the buffer being killed/stopped.
      (should (or (string-match "killed" msg)
                  (string-match "stopped" msg))))))

(ert-deftest pel-test/pel-eval-info/active-shows-binding-type ()
  "Active message mentions binding type."
  (let* ((buf (get-buffer-create "*pel-test/eval-info/binding*"))
         (pel--eval-target-buffer buf))
    (unwind-protect
        (let ((msg (pel-test--capture-message
                    ;; In a lexical-binding buffer:
                    (with-temp-buffer
                      (setq-local lexical-binding t)
                      (pel-eval-info)))))
          (should (or (string-match "lexical" msg)
                      (string-match "dynamic" msg))))
      (kill-buffer buf))))

;; ---------------------------------------------------------------------------
;;* pel-eval-set-target-buffer
;; ----------------------------

(ert-deftest pel-test/pel-eval-set-target-buffer/sets-variable ()
  "Sets pel--eval-target-buffer to the buffer object for the chosen name."
  (let* ((target (get-buffer-create "*pel-test/set-target*"))
         (pel--eval-target-buffer nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'read-buffer)
                     (lambda (&rest _) (buffer-name target)))
                    ((symbol-function 'message)
                     (lambda (&rest _) nil)))
            (pel-eval-set-target-buffer))
          (should (pel-equal pel--eval-target-buffer target
                             "pel--eval-target-buffer should equal the target buffer")))
      (kill-buffer target))))

(ert-deftest pel-test/pel-eval-set-target-buffer/message-contains-name ()
  "Confirmation message contains the buffer name, not a raw buffer object."
  (let* ((target (get-buffer-create "*pel-test/set-name*"))
         (pel--eval-target-buffer nil))
    (unwind-protect
        (let (msg)
          (cl-letf (((symbol-function 'read-buffer)
                     (lambda (&rest _) (buffer-name target)))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq msg (apply #'format fmt args)))))
            (pel-eval-set-target-buffer))
          ;; Must NOT contain raw buffer object syntax.
          (should-not (string-match "#<buffer" msg))
          ;; Must contain the buffer name.
          (should (string-match (regexp-quote "*pel-test/set-name*") msg)))
      (kill-buffer target))))

;; ---------------------------------------------------------------------------
;;* pel-eval-stop
;; --------------

(ert-deftest pel-test/pel-eval-stop/clears-target ()
  "Clears pel--eval-target-buffer to nil."
  (let* ((buf (get-buffer-create "*pel-test/stop*"))
         (pel--eval-target-buffer buf))
    (unwind-protect
        (progn
          (pel-eval-stop)
          (should (null pel--eval-target-buffer)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest pel-test/pel-eval-stop/user-error-when-no-target ()
  "Signals user-error when pel--eval-target-buffer is nil."
  (let ((pel--eval-target-buffer nil))
    (should-error (pel-eval-stop) :type 'user-error)))

;; ---------------------------------------------------------------------------
;;* pel--safe-forward-sexp
;; ------------------------

(ert-deftest pel-test/safe-forward-sexp/list-sexp ()
  "Moves past a list sexp successfully."
  (pel-test--with-elisp-buffer "(setq x 1)"
    (should-not (condition-case err
                    (progn (pel--safe-forward-sexp) nil)
                  (error err)))
    (should (= (point) (point-max)))))

(ert-deftest pel-test/safe-forward-sexp/atom-integer ()
  "Moves past an integer atom without error.
NOTE: This test documents a known bug — the current (< original-pos (point))
and (eq (char-before) ?\\)) check spuriously fires for atoms.
Mark :expected-result :failed until the bug is fixed."
  (pel-test--with-elisp-buffer "42 "
    (should-not (condition-case err
                    (progn (pel--safe-forward-sexp) nil)
                  (user-error err)))))

(ert-deftest pel-test/safe-forward-sexp/atom-string ()
  "Moves past a string atom without error.
NOTE: Same known bug as for integers."
  (pel-test--with-elisp-buffer "\"hello\" "
    (should-not (condition-case err
                    (progn (pel--safe-forward-sexp) nil)
                  (user-error err)))))

(ert-deftest pel-test/safe-forward-sexp/atom-symbol ()
  "Moves past a symbol atom without error.
NOTE: Same known bug as for integers."
  (pel-test--with-elisp-buffer "my-symbol "
    (should-not (condition-case err
                    (progn (pel--safe-forward-sexp) nil)
                  (user-error err)))))

(ert-deftest pel-test/safe-forward-sexp/at-eob-signals-user-error ()
  "Signals user-error (not scan-error) when there is nothing to move over.
NOTE: Currently forward-sexp throws raw scan-error since there is no
condition-case wrapper.  Mark :expected-result :failed until bug `#1` is fixed."
  (pel-test--with-elisp-buffer ""
    (should-error (pel--safe-forward-sexp) :type 'user-error)))

;; ---------------------------------------------------------------------------
;;* pel-eval-to-target — basic evaluation
;; ----------------------------------------

(ert-deftest pel-test/pel-eval-to-target/evaluates-list-sexp-in-target ()
  "Evaluates a list sexp in the target buffer."
  (let* ((target (get-buffer-create "*pel-test/eval-target*"))
         (pel--eval-target-buffer target))
    (unwind-protect
        (pel-test--with-elisp-buffer "(setq pel--test-sentinel-value 99)\n"
          ;; Make sure the variable is not set in target beforehand.
          (with-current-buffer target
            (makunbound 'pel--test-sentinel-value))
          (pel-eval-to-target)
          (should (pel-equal
                   (buffer-local-value 'pel--test-sentinel-value target)
                   99
                   "sentinel must be 99 in target buffer")))
      (kill-buffer target))))

(ert-deftest pel-test/pel-eval-to-target/advances-point ()
  "Point moves past the evaluated sexp."
  (let* ((target (get-buffer-create "*pel-test/advance*"))
         (pel--eval-target-buffer target))
    (unwind-protect
        (pel-test--with-elisp-buffer "(setq x 1) (setq y 2)\n"
          (let ((pos-before (point)))
            (pel-eval-to-target)
            (should (> (point) pos-before))))
      (kill-buffer target))))

(ert-deftest pel-test/pel-eval-to-target/skips-comment-between-sexps ()
  "After evaluation, comments between sexps are skipped."
  (let* ((target (get-buffer-create "*pel-test/skip-comment*"))
         (pel--eval-target-buffer target))
    (unwind-protect
        (pel-test--with-elisp-buffer
            "(setq a 1)\n;; a comment\n(setq b 2)\n"
          (pel-eval-to-target)
          ;; Point should not be sitting on a semicolon.
          (should-not (equal (char-after (point)) ?\;)))
      (kill-buffer target))))

(ert-deftest pel-test/pel-eval-to-target/steps-through-multiple-sexps ()
  "Two consecutive calls step through two consecutive sexps."
  (let* ((target (get-buffer-create "*pel-test/multi-step*"))
         (pel--eval-target-buffer target))
    (unwind-protect
        (pel-test--with-elisp-buffer
            "(setq pel--step-a 10)\n(setq pel--step-b 20)\n"
          ;; Step 1
          (pel-eval-to-target)
          (should (pel-equal
                   (buffer-local-value 'pel--step-a target) 10
                   "pel--step-a should be 10 after step 1"))
          ;; Step 2
          (pel-eval-to-target)
          (should (pel-equal
                   (buffer-local-value 'pel--step-b target) 20
                   "pel--step-b should be 20 after step 2")))
      (kill-buffer target))))

(ert-deftest pel-test/pel-eval-to-target/uses-lexical-binding-of-target ()
  "Evaluation uses the lexical-binding setting of the target buffer."
  (let* ((target (get-buffer-create "*pel-test/lexical*"))
         (pel--eval-target-buffer target))
    (with-current-buffer target
      (setq-local lexical-binding t))
    (unwind-protect
        (pel-test--with-elisp-buffer "(setq pel--lexical-test 42)\n"
          (pel-eval-to-target)
          (should (pel-equal
                   (buffer-local-value 'pel--lexical-test target)
                   42
                   "evaluation should succeed in a lexical-binding target")))
      (kill-buffer target))))

(ert-deftest pel-test/pel-eval-to-target/prompts-when-no-target ()
  "Prompts for a target buffer when pel--eval-target-buffer is nil."
  (let* ((pel--eval-target-buffer nil)
         (prompted nil))
    (cl-letf (((symbol-function 'pel-eval-set-target-buffer)
               (lambda ()
                 (interactive)
                 (setq prompted t))))
      (pel-test--with-elisp-buffer "(+ 1 2)\n"
        (ignore-errors (call-interactively 'pel-eval-to-target))))
    (should prompted)))

(ert-deftest pel-test/pel-eval-to-target/prompts-when-target-killed ()
  "Prompts for a new target when the previous target has been killed."
  (let* ((buf (generate-new-buffer "*pel-test/killed-target*"))
         (pel--eval-target-buffer buf)
         (prompted nil))
    (kill-buffer buf)
    (cl-letf (((symbol-function 'pel-eval-set-target-buffer)
               (lambda ()
                 (interactive)
                 (setq prompted t))))
      (pel-test--with-elisp-buffer "(+ 1 2)\n"
        (ignore-errors (call-interactively 'pel-eval-to-target))))
    (should prompted)))

(ert-deftest pel-test/pel-eval-to-target/at-eob-does-not-scan-error ()
  "Calling pel-eval-to-target with no sexp forward raises user-error, not scan-error.
NOTE: Currently the outer (forward-sexp) is unguarded so a raw scan-error
propagates.  Mark :expected-result :failed until bug `#4` is fixed."
  (let* ((target (get-buffer-create "*pel-test/eob*"))
         (pel--eval-target-buffer target))
    (unwind-protect
        (pel-test--with-elisp-buffer "(setq z 0)\n"
          ;; Step past the only sexp.
          (ignore-errors (pel-eval-to-target))
          ;; Now at EOB — second call should raise user-error, not scan-error.
          (should-error (pel-eval-to-target) :type 'user-error))
      (kill-buffer target))))

;; ---------------------------------------------------------------------------
;;* pel-eval-last-sexp-and-copy
;; -----------------------------

(ert-deftest pel-test/pel-eval-last-sexp-and-copy/numeric-result ()
  "Evaluates preceding sexp; result in echo area and kill-ring."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local lexical-binding t)
    (insert "(+ 2 3)")
    (goto-char (point-max))
    (let (last-msg)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq last-msg (apply #'format fmt args)))))
        (pel-eval-last-sexp-and-copy))
      (should (pel-equal last-msg "5" "echo area should show 5"))
      (should (pel-equal (car kill-ring) "5" "kill-ring should hold 5")))))

(ert-deftest pel-test/pel-eval-last-sexp-and-copy/string-result ()
  "Works correctly when the result is a string."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local lexical-binding t)
    (insert "(concat \"foo\" \"-\" \"bar\")")
    (goto-char (point-max))
    (let (last-msg)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq last-msg (apply #'format fmt args)))))
        (pel-eval-last-sexp-and-copy))
      (should (pel-equal last-msg "foo-bar" "echo area"))
      (should (pel-equal (car kill-ring) "foo-bar" "kill-ring")))))

(ert-deftest pel-test/pel-eval-last-sexp-and-copy/nil-result ()
  "Works without error when the evaluated form returns nil."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local lexical-binding t)
    (insert "(and)")
    (goto-char (point-max))
    (should-not (condition-case err
                    (progn (pel-eval-last-sexp-and-copy) nil)
                  (error err)))))

;;; --------------------------------------------------------------------------
(provide 'pel-elisp-eval-test)

;;; pel-elisp-eval-test.el ends here
