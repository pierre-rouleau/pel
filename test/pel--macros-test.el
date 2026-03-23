;;; pel--macros-test.el --- ERT tests for pel--macros.el  -*- lexical-binding: t; -*-

;; Created   : Monday, March 23 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-22 23:16:21 EDT, updated by Pierre Rouleau>

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
;; ERT tests for pel--macros.el.  Covers all testable macros and functions:
;;
;;   pel-make-apply-to-elems        - pure helper function
;;   pel-when-fbound                - conditional on function boundedness
;;   pel-when-bound                 - conditional on variable boundedness
;;   bound-and-true-p               - value of bound variable or nil
;;   pel-append-to                  - append elements to a list variable
;;   pel-concat-to                  - append text to a string variable
;;   pel-setq                       - warning-suppressing setq
;;   pel-setq-local                 - warning-suppressing setq-local
;;   pel-setq-default               - warning-suppressing setq-default
;;   pel-setq-local-unless-filevar  - setq-local guarded by file-local-variable-alist
;;   while-n                        - bounded while loop
;;   pel-with-required              - conditional body on resource availability
;;   pel-turn-mode-on-when-off      - turn a mode on only when it is currently off
;;   pel-turn-mode-off-when-on      - turn a mode off only when it is currently on
;;
;; Run interactively : M-x ert RET "^pel--macros-test" RET
;; Run in batch      : emacs -batch -l ert -l pel--macros.el \
;;                       -l test/pel--macros-test.el -f ert-run-tests-batch-and-exit

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--macros)
(require 'ert)
(require 'cl-lib)                       ; needed by while-n (decf)

;;; --------------------------------------------------------------------------
;;; Code:

;; ---------------------------------------------------------------------------
;; Helper: synthetic minor mode used by pel-turn-mode-on/off tests.
;;
;; The mode feature must be `provide'd so that (featurep 'NAME) is t and the
;; conditional branches inside pel-turn-mode-on-when-off /
;; pel-turn-mode-off-when-on are exercised correctly.
;; ---------------------------------------------------------------------------

(define-minor-mode pel--macros-test--fake-mode
  "Fake minor mode used exclusively by the pel--macros ERT tests."
  :init-value nil
  :lighter " FkMd")

(provide 'pel--macros-test--fake-mode)  ; enable featurep check in mode macros

;; ---------------------------------------------------------------------------
;; Tests for pel-make-apply-to-elems
;; ---------------------------------------------------------------------------

(ert-deftest pel--macros-test/make-apply-to-elems/basic ()
  "Returns a list of (FUNC ELEM) forms for each element."
  (should (equal (pel-make-apply-to-elems 'run '(a b c))
                 '((run a) (run b) (run c))))
  (should (equal (pel-make-apply-to-elems 'foo '(x))
                 '((foo x)))))

(ert-deftest pel--macros-test/make-apply-to-elems/empty-elems ()
  "Returns an empty list when ELEMS is empty."
  (should (equal (pel-make-apply-to-elems 'f '()) '())))

(ert-deftest pel--macros-test/make-apply-to-elems/with-extra-args ()
  "Extra ARGS are threaded into every call form."
  (should (equal (pel-make-apply-to-elems 'require '(feat1 feat2) nil :noerror)
                 '((require feat1 nil :noerror)
                   (require feat2 nil :noerror)))))

;; ---------------------------------------------------------------------------
;; Tests for pel-when-fbound
;; ---------------------------------------------------------------------------

(ert-deftest pel--macros-test/when-fbound/executes-body-when-bound ()
  "Body is evaluated and its value returned when the function is bound."
  ;; `car' is always bound in Emacs.
  (should (equal (pel-when-fbound 'car (+ 1 2)) 3)))

(ert-deftest pel--macros-test/when-fbound/user-error-when-unbound ()
  "A user-error is signalled when the function symbol is not fbound."
  (should-error
   (pel-when-fbound 'pel--macros-test--nonexistent-fn-zzz (+ 1 2))
   :type 'user-error))

;; ---------------------------------------------------------------------------
;; Tests for pel-when-bound
;; ---------------------------------------------------------------------------

(defvar pel--macros-test--wb-var 42
  "Variable used by pel-when-bound tests.")

(ert-deftest pel--macros-test/when-bound/executes-body-when-bound ()
  "Body is evaluated and its value returned when the variable is bound."
  (should (equal (pel-when-bound 'pel--macros-test--wb-var
                   pel--macros-test--wb-var)
                 42)))

(ert-deftest pel--macros-test/when-bound/user-error-when-unbound ()
  "A user-error is signalled when the variable symbol is not bound."
  ;; Guarantee the symbol is unbound before the test.
  (when (boundp 'pel--macros-test--unbound-var-zzz)
    (makunbound 'pel--macros-test--unbound-var-zzz))
  (should-error
   (pel-when-bound 'pel--macros-test--unbound-var-zzz (ignore))
   :type 'user-error))

;; ---------------------------------------------------------------------------
;; Tests for bound-and-true-p
;; ---------------------------------------------------------------------------

(defvar pel--macros-test--bat-true  t   "Bound-and-true-p helper: true value.")
(defvar pel--macros-test--bat-false nil "Bound-and-true-p helper: false value.")
(defvar pel--macros-test--bat-num   7   "Bound-and-true-p helper: numeric value.")

(ert-deftest pel--macros-test/bound-and-true-p/returns-value-when-bound-and-true ()
  "Returns the variable's value when it is bound and non-nil."
  (should (eq   (bound-and-true-p pel--macros-test--bat-true)  t))
  (should (eql  (bound-and-true-p pel--macros-test--bat-num)   7)))

(ert-deftest pel--macros-test/bound-and-true-p/returns-nil-when-bound-and-nil ()
  "Returns nil when the variable is bound but its value is nil."
  (should (null (bound-and-true-p pel--macros-test--bat-false))))

(ert-deftest pel--macros-test/bound-and-true-p/returns-nil-when-unbound ()
  "Returns nil when the variable is not bound."
  (defvar pel--macros-test--bat-unbound)
  (makunbound 'pel--macros-test--bat-unbound)
  (should (null (bound-and-true-p pel--macros-test--bat-unbound))))

;; ---------------------------------------------------------------------------
;; Tests for pel-append-to
;; ---------------------------------------------------------------------------

(ert-deftest pel--macros-test/append-to/builds-list-incrementally ()
  "Successive appends accumulate elements in order."
  (let (lst)
    (pel-append-to lst '(1 2 3))
    (should (equal lst '(1 2 3)))
    (pel-append-to lst '(4 5))
    (should (equal lst '(1 2 3 4 5)))))

(ert-deftest pel--macros-test/append-to/appending-empty-list-is-noop ()
  "Appending an empty list leaves the variable unchanged."
  (let ((lst '(a b c)))
    (pel-append-to lst '())
    (should (equal lst '(a b c)))))

(ert-deftest pel--macros-test/append-to/starts-from-nil ()
  "Works correctly when the list variable starts as nil."
  (let (lst)
    (pel-append-to lst '(x))
    (should (equal lst '(x)))))

;; ---------------------------------------------------------------------------
;; Tests for pel-concat-to
;; ---------------------------------------------------------------------------

(ert-deftest pel--macros-test/concat-to/appends-text ()
  "Successive calls concatenate text in order."
  (let ((s "hello"))
    (pel-concat-to s ", world")
    (should (string= s "hello, world"))))

(ert-deftest pel--macros-test/concat-to/starts-from-empty-string ()
  "Works correctly when the string variable starts empty."
  (let ((s ""))
    (pel-concat-to s "foo")
    (pel-concat-to s "bar")
    (should (string= s "foobar"))))

;; ---------------------------------------------------------------------------
;; Tests for pel-setq
;; ---------------------------------------------------------------------------

(ert-deftest pel--macros-test/setq/sets-value ()
  "pel-setq sets the variable to the given value."
  (pel-setq pel--macros-test--sq-var 99)
  (should (equal pel--macros-test--sq-var 99)))

(ert-deftest pel--macros-test/setq/overwrites-value ()
  "pel-setq overwrites a previously set value."
  (pel-setq pel--macros-test--sq-var 1)
  (pel-setq pel--macros-test--sq-var 2)
  (should (equal pel--macros-test--sq-var 2)))

;; ---------------------------------------------------------------------------
;; Tests for pel-setq-local
;; ---------------------------------------------------------------------------

(ert-deftest pel--macros-test/setq-local/sets-buffer-local-value ()
  "pel-setq-local sets a buffer-local binding in the current buffer."
  (with-temp-buffer
    (pel-setq-local pel--macros-test--sl-var 77)
    (should (equal pel--macros-test--sl-var 77))
    (should (local-variable-p 'pel--macros-test--sl-var))))

(ert-deftest pel--macros-test/setq-local/does-not-affect-other-buffers ()
  "A value set with pel-setq-local in one buffer is not seen in another."
  (let (foreign-value)
    (with-temp-buffer
      (pel-setq-local pel--macros-test--sl-isolation-var :local))
    ;; Outside the temp buffer the variable should be unbound or differ.
    (setq foreign-value
          (if (boundp 'pel--macros-test--sl-isolation-var)
              pel--macros-test--sl-isolation-var
            :unbound))
    (should-not (eq foreign-value :local))))

;; ---------------------------------------------------------------------------
;; Tests for pel-setq-default
;; ---------------------------------------------------------------------------

(ert-deftest pel--macros-test/setq-default/sets-default-value ()
  "pel-setq-default updates the default (global) value of a variable."
  (pel-setq-default pel--macros-test--sd-var 55)
  (should (equal (default-value 'pel--macros-test--sd-var) 55)))

(ert-deftest pel--macros-test/setq-default/local-binding-shadows-default ()
  "A buffer-local binding shadows the default set by pel-setq-default."
  (pel-setq-default pel--macros-test--sd-shadow-var :default)
  (with-temp-buffer
    (setq-local pel--macros-test--sd-shadow-var :local)
    (should (eq pel--macros-test--sd-shadow-var :local))
    (should (eq (default-value 'pel--macros-test--sd-shadow-var) :default))))

;; ---------------------------------------------------------------------------
;; Tests for pel-setq-local-unless-filevar
;; ---------------------------------------------------------------------------

(ert-deftest pel--macros-test/setq-local-unless-filevar/sets-when-no-filevar ()
  "Sets the local value when the symbol is absent from file-local-variable-alist."
  (with-temp-buffer
    (setq-local file-local-variable-alist nil)
    (pel-setq-local-unless-filevar pel--macros-test--fv-var :new)
    (should (eq pel--macros-test--fv-var :new))))

(ert-deftest pel--macros-test/setq-local-unless-filevar/does-not-override-filevar ()
  "Does NOT change the value when the symbol appears in file-local-variable-alist."
  (defvar pel--macros-test--fv-var)
  (with-temp-buffer
    (setq-local file-local-variable-alist
                '((pel--macros-test--fv-var . :from-file)))
    (setq-local pel--macros-test--fv-var :from-file)
    (pel-setq-local-unless-filevar pel--macros-test--fv-var :override)
    ;; Value must remain :from-file, not :override.
    (should (eq pel--macros-test--fv-var :from-file))))

;; ---------------------------------------------------------------------------
;; Tests for while-n
;; ---------------------------------------------------------------------------

(ert-deftest pel--macros-test/while-n/runs-at-most-count-times ()
  "Body executes exactly COUNT times when COND never becomes false."
  (let ((n 0))
    (while-n 5 t (setq n (1+ n)))
    (should (= n 5))))

(ert-deftest pel--macros-test/while-n/stops-early-on-false-cond ()
  "Body stops executing as soon as COND becomes false, before COUNT is reached."
  (let ((counter 0) (iters 0))
    (while-n 100 (< counter 3)
      (setq counter (1+ counter))
      (setq iters   (1+ iters)))
    (should (= iters 3))))

(ert-deftest pel--macros-test/while-n/zero-count-never-runs ()
  "Body never executes when COUNT is 0."
  (let ((n 0))
    (while-n 0 t (setq n (1+ n)))
    (should (= n 0))))

(ert-deftest pel--macros-test/while-n/false-cond-never-runs ()
  "Body never executes when COND is initially nil."
  (let ((n 0))
    (while-n 10 nil (setq n (1+ n)))
    (should (= n 0))))

(ert-deftest pel--macros-test/while-n/negative-count-never-runs ()
  "Body never executes when COUNT is negative."
  (let ((n 0))
    (while-n -3 t (setq n (1+ n)))
    (should (= n 0))))

;; ---------------------------------------------------------------------------
;; Tests for pel-with-required
;; ---------------------------------------------------------------------------

;; (ert-deftest pel--macros-test/with-required/executes-body-when-all-available ()
;;   "Body is evaluated when all features, functions, and variables are available."
;;   ;; cl-lib is always present; car/cdr are always fbound;
;;   ;; emacs-version is always bound.
;;   (let ((result nil))
;;     (pel-with-required (cl-lib) (car cdr) (emacs-version)
;;       (setq result :ok))
;;     (should (eq result :ok))))

(ert-deftest pel--macros-test/with-required/error-on-missing-function ()
  "Signals an error when a required function is not fbound."
  (should-error
   (pel-with-required () (pel--macros-test--nonexistent-fn-zzz) ()
     (ignore))
   :type 'error))

(ert-deftest pel--macros-test/with-required/error-on-missing-variable ()
  "Signals an error when a required variable is not bound."
  (when (boundp 'pel--macros-test--unbound-req-var-zzz)
    (makunbound 'pel--macros-test--unbound-req-var-zzz))
  (should-error
   (pel-with-required () () (pel--macros-test--unbound-req-var-zzz)
     (ignore))
   :type 'error))

(ert-deftest pel--macros-test/with-required/empty-requirements-runs-body ()
  "Body runs when all requirement lists are empty."
  (let ((result nil))
    (pel-with-required () () ()
      (setq result :ran))
    (should (eq result :ran))))

;; ---------------------------------------------------------------------------
;; Tests for pel-turn-mode-on-when-off / pel-turn-mode-off-when-on
;;
;; The synthetic `pel--macros-test--fake-mode' defined above is used.
;; Its feature was provided with `(provide 'pel--macros-test--fake-mode)' so
;; that (featurep 'pel--macros-test--fake-mode) returns t and the correct
;; conditional branch is exercised in both macros.
;; ---------------------------------------------------------------------------

(ert-deftest pel--macros-test/turn-mode-on-when-off/enables-when-off ()
  "Mode is activated when it is currently off."
  (pel--macros-test--fake-mode -1)          ; ensure off
  (should-not pel--macros-test--fake-mode)
  (pel-turn-mode-on-when-off pel--macros-test--fake-mode)
  (should pel--macros-test--fake-mode)
  ;; Cleanup
  (pel--macros-test--fake-mode -1))

(ert-deftest pel--macros-test/turn-mode-on-when-off/noop-when-already-on ()
  "No error is raised and mode stays on when it is already active."
  (pel--macros-test--fake-mode 1)           ; ensure on
  (should pel--macros-test--fake-mode)
  (pel-turn-mode-on-when-off pel--macros-test--fake-mode)
  (should pel--macros-test--fake-mode)
  ;; Cleanup
  (pel--macros-test--fake-mode -1))

(ert-deftest pel--macros-test/turn-mode-off-when-on/disables-when-on ()
  "Mode is deactivated when it is currently on."
  (pel--macros-test--fake-mode 1)           ; ensure on
  (should pel--macros-test--fake-mode)
  (pel-turn-mode-off-when-on pel--macros-test--fake-mode)
  (should-not pel--macros-test--fake-mode))

(ert-deftest pel--macros-test/turn-mode-off-when-on/noop-when-already-off ()
  "No error is raised and mode stays off when it is already inactive."
  (pel--macros-test--fake-mode -1)          ; ensure off
  (should-not pel--macros-test--fake-mode)
  (pel-turn-mode-off-when-on pel--macros-test--fake-mode)
  (should-not pel--macros-test--fake-mode))

;;; --------------------------------------------------------------------------
(provide 'pel--macros-test)

;;; pel--macros-test.el ends here
