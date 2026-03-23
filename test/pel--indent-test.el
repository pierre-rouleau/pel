;;; pel--indent-test.el --- ERT tests for pel--indent.el  -*- lexical-binding: t; -*-

;; Created   : Monday, March 23 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-23 14:19:37 EDT, updated by Pierre Rouleau>

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
;; ERT tests for pel--indent.el.
;;
;; The file defines three `defvar-local' declarations.  There are no
;; functions or macros to test, so the tests verify the contractual
;; properties of those variables:
;;
;;   pel-indentation-width-control-variables
;;   pel-indentation-other-control-variables
;;   pel-tab-width-control-variables
;;
;; For each variable the tests check:
;;   1. The variable is defined (boundp).
;;   2. Its global default value is nil.
;;   3. It is buffer-local: a value set in one buffer is not visible in
;;      a different buffer (isolation).
;;   4. It accepts every value shape described in its docstring.
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--indent)
(require 'ert)
(require 'cl-lib)                       ; use `cl-every'

;;; --------------------------------------------------------------------------
;;; Code:

;; ===========================================================================
;; pel-indentation-width-control-variables
;; ===========================================================================

(ert-deftest pel--indent-test/indentation-width/is-defined ()
  "Variable `pel-indentation-width-control-variables' must be defined."
  (should (boundp 'pel-indentation-width-control-variables)))

(ert-deftest pel--indent-test/indentation-width/default-is-nil ()
  "Global default value of `pel-indentation-width-control-variables' is nil."
  ;; `default-value' returns the value outside any buffer-local binding.
  (should (null (default-value 'pel-indentation-width-control-variables))))

(ert-deftest pel--indent-test/indentation-width/is-buffer-local ()
  "A value set in one buffer is not seen in a different buffer."
  (let (foreign)
    (with-temp-buffer
      (setq-local pel-indentation-width-control-variables 'c-basic-offset)
      (should (eq pel-indentation-width-control-variables 'c-basic-offset))
      ;; Confirm the binding is local to this buffer.
      (should (local-variable-p 'pel-indentation-width-control-variables)))
    ;; Outside the temp buffer the value must not be 'c-basic-offset.
    (setq foreign
          (if (local-variable-p 'pel-indentation-width-control-variables)
              pel-indentation-width-control-variables
            :unbound))
    (should-not (eq foreign 'c-basic-offset))))

(ert-deftest pel--indent-test/indentation-width/accepts-single-symbol ()
  "Variable accepts a single symbol as documented."
  (with-temp-buffer
    (setq-local pel-indentation-width-control-variables 'c-basic-offset)
    (should (symbolp pel-indentation-width-control-variables))))

(ert-deftest pel--indent-test/indentation-width/accepts-list-of-symbols ()
  "Variable accepts a list of symbols as documented (last one controls width)."
  (with-temp-buffer
    (setq-local pel-indentation-width-control-variables
                '(js-indent-level js2-basic-offset))
    (should (listp pel-indentation-width-control-variables))
    (should (cl-every #'symbolp pel-indentation-width-control-variables))))

;; ===========================================================================
;; pel-indentation-other-control-variables
;; ===========================================================================

(ert-deftest pel--indent-test/indentation-other/is-defined ()
  "Variable `pel-indentation-other-control-variables' must be defined."
  (should (boundp 'pel-indentation-other-control-variables)))

(ert-deftest pel--indent-test/indentation-other/default-is-nil ()
  "Global default value of `pel-indentation-other-control-variables' is nil."
  (should (null (default-value 'pel-indentation-other-control-variables))))

(ert-deftest pel--indent-test/indentation-other/is-buffer-local ()
  "A value set in one buffer is not seen in a different buffer."
  (let (foreign)
    (with-temp-buffer
      (setq-local pel-indentation-other-control-variables '(indent-tabs-mode))
      (should (equal pel-indentation-other-control-variables '(indent-tabs-mode)))
      (should (local-variable-p 'pel-indentation-other-control-variables)))
    (setq foreign
          (if (local-variable-p 'pel-indentation-other-control-variables)
              pel-indentation-other-control-variables
            :unbound))
    (should-not (equal foreign '(indent-tabs-mode)))))

(ert-deftest pel--indent-test/indentation-other/accepts-list-of-symbols ()
  "Variable accepts a list of symbols as documented."
  (with-temp-buffer
    (setq-local pel-indentation-other-control-variables
                '(indent-tabs-mode tab-always-indent))
    (should (listp pel-indentation-other-control-variables))
    (should (cl-every #'symbolp pel-indentation-other-control-variables))))

(ert-deftest pel--indent-test/indentation-other/accepts-nil ()
  "Variable accepts nil (empty / not set) as a valid state."
  (with-temp-buffer
    (setq-local pel-indentation-other-control-variables nil)
    (should (null pel-indentation-other-control-variables))))

;; ===========================================================================
;; pel-tab-width-control-variables
;; ===========================================================================

(ert-deftest pel--indent-test/tab-width/is-defined ()
  "Variable `pel-tab-width-control-variables' must be defined."
  (should (boundp 'pel-tab-width-control-variables)))

(ert-deftest pel--indent-test/tab-width/default-is-nil ()
  "Global default value of `pel-tab-width-control-variables' is nil."
  (should (null (default-value 'pel-tab-width-control-variables))))

(ert-deftest pel--indent-test/tab-width/is-buffer-local ()
  "A value set in one buffer is not seen in a different buffer."
  (let (foreign)
    (with-temp-buffer
      (setq-local pel-tab-width-control-variables 'tab-width)
      (should (eq pel-tab-width-control-variables 'tab-width))
      (should (local-variable-p 'pel-tab-width-control-variables)))
    (setq foreign
          (if (local-variable-p 'pel-tab-width-control-variables)
              pel-tab-width-control-variables
            :unbound))
    (should-not (eq foreign 'tab-width))))

(ert-deftest pel--indent-test/tab-width/accepts-single-symbol ()
  "Variable accepts a single symbol as documented."
  (with-temp-buffer
    (setq-local pel-tab-width-control-variables 'tab-width)
    (should (symbolp pel-tab-width-control-variables))))

(ert-deftest pel--indent-test/tab-width/accepts-list-of-symbols ()
  "Variable accepts a list of symbols as documented."
  (with-temp-buffer
    (setq-local pel-tab-width-control-variables '(tab-width c-basic-offset))
    (should (listp pel-tab-width-control-variables))
    (should (cl-every #'symbolp pel-tab-width-control-variables))))

(ert-deftest pel--indent-test/tab-width/accepts-list-of-cons-cells ()
  "Variable accepts a list of (symbol . offset) cons cells as documented."
  (with-temp-buffer
    (setq-local pel-tab-width-control-variables
                '((c-basic-offset . 0) (c-indent-level . -2)))
    (should (listp pel-tab-width-control-variables))
    ;; Every element must be a cons of (symbol . integer).
    (should (cl-every (lambda (elem)
                        (and (consp elem)
                             (symbolp (car elem))
                             (integerp (cdr elem))))
                      pel-tab-width-control-variables))))

(ert-deftest pel--indent-test/tab-width/accepts-mixed-list ()
  "Variable accepts a mixed list (plain symbols and cons cells) as documented."
  ;; The docstring says each entry is either a symbol or a (symbol . offset)
  ;; cons cell; both may appear in the same list.
  (with-temp-buffer
    (setq-local pel-tab-width-control-variables
                '(tab-width (c-basic-offset . 0)))
    (should (listp pel-tab-width-control-variables))
    (should (symbolp (nth 0 pel-tab-width-control-variables)))
    (should (consp   (nth 1 pel-tab-width-control-variables)))))

;; ---------------------------------------------------------------------------
;; Cross-variable buffer isolation (all three variables at once)
;; ---------------------------------------------------------------------------

(ert-deftest pel--indent-test/all-vars/buffer-isolation ()
  "Setting all three variables in a temp buffer does not affect the caller."
  (let ((before-width (default-value 'pel-indentation-width-control-variables))
        (before-other (default-value 'pel-indentation-other-control-variables))
        (before-tab   (default-value 'pel-tab-width-control-variables)))
    (with-temp-buffer
      (setq-local pel-indentation-width-control-variables 'python-indent-offset)
      (setq-local pel-indentation-other-control-variables '(indent-tabs-mode))
      (setq-local pel-tab-width-control-variables '((tab-width . 0))))
    ;; After exiting temp buffer the global defaults must be unchanged.
    (should (equal (default-value 'pel-indentation-width-control-variables)
                   before-width))
    (should (equal (default-value 'pel-indentation-other-control-variables)
                   before-other))
    (should (equal (default-value 'pel-tab-width-control-variables)
                   before-tab))))

;;; --------------------------------------------------------------------------
(provide 'pel--indent-test)

;;; pel--indent-test.el ends here
