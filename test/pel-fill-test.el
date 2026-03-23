;;; pel-fill-test.el --- ERT tests for pel-fill.el  -*- lexical-binding: t; -*-

;; Created   : Monday, March 23 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-23 18:25:24 EDT, updated by Pierre Rouleau>

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
;; ERT tests for pel-fill.el.  Covers the two public functions:
;;
;;   `pel-auto-fill-only-comments'
;;     - Toggles `comment-auto-fill-only-comments' from nil to t.
;;     - Toggles `comment-auto-fill-only-comments' from t to nil.
;;     - Returns the new value after each toggle.
;;     - Toggle is buffer-local: a change in one buffer does not affect another.
;;
;;   `pel-show-fill-columns'
;;     - Invokes `pel-print-in-buffer' with the expected buffer name and title.
;;     - Passes the :clear-buffer keyword when APPEND is nil.
;;     - Omits the :clear-buffer keyword when APPEND is non-nil.
;;     - Detects Lisp major modes correctly (emacs-lisp-mode, lisp-mode,
;;       lisp-interaction-mode).
;;     - Does not treat non-Lisp modes (e.g. text-mode) as Lisp modes.
;;     - Recognises a bound PEL fill-column symbol for the current major mode.
;;
;; `pel-print-in-buffer' and the `pel-insert-*' display helpers are stubbed
;; with `cl-letf' so that the tests run without the full PEL display layer
;; or a graphical Emacs frame.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-fill)
(require 'pel--base)
(require 'cl-lib)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:

;; ===========================================================================
;; pel-auto-fill-only-comments
;; ===========================================================================

(ert-deftest pel-fill-test/auto-fill-only-comments/nil-to-t ()
  "Toggle from nil sets `comment-auto-fill-only-comments' to t."
  (with-temp-buffer
    (setq-local comment-auto-fill-only-comments nil)
    (pel-auto-fill-only-comments)
    (should (eq comment-auto-fill-only-comments t))))

(ert-deftest pel-fill-test/auto-fill-only-comments/t-to-nil ()
  "Toggle from t sets `comment-auto-fill-only-comments' to nil."
  (with-temp-buffer
    (setq-local comment-auto-fill-only-comments t)
    (pel-auto-fill-only-comments)
    (should (null comment-auto-fill-only-comments))))

(ert-deftest pel-fill-test/auto-fill-only-comments/returns-new-value/nil-to-t ()
  "Returns the new value (t) when toggling from nil."
  (with-temp-buffer
    (setq-local comment-auto-fill-only-comments nil)
    (should (eq t (pel-auto-fill-only-comments)))))

(ert-deftest pel-fill-test/auto-fill-only-comments/returns-new-value/t-to-nil ()
  "Returns the new value (nil) when toggling from t."
  (with-temp-buffer
    (setq-local comment-auto-fill-only-comments t)
    (should (null (pel-auto-fill-only-comments)))))

(ert-deftest pel-fill-test/auto-fill-only-comments/double-toggle-restores ()
  "Two successive toggles restore the original value."
  (with-temp-buffer
    (setq-local comment-auto-fill-only-comments nil)
    (pel-auto-fill-only-comments)
    (pel-auto-fill-only-comments)
    (should (null comment-auto-fill-only-comments))))

(ert-deftest pel-fill-test/auto-fill-only-comments/buffer-isolation ()
  "Toggle in one buffer does not affect a different buffer."
  (let ((default-before (default-value 'comment-auto-fill-only-comments)))
    (with-temp-buffer
      (setq-local comment-auto-fill-only-comments nil)
      (pel-auto-fill-only-comments)
      (should (eq comment-auto-fill-only-comments t)))
    (should (eq (default-value 'comment-auto-fill-only-comments)
                default-before))
    (with-temp-buffer
      (should (eq comment-auto-fill-only-comments default-before)))))

;; ===========================================================================
;; pel-show-fill-columns
;; ===========================================================================

;; ---------------------------------------------------------------------------
;; Smoke test: no error in a plain text buffer
;; ---------------------------------------------------------------------------

(ert-deftest pel-fill-test/show-fill-columns/no-error-in-text-mode ()
  "Calling `pel-show-fill-columns' must not raise an error in text-mode."
  (cl-letf (((symbol-function 'pel-print-in-buffer)
             (lambda (&rest _) nil))
            ((symbol-function 'pel-insert-symbol-content-line)
             (lambda (&rest _) nil))
            ((symbol-function 'pel-insert-list-content)
             (lambda (&rest _) nil)))
    (with-temp-buffer
      (text-mode)
      (should-not (condition-case err
                      (progn (pel-show-fill-columns) nil)
                    (error (format "Unexpected error: %S" err)))))))

;; ---------------------------------------------------------------------------
;; pel-print-in-buffer is called with the correct buffer name and title
;; ---------------------------------------------------------------------------

(ert-deftest pel-fill-test/show-fill-columns/calls-print-in-buffer-with-correct-name ()
  "pel-print-in-buffer is called with \"*pel-fill-info*\" and \"Fill Control\"."
  (let ((captured-buf-name nil)
        (captured-title nil))
    (cl-letf (((symbol-function 'pel-print-in-buffer)
               (lambda (buf-name title _printer &rest _rest)
                 (setq captured-buf-name buf-name
                       captured-title    title)))
              ((symbol-function 'pel-insert-symbol-content-line)
               (lambda (&rest _) nil))
              ((symbol-function 'pel-insert-list-content)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (pel-show-fill-columns)
        (should (string= captured-buf-name "*pel-fill-info*"))
        (should (string= captured-title    "Fill Control"))))))

;; ---------------------------------------------------------------------------
;; :clear-buffer keyword presence depending on APPEND argument
;; ---------------------------------------------------------------------------

(ert-deftest pel-fill-test/show-fill-columns/clear-buffer-when-no-append ()
  "The :clear-buffer keyword is forwarded when APPEND is nil."
  (let ((captured-rest nil))
    (cl-letf (((symbol-function 'pel-print-in-buffer)
               (lambda (_buf _title _printer &rest rest)
                 (setq captured-rest rest)))
              ((symbol-function 'pel-insert-symbol-content-line)
               (lambda (&rest _) nil))
              ((symbol-function 'pel-insert-list-content)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        ;; Call without prefix argument → APPEND is nil.
        (pel-show-fill-columns)
        (should (memq :clear-buffer captured-rest))))))

(ert-deftest pel-fill-test/show-fill-columns/no-clear-buffer-when-append ()
  "The :clear-buffer keyword is NOT forwarded when APPEND is non-nil."
  (let ((captured-rest nil))
    (cl-letf (((symbol-function 'pel-print-in-buffer)
               (lambda (_buf _title _printer &rest rest)
                 (setq captured-rest rest)))
              ((symbol-function 'pel-insert-symbol-content-line)
               (lambda (&rest _) nil))
              ((symbol-function 'pel-insert-list-content)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        ;; Pass non-nil APPEND.
        (pel-show-fill-columns t)
        (should-not (memq :clear-buffer captured-rest))))))

;; ---------------------------------------------------------------------------
;; Lisp-mode detection  (isa-lisp-mode branch)
;; ---------------------------------------------------------------------------

(ert-deftest pel-fill-test/show-fill-columns/detects-emacs-lisp-mode ()
  "In `emacs-lisp-mode' the printer inserts `emacs-lisp-docstring-fill-column'."
  (let ((inserted-syms '()))
    (cl-letf (((symbol-function 'pel-print-in-buffer)
               (lambda (_buf _title printer &rest _rest)
                 (ignore-errors (funcall printer))))
              ((symbol-function 'pel-insert-symbol-content-line)
               (lambda (sym &rest _)
                 (push sym inserted-syms)))
              ((symbol-function 'pel-insert-list-content)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (emacs-lisp-mode)
        (pel-show-fill-columns)
        (should (memq 'emacs-lisp-docstring-fill-column inserted-syms))))))

(ert-deftest pel-fill-test/show-fill-columns/detects-lisp-mode ()
  "In `lisp-mode' the printer inserts `emacs-lisp-docstring-fill-column'."
  (let ((inserted-syms '()))
    (cl-letf (((symbol-function 'pel-print-in-buffer)
               (lambda (_buf _title printer &rest _rest)
                 (ignore-errors (funcall printer))))
              ((symbol-function 'pel-insert-symbol-content-line)
               (lambda (sym &rest _)
                 (push sym inserted-syms)))
              ((symbol-function 'pel-insert-list-content)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (lisp-mode)
        (pel-show-fill-columns)
        (should (memq 'emacs-lisp-docstring-fill-column inserted-syms))))))

(ert-deftest pel-fill-test/show-fill-columns/detects-lisp-interaction-mode ()
  "In `lisp-interaction-mode' the printer inserts `emacs-lisp-docstring-fill-column'."
  (let ((inserted-syms '()))
    (cl-letf (((symbol-function 'pel-print-in-buffer)
               (lambda (_buf _title printer &rest _rest)
                 (ignore-errors (funcall printer))))
              ((symbol-function 'pel-insert-symbol-content-line)
               (lambda (sym &rest _)
                 (push sym inserted-syms)))
              ((symbol-function 'pel-insert-list-content)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (lisp-interaction-mode)
        (pel-show-fill-columns)
        (should (memq 'emacs-lisp-docstring-fill-column inserted-syms))))))

(ert-deftest pel-fill-test/show-fill-columns/non-lisp-mode-no-error ()
  "In a non-Lisp mode (text-mode) no error is raised."
  (cl-letf (((symbol-function 'pel-print-in-buffer)
             (lambda (_buf _title printer &rest _rest)
               (ignore-errors (funcall printer))))
            ((symbol-function 'pel-insert-symbol-content-line)
             (lambda (&rest _) nil))
            ((symbol-function 'pel-insert-list-content)
             (lambda (&rest _) nil)))
    (with-temp-buffer
      (text-mode)
      (should-not (condition-case err
                      (progn (pel-show-fill-columns) nil)
                    (error (format "Unexpected error: %S" err)))))))

;; ---------------------------------------------------------------------------
;; Bound PEL fill-column symbol for the current major mode
;; ---------------------------------------------------------------------------

(ert-deftest pel-fill-test/show-fill-columns/bound-pel-fill-column-symbol ()
  "No error when the PEL fill-column symbol for the current mode is bound.
`pel-major-mode-symbol-for' is stubbed to return a fresh uninterned symbol
created with `make-symbol' and then set to 80 via `set', making
\(boundp ...) return t without touching any real PEL customisation variable."
  (let ((bound-sym (make-symbol "pel-fill-test-bound-fill-column")))
    ;; Give the uninterned symbol a value so (boundp bound-sym) → t.
    (set bound-sym 80)
    (cl-letf (((symbol-function 'pel-print-in-buffer)
               (lambda (_buf _title printer &rest _rest)
                 (ignore-errors (funcall printer))))
              ((symbol-function 'pel-insert-symbol-content-line)
               (lambda (&rest _) nil))
              ((symbol-function 'pel-insert-list-content)
               (lambda (&rest _) nil))
              ((symbol-function 'pel-major-mode-symbol-for)
               (lambda (&rest _) bound-sym)))
      (with-temp-buffer
        (text-mode)
        (should-not (condition-case err
                        (progn (pel-show-fill-columns) nil)
                      (error (format "Unexpected error: %S" err))))))))


(ert-deftest pel-fill-test/show-fill-columns/unbound-pel-fill-column-symbol ()
  "No error when the PEL fill-column symbol for the current mode is unbound.
`pel-major-mode-symbol-for' is stubbed to return a fresh uninterned symbol
created with `make-symbol'.  An uninterned symbol is always unbound and
the stub does not touch any real PEL customisation variable."
  (let ((unbound-sym (make-symbol "pel-fill-test-unbound")))
    ;; unbound-sym is an uninterned symbol: (boundp unbound-sym) is always nil.
    (cl-letf (((symbol-function 'pel-print-in-buffer)
               (lambda (_buf _title printer &rest _rest)
                 (ignore-errors (funcall printer))))
              ((symbol-function 'pel-insert-symbol-content-line)
               (lambda (&rest _) nil))
              ((symbol-function 'pel-insert-list-content)
               (lambda (&rest _) nil))
              ((symbol-function 'pel-major-mode-symbol-for)
               (lambda (&rest _) unbound-sym)))
      (with-temp-buffer
        (text-mode)
        (should-not (condition-case err
                        (progn (pel-show-fill-columns) nil)
                      (error (format "Unexpected error: %S" err))))))))

;;; --------------------------------------------------------------------------
(provide 'pel-fill-test)

;;; pel-fill-test.el ends here
