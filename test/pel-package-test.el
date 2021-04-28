;;; pel-package-test.el --- Test package and options management.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, March 24 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-04-28 14:49:54, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021  Pierre Rouleau
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
;;  Partial test of the pel-package and the declarations of pel--options.
;;  TODO: test all the low level code.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)
(require 'pel--options)
(require 'pel-package)
(require 'pel-ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; Test low level operations
;; =========================

;; Test detection of invalid :also-required-when values
;; ----------------------------------------------------
;;
;; The values of the :also-required-when property should be Lisp expressions
;; that will be evaluated later. Something like a quoted symbol or a quoted
;; form.  They should not accept non-quoted symbols or form that evaluate to a
;; value.  Also, the property must be stored in a `pel-use-' defcustom symbol.
;; Therefore, for testing it we declare one just for testing purposes.

(defgroup pel-internal-ert-testing nil
  "Used for internal testing only."
  :group 'pel)

(defcustom pel-use--package-test nil
  "Used for internal testing only."
  :group 'pel-internal-ert-testing
  :type 'boolean)

(defvar pel--test-value "expected value"
  "Used for internal testing only.")

(ert-deftest ert-test-package-also-required-p ()
  "Test that we cannot use a value for the :also-required property."
  (put 'pel-use--package-test :also-required-when 'pel--test-value)
  (should (string= pel--test-value "expected value"))
  (should (eq
           (get 'pel-use--package-test :also-required-when)
           'pel--test-value))
  (should (string= (pel-package-also-required-p 'pel-use--package-test)
                   "expected value"))
  ;;
  ;; Put an invalid property: put a value and check that the code detects it
  ;; via an error
  (put 'pel-use--package-test :also-required-when 55)
  (should-error (pel-package-also-required-p 'pel-use--package-test))
  ;;
  (put 'pel-use--package-test :also-required-when pel--test-value)
  (should-error (pel-package-also-required-p 'pel-use--package-test)))

;; ---------------------------------------------------------------------------
;; Test top level operations
;; -------------------------
;;
;; The following test must provide all the let-bound variables invoked in the
;; verification of the logic.

(ert-deftest ert-test-package-goflymake ()
  "Test pel-use-goflymake."
  (should (equal
           (let ((pel-use-go nil)
                 (pel-use-goflymake 'with-flymake))
             (pel-packages-for 'pel-use-goflymake))
           nil))
  (should (equal
           (let ((pel-use-go t)
                 (pel-use-goflymake 'with-flymake))
             (pel-packages-for 'pel-use-goflymake))
           '((utils . go-flymake))))
  (should (equal
           (let ((pel-use-go t)
                 (pel-use-goflymake 'with-flycheck))
             (pel-packages-for 'pel-use-goflymake))
           '((elpa . flycheck)
             (utils . go-flycheck))))
  (should (equal
           (let ((pel-use-go t)
                 (pel-use-goflymake nil))
             (pel-packages-for 'pel-use-goflymake))
           nil)))

(ert-deftest ert-test-package-lice ()
  "Test pel-use-lice."
  ;; lice is installed as as soon as pel-use-lice is on,
  ;; but also as soon as one of the skeletons are used.
  ;; The extra dependencies are identified via the
  ;; :also-required-when property
  (should (equal
           (let ((pel-use-lice nil)
                 (pel-c-skel-with-license nil)
                 (pel-clisp-skel-with-license nil)
                 (pel-elisp-skel-with-license nil)
                 (pel-erlang-skel-with-license nil))
             (pel-packages-for 'pel-use-lice))
           nil))
  (should (equal
           (let ((pel-use-lice nil)
                 (pel-c-skel-with-license t)
                 (pel-clisp-skel-with-license nil)
                 (pel-elisp-skel-with-license nil)
                 (pel-erlang-skel-with-license nil))
             (pel-packages-for 'pel-use-lice))
           '((elpa . lice))))
  (should (equal
           (let ((pel-use-lice nil)
                 (pel-c-skel-with-license nil)
                 (pel-clisp-skel-with-license t)
                 (pel-elisp-skel-with-license nil)
                 (pel-erlang-skel-with-license nil))
             (pel-packages-for 'pel-use-lice))
           '((elpa . lice))))
  (should (equal
           (let ((pel-use-lice nil)
                 (pel-c-skel-with-license nil)
                 (pel-clisp-skel-with-license nil)
                 (pel-elisp-skel-with-license t)
                 (pel-erlang-skel-with-license nil))
             (pel-packages-for 'pel-use-lice))
           '((elpa . lice))))
  (should (equal
           (let ((pel-use-lice nil)
                 (pel-c-skel-with-license nil)
                 (pel-clisp-skel-with-license nil)
                 (pel-elisp-skel-with-license nil)
                 (pel-erlang-skel-with-license t))
             (pel-packages-for 'pel-use-lice))
           '((elpa . lice))))
  (should (equal
           (let ((pel-use-lice t)
                 (pel-c-skel-with-license nil)
                 (pel-clisp-skel-with-license nil)
                 (pel-elisp-skel-with-license nil)
                 (pel-erlang-skel-with-license nil))
             (pel-packages-for 'pel-use-lice))
           '((elpa . lice)))))


(ert-deftest ert-test-package-markdown ()
  "Test pel-use-markdown controlled packages."
  ;; Test when nothing is requested.
  ;; Markdown is used by cargo
  (let ((pel-use-markdown nil)
        (pel-use-markdown-mode nil)
        (pel-use-cargo nil))
    (should (equal
             (pel-packages-for 'pel-use-markdown)
             nil))
    (should (equal
             (pel-packages-for 'pel-use-markdown-mode)
             nil)))

  (let ((pel-use-markdown nil)
        (pel-use-markdown-mode nil)
        (pel-use-cargo t))
    (should (equal
             (pel-packages-for 'pel-use-markdown)
             nil))
    (should (equal
             (pel-packages-for 'pel-use-markdown-mode)
             '((elpa . markdown-mode))))))

;;; --------------------------------------------------------------------------
(provide 'pel-package-test)

;;; pel-package-test.el ends here
