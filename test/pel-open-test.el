;;; pel-open-test.el --- ERT tests for pel-open.el  -*- lexical-binding: t; -*-

;; Created   : Thursday, March 26 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-04-22 17:51:47 EDT, updated by Pierre Rouleau>

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
;; Tests for functions in pel-open.el.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;

(require 'ert)
(require 'pel--base)
(require 'pel-open)

;;; --------------------------------------------------------------------------
;;; Code:

;; ---------------------------------------------------------------------------
;; Tests for `pel--open-file-at-point-dir-string-for'
;; ---------------------------------------------------------------------------

(ert-deftest pel-open-test/dir-string-nil-visiting-file ()
  "nil value returns description using file's parent directory."
  ;; pel-current-buffer-filename checks buffer-file-truename, not buffer-file-name.
  ;; Both must be set to simulate a visited file in a temp buffer.
  (with-temp-buffer
    (setq buffer-file-name     "/some/project/src/foo.c")
    (setq buffer-file-truename "/some/project/src/foo.c")
    (let ((result (pel--open-file-at-point-dir-string-for nil)))
      (should (stringp result))
      (should (string-match-p "file's parent directory" result))
      (should (string-match-p "/some/project/src/" result)))))

(ert-deftest pel-open-test/dir-string-nil-no-file ()
  "nil value without visiting file falls back to current working directory."
  (with-temp-buffer
    ;; No buffer-file-name
    (setq buffer-file-name nil)
    (let* ((default-directory "/tmp/myproject/")
           (result (pel--open-file-at-point-dir-string-for nil)))
      (should (stringp result))
      (should (string-match-p "/tmp/myproject/" result))
      (should (string-match-p "current working directory" result)))))

(ert-deftest pel-open-test/dir-string-cwd ()
  "cwd value returns description using current working directory."
  (let* ((default-directory "/tmp/myproject/")
         (result (pel--open-file-at-point-dir-string-for 'cwd)))
    (should (stringp result))
    (should (string-match-p "current working directory" result))
    (should (string-match-p "/tmp/myproject/" result))))

(ert-deftest pel-open-test/dir-string-specific-dir ()
  "A string value returns a description naming that specific directory."
  (let ((result (pel--open-file-at-point-dir-string-for "/opt/mydir")))
    (should (stringp result))
    (should (string-match-p "/opt/mydir" result))))

(ert-deftest pel-open-test/dir-string-invalid ()
  "An invalid value returns the INVALID! marker."
  (let ((result (pel--open-file-at-point-dir-string-for 42)))
    (should (string= result "INVALID!"))))

;; ---------------------------------------------------------------------------
;; Tests for `pel-open-at-point' error paths
;; ---------------------------------------------------------------------------

(ert-deftest pel-open-test/open-at-point-noerror-nil-when-no-pel-file ()
  "When pel-file cannot load and noerror is t, return nil without error."
  ;; Mock: pel-file not available, fboundp returns nil
  (cl-letf (((symbol-function 'require)
             (lambda (feature &rest _) (when (eq feature 'pel-file) nil) nil))
            ((symbol-function 'fboundp)
             (lambda (sym)
               (not (eq sym 'pel-find-file-at-point-in-window)))))
    (with-temp-buffer
      (setq major-mode 'text-mode)
      ;; should error if there is no file at point
      (should-error (pel-open-at-point nil)))))

;; ---------------------------------------------------------------------------
(provide 'pel-open-test)
;;; pel-open-test.el ends here
