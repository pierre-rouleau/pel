;;; pel-fast-startup-init-test.el --- Test generation of pel-fast-startup-init  -*- lexical-binding: t; -*-

;; Created   : Sunday, May 17 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-05-17 21:50:39 EDT, updated by Pierre Rouleau>

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
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-setup)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel--test-byte-compile-file-clean (src)
  "Byte-compile SRC and fail the test on any error or warning."
  (let* ((byte-compile-warnings t)
         (byte-compile-error-on-warn t)        ; Emacs ≥ 29 honors this
         (log (get-buffer-create "*Compile-Log*"))
         (start (with-current-buffer log (point-max))))
    (should (byte-compile-file src))
    ;; For Emacs < 29 (no error-on-warn), detect warnings in the new log tail.
    (with-current-buffer log
      (goto-char start)
      (should-not (re-search-forward "\\<[Ww]arning\\>:" nil t)))))

(ert-deftest ert-test-pel-fast-startup-init-compiles-clean ()
  "Generate pel-fast-startup-init.el text and ensure byte-compilation is clean."
  (let* ((deps '((dash (2 19 1)) (s (1 13 0))))
         ;; Ensure \"elpa-reduced\" is present so %s substitution is active.
         (bundle-dp (expand-file-name
                     "X/elpa-reduced/pel-bundle-TEST"
                     temporary-file-directory))
         (extra (pel--setup-fast-startup-init-extra-code bundle-dp))
         (text  (pel-setup-fast-startup-init-text deps extra))
         (src   (make-temp-file "pel-fast-init" nil ".el"))
         (elc   (concat (file-name-sans-extension src) ".elc")))
    (unwind-protect
        (progn
          (with-temp-file src (insert text))
          ;; Sanity: the quickstart guard should be present in the generated code.
          (should (string-match-p "(when using-package-quickstart" text))
          ;; Also ensure the once-only helper is referenced.
          (should (string-match-p "(pel--add-to-load-path-once" text))
          ;; Compile and enforce no warnings.
          (pel--test-byte-compile-file-clean src))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file elc)))))

;;; --------------------------------------------------------------------------
(provide 'pel-fast-startup-init-test)

;;; pel-fast-startup-init-test.el ends here
