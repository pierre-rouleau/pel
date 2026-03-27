;;; pel-cc-find-test.el --- ERT tests for pel-cc-find.el  -*- lexical-binding: t; -*-

;; Created   : Wednesday, March 25 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-26 22:37:18 EDT, updated by Pierre Rouleau>

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


;; Tests for pel-cc-find.el functions covering:
;;  - `pel-envar-in-string'
;;  - `pel-substitute-in-file-name'
;;  - `pel-cc-find-activate-finder-method' (method dispatch)

;;; --------------------------------------------------------------------------
;;; Code:

(require 'pel-cc-find)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Tests for `pel-envar-in-string'
;;; --------------------------------------------------------------------------

(ert-deftest pel-cc-find-test/envar-in-string/no-vars ()
  "Return nil when string contains no environment variable references."
  (should (equal nil (pel-envar-in-string "no/var/here"))))

(ert-deftest pel-cc-find-test/envar-in-string/single-var ()
  "Return list with one var name when string has a single $VAR."
  (should (equal '("HOME") (pel-envar-in-string "$HOME/projects"))))

(ert-deftest pel-cc-find-test/envar-in-string/multiple-vars ()
  "Return list of var names in order when string has multiple $VARs."
  (should (equal '("HOME" "USER")
                 (pel-envar-in-string "$HOME/$USER/project"))))

(ert-deftest pel-cc-find-test/envar-in-string/var-with-underscore ()
  "Recognize env variable names containing underscores."
  (should (equal '("MY_TOOL_DIR")
                 (pel-envar-in-string "/path/$MY_TOOL_DIR/include"))))

(ert-deftest pel-cc-find-test/envar-in-string/adjacent-vars ()
  "Return both vars when two $VARs appear adjacent with separator."
  (should (equal '("FOO" "BAR")
                 (pel-envar-in-string "$FOO/$BAR"))))

(ert-deftest pel-cc-find-test/envar-in-string/empty-string ()
  "Return nil for an empty string."
  (should (equal nil (pel-envar-in-string ""))))

(ert-deftest pel-cc-find-test/envar-in-string/dollar-at-end ()
  "Handle a trailing dollar sign with no name after it gracefully."
  ;; A bare '$' with no following alphanumeric/underscore chars produces
  ;; an empty name, which is filtered out; result is nil.
  (let ((result (pel-envar-in-string "path/$")))
    (should (equal result nil))))

;;; --------------------------------------------------------------------------
;;; Tests for `pel-substitute-in-file-name'
;;; --------------------------------------------------------------------------

(ert-deftest pel-cc-find-test/substitute-in-file-name/no-vars ()
  "String with no env vars is returned unchanged."
  (should (string= "/usr/local/include"
                   (pel-substitute-in-file-name "/usr/local/include"))))

(ert-deftest pel-cc-find-test/substitute-in-file-name/known-var ()
  "Substitute a known environment variable in the filename."
  (let ((home (getenv "HOME")))
    (when home  ; skip if HOME not set in sandbox
      (should (string= (concat home "/projects")
                       (pel-substitute-in-file-name "$HOME/projects"))))))

(ert-deftest pel-cc-find-test/substitute-in-file-name/unknown-var ()
  "Signal a user-error when an unknown env variable is referenced."
  (should-error
   (pel-substitute-in-file-name "$DEFINITELY_NOT_SET_VAR_XYZ/path")
   :type 'user-error))

(ert-deftest pel-cc-find-test/substitute-in-file-name/mixed-known-unknown ()
  "Signal user-error even if one var is known and another is not."
  (should-error
   (pel-substitute-in-file-name "$HOME/$DEFINITELY_NOT_SET_VAR_XYZ")
   :type 'user-error))

;; ---------------------------------------------------------------------------
;; Tests for `pel-cc-find-activate-finder-method' — method dispatch
;; ---------------------------------------------------------------------------

(ert-deftest pel-cc-find-test/activate-generic ()
  "The 'generic method sets pel-filename-at-point-finders to pel-generic-find-file."
  (with-temp-buffer
    (setq major-mode 'c-mode)
    (pel-cc-find-activate-finder-method 'generic nil)
    (should (equal pel-filename-at-point-finders '(pel-generic-find-file)))))

(ert-deftest pel-cc-find-test/activate-pel-ini-file ()
  "The 'pel-ini-file method sets finders to pel-cc-find-via-pel-ini."
  (with-temp-buffer
    (setq major-mode 'c-mode)
    (pel-cc-find-activate-finder-method 'pel-ini-file nil)
    (should (= (length pel-filename-at-point-finders) 1))
    (should (functionp (car pel-filename-at-point-finders)))))

(ert-deftest pel-cc-find-test/activate-envvar-string ()
  "A string method sets a lambda that calls pel-ffind-inpath-include."
  (with-temp-buffer
    (setq major-mode 'c-mode)
    (pel-cc-find-activate-finder-method "INCLUDE" nil)
    (should (= (length pel-filename-at-point-finders) 1))
    (should (functionp (car pel-filename-at-point-finders)))))

(ert-deftest pel-cc-find-test/activate-list-two-elements ()
  "A two-element list method sets a lambda using pel-ffind-inpath."
  (with-temp-buffer
    (setq major-mode 'c-mode)
    (pel-cc-find-activate-finder-method
     '(("/proj/include") ("/usr/include"))
     nil)
    (should (= (length pel-filename-at-point-finders) 1))
    (should (functionp (car pel-filename-at-point-finders)))))

(ert-deftest pel-cc-find-test/activate-invalid-method-signals-error ()
  "An invalid method signals an error."
  (with-temp-buffer
    (setq major-mode 'c-mode)
    (should-error
     (pel-cc-find-activate-finder-method 'unsupported-method nil)
     :type 'user-error)))

(ert-deftest pel-cc-find-test/activate-with-extra-dirs-appends-finder ()
  "Extra searched directory trees appends a second finder."
  (with-temp-buffer
    (setq major-mode 'c-mode)
    (pel-cc-find-activate-finder-method 'generic '("/extra/dir"))
    ;; Should now have 2 finders: generic + extra dirs
    (should (= (length pel-filename-at-point-finders) 2))))

(ert-deftest pel-cc-find-test/activate-generic-no-extra-single-finder ()
  "Generic method with no extra dirs yields exactly one finder."
  (with-temp-buffer
    (setq major-mode 'c-mode)
    (pel-cc-find-activate-finder-method 'generic nil)
    (should (= (length pel-filename-at-point-finders) 1))))

(ert-deftest pel-cc-find-test/activate-default-from-awk-mode-option ()
  "When method is nil, awk-mode should use `pel-awk-file-finder-method'."
  (let ((pel-awk-file-finder-method 'generic))
    (with-temp-buffer
      (setq major-mode 'awk-mode)
      (pel-cc-find-activate-finder-method nil nil)
      (should (equal pel-filename-at-point-finders '(pel-generic-find-file))))))

(ert-deftest pel-cc-find-test/activate-default-from-c++-mode-option ()
  "When method is nil, c++-mode should use `pel-c++-file-finder-method'."
  (let ((pel-c++-file-finder-method 'generic))
    (with-temp-buffer
      (setq major-mode 'c++-mode)
      (pel-cc-find-activate-finder-method nil nil)
      (should (equal pel-filename-at-point-finders '(pel-generic-find-file))))))

;; ---------------------------------------------------------------------------
;; Tests for `pel--cc-find-info-msg'
;; ---------------------------------------------------------------------------

(ert-deftest pel-cc-find-test/info-msg-returns-string ()
  "pel--cc-find-info-msg returns a non-empty string for a bound varname-suffix."
  (with-temp-buffer
    (setq major-mode 'c-mode)
    ;; Ensure the user-option variable exists (it's defined in pel--options.el)
    (let ((result (pel--cc-find-info-msg "file-finder-method")))
      (should (stringp result))
      (should (> (length result) 0))
      ;; Should contain both "User option" and "buffer local" labels
      (should (string-match-p "User option" result))
      (should (string-match-p "buffer local" result)))))

;;; --------------------------------------------------------------------------
(provide 'pel-cc-find-test)
;;; pel-cc-find-test.el ends here
