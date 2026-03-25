;;; pel-cc-find-test.el --- ERT tests for pel-cc-find.el  -*- lexical-binding: t; -*-

;; Created   : Wednesday, March 25 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-25 17:22:41 EDT, updated by Pierre Rouleau>

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

;; ERT tests for the pure/utility functions in pel-cc-find.el.
;; Tests focus on `pel-envar-in-string' and `pel-substitute-in-file-name'.

;;; --------------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'pel-cc-find)

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
  ;; A bare '$' at the end matches with an empty name string.
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

;;; --------------------------------------------------------------------------
(provide 'pel-cc-find-test)
;;; pel-cc-find-test.el ends here
