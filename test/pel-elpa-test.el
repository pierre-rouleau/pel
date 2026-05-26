;;; pel-elpa-test.el --- Test pel-elpa.el.  -*- lexical-binding: t; -*-

;; Created   : Thursday, September  2 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-02-26 17:14:47 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2026  Pierre Rouleau
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
;; Test the pel-elpa code.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel-elpa)
(require 'pel-filedir)                  ; use `pel-dpath-of'
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;


(ert-deftest test-pel-elpa-name ()
  "Test `pel-elpa-name'."

  (should (string= (pel-elpa-name "~/elpa-complete/" nil)        "~/elpa-complete/"))
  (should (string= (pel-elpa-name "~/elpa-complete" nil)         "~/elpa-complete"))
  (should (string= (pel-elpa-name "~/elpa-reduced/" nil)         "~/elpa-reduced/"))
  (should (string= (pel-elpa-name "~/elpa-reduced" nil)          "~/elpa-reduced"))
  (should (string= (pel-elpa-name "~/elpa-complete/file.el" nil) "~/elpa-complete/file.el"))
  (should (string= (pel-elpa-name "~/elpa-reduced/file.elc" nil) "~/elpa-reduced/file.elc"))

  (should (string= (pel-elpa-name "~/elpa-complete-graphics/" nil)        "~/elpa-complete/"))
  (should (string= (pel-elpa-name "~/elpa-complete-graphics" nil)         "~/elpa-complete"))
  (should (string= (pel-elpa-name "~/elpa-reduced-graphics/" nil)         "~/elpa-reduced/"))
  (should (string= (pel-elpa-name "~/elpa-reduced-graphics" nil)          "~/elpa-reduced"))
  (should (string= (pel-elpa-name "~/elpa-complete/file-graphics.el" nil) "~/elpa-complete/file.el"))
  (should (string= (pel-elpa-name "~/elpa-reduced/file-graphics.elc" nil) "~/elpa-reduced/file.elc"))

  (should (string= (pel-elpa-name "~/elpa-complete/" t)        "~/elpa-complete-graphics/"))
  (should (string= (pel-elpa-name "~/elpa-complete" t)         "~/elpa-complete-graphics"))
  (should (string= (pel-elpa-name "~/elpa-reduced/" t)         "~/elpa-reduced-graphics/"))
  (should (string= (pel-elpa-name "~/elpa-reduced" t)          "~/elpa-reduced-graphics"))
  (should (string= (pel-elpa-name "~/elpa-complete/file.el" t) "~/elpa-complete/file-graphics.el"))
  (should (string= (pel-elpa-name "~/elpa-reduced/file.elc" t) "~/elpa-reduced/file-graphics.elc"))

  (should (string= (pel-elpa-name "~/elpa-complete-graphics/" t)        "~/elpa-complete-graphics/"))
  (should (string= (pel-elpa-name "~/elpa-complete-graphics" t)         "~/elpa-complete-graphics"))
  (should (string= (pel-elpa-name "~/elpa-reduced-graphics/" t)         "~/elpa-reduced-graphics/"))
  (should (string= (pel-elpa-name "~/elpa-reduced-graphics" t)          "~/elpa-reduced-graphics"))
  (should (string= (pel-elpa-name "~/elpa-complete/file-graphics.el" t) "~/elpa-complete/file-graphics.el"))
  (should (string= (pel-elpa-name "~/elpa-reduced/file-graphics.elc" t) "~/elpa-reduced/file-graphics.elc")))



(ert-deftest test-pel-el-file-in ()
  "Test `pel-el-files-in'."
  (let ((test-dir (pel-dpath-of (if (boundp 'pel-home-dirpath-name)
                                    pel-home-dirpath-name
                                  (expand-file-name "."))
                                "test" "test-files" "el-files")))
    ;; The test directory has 3 empty files pre-defined, stored in the Git repo.
    (should (equal  (pel-el-files-in test-dir)
                    '("file-a.el" "file-b.el" "file-c.el")))))


;;; --------------------------------------------------------------------------

(ert-deftest test-pel-elpa-package-name-for ()
  "Test `pel-elpa-package-name-for' with various package directory name formats."

  ;; Standard MELPA-style timestamp version
  (should (string= (pel-elpa-package-name-for "ztree-20250209.1933")        "ztree"))
  (should (string= (pel-elpa-package-name-for "ace-link-20241101.1344")     "ace-link"))

  ;; Full path inputs
  (should (string= (pel-elpa-package-name-for "~/.emacs.d/elpa/ztree-20250209.1933")
                   "ztree"))
  (should (string= (pel-elpa-package-name-for "~/.emacs.d/elpa-1.2/ace-link-20241101.1344")
                   "ace-link"))

  ;; Pre-release / alphanumeric version suffixes (the broadened support)
  (should (string= (pel-elpa-package-name-for "cask-0.9.1pre")              "cask"))
  (should (string= (pel-elpa-package-name-for "some-pkg-1.0beta")           "some-pkg"))
  (should (string= (pel-elpa-package-name-for "my-package-2.3alpha")        "my-package"))

  ;; Simple Major.minor versions
  (should (string= (pel-elpa-package-name-for "pkg-12.23")                  "pkg"))
  (should (string= (pel-elpa-package-name-for "use-package-2.4.5")          "use-package"))

  ;; Package names with underscores and plus signs
  (should (string= (pel-elpa-package-name-for "cl++-1.0")                   "cl++"))

  ;; Should return nil when no version suffix is present
  (should (eq nil (pel-elpa-package-name-for "not-a-package")))
  (should (eq nil (pel-elpa-package-name-for "nodash")))
  ;; Version must start with a digit
  (should (eq nil (pel-elpa-package-name-for "pkg-abc"))))

;;; --------------------------------------------------------------------------
(provide 'pel-elpa-test)


