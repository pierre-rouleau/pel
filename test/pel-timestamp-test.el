;;; pel-timestamp-test.el --- Test the pel-timestamp code.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, February 25 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-02-25 16:53:43 EST, updated by Pierre Rouleau>

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
;; A unit test with mocking that does not depend on anything external; it use
;; `cl-letf' to rebind the function to mock.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)    ; use `pel-string-starts-with-p'
(require 'pel-timestamp)
;; (require 'pel-ert)
(require 'cl-lib)       ; use `cl-letf' used for mocking


(defun pel-mocked-file-directory-p  (path)
  "Mocked `file-directory-p' for PATH."
  (cond
   ((string= path "/a/") t)
   ((string= path "/b/") t)
   ((pel-string-starts-with-p path "/a/") nil)
   ((pel-string-starts-with-p path "/b/") nil)
   (t (error "Invalid %s" path))))

(ert-deftest pel-timestamp-test-skip-copyright-for-1 ()
  "Test `pel--skip-copyright-for'."
  (let ((pel-skip-copyright-in '("/a/" "/b/b.c" "/b/b1.c")))
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (fp) (pel-mocked-file-directory-p fp))))
      ;; Directory
      ;; anything in the directory /a should be skipped
      (should (string=  (pel--skip-copyright-for "/a/") "/a/"))
      (should (string=  (pel--skip-copyright-for "/a") "/a/"))
      (should (string=  (pel--skip-copyright-for "/a/f.c") "/a/"))
      (should (string=  (pel--skip-copyright-for "/a/b/f.c") "/a/"))

      ;; everything else should not be skipped
      (should-not (pel--skip-copyright-for "/ab"))
      (should-not (pel--skip-copyright-for "/ab/f.c"))
      (should-not (pel--skip-copyright-for "/b/f.c"))
      (should-not (pel--skip-copyright-for "~/my/project/file.el"))
      ;;
      ;; File
      (should (pel--skip-copyright-for "/a/b.c")) ; file inside /a
      (should-not (pel--skip-copyright-for "/abc"))
      (should (pel--skip-copyright-for "/b/b.c"))
      (should (string=  (pel--skip-copyright-for "/b/b.c") "/b/b.c"))
      (should (pel--skip-copyright-for "/b/b1.c"))
      (should-not (pel--skip-copyright-for "/b/b1.c.txt")))))

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;; --------------------------------------------------------------------------
(provide 'pel-timestamp-test)

;;; pel-timestamp-test.el ends here
