;;; pel-timestamp-test.el --- Test the pel-timestamp code.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, February 25 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-02-27 09:20:22 EST, updated by Pierre Rouleau>

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
  "Test `pel-file-in'."
  (let ((skip-copyright-in '("/a/" "/b/b.c" "/b/b1.c")))
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (fp) (pel-mocked-file-directory-p fp))))
      ;; Directory
      ;; anything in the directory /a should be skipped
      (should (string=  (pel-file-in "/a/" skip-copyright-in) "/a/"))
      (should (string=  (pel-file-in "/a"  skip-copyright-in) "/a/"))
      (should (string=  (pel-file-in "/a/f.c" skip-copyright-in) "/a/"))
      (should (string=  (pel-file-in "/a/b/f.c" skip-copyright-in) "/a/"))

      ;; everything else should not be skipped
      (should-not (pel-file-in "/ab" skip-copyright-in))
      (should-not (pel-file-in "/ab/f.c" skip-copyright-in))
      (should-not (pel-file-in "/b/f.c" skip-copyright-in))
      (should-not (pel-file-in "~/my/project/file.el" skip-copyright-in))
      ;;
      ;; File
      (should (pel-file-in "/a/b.c" skip-copyright-in)) ; file inside /a
      (should-not (pel-file-in "/abc" skip-copyright-in))
      (should (pel-file-in "/b/b.c" skip-copyright-in))
      (should (string=  (pel-file-in "/b/b.c" skip-copyright-in) "/b/b.c"))
      (should (pel-file-in "/b/b1.c" skip-copyright-in))
      (should-not (pel-file-in "/b/b1.c.txt" skip-copyright-in)))))

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;; --------------------------------------------------------------------------
(provide 'pel-timestamp-test)

;;; pel-timestamp-test.el ends here
