;;; pel-ffind-inpath-test.el --- ERT tests for pel-ffind-inpath  -*- lexical-binding: t; -*-

;; Created   : Saturday, March 28 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-28 15:58:46 EDT, updated by Pierre Rouleau>

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
;; ERT tests for the two public functions in pel-ffind-inpath.el:
;;
;;   - `pel-ffind-inpath'        : find files in an explicit path list
;;   - `pel-ffind-inpath-include': find files via a PATH-like env variable
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-ffind-inpath)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Test helpers
;;; --------------------------------------------------------------------------

(defmacro pel-ffind-inpath-test--with-temp-dir (dir-sym &rest body)
  "Evaluate BODY with DIR-SYM bound to a fresh temporary directory.
The directory (and all its contents) is removed on exit."
  (declare (indent 1))
  `(let ((,dir-sym (make-temp-file "pel-inpath-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,dir-sym t))))

(defun pel-ffind-inpath-test--touch (path)
  "Create an empty file at PATH, creating parent directories as needed."
  (make-directory (file-name-directory (expand-file-name path)) t)
  (with-temp-file (expand-file-name path) (insert "")))

(defmacro pel-ffind-inpath-test--with-env (var value &rest body)
  "Evaluate BODY with environment variable VAR set to VALUE (may be nil to unset).
The original value is restored on exit."
  (declare (indent 2))
  `(let ((--orig-- (getenv ,var)))
     (unwind-protect
         (progn
           (setenv ,var ,value)
           ,@body)
       (setenv ,var --orig--))))

;;; --------------------------------------------------------------------------
;;; Tests for `pel-ffind-inpath'
;;; --------------------------------------------------------------------------

(ert-deftest pel-ffind-inpath/nil-paths-returns-nil ()
  "Passing nil as PATHS returns nil without signalling an error."
  (should (null (pel-ffind-inpath "anything.h" nil))))

(ert-deftest pel-ffind-inpath/file-absent-returns-nil ()
  "Returns nil when the requested file is not in any listed directory."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (should (null (pel-ffind-inpath "nonexistent.h" (list tmpdir))))))

(ert-deftest pel-ffind-inpath/single-string-path-file-found ()
  "Accepts a bare directory string (not wrapped in a list) and finds the file."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (let ((f (expand-file-name "stdio.h" tmpdir)))
      (pel-ffind-inpath-test--touch f)
      (should (equal (pel-ffind-inpath "stdio.h" tmpdir)
                     (list f))))))

(ert-deftest pel-ffind-inpath/single-element-list-path-file-found ()
  "Finds a file when PATHS is a single-element list."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (let ((f (expand-file-name "stdio.h" tmpdir)))
      (pel-ffind-inpath-test--touch f)
      (should (equal (pel-ffind-inpath "stdio.h" (list tmpdir))
                     (list f))))))

(ert-deftest pel-ffind-inpath/multi-dirs-file-only-in-first ()
  "Returns the path from the first directory when file exists there only."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (let* ((d1 (expand-file-name "d1" tmpdir))
           (d2 (expand-file-name "d2" tmpdir))
           (f1 (expand-file-name "foo.h" d1)))
      (make-directory d1 t)
      (make-directory d2 t)
      (pel-ffind-inpath-test--touch f1)
      (should (equal (pel-ffind-inpath "foo.h" (list d1 d2))
                     (list f1))))))

(ert-deftest pel-ffind-inpath/multi-dirs-file-only-in-second ()
  "Returns the path from the second directory when file exists there only."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (let* ((d1 (expand-file-name "d1" tmpdir))
           (d2 (expand-file-name "d2" tmpdir))
           (f2 (expand-file-name "foo.h" d2)))
      (make-directory d1 t)
      (make-directory d2 t)
      (pel-ffind-inpath-test--touch f2)
      (should (equal (pel-ffind-inpath "foo.h" (list d1 d2))
                     (list f2))))))

(ert-deftest pel-ffind-inpath/multi-dirs-file-in-all-returns-all-in-order ()
  "All copies are returned; the order follows the order of PATHS."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (let* ((d1 (expand-file-name "d1" tmpdir))
           (d2 (expand-file-name "d2" tmpdir))
           (d3 (expand-file-name "d3" tmpdir))
           (f1 (expand-file-name "common.h" d1))
           (f2 (expand-file-name "common.h" d2))
           (f3 (expand-file-name "common.h" d3)))
      (dolist (f (list f1 f2 f3))
        (pel-ffind-inpath-test--touch f))
      (should (equal (pel-ffind-inpath "common.h" (list d1 d2 d3))
                     (list f1 f2 f3))))))

(ert-deftest pel-ffind-inpath/result-order-follows-paths-not-dirname-alpha ()
  "Result order mirrors PATHS order, not alphabetical directory order."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (let* ((da (expand-file-name "aaa" tmpdir))
           (dz (expand-file-name "zzz" tmpdir))
           (fa (expand-file-name "x.h" da))
           (fz (expand-file-name "x.h" dz)))
      (pel-ffind-inpath-test--touch fa)
      (pel-ffind-inpath-test--touch fz)
      ;; Pass zzz first — result must start with zzz's copy.
      (should (equal (pel-ffind-inpath "x.h" (list dz da))
                     (list fz fa))))))

(ert-deftest pel-ffind-inpath/no-recursive-descent ()
  "Does NOT descend into subdirectories — only the exact listed dirs are checked."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (let* ((sub (expand-file-name "sub" tmpdir))
           (f   (expand-file-name "deep.h" sub)))
      (pel-ffind-inpath-test--touch f)
      ;; tmpdir itself does not contain deep.h directly.
      (should (null (pel-ffind-inpath "deep.h" (list tmpdir)))))))

(ert-deftest pel-ffind-inpath/relative-path-prefix-in-filename ()
  "FILENAME may include a relative sub-directory prefix component."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (let ((f (expand-file-name "sys/types.h" tmpdir)))
      (pel-ffind-inpath-test--touch f)
      (should (equal (pel-ffind-inpath "sys/types.h" (list tmpdir))
                     (list (expand-file-name "sys/types.h" tmpdir)))))))

(ert-deftest pel-ffind-inpath/duplicate-dir-in-paths-finds-file-twice ()
  "When the same directory appears N times in PATHS the file is returned N times.
This test documents current behaviour.  If deduplication is added, update it."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (let ((f (expand-file-name "dup.h" tmpdir)))
      (pel-ffind-inpath-test--touch f)
      (should (equal (pel-ffind-inpath "dup.h" (list tmpdir tmpdir))
                     (list f f))))))

;;; --------------------------------------------------------------------------
;;; Tests for `pel-ffind-inpath-include'
;;; --------------------------------------------------------------------------

(ert-deftest pel-ffind-inpath-include/unset-default-var-signals-user-error ()
  "Signals `user-error' when the INCLUDE environment variable is not set."
  (pel-ffind-inpath-test--with-env "INCLUDE" nil
    (should-error (pel-ffind-inpath-include "any.h") :type 'user-error)))

(ert-deftest pel-ffind-inpath-include/unset-custom-var-signals-user-error ()
  "Signals `user-error' when a custom environment variable is not set."
  (pel-ffind-inpath-test--with-env "MY_CUSTOM_INC_XYZ" nil
    (should-error (pel-ffind-inpath-include "any.h" "MY_CUSTOM_INC_XYZ")
                  :type 'user-error)))

(ert-deftest pel-ffind-inpath-include/error-message-contains-var-name ()
  "The user-error message includes the name of the offending variable."
  (pel-ffind-inpath-test--with-env "NONEXISTENT_INC_ABC" nil
    (let ((err (should-error
                (pel-ffind-inpath-include "x.h" "NONEXISTENT_INC_ABC")
                :type 'user-error)))
      (should (string-match-p "NONEXISTENT_INC_ABC"
                              (error-message-string err))))))

(ert-deftest pel-ffind-inpath-include/empty-env-var-signals-user-error ()
  "Signals `user-error' when the environment variable exists but is empty."
  (pel-ffind-inpath-test--with-env "INCLUDE" ""
    (should-error (pel-ffind-inpath-include "any.h") :type 'user-error)))

(ert-deftest pel-ffind-inpath-include/default-var-single-dir-file-found ()
  "Finds a file when INCLUDE env var points to the directory containing it."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (let ((f (expand-file-name "myheader.h" tmpdir)))
      (pel-ffind-inpath-test--touch f)
      (pel-ffind-inpath-test--with-env "INCLUDE" tmpdir
        (should (equal (pel-ffind-inpath-include "myheader.h")
                       (list f)))))))

(ert-deftest pel-ffind-inpath-include/default-var-file-not-present-returns-nil ()
  "Returns nil when INCLUDE is set but the file does not exist in that dir."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (pel-ffind-inpath-test--with-env "INCLUDE" tmpdir
      (should (null (pel-ffind-inpath-include "nosuchfile.h"))))))

(ert-deftest pel-ffind-inpath-include/custom-var-file-found ()
  "The INCLUDE-ENV-VAR optional argument selects the env variable to use."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (let ((f (expand-file-name "proto.h" tmpdir)))
      (pel-ffind-inpath-test--touch f)
      (pel-ffind-inpath-test--with-env "MY_INCLUDES" tmpdir
        (should (equal (pel-ffind-inpath-include "proto.h" "MY_INCLUDES")
                       (list f)))))))

(ert-deftest pel-ffind-inpath-include/multi-path-env-var-finds-all ()
  "When INCLUDE lists several directories, all matching files are returned in order."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (let* ((d1  (expand-file-name "inc1" tmpdir))
           (d2  (expand-file-name "inc2" tmpdir))
           (f1  (expand-file-name "a.h" d1))
           (f2  (expand-file-name "a.h" d2))
           (env (mapconcat #'identity (list d1 d2) path-separator)))
      (pel-ffind-inpath-test--touch f1)
      (pel-ffind-inpath-test--touch f2)
      (pel-ffind-inpath-test--with-env "INCLUDE" env
        (should (equal (pel-ffind-inpath-include "a.h")
                       (list f1 f2)))))))

(ert-deftest pel-ffind-inpath-include/multi-path-env-var-file-only-in-second ()
  "With multiple dirs in INCLUDE, returns only the path(s) where file exists."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (let* ((d1  (expand-file-name "inc1" tmpdir))
           (d2  (expand-file-name "inc2" tmpdir))
           (f2  (expand-file-name "b.h" d2))
           (env (mapconcat #'identity (list d1 d2) path-separator)))
      (make-directory d1 t)
      (pel-ffind-inpath-test--touch f2)
      (pel-ffind-inpath-test--with-env "INCLUDE" env
        (should (equal (pel-ffind-inpath-include "b.h")
                       (list f2)))))))

(ert-deftest pel-ffind-inpath-include/trailing-separator-does-not-add-cwd ()
  "A trailing path-separator must not silently search `default-directory'."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (let* ((d1       (expand-file-name "real" tmpdir))
           (f-in-d1  (expand-file-name "t.h" d1))
           ;; Put a copy directly in tmpdir to detect the bug:
           (f-in-cwd (expand-file-name "t.h" tmpdir))
           (env      (concat d1 path-separator))) ; trailing separator
      (pel-ffind-inpath-test--touch f-in-d1)
      (pel-ffind-inpath-test--touch f-in-cwd)
      (let ((default-directory tmpdir))
        (pel-ffind-inpath-test--with-env "INCLUDE" env
          ;; Only d1's copy should be returned, NOT the cwd copy via "".
          (should (equal (pel-ffind-inpath-include "t.h")
                         (list f-in-d1))))))))

(ert-deftest pel-ffind-inpath-include/double-separator-does-not-add-cwd ()
  "Consecutive path separators (e.g. foo::bar) must not inject default-directory."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (let* ((d1      (expand-file-name "r1" tmpdir))
           (d2      (expand-file-name "r2" tmpdir))
           (f1      (expand-file-name "q.h" d1))
           (f-cwd   (expand-file-name "q.h" tmpdir))
           ;; double separator between d1 and d2
           (env     (concat d1 path-separator path-separator d2)))
      (pel-ffind-inpath-test--touch f1)
      (pel-ffind-inpath-test--touch f-cwd)
      (let ((default-directory tmpdir))
        (pel-ffind-inpath-test--with-env "INCLUDE" env
          (should (equal (pel-ffind-inpath-include "q.h")
                         (list f1))))))))

(ert-deftest pel-ffind-inpath-include/duplicate-dir-in-env-var-deduplicates ()
  "When INCLUDE lists the same directory twice, seq-uniq ensures only one
result is returned (not two copies of the same file path).

This contrasts with `pel-ffind-inpath/duplicate-dir-in-paths-finds-file-twice',
which documents that the lower-level function does NOT deduplicate.
The deduplication is performed by `seq-uniq' in `pel-ffind-inpath-include'
before the path list is passed to `pel-ffind-inpath'."
  (pel-ffind-inpath-test--with-temp-dir tmpdir
    (let* ((f   (expand-file-name "header.h" tmpdir))
           ;; Same directory listed twice in the env var.
           (env (mapconcat #'identity (list tmpdir tmpdir) path-separator)))
      (pel-ffind-inpath-test--touch f)
      (pel-ffind-inpath-test--with-env "INCLUDE" env
        (should (equal (pel-ffind-inpath-include "header.h")
                       (list f)))))))

;;; --------------------------------------------------------------------------
(provide 'pel-ffind-inpath-test)

;;; test/pel-ffind-inpath-test.el ends here
