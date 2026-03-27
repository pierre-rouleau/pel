;;; pel-file-open-test.el --- ERT tests for pel-file.el open-at-point logic  -*- lexical-binding: t; -*-

;; Created   : Thursday, March 26 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-26 23:04:09 EDT, updated by Pierre Rouleau>

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
;; Tests for the filename extraction and file-finding logic in pel-file.el.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'ert)
(require 'pel-file)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; ---------------------------------------------------------------------------
;; Tests for `pel-filename-parts-at-point'
;; ---------------------------------------------------------------------------

(defmacro pel-file-test/with-point-on-string (str &rest body)
  "Execute BODY in a temp buffer with STR inserted and point at start."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,str)
     (goto-char (point-min))
     ,@body))

(ert-deftest pel-file-test/parts-simple-filename ()
  "A plain filename returns (fname filename nil nil)."
  (pel-file-test/with-point-on-string "foo.c"
    (let ((parts (pel-filename-parts-at-point)))
      (should parts)
      (should (eq (nth 0 parts) 'fname))
      (should (string= (nth 1 parts) "foo.c"))
      (should (null (nth 2 parts)))   ; no line
      (should (null (nth 3 parts)))))) ; no column

(ert-deftest pel-file-test/parts-filename-with-line ()
  "A filename:line string returns correct line number."
  (pel-file-test/with-point-on-string "foo.c:42"
    (let ((parts (pel-filename-parts-at-point)))
      (should parts)
      (should (eq (nth 0 parts) 'fname))
      (should (string= (nth 1 parts) "foo.c"))
      (should (equal (nth 2 parts) 42)))))

(ert-deftest pel-file-test/parts-filename-with-line-and-column ()
  "A filename:line:column string returns correct line and column."
  (pel-file-test/with-point-on-string "src/bar.h:10:5"
    (let ((parts (pel-filename-parts-at-point)))
      (should parts)
      (should (string-match-p "bar\\.h" (nth 1 parts)))
      (should (equal (nth 2 parts) 10))
      (should (equal (nth 3 parts) 5)))))

(ert-deftest pel-file-test/parts-line-zero-becomes-one ()
  "A line number of 0 is coerced to 1."
  (pel-file-test/with-point-on-string "foo.c:0"
    (let ((parts (pel-filename-parts-at-point)))
      (should (equal (nth 2 parts) 1)))))

(ert-deftest pel-file-test/parts-http-url ()
  "An http URL returns a (http . url) cons."
  (pel-file-test/with-point-on-string "https://example.com/page"
    (let ((parts (pel-filename-parts-at-point)))
      (should (consp parts))
      (should (eq (car parts) 'http))
      (should (string= (cdr parts) "https://example.com/page")))))

(ert-deftest pel-file-test/parts-http-url-with-http-scheme ()
  "An http:// URL returns a (http . url) cons."
  (pel-file-test/with-point-on-string "http://example.com/foo"
    (let ((parts (pel-filename-parts-at-point)))
      (should (consp parts))
      (should (eq (car parts) 'http)))))

(ert-deftest pel-file-test/parts-file-uri-stripped ()
  "A file:// URI prefix is stripped unless keep-file-url is set."
  (pel-file-test/with-point-on-string "file:///usr/include/stdio.h"
    (let ((parts (pel-filename-parts-at-point nil)))
      ;; After stripping file:// the result should be a normal filename
      (should parts)
      (should (not (eq (car parts) 'http)))
      ;; Verify the path was correctly extracted
      (should (eq (nth 0 parts) 'fname))
      (should (string= (nth 1 parts) "/usr/include/stdio.h")))))

(ert-deftest pel-file-test/parts-windows-drive-letter ()
  "A Windows-style path returns fname-w-ddrv type."
  ;; pel-filename-parts-at-point supports Windows-style filename
  ;; on all platforms.
  (pel-file-test/with-point-on-string "C:\\Users\\foo\\bar.txt"
    (let ((parts (pel-filename-parts-at-point)))
      (should parts)
      (should (eq (nth 0 parts) 'fname-w-ddrv)))))

;; ---------------------------------------------------------------------------
;; Tests for `pel--find-by-finders'
;; ---------------------------------------------------------------------------

(ert-deftest pel-file-test/find-by-finders-nil-when-no-finders ()
  "Returns nil when pel-filename-at-point-finders is nil."
  (with-temp-buffer
    (setq-local pel-filename-at-point-finders nil)
    (should-not (pel--find-by-finders "stdio.h"))))

(ert-deftest pel-file-test/find-by-finders-calls-first-finder ()
  "Uses first finder that returns a result; stops there."
  (with-temp-buffer
    (let ((second-called nil))
      (setq-local pel-filename-at-point-finders
                  (list
                   (lambda (_fn) '("/usr/include/stdio.h"))
                   (lambda (_fn) (setq second-called t) nil)))
      (let ((result (pel--find-by-finders "stdio.h")))
        (should (equal result '("/usr/include/stdio.h")))
        (should-not second-called)))))

(ert-deftest pel-file-test/find-by-finders-tries-second-on-nil ()
  "Tries the second finder when the first returns nil."
  (with-temp-buffer
    (setq-local pel-filename-at-point-finders
                (list
                 (lambda (_fn) nil)
                 (lambda (_fn) '("/opt/include/mylib.h"))))
    (let ((result (pel--find-by-finders "mylib.h")))
      (should (equal result '("/opt/include/mylib.h"))))))

(ert-deftest pel-file-test/find-by-finders-returns-nil-when-all-fail ()
  "Returns nil when all finders return nil."
  (with-temp-buffer
    (setq-local pel-filename-at-point-finders
                (list (lambda (_fn) nil)
                      (lambda (_fn) nil)))
    (should-not (pel--find-by-finders "nonexistent.h"))))

;; ---------------------------------------------------------------------------
(provide 'pel-file-open-test)
;;; pel-file-open-test.el ends here
