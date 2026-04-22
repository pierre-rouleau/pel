;;; pel-fs-test.el --- Test pel-fs.el  -*- lexical-binding: t; -*-

;; Created   : Tuesday, April 21 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-04-21 10:07:59 EDT, updated by Pierre Rouleau>

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
;; Tests the filesystem/exec helpers in pel-fs.el without relying on system
;; tools.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel-fs)
(require 'ert)
(eval-when-compile (require 'cl-lib))

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-fs-test--make-script (dir name content)
  "Create an executable script NAME under DIR with CONTENT.
Return absolute path."
  (let* ((path (expand-file-name name dir)))
    (with-temp-file path
      (insert content))
    (set-file-modes path #o755)
    path))

;; ---------------------------------------------------------------------------
;; pel-exec-cmd
;; ---------------------------------------------------------------------------

(ert-deftest pel-fs/exec-cmd/returns-exit-stdout-stderr-trimmed ()
  "Executes a temp script and returns (exit stdout stderr) with no trailing NL."
  (let* ((tmp (make-temp-file "pel-fs-test-" t))
         (script (pel-fs-test--make-script
                  tmp "runner.sh"
                  "#!/usr/bin/env sh\nprintf 'OUT\\n'; >&2 printf 'ERR\\n'; exit 7\n"))
         (res (apply #'pel-exec-cmd script nil)))
    (unwind-protect
        (progn
          (should (equal (nth 0 res) 7))
          (should (equal (nth 1 res) "OUT"))
          (should (equal (nth 2 res) "ERR")))
      (ignore-errors (delete-directory tmp t)))))

(ert-deftest pel-fs/exec-cmd/nonexistent-command-yields-error-cons ()
  "Calling a non-existent command returns (nil . error) via condition-case."
  (let ((res (pel-exec-cmd "/definitely/not/a/command")))
    (should (consp res))
    (should (null (car res)))           ; (nil . <error>)
    (should (string-match-p "error\\|file\\|void" (format "%S" (cdr res))))))

;; ---------------------------------------------------------------------------
;; pel-file-content and pel-file-content-md5
;; ---------------------------------------------------------------------------

(ert-deftest pel-fs/file-content-and-md5 ()
  "pel-file-content returns exact content; pel-file-content-md5 matches Emacs md5."
  (let* ((tmp (make-temp-file "pel-fs-md5-" nil ".txt"))
         (data "abc\n123\n")
         (_ (with-temp-file tmp (insert data))))
    (unwind-protect
        (progn
          (should (equal (pel-file-content tmp) data))
          (should (equal (pel-file-content-md5 tmp) (md5 data))))
      (ignore-errors (delete-file tmp)))))

;; ---------------------------------------------------------------------------
;; pel-bin-path
;; ---------------------------------------------------------------------------

(ert-deftest pel-fs/bin-path/returns-existing-directory-and-file ()
  "pel-bin-path returns the PEL bin directory; with a file-name returns the file."
  (let* ((bin-dir (pel-bin-path))
         (any-file (pel-bin-path "version-erl")))
    (should (stringp bin-dir))
    (should (file-directory-p bin-dir))
    (should (stringp any-file))
    (should (file-exists-p any-file))
    (should (file-readable-p any-file))))

;; ---------------------------------------------------------------------------
;; pel-check-pel-bin-cmd and pel-exec-pel-bin (mocked bin directory)
;; ---------------------------------------------------------------------------

(ert-deftest pel-fs/check-and-exec-pel-bin/ok-path-makes-executable-and-runs ()
  "With mocked bin path and MD5, check function sets exec bit and exec works."
  (let* ((tmp (make-temp-file "pel-fs-bin-" t))
         (cmd "hello-cmd")
         (script (expand-file-name cmd tmp))
         ;; Non-executable initially
         (_ (with-temp-file script
              (insert "#!/usr/bin/env sh\nprintf 'OK'")))
         (_ (set-file-modes script #o644)) ; remove exec
         (good-md5 (md5 (with-temp-buffer (insert-file-contents script)
                          (buffer-string)))))
    (unwind-protect
        (cl-letf (((symbol-function 'pel-bin-path)
                   (lambda (&optional file) (if file (expand-file-name file tmp) tmp)))
                  (pel-bin-md5-alist (list (cons cmd good-md5))))
          ;; Check: becomes executable and returns t
          (should (pel-check-pel-bin-cmd cmd))
          (should (file-executable-p script))
          ;; Exec and capture result
          (let ((res (pel-exec-pel-bin cmd)))
            (should (equal (nth 0 res) 0))
            (should (equal (nth 1 res) "OK"))
            (should (equal (nth 2 res) ""))))
      (ignore-errors (delete-directory tmp t)))))

(ert-deftest pel-fs/check-pel-bin-cmd/md5-mismatch-signals-user-error ()
  "MD5 mismatch must signal a user-error."
  (let* ((tmp (make-temp-file "pel-fs-bin-md5-" t))
         (cmd "bad-cmd")
         (wrong-md5 "00000000000000000000000000000000"))
    (unwind-protect
        (progn
          ;; create the bad-cmd script
          (pel-fs-test--make-script tmp cmd "#!/usr/bin/env sh\nprintf 'X'")
          ;;
          (cl-letf (((symbol-function 'pel-bin-path)
                     (lambda (&optional file) (if file (expand-file-name file tmp) tmp)))
                    (pel-bin-md5-alist (list (cons cmd wrong-md5))))
            (should-error (pel-check-pel-bin-cmd cmd) :type 'user-error)))
      (ignore-errors (delete-directory tmp t)))))

(ert-deftest pel-fs/check-pel-bin-cmd/nonexistent-file-signals-user-error ()
  "Nonexistent bin command must signal a user-error."
  (should-error (pel-check-pel-bin-cmd "does-not-exist-xyz") :type 'user-error))

;;; --------------------------------------------------------------------------
(provide 'pel-fs-test)

;;; pel-fs-test.el ends here
