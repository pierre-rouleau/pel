;;; pel-comp-test.el --- Test pel-comp.el  -*- lexical-binding: t; -*-

;; Created   : Saturday, March 21 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-21 17:00:54 EDT, updated by Pierre Rouleau>

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
;; ERT tests for pel-comp.el.
;;
;; Run interactively : M-x ert RET "^pel-comp" RET
;; Run in batch      : emacs -batch -l ert -l pel-comp.el \
;;                       -l test/pel-comp-test.el -f ert-run-tests-batch-and-exit

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-comp)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; --------------------------------------------------------------------------
;; Test fixtures

(defconst pel-comp-test--fake-emacs-dir
  (file-name-as-directory (expand-file-name "/tmp/fake-emacs.d"))
  "Fake `user-emacs-directory' used in tests.")

(defconst pel-comp-test--fake-version-dir "28.2-x86_64-linux"
  "Fake `comp-native-version-dir' value used in tests.")

;; --------------------------------------------------------------------------
;; Tests for `pel-comp-eln-file-for-util'

(ert-deftest pel-comp-eln-file-for-util-test--returns-path-when-supported ()
  "Happy path: return a correctly structured .eln path."
  ;; Defining comp-el-to-eln-rel-filename via cl-letf makes fboundp return t
  ;; for it naturally.  let-binding comp-native-version-dir makes boundp
  ;; return t for it naturally.
  (let ((user-emacs-directory pel-comp-test--fake-emacs-dir)
        (comp-native-version-dir pel-comp-test--fake-version-dir))
    (cl-letf (((symbol-function 'comp-el-to-eln-rel-filename)
               (lambda (path)
                 ;; Simulate the real function: return "<base>.eln"
                 (concat (file-name-base path) ".eln"))))
      (let ((result (pel-comp-eln-file-for-util "my-util.el")))
        (should (stringp result))
        (should (string-suffix-p ".eln" result))
        (should (string-match-p "eln-cache" result))
        (should (string-match-p pel-comp-test--fake-version-dir result))
        (should (string-match-p "my-util" result))))))

(ert-deftest pel-comp-eln-file-for-util-test--path-uses-utils-subdir ()
  "The path passed to comp-el-to-eln-rel-filename must include the utils subdir."
  (let ((user-emacs-directory pel-comp-test--fake-emacs-dir)
        (comp-native-version-dir pel-comp-test--fake-version-dir)
        captured-el-path)
    (cl-letf (((symbol-function 'comp-el-to-eln-rel-filename)
               (lambda (path)
                 (setq captured-el-path path)
                 "my-util.eln")))
      (pel-comp-eln-file-for-util "my-util.el")
      (should (string-match-p "utils" captured-el-path))
      (should (string-match-p "my-util\\.el" captured-el-path))
      (should (string-prefix-p pel-comp-test--fake-emacs-dir captured-el-path)))))

(ert-deftest pel-comp-eln-file-for-util-test--eln-dir-uses-eln-cache-subdir ()
  "The returned path must be rooted under <user-emacs-directory>/eln-cache."
  (let ((user-emacs-directory pel-comp-test--fake-emacs-dir)
        (comp-native-version-dir pel-comp-test--fake-version-dir))
    (cl-letf (((symbol-function 'comp-el-to-eln-rel-filename)
               (lambda (_path) "my-util.eln")))
      (let ((result (pel-comp-eln-file-for-util "my-util.el")))
        (should (string-prefix-p
                 (expand-file-name "eln-cache" pel-comp-test--fake-emacs-dir)
                 result))))))

(ert-deftest pel-comp-eln-file-for-util-test--error-when-function-absent ()
  "Signal error when comp-el-to-eln-rel-filename is not fboundp."
  ;; Save original fboundp so we can delegate to it for all other symbols,
  ;; preventing infinite recursion inside the mock closure.
  (let ((orig-fboundp (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (if (eq sym 'comp-el-to-eln-rel-filename)
                     nil
                   (funcall orig-fboundp sym)))))
      (should-error (pel-comp-eln-file-for-util "my-util.el")
                    :type 'user-error))))

(ert-deftest pel-comp-eln-file-for-util-test--error-when-var-absent ()
  "Signal error when comp-native-version-dir is not boundp."
  ;; fboundp must return t for the function so the (and …) proceeds to check
  ;; boundp; then boundp returns nil for the variable to trigger the error.
  (let ((orig-fboundp (symbol-function 'fboundp))
        (orig-boundp  (symbol-function 'boundp)))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (if (eq sym 'comp-el-to-eln-rel-filename)
                     t
                   (funcall orig-fboundp sym))))
              ((symbol-function 'boundp)
               (lambda (sym)
                 (if (eq sym 'comp-native-version-dir)
                     nil
                   (funcall orig-boundp sym)))))
      (should-error (pel-comp-eln-file-for-util "my-util.el")
                    :type 'user-error))))

(ert-deftest pel-comp-eln-file-for-util-test--error-when-both-absent ()
  "Signal error when both comp-el-to-eln-rel-filename and comp-native-version-dir are absent."
  (let ((orig-fboundp (symbol-function 'fboundp))
        (orig-boundp  (symbol-function 'boundp)))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (if (eq sym 'comp-el-to-eln-rel-filename)
                     nil
                   (funcall orig-fboundp sym))))
              ((symbol-function 'boundp)
               (lambda (sym)
                 (if (eq sym 'comp-native-version-dir)
                     nil
                   (funcall orig-boundp sym)))))
      (should-error (pel-comp-eln-file-for-util "my-util.el")
                    :type 'user-error))))

(ert-deftest pel-comp-eln-file-for-util-test--user-error-when-no-el-extension ()
  "Signal user-error when FNAME does not end with \".el\"."
  (let ((user-emacs-directory pel-comp-test--fake-emacs-dir)
        (comp-native-version-dir pel-comp-test--fake-version-dir))
    (cl-letf (((symbol-function 'comp-el-to-eln-rel-filename)
               (lambda (path) (concat (file-name-base path) ".eln"))))
      (should-error (pel-comp-eln-file-for-util "my-util")   ; no .el
                    :type 'user-error)
      (should-error (pel-comp-eln-file-for-util "my-util.elc") ; wrong ext
                    :type 'user-error))))

;; --------------------------------------------------------------------------
;; Tests for `pel-native-compile-util'
;;
;; `pel-comp-eln-file-for-util' is mocked in all tests below so that these
;; tests remain independent of native-compile availability in the host Emacs.

(ert-deftest pel-native-compile-util-test--eln-already-present ()
  "Return \\='eln-present when the .eln file already exists and is newer than the .el source."
  (let ((orig-featurep (symbol-function 'featurep)))
    (cl-letf (((symbol-function 'pel-comp-eln-file-for-util)
               (lambda (_fname) "/fake/eln-cache/28.2/my-util.eln"))
              ((symbol-function 'featurep)
               (lambda (feature &optional _subfeature)
                 (if (eq feature 'native-compile)
                     t
                   (funcall orig-featurep feature))))
              ((symbol-function 'file-exists-p)
               (lambda (_path) t))
              ((symbol-function 'file-newer-than-file-p)
               (lambda (_newer _older) t)))
      (should (eq 'eln-present
                  (pel-native-compile-util "my-util.el"))))))

(ert-deftest pel-native-compile-util-test--compiles-when-eln-absent ()
  "Initiate compilation and return t when .eln absent and native-compile present."
  (let ((orig-featurep (symbol-function 'featurep))
        (orig-fboundp  (symbol-function 'fboundp))
        (compile-called-with nil))
    (cl-letf (((symbol-function 'pel-comp-eln-file-for-util)
               (lambda (_fname) "/fake/eln-cache/28.2/my-util.eln"))
              ((symbol-function 'file-exists-p)
               (lambda (path) (string-suffix-p ".el" path)))
              ((symbol-function 'featurep)
               (lambda (feature &optional _subfeature)
                 (if (eq feature 'native-compile)
                     t
                   (funcall orig-featurep feature))))
              ((symbol-function 'fboundp)
               (lambda (sym)
                 (if (eq sym 'native-compile-async)
                     t
                   (funcall orig-fboundp sym))))
              ((symbol-function 'require)
               (lambda (feature &rest _)
                 ;; No-op for comp-run; forward anything else.
                 (unless (eq feature 'comp-run)
                   (require feature))))
              ((symbol-function 'native-compile-async)
               (lambda (path &rest _)
                 (setq compile-called-with path))))
      (should (eq t (pel-native-compile-util "my-util.el")))
      ;; native-compile-async must have been called with the .el path
      (should (stringp compile-called-with))
      (should (string-match-p "my-util\\.el" compile-called-with)))))

(ert-deftest pel-native-compile-util-test--returns-nil-when-async-absent ()
  "Return nil when native-compile-async is not bound (compilation skipped)."
  (let ((orig-featurep (symbol-function 'featurep))
        (orig-fboundp  (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'pel-comp-eln-file-for-util)
               (lambda (_fname) "/fake/eln-cache/28.2/my-util.eln"))
              ((symbol-function 'file-exists-p)
               (lambda (path) (string-suffix-p ".el" path)))
              ((symbol-function 'featurep)
               (lambda (feature &optional _subfeature)
                 (if (eq feature 'native-compile)
                     t
                   (funcall orig-featurep feature))))
              ((symbol-function 'fboundp)
               (lambda (sym)
                 (if (eq sym 'native-compile-async)
                     nil                ; async function absent
                   (funcall orig-fboundp sym))))
              ((symbol-function 'require)
               (lambda (feature &rest _)
                 (unless (eq feature 'comp-run)
                   (require feature)))))
      (should (eq nil (pel-native-compile-util "my-util.el"))))))

(ert-deftest pel-native-compile-util-test--user-error-when-no-native-compile ()
  "Signal user-error when native-compile feature is absent."
  (let ((orig-featurep (symbol-function 'featurep)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &optional _subfeature)
                 (if (eq feature 'native-compile)
                     nil
                   (funcall orig-featurep feature)))))
      (should-error (pel-native-compile-util "my-util.el")
                    :type 'user-error))))

;;; --------------------------------------------------------------------------
(provide 'pel-comp-test)

;;; pel-comp-test.el ends here
