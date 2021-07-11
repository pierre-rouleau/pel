;;; pel-base-tests.el --- Regression tests for pel--base.el.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, February 16 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-07-11 18:51:41, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021  Pierre Rouleau
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
;; Currently very sparse tests of the pel--base functions.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)
(require 'pel-ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(ert-deftest ert-test-pel-base-action-for ()
  "Test pel-action-for."
  ;; test toggle request
  (should (eq (pel-action-for nil nil) 'activate ))
  (should (eq (pel-action-for nil t)   'deactivate ))
  (should (eq (pel-action-for 0   nil) 'activate ))
  (should (eq (pel-action-for 0   t)   'deactivate ))
  ;; test activate request
  (should (eq (pel-action-for 1 nil)   'activate ))
  (should (eq (pel-action-for 1 t)      nil))
  (should (eq (pel-action-for 10 nil)  'activate ))
  (should (eq (pel-action-for 10 t)      nil))
  ;; test activate with a list - as C-u would cause
  (should (eq (pel-action-for '(4) nil)   'activate ))
  (should (eq (pel-action-for '(4) t)      nil))
  (should (eq (pel-action-for '(16) nil)  'activate ))
  (should (eq (pel-action-for '(16) t)     nil))
  ;; test deactivate request
  (should (eq (pel-action-for -1 nil)   nil ))
  (should (eq (pel-action-for -1 t)     'deactivate))
  (should (eq (pel-action-for -10 nil)  nil))
  (should (eq (pel-action-for -10 t)    'deactivate)))

(ert-deftest ert-test-pel-base-expression ()
  "Test pel-expression-p."
  (should (eq nil (pel-expression-p nil)))
  (should (eq nil (pel-expression-p t)))
  (should (eq nil (pel-expression-p 't)))
  (should (eq nil (pel-expression-p 'nil)))
  (should (eq nil (pel-expression-p 22)))
  (should (eq nil (pel-expression-p 22.3)))
  (should (eq nil (pel-expression-p "22.3")))
  (let ((some-val 3))
    (should (eq nil (pel-expression-p some-val))))
  ;;
  (should (eq t (pel-expression-p 'a-symbol)))
  (should (eq t (pel-expression-p '(+ 2 3))))
  (let ((indirect-symbol 'a-symbol)
        (indirect-expr   '(* 3 4)))
    (should (eq t   (pel-expression-p indirect-symbol)))
    (should (eq t   (pel-expression-p indirect-expr)))))

(ert-deftest ert-test-lowercase-p ()
  "Test pel-lowercase-p."
  (should (pel-lowercase-p "a"))
  (should (pel-lowercase-p "abc"))
  (should (pel-lowercase-p "abcéèêëüïôûîç"))
  (should (pel-lowercase-p "a 0123456789-=+_!@#$%^&*()~`,.<>/?;:'"))
  (should (pel-lowercase-p "0123456789-=+_!@#$%^&*()~`,.<>/?;:'"))
  (should-not (pel-lowercase-p "aAbcdefghijkl"))
  (should-not (pel-lowercase-p "A")))

(ert-deftest ert-test-uppercase-p ()
  "Test pel-uppercase-p."
  (should (pel-uppercase-p "A"))
  (should (pel-uppercase-p "ABC"))
  (should (pel-uppercase-p "ABCÉÈÊËÜÏÔÛÎÇ"))
  (should (pel-uppercase-p "A 0123456789-=+_!@#$%^&*()~`,.<>/?;:'"))
  (should (pel-uppercase-p "0123456789-=+_!@#$%^&*()~`,.<>/?;:'"))
  (should-not (pel-uppercase-p "AaBCDEFGHIJKL"))
  (should-not (pel-uppercase-p "a")))

(ert-deftest ert-test-pel-whitespace-in-str-p ()
  "Test pel-whitespace-in-str-p."
  (should (pel-whitespace-in-str-p " "))
  (should-not (pel-whitespace-in-str-p ""))
  (should (pel-whitespace-in-str-p "- -"))
  (should (pel-whitespace-in-str-p " -"))
  (should (pel-whitespace-in-str-p "- "))
  (should-not (pel-whitespace-in-str-p "---"))

  (should (pel-whitespace-in-str-p "-\n-"))
  (should (pel-whitespace-in-str-p "\n-"))
  (should (pel-whitespace-in-str-p "-\n"))

  (should (pel-whitespace-in-str-p "-\r-"))
  (should (pel-whitespace-in-str-p "\r-"))
  (should (pel-whitespace-in-str-p "-\r"))

  (should (pel-whitespace-in-str-p "-\t-"))
  (should (pel-whitespace-in-str-p "\t-"))
  (should (pel-whitespace-in-str-p "-\t"))

  (should (pel-whitespace-in-str-p "Ce que l'on conçoit bien s'énonce clairement"))
  (should (pel-whitespace-in-str-p "conçoit bien s'énonce clairement")))

(ert-deftest ert-test-pel-ends-with-space-p ()
  "Test pel-ends-with-space-p."
  (should (pel-ends-with-space-p " "))
  (should-not (pel-ends-with-space-p ""))
  (should-not (pel-ends-with-space-p "- -"))
  (should-not (pel-ends-with-space-p " -"))
  (should (pel-ends-with-space-p "- "))
  (should-not (pel-ends-with-space-p "---")))

(ert-deftest ert-test-pel-starts-with-space-p ()
  "Test pel-starts-with-space-p."
  (should (pel-starts-with-space-p " "))
  (should-not (pel-starts-with-space-p ""))
  (should-not (pel-starts-with-space-p "- -"))
  (should (pel-starts-with-space-p " -"))
  (should-not (pel-starts-with-space-p "- "))
  (should-not (pel-starts-with-space-p "---")))

(ert-deftest ert-test-pel-string-starts-with-p ()
  "Test pel-string-starts-with-p."
  (should (pel-string-starts-with-p "" "")) ; Notice this special case! nothing starts with nothing!
  (should (pel-string-starts-with-p " hello" " "))
  (should-not (pel-string-starts-with-p "" " "))
  (should (pel-string-starts-with-p "- -" "-"))
  (should (pel-string-starts-with-p "- -" "- "))
  (should (pel-string-starts-with-p " -" " "))
  (should (pel-string-starts-with-p "- " "-"))
  (should (pel-string-starts-with-p "---" "---"))
  (should-not (pel-string-starts-with-p "---" "----"))
  (should-not (pel-string-starts-with-p "-.-" "---")))

(ert-deftest ert-test-pel-string-ends-with-p ()
  "Test pel-string-ends-with-p."
  (should (pel-string-ends-with-p "" "")) ; Notice this special case! nothing ends with nothing!
  (should (pel-string-ends-with-p " hello" "llo"))
  (should-not (pel-string-ends-with-p " hello" "llo la"))
  (should-not (pel-string-ends-with-p "" " "))
  (should (pel-string-ends-with-p "- -" "-"))
  (should-not (pel-string-ends-with-p "- -" "- "))
  (should-not (pel-string-ends-with-p " -" " "))
  (should-not (pel-string-ends-with-p "- " "-"))
  (should (pel-string-ends-with-p "---" "---"))
  (should-not (pel-string-ends-with-p "---" "----"))
  (should-not (pel-string-ends-with-p "-.-" "---")))

;; ---------------------------------------------------------------------------

(ert-deftest ert-test-pel-normalize-fname ()
  "Test pel-normalize-fname."
  (should (string= (pel-normalize-fname "/abc/def/../ghi/") "/abc/ghi"))
  (should (string= (pel-normalize-fname "~") (file-truename "~")))
  (should (string= (pel-normalize-fname "~/..") (file-truename "~/..")))
  (should (string= (pel-normalize-fname "~/abc/") (pel-normalize-fname "~/abc"))))

(ert-deftest ert-test-pel-parent-dirpath ()
  "Test pel-parent-dirpath."
  (should (string= (pel-parent-dirpath "/abc/def/") "/abc/"))
  (should (string= (pel-parent-dirpath "/abc/def")  "/abc/")))

(ert-deftest ert-test-pel-expand-url-file-name ()
  "Test pel-expand-url-file-name."
  (should (string= (pel-expand-url-file-name "http://www.lispworks.com") "http://www.lispworks.com"))
  (should (string= (pel-expand-url-file-name "file://~/somedir/somefile")
                   (concat "file://" (pel-normalize-fname "~/somedir/somefile")))))

(ert-deftest ert-test-pel-path-strip ()
  "Test pel-path-strip."
  (should (string= (pel-path-strip "  /abc") "abc"))
  (should (string= (pel-path-strip "\t\r\n  /abc") "abc"))
  (should (string= (pel-path-strip "  /abc ") "abc"))
  (should (string= (pel-path-strip "\t\r\n  /abc\t\n\r ") "abc"))
  ;; the function does not remove spaces *inside* paths
  (should (string= (pel-path-strip "/ab c") "ab c"))
  (should (string= (pel-path-strip "  /ab c  ") "ab c")))

(ert-deftest ert-test-pel-url-location ()
  "Test pel-url-location."
  (should (string= (pel-url-location "file://~/docs/HyperSpec/") "Local"))
  (should (string= (pel-url-location "http://www.lispworks.com") "Remote")))

(ert-deftest ert-test-pel-same-fname-p ()
  "Test pel-same-fname-p."
  (should (pel-same-fname-p "~/abc/.." "~"))
  (should (pel-same-fname-p "/abc/"    "/abc"))
  (should (pel-same-fname-p "/abc/def/ghi/"  "/abc/22/../def/ghi")))

;;; --------------------------------------------------------------------------
(provide 'pel-base-tests)

;;; pel-base-tests.el ends here
