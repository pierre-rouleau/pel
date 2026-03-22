;;; pel-base-tests.el --- Regression tests for pel--base.el.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, February 16 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-22 12:43:15 EDT, updated by Pierre Rouleau>

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
;; Currently very sparse tests of the pel--base functions.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(ert-deftest ert-test-pel-version ()
  "Test `pel-version'."
  ;; Returns a non-empty string.
  (should (stringp (pel-version)))
  (should (> (length (pel-version)) 0))
  ;; Returns the same string each time.
  (should (string= (pel-version) (pel-version))))

(ert-deftest ert-test-increment-decrement ()
  "Test the `pel+=' and `pel-=' macros."
  ;; First test the macro expansions
  ;; - The macro optimizes when the offset is 1:
  (should (equal (macroexpand-1 '(pel+= val 1))
                 '(setq val (1+ val))))
  (should (equal (macroexpand-1 '(pel-= val 1))
                 '(setq val (1- val))))

  ;; - Otherwise, when the offset is something else the standard arithmetic
  ;;   function is used.
  (should (equal (macroexpand-1 '(pel+= val 3))
                 '(setq val (+ val 3))))
  (should (equal (macroexpand-1 '(pel-= val 3))
                 '(setq val (- val 3))))

  ;; Then check the effects of the macros
  (let ((val 0))
    (should (eq 1 (pel+= val 1)))
    (should (eq 3 (pel+= val 2)))
    (should (eq 0 (pel-= val 3)))
    (should (eq -1 (pel-= val 1)))))


(ert-deftest ert-test-pel-lambda-c ()
  "Test `λc' (funcall alias macro)."
  (should (eq 3        (λc #'+ 1 2)))
  (should (eq 6        (λc #'* 2 3)))
  (should (string= "A" (λc #'upcase "a")))
  ;; Works with lambdas too.
  (should (eq 10 (λc (lambda (x) (* x 2)) 5))))


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


;; --

;; Top-level defvar declares a true dynamic (special) variable,
;; making it visible to pel-set-if-non-nil's internal (set symbol value).
(defvar pel--test-set-if-non-nil-var nil
  "Dynamic variable used only by `ert-test-pel-set-if-non-nil'.")

(ert-deftest ert-test-pel-set-if-non-nil ()
  "Test `pel-set-if-non-nil'."
  ;; Reset to a known state before each run.
  (setq pel--test-set-if-non-nil-var nil)
  ;; When value is nil, the symbol is not changed.
  (pel-set-if-non-nil 'pel--test-set-if-non-nil-var nil)
  (should (eq nil pel--test-set-if-non-nil-var))
  ;; When value is non-nil, the symbol is updated.
  (pel-set-if-non-nil 'pel--test-set-if-non-nil-var 42)
  (should (eq 42 pel--test-set-if-non-nil-var))
  ;; A second non-nil value updates again.
  (pel-set-if-non-nil 'pel--test-set-if-non-nil-var "hello")
  (should (string= "hello" pel--test-set-if-non-nil-var))
  ;; A nil value leaves the prior assignment in place.
  (pel-set-if-non-nil 'pel--test-set-if-non-nil-var nil)
  (should (string= "hello" pel--test-set-if-non-nil-var)))

;; --

(ert-deftest ert-test-pel-not-zero ()
  "Test `pel-!0'."
  (should-not (pel-!0 0))
  (should     (pel-!0 1))
  (should     (pel-!0 -1))
  (should     (pel-!0 42))
  (should     (pel-!0 0.1)))

(ert-deftest ert-test-pel-as-boolean ()
  "Test `pel-as-boolean'."
  (should (eq t   (pel-as-boolean t)))
  (should (eq t   (pel-as-boolean 1)))
  (should (eq t   (pel-as-boolean "x")))
  (should (eq t   (pel-as-boolean '(a))))
  (should (eq nil (pel-as-boolean nil)))
  (should (eq nil (pel-as-boolean nil))))

(ert-deftest ert-test-pel-all-bitset-p ()
  "Test `pel-all-bitset-p'."
  ;; Exact match: value has exactly those bits set.
  (should     (pel-all-bitset-p #b0001 #b0001))
  (should     (pel-all-bitset-p #b0011 #b0001 #b0010))
  (should     (pel-all-bitset-p #b0111 #b0001 #b0010 #b0100))
  ;; Fails when extra bits are set.
  (should-not (pel-all-bitset-p #b0011 #b0001))
  (should-not (pel-all-bitset-p #b0111 #b0001 #b0010))
  ;; Fails when a required bit is missing.
  (should-not (pel-all-bitset-p #b0001 #b0010))
  (should-not (pel-all-bitset-p #b0110 #b0001 #b0010 #b0100))
  ;; Zero value with zero bitmask.
  (should     (pel-all-bitset-p 0)))

(ert-deftest ert-test-list-of ()
  "Test `pel-list-of'."
  (should (equal (pel-list-of 1) '(1)))
  (should (equal (pel-list-of '(1)) '(1))))

(ert-deftest ert-test-transpose-alist ()
  "Test `pel-transpose-alist'."
  (should (equal (pel-transpose-alist '((1 . a)))
                 '((a . 1))))
  (should (equal (pel-transpose-alist '((1 . a) (2 . b)))
                 '((a . 1) (b . 2))))
  (should (equal (pel-transpose-alist '((1 . a) (2 . b) (3 . c)))
                 '((a . 1) (b . 2) (c . 3)))))

(when noninteractive
  ;; The following test can only execute successfully under batch control.

  (eval-and-compile
    (defvar pel--is-in-fast-startup-mode (bound-and-true-p pel-running-in-fast-startup-p))
    "Detect if PEL environment is in fast startup or not.")

  (ert-deftest ert-test-fast-startup-p ()
    "Test `pel-in-fast-startup-p'."
    (should (eq (pel-in-fast-startup-p) pel--is-in-fast-startup-mode))

    ;; Show that attempt to simulate PEL being in fast startup does not work.
    (let ((pel-running-in-fast-startup-p nil))
      (should (eq (pel-in-fast-startup-p) pel--is-in-fast-startup-mode)))

    ;; Note: this next test will pass when run from the shell but not executed from with Emacs
    (let ((pel-running-in-fast-startup-p t))
      (should (eq (pel-in-fast-startup-p) pel--is-in-fast-startup-mode)))))

(ert-deftest ert-test-pel-major-mode-of ()
  "Test `pel-major-mode-of'."
  ;; In a temp buffer with emacs-lisp-mode, it returns that mode.
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (eq 'emacs-lisp-mode (pel-major-mode-of)))
    (should (eq 'emacs-lisp-mode (pel-major-mode-of (current-buffer)))))
  ;; text-mode in another temp buffer.
  (let ((buf (get-buffer-create " *pel-test-text-mode*")))
    (unwind-protect
        (with-current-buffer buf
          (text-mode)
          (should (eq 'text-mode (pel-major-mode-of)))
          (should (eq 'text-mode (pel-major-mode-of buf))))
      (kill-buffer buf))))

(ert-deftest ert-test-pel-file-type-for ()
  "Test `pel-file-type-for'."
  ;; Default: strips -mode suffix.
  (should (string= "emacs-lisp"  (pel-file-type-for 'emacs-lisp-mode)))
  (should (string= "c"           (pel-file-type-for 'c-mode)))
  (should (string= "python"      (pel-file-type-for 'python-mode)))
  ;; Tree-sitter mode: strips -ts-mode suffix.
  (should (string= "python"      (pel-file-type-for 'python-ts-mode)))
  (should (string= "c"           (pel-file-type-for 'c-ts-mode)))
  ;; Custom suffix.
  (should (string= "flyspell"    (pel-file-type-for 'flyspell-mode "-mode")))
  (should (string= "subword"     (pel-file-type-for 'subword-minor-mode
                                                    "-minor-mode"))))

(ert-deftest ert-test-pel-buffers-in-mode ()
  "Test `pel-buffers-in-mode'."
  (let* ((buf1 (get-buffer-create " *pel-test-bim-1*"))
         (buf2 (get-buffer-create " *pel-test-bim-2*"))
         (buf3 (get-buffer-create " *pel-test-bim-3*")))
    (unwind-protect
        (progn
          (with-current-buffer buf1 (emacs-lisp-mode))
          (with-current-buffer buf2 (text-mode))
          (with-current-buffer buf3 (emacs-lisp-mode))
          ;; Both buf1 and buf3 should appear.
          (let ((result (pel-buffers-in-mode 'emacs-lisp-mode)))
            (should (memq buf1 result))
            (should (memq buf3 result))
            (should-not (memq buf2 result)))
          ;; Only buf2 for text-mode.
          (let ((result (pel-buffers-in-mode 'text-mode)))
            (should (memq buf2 result))
            (should-not (memq buf1 result)))
          ;; A mode with no buffers returns nil.
          (should-not (pel-buffers-in-mode 'fundamental-mode-xyz-nonexistent)))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3))))

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

(ert-deftest ert-test-pel-alnum-p ()
  "Test `pel-alnum-p'."
  (should     (pel-alnum-p "abc"))
  (should     (pel-alnum-p "ABC"))
  (should     (pel-alnum-p "abc123"))
  (should     (pel-alnum-p "123"))
  ;; Punctuation or whitespace → nil.
  (should-not (pel-alnum-p "abc!"))
  (should-not (pel-alnum-p "abc def"))
  (should-not (pel-alnum-p "abc.def"))
  (should-not (pel-alnum-p "."))
  (should-not (pel-alnum-p "-"))
  ; empty string: no alnum chars
  (should-not (pel-alnum-p "")))

(ert-deftest ert-test-pel-plural-of ()
  "Test `pel-plural-of'."
  ;; -s / -o endings → -es
  (should (string= "classes"  (pel-plural-of "class")))
  (should (string= "tomatoes" (pel-plural-of "tomato")))
  ;; -y ending → -ies
  (should (string= "skies"    (pel-plural-of "sky")))
  (should (string= "entries"  (pel-plural-of "entry")))
  ;; -f ending → -ves
  (should (string= "calves"   (pel-plural-of "calf")))
  (should (string= "halves"   (pel-plural-of "half")))
  ;; default → -s
  (should (string= "commands" (pel-plural-of "command")))
  (should (string= "buffers"  (pel-plural-of "buffer"))))

(ert-deftest ert-test-pel-count-string ()
  "Test `pel-count-string'."
  ;; n = 0 : singular form with count
  (should (string= "0 item"  (pel-count-string 0 "item")))
  ;; n = 1 : singular form with count
  (should (string= "1 item"  (pel-count-string 1 "item")))
  ;; n = 1 no-count-for-1 : bare singular
  (should (string= "item"    (pel-count-string 1 "item" nil t)))
  ;; n > 1 : computed plural
  (should (string= "2 items" (pel-count-string 2 "item")))
  (should (string= "5 items" (pel-count-string 5 "item")))
  ;; n > 1 : explicit plural overrides pel-plural-of
  (should (string= "2 oxen"  (pel-count-string 2 "ox" "oxen")))
  (should (string= "3 geese" (pel-count-string 3 "goose" "geese"))))

(ert-deftest ert-test-pel-pluralize ()
  "Test `pel-pluralize'."
  ;; n <= 1 : return singular
  (should (string= "item"  (pel-pluralize 0 "item")))
  (should (string= "item"  (pel-pluralize 1 "item")))
  ;; n > 1 : computed plural
  (should (string= "items" (pel-pluralize 2 "item")))
  (should (string= "skies" (pel-pluralize 3 "sky")))
  ;; n > 1 : explicit plural
  (should (string= "geese" (pel-pluralize 2 "goose" "geese"))))

(ert-deftest ert-test-pel-as-symbol ()
  "Test `pel-as-symbol'."
  ;; A symbol is returned unchanged.
  (should (eq 'foo (pel-as-symbol 'foo)))
  ;; A string is interned to a symbol.
  (should (eq 'foo (pel-as-symbol "foo")))
  (should (symbolp (pel-as-symbol "bar"))))

;; --
;; Top-level defvar declares a true dynamic (special) variable.
(defvar pel--test-symbol-value-or-var nil
  "Dynamic variable used only by `ert-test-pel-symbol-value-or'.")

(ert-deftest ert-test-pel-symbol-value-or ()
  "Test `pel-symbol-value-or'."
  ;; Reset to a known non-nil value so the bound branch is exercised.
  (setq pel--test-symbol-value-or-var 42)
  ;; Bound symbol → returns its value.
  (should (eq 42 (pel-symbol-value-or 'pel--test-symbol-value-or-var)))
  ;; Bound symbol with formatter → formatter receives the symbol and returns a string.
  (should (string= "pel--test-symbol-value-or-var"
                   (pel-symbol-value-or 'pel--test-symbol-value-or-var
                                        nil
                                        #'symbol-name)))
  ;; Unbound symbol → replacement string.
  (should (string= "N/A" (pel-symbol-value-or 'pel--unbound-xyz-test "N/A")))
  ;; Unbound symbol with :nil-for-void → nil.
  (should (eq nil (pel-symbol-value-or 'pel--unbound-xyz-test :nil-for-void)))
  ;; Unbound with no replacement → informative string is returned.
  (should (stringp (pel-symbol-value-or 'pel--unbound-xyz-test))))

;; --

(ert-deftest ert-test-pel-on-off-string ()
  "Test `pel-on-off-string'."
  (should (string= "on"     (pel-on-off-string t)))
  (should (string= "off"    (pel-on-off-string nil)))
  (should (string= "yes"    (pel-on-off-string t "yes" "no")))
  (should (string= "no"     (pel-on-off-string nil "yes" "no")))
  (should (string= "active" (pel-on-off-string 42 "active")))
  (should (string= "off"    (pel-on-off-string nil nil "off"))))

(ert-deftest ert-test-pel-yes-no-string ()
  "Test `pel-yes-no-string'."
  (should (string= "yes"  (pel-yes-no-string t)))
  (should (string= "no"   (pel-yes-no-string nil)))
  (should (string= "true" (pel-yes-no-string t "true" "false")))
  (should (string= "false"(pel-yes-no-string nil "true" "false")))
  (should (string= "yes"  (pel-yes-no-string 42)))
  (should (string= "yes"  (pel-yes-no-string '(a b)))))

;; --

(ert-deftest ert-test-pel-as-string ()
  "Test `pel-as-string'."
  ;; String passes through.
  (should (string= "hello"  (pel-as-string "hello")))
  (should (string= ""       (pel-as-string "")))
  ;; Symbol → symbol-name.
  (should (string= "foo"    (pel-as-string 'foo)))
  ;; Number → decimal string.
  (should (string= "42"     (pel-as-string 42)))
  (should (string= "3.14"   (pel-as-string 3.14)))
  ;; Character → single-char string.
  (should (string= "65"     (pel-as-string ?A)))
  (should (string= "A"      (pel-as-string ?A t)))
  ;; Unsupported type → error.
  (should-error (pel-as-string '(1 2 3)))
  (should-error (pel-as-string [1 2 3])))

(ert-deftest ert-test-pel-end-text-with-period ()
  "Test `pel-end-text-with-period'."
  ;; Already ends with period → unchanged.
  (should (string= "Hello."     (pel-end-text-with-period "Hello.")))
  ;; Does not end with period → period appended.
  (should (string= "Hello."     (pel-end-text-with-period "Hello")))
  ;; Empty string → empty string.
  (should (string= ""           (pel-end-text-with-period "")))
  ;; Multiple periods: only checks last character.
  (should (string= "e.g."       (pel-end-text-with-period "e.g.")))
  (should (string= "see note."  (pel-end-text-with-period "see note"))))

(ert-deftest ert-test-pel-hastext ()
  "Test `pel-hastext'."
  (should     (pel-hastext "a"))
  (should     (pel-hastext " "))
  (should     (pel-hastext "hello world"))
  (should-not (pel-hastext "")))

(ert-deftest ert-test-pel-when-text-in ()
  "Test `pel-when-text-in'."
  (should (eq 42    (pel-when-text-in "hello" 42)))
  (should (eq nil   (pel-when-text-in "" 42)))
  (should (eq 'sym  (pel-when-text-in "x" 'sym)))
  (should (eq nil   (pel-when-text-in "" 'sym))))

(ert-deftest ert-test-pel-string-or-nil ()
  "Test `pel-string-or-nil'."
  (should (string= "hello" (pel-string-or-nil "hello")))
  (should (string= " "     (pel-string-or-nil " ")))
  (should (eq nil          (pel-string-or-nil ""))))

(ert-deftest ert-test-pel-string-for ()
  "Test `pel-string-for'."
  (should (string= "hello" (pel-string-for "hello")))
  (should (string= ""      (pel-string-for nil)))
  (should (string= ""      (pel-string-for ""))))

(ert-deftest ert-test-pel-string-when ()
  "Test `pel-string-when'."
  ;; Condition is non-nil and text is supplied → text.
  (should (string= "yes"   (pel-string-when t "yes")))
  (should (string= "found" (pel-string-when 42 "found")))
  ;; Condition is non-nil but no text → condition itself (must be string).
  (should (string= "hello" (pel-string-when "hello")))
  ;; Condition is nil → empty string.
  (should (string= "" (pel-string-when nil "yes")))
  (should (string= "" (pel-string-when nil))))

(ert-deftest ert-test-pel-string-spread ()
  "Test `pel-string-spread'."
  (should (string= "a b c" (pel-string-spread "abc")))
  (should (string= "a.b.c" (pel-string-spread "abc" ".")))
  (should (string= "a--b--c" (pel-string-spread "abc" "--")))
  ;; Single character: nothing to spread.
  (should (string= "a" (pel-string-spread "a")))
  ;; Two characters.
  (should (string= "a b" (pel-string-spread "ab"))))

(ert-deftest ert-test-pel-list-str ()
  "Test `pel-list-str'."
  (should (string= "a, b, c" (pel-list-str '(a b c))))
  (should (string= "foo"     (pel-list-str '(foo))))
  (should (string= ""        (pel-list-str '()))))

(ert-deftest ert-test-pel-title-case-to-dash-separated ()
  "Test `pel-title-case-to-dash-separated'."
  (should (string= "pdb-track-stack-from-shell-p"
                   (pel-title-case-to-dash-separated
                    "Pdb Track Stack From Shell P")))
  (should (string= "py--execute-use-temp-file-p"
                   (pel-title-case-to-dash-separated
                    "Py  Execute Use Temp File P")))
  (should (string= "flycheck-mode"
                   (pel-title-case-to-dash-separated "Flycheck Mode"))))

(ert-deftest ert-test-pel-grp-regex ()
  "Test `pel-grp-regex'."
  (should (string= "\\(foo\\)"   (pel-grp-regex "foo")))
  (should (string= "\\(foo\\)?"  (pel-grp-regex "foo" "?")))
  (should (string= "\\([0-9]+\\)*" (pel-grp-regex "[0-9]+" "*"))))

;; --

(ert-deftest ert-test-pel-use-or ()
  "Test `pel-use-or'."
  ;; Value satisfies check → return it.
  (should (string= "abc"  (pel-use-or "abc" #'pel-hastext "empty!")))
  ;; Value fails check → return alternative.
  (should (string= "empty!" (pel-use-or "" #'pel-hastext "empty!")))
  ;; With transform functions applied to a passing value.
  (should (string= "Abc." (pel-use-or "abc" #'pel-hastext 0
                                      #'capitalize
                                      (lambda (s)
                                        (pel-end-text-with-period s)))))
  ;; With transform functions: value fails check → alternative returned as-is.
  (should (eq 0 (pel-use-or "" #'pel-hastext 0
                             #'capitalize))))

;; --

(ert-deftest ert-test-pel-concat-strings-in-list ()
  "Test `pel-concat-strings-in-list'."
  (should (string= "abc"    (pel-concat-strings-in-list '("a" "b" "c"))))
  (should (string= "hello"  (pel-concat-strings-in-list '("hello"))))
  (should (string= ""       (pel-concat-strings-in-list '())))
  (should (string= "ab"     (pel-concat-strings-in-list '("a" "b")))))

(ert-deftest ert-test-pel-prepend-to ()
  "Test `pel-prepend-to' macro."
  (let ((lst '(1 2 3)))
    (pel-prepend-to lst '(10 11))
    (should (equal '(10 11 1 2 3) lst)))
  (let ((lst '()))
    (pel-prepend-to lst '(a b))
    (should (equal '(a b) lst))))

(ert-deftest ert-test-pel-nth-elt ()
  "Test `pel-nth-elt'."
  (should (eq 0   (pel-nth-elt 'a '(a b c d))))
  (should (eq 1   (pel-nth-elt 'b '(a b c d))))
  (should (eq 3   (pel-nth-elt 'd '(a b c d))))
  ;; Absent element → nil.
  (should (eq nil (pel-nth-elt 'z '(a b c d))))
  ;; Works with numbers.
  (should (eq 2   (pel-nth-elt 30 '(10 20 30 40)))))

(ert-deftest ert-test-pel-list-insert-before ()
  "Test `pel-list-insert-before'."
  (should (equal '(a new b c d) (pel-list-insert-before '(a b c d) 1 'new)))
  (should (equal '(new a b c d) (pel-list-insert-before '(a b c d) 0 'new)))
  ;; Negative index → prepend.
  (should (equal '(other a b c d) (pel-list-insert-before '(a b c d) -10 'other)))
  ;; Out-of-range index → error.
  (should-error (pel-list-insert-before '(a b c d) 4 'x))
  (should-error (pel-list-insert-before '(a b c d) 10 'x)))

(ert-deftest ert-test-pel-list-prepend-nth ()
  "Test `pel-list-prepend-nth'."
  (should (equal '(c a b d) (pel-list-prepend-nth '(a b c d) 2)))
  (should (equal '(b a c d) (pel-list-prepend-nth '(a b c d) 1)))
  ;; idx = 0 → list unchanged.
  (should (equal '(a b c d) (pel-list-prepend-nth '(a b c d) 0)))
  ;; Out-of-range → error.
  (should-error (pel-list-prepend-nth '(a b c d) 4))
  ;; Original list is not mutated.
  (let ((orig '(a b c d)))
    (pel-list-prepend-nth orig 2)
    (should (equal '(a b c d) orig))))

(ert-deftest ert-test-pel-list-insert-car-at ()
  "Test `pel-list-insert-car-at'."
  (should (equal '(b c a d) (pel-list-insert-car-at '(a b c d) 2)))
  (should (equal '(b a c d) (pel-list-insert-car-at '(a b c d) 1)))
  ;; Original list is not mutated.
  (let ((orig '(a b c d)))
    (pel-list-insert-car-at orig 2)
    (should (equal '(a b c d) orig))))

(ert-deftest ert-test-pel-delqs ()
  "Test `pel-delqs'."
  (should (equal '(b d)     (pel-delqs '(a c) '(a b c d))))
  (should (equal '(a b c d) (pel-delqs '(x y) '(a b c d))))
  (should (equal '()        (pel-delqs '(a b c d) '(a b c d))))
  (should (equal '(a b)     (pel-delqs '() '(a b)))))

;; --

(ert-deftest ert-test-pel-val-or-default ()
  "Test `pel-val-or-default'."
  (should (eq 42      (pel-val-or-default 42 99)))
  (should (eq 99      (pel-val-or-default nil 99)))
  (should (string= "x" (pel-val-or-default "x" "y")))
  (should (string= "y"  (pel-val-or-default nil "y")))
  (should (eq 'sym    (pel-val-or-default 'sym 'other))))

;; --

(defvar pel--test-toggle-flag nil
  "Dynamic flag variable used only by `ert-test-pel-toggle'.")

(ert-deftest ert-test-pel-toggle ()
  "Test `pel-toggle'."
  ;; Reset to a known state before each run.
  (setq pel--test-toggle-flag nil)
  (pel-toggle 'pel--test-toggle-flag)
  (should (eq t   pel--test-toggle-flag))
  (pel-toggle 'pel--test-toggle-flag)
  (should (eq nil pel--test-toggle-flag))
  ;; A second round-trip confirms symmetry.
  (pel-toggle 'pel--test-toggle-flag)
  (should (eq t   pel--test-toggle-flag))
  ;; Passing a non-symbol should signal an error.
  (should-error (pel-toggle 42))
  (should-error (pel-toggle "string")))

;; --

(ert-deftest ert-test-pel-hook-symbol-for ()
  "Test `pel-hook-symbol-for'."
  (should (eq 'emacs-lisp-mode-hook (pel-hook-symbol-for 'emacs-lisp-mode)))
  (should (eq 'text-mode-hook       (pel-hook-symbol-for 'text-mode)))
  (should (eq 'c-mode-hook          (pel-hook-symbol-for 'c-mode))))

;;; --------------------------------------------------------------------------
(provide 'pel-base-tests)

;;; pel-base-tests.el ends here
