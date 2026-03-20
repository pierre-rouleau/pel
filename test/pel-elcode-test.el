;;; pel-elcode-test.el --- Test pel-elcode.el.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, March 17 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-20 13:34:39 EDT, updated by Pierre Rouleau>

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
;; Test the pel-elcode.el file.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel-elcode)
(require 'pel--base)
(require 'pel-ert)
(require 'cl-lib)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;* `pel-elcode-operators-in'
;;  =========================

(ert-deftest ert-test-pel-elcode-operators-in ()
  "Test `pel-elcode-operators-in'."

  ;; If sexp is empty nothing gets returned
  (should-not (pel-elcode-operators-in ()))

  ;; A simple defun
  (should (equal
           (pel-elcode-operators-in
            '(defun div-by-2 (val) "A division by 2" (/ val 2)))
           '(defun /)))

  (should (equal
           (pel-elcode-operators-in
            '(defun my-func (x) (message "value: %s" (+ x 1))))
           '(defun message +)))


  (should (equal
           (pel-elcode-operators-in
            '(defun pel-inside-code (&optional pos)
               "Return non-nil when point or POS is in code, nil if in comment or string.
Note that this changes the search match data!"
               (let ((syntax (syntax-ppss (or pos (point)))))
                 (and (not (nth 3 syntax))
                      (not (nth 4 syntax))))))
           '(defun let syntax-ppss or point and not nth)))

  (should (equal
           (pel-elcode-operators-in
            '(defun pel-inside-code (&optional pos)
               "Return non-nil when point or POS is in code, nil if in comment or string.
Note that this changes the search match data!"
               (let ((syntax (syntax-ppss (or pos (point))))
                     (just-for-test (* (+ (/ 33 22) 9) pos)))
                 (and (not (nth 3 syntax))
                      (not (nth 4 syntax))))))
           '(defun let syntax-ppss or point * + / and not nth)))


  ;; Test that the declare, pure and side-effect-free are not
  ;; included in the list of symbols extracted: they should not be
  ;; extracted.
  (should (equal
           (pel-elcode-operators-in
            '(defun pel-expression-p (val)
               "Return non-nil if VAL is an expression, nil if it is a value.
Return nil for t and nil.
Return t for \\='some-symbols or \\='(some expressions), nothing else.
Meant to be used to identify code that is quoted (for delayed
code execution)."
               (declare (pure t) (side-effect-free error-free))
               (and (not (eq val t))
                    (not (eq val nil))
                    (or (symbolp val)
                        (consp val)))))
           '(defun and not eq or symbolp consp))))


(ert-deftest ert-test-pel-elcode-operators-in--empty ()
  "Empty / atom inputs must yield nil."
  ;; Truly empty
  (should-not (pel-elcode-operators-in ()))
  ;; Bare atoms are not operator lists
  (should-not (pel-elcode-operators-in 42))
  (should-not (pel-elcode-operators-in 3.14))
  (should-not (pel-elcode-operators-in "hello"))
  (should-not (pel-elcode-operators-in t))
  (should-not (pel-elcode-operators-in nil))
  ;; A lone symbol is not a call; no operator is extracted
  (should-not (pel-elcode-operators-in 'some-variable)))

(ert-deftest ert-test-pel-elcode-operators-in--simple-defun ()
  "Simple defun forms."
  (should (equal
           (pel-elcode-operators-in
            '(defun div-by-2 (val) "A division by 2" (/ val 2)))
           '(defun /)))

  (should (equal
           (pel-elcode-operators-in
            '(defun my-func (x) (message "value: %s" (+ x 1))))
           '(defun message +))))

(ert-deftest ert-test-pel-elcode-operators-in--defsubst ()
  "`defsubst' is treated identically to `defun': name and arg-list are skipped."
  (should (equal
           (pel-elcode-operators-in
            '(defsubst my-subst (x) (+ x 1)))
           '(defsubst +)))

  (should (equal
           (pel-elcode-operators-in
            '(defsubst pel-expression-p (val)
               (and (not (eq val t))
                    (not (eq val nil))
                    (or (symbolp val)
                        (consp val)))))
           '(defsubst and not eq or symbolp consp))))

(ert-deftest ert-test-pel-elcode-operators-in--lambda ()
  "`lambda': arg-list is skipped; body is processed."
  (should (equal
           (pel-elcode-operators-in
            '(lambda (x) (+ x 1)))
           '(lambda +)))

  (should (equal
           (pel-elcode-operators-in
            '(lambda (x y) (if (> x y) x y)))
           '(lambda if >))))

(ert-deftest ert-test-pel-elcode-operators-in--let ()
  "`let' and `let*': variable names skipped; binding values and body processed."
  (should (equal
           (pel-elcode-operators-in
            '(defun pel-inside-code (&optional pos)
               (let ((syntax (syntax-ppss (or pos (point)))))
                 (and (not (nth 3 syntax))
                      (not (nth 4 syntax))))))
           '(defun let syntax-ppss or point and not nth)))

  ;; Multiple bindings with expressions
  (should (equal
           (pel-elcode-operators-in
            '(defun pel-inside-code (&optional pos)
               (let ((syntax (syntax-ppss (or pos (point))))
                     (just-for-test (* (+ (/ 33 22) 9) pos)))
                 (and (not (nth 3 syntax))
                      (not (nth 4 syntax))))))
           '(defun let syntax-ppss or point * + / and not nth)))

  ;; let* is handled the same way
  (should (equal
           (pel-elcode-operators-in
            '(let* ((a (+ 1 2))
                    (b (* a 3)))
               (- b a)))
           '(let* + * -))))

(ert-deftest ert-test-pel-elcode-operators-in--declare-skipped ()
  "`declare' forms are never added to the operator list."
  (should (equal
           (pel-elcode-operators-in
            '(defun pel-expression-p (val)
               (declare (pure t) (side-effect-free error-free))
               (and (not (eq val t))
                    (not (eq val nil))
                    (or (symbolp val)
                        (consp val)))))
           '(defun and not eq or symbolp consp))))

(ert-deftest ert-test-pel-elcode-operators-in--quote ()
  "`quote' (i.e. \\='foo) IS extracted as an operator symbol.
It is the job of `pel-elcode-properties-of-sexp' to filter it out as
non-impacting."
  ;; A bare quoted atom
  (should (equal (pel-elcode-operators-in '(quote foo))
                 '(quote)))

  ;; quote inside a defun body
  (should (equal
           (pel-elcode-operators-in
            '(defun check-mode (mode)
               (eq mode 'text-mode)))       ; 'text-mode => (quote text-mode)
           '(defun eq quote)))

  ;; Multiple quoted atoms — duplicates are removed
  (should (equal
           (pel-elcode-operators-in
            '(defun f (x)
               (or (eq x 'a)
                   (eq x 'b)
                   (eq x 'c))))
           '(defun or eq quote))))

(ert-deftest ert-test-pel-elcode-operators-in--function ()
  "`function' (i.e. #\\='foo) IS extracted as an operator symbol.
It is the job of `pel-elcode-properties-of-sexp' to filter it out as
non-impacting."
  ;; Bare function form
  (should (equal (pel-elcode-operators-in '(function identity))
                 '(function)))

  ;; #'foo inside a defun body
  (should (equal
           (pel-elcode-operators-in
            '(defun f (lst)
               (mapcar #'identity lst)))    ; #'identity => (function identity)
           '(defun mapcar function))))

(ert-deftest ert-test-pel-elcode-operators-in--non-symbol-head ()
  "List whose car is not a symbol (e.g. an immediately-invoked lambda)."
  ;; ((lambda (x) (+ x 1)) 5) — top-level head is a list, not a symbol
  (should (equal
           (pel-elcode-operators-in
            '((lambda (x) (+ x 1)) 5))
           '(lambda +))))

(ert-deftest ert-test-pel-elcode-operators-in--deduplication ()
  "Each operator symbol appears at most once, in first-appearance order."
  (should (equal
           (pel-elcode-operators-in
            '(defun f (x)
               (+ (+ x 1) (+ x 2))))
           '(defun +))))

(ert-deftest ert-test-pel-elcode-operators-in--flow-control ()
  "`if', `when', `unless', `cond', `progn', `prog1', `while', `dolist'
are all captured (they get filtered later in `pel-elcode-properties-of-sexp')."
  (should (equal
           (pel-elcode-operators-in
            '(defun f (x)
               (if (> x 0)
                   (progn
                     (when (< x 10) x)
                     (unless (= x 5) x))
                 0)))
           '(defun if > progn when < unless =)))

  (should (equal
           (pel-elcode-operators-in
            '(defun f (lst)
               (let ((acc 0))
                 (dolist (x lst acc)
                   (setq acc (+ acc x))))))
           '(defun let dolist +))))

;; ---------------------------------------------------------------------------
;;* `pel-elcode-properties-of-sexp'
;;   =============================

(ert-deftest ert-test-pel-elcode-properties-of-sexp ()
  "Test `pel-elcode-properties-of-sexp'."

  (should-not (pel-elcode-properties-of-sexp
               '(defun pel-inside-code (&optional pos)
                  "Return non-nil when point or POS is in code, nil if in comment or string.
Note that this changes the search match data!"
                  (let ((syntax (syntax-ppss (or pos (point)))))
                    (and (not (nth 3 syntax))
                         (not (nth 4 syntax)))))))

  (should (equal
           (pel-elcode-properties-of-sexp
            '(defun pel-user-option-p (symbol)
               "Return t when SYMBOL is a valid PEL user-option, nil otherwise."
               (declare (side-effect-free t))
               (and (custom-variable-p symbol)
                    (eq t (compare-strings "pel-use-" nil nil
                                           (symbol-name symbol) 0 8)))))
           '(declare (side-effect-free t))))

  (should (equal
           (pel-elcode-properties-of-sexp
            '(defun pel-expression-p (val)
               "Return non-nil if VAL is an expression, nil if it is a value.
  Return nil for t and nil.
  Return t for \\='some-symbols or \\='(some expressions), nothing else.
  Meant to be used to identify code that is quoted (for delayed
  code execution)."
               (declare (pure t) (side-effect-free error-free))
               (and (not (eq val t))
                    (not (eq val nil))
                    (or (symbolp val)
                        (consp val)))))
           ;; In Emacs 26 and 27, the 'not function is not declared pure.
           ;; It is declared pure in Emacs 28 and later.
           (if pel-emacs-28-or-later-p
               '(declare (pure t) (side-effect-free error-free))
             '(declare (side-effect-free error-free))))))

(ert-deftest ert-test-pel-elcode-properties-of-sexp-stability ()
  "Regression: repeated extraction should remain stable."
  (let* ((sexp '(defun pel-expression-p (val)
                  (declare (pure t) (side-effect-free error-free))
                  (and (not (eq val t))
                       (not (eq val nil))
                       (or (symbolp val) (consp val)))))
         (expected (if pel-emacs-28-or-later-p
                       '(declare (pure t) (side-effect-free error-free))
                     '(declare (side-effect-free error-free)))))
    (dotimes (_ 200)
      (should (equal (pel-elcode-properties-of-sexp sexp) expected)))))

(ert-deftest ert-test-pel-elcode-properties-of-sexp--nil-input ()
  "Empty / nil input returns nil."
  (should-not (pel-elcode-properties-of-sexp ()))
  (should-not (pel-elcode-properties-of-sexp nil)))

(ert-deftest ert-test-pel-elcode-properties-of-sexp--impure ()
  "A defun that calls side-effecting functions has no properties."
  (should-not (pel-elcode-properties-of-sexp
               '(defun pel-inside-code (&optional pos)
                  "Return non-nil when point or POS is in code."
                  (let ((syntax (syntax-ppss (or pos (point)))))
                    (and (not (nth 3 syntax))
                         (not (nth 4 syntax))))))))

(ert-deftest ert-test-pel-elcode-properties-of-sexp--side-effect-free ()
  "A defun that is only side-effect-free (not pure, not error-free)."
  (should (equal
           (pel-elcode-properties-of-sexp
            '(defun pel-user-option-p (symbol)
               "Return t when SYMBOL is a valid PEL user-option, nil otherwise."
               (declare (side-effect-free t))
               (and (custom-variable-p symbol)
                    (eq t (compare-strings "pel-use-" nil nil
                                           (symbol-name symbol) 0 8)))))
           '(declare (side-effect-free t)))))

(ert-deftest ert-test-pel-elcode-properties-of-sexp--pure-and-sef ()
  "A defun that is pure AND side-effect-free (error-free)."
  (should (equal
           (pel-elcode-properties-of-sexp
            '(defun pel-expression-p (val)
               "Return non-nil if VAL is an expression."
               (declare (pure t) (side-effect-free error-free))
               (and (not (eq val t))
                    (not (eq val nil))
                    (or (symbolp val)
                        (consp val)))))
           (if pel-emacs-28-or-later-p
               '(declare (pure t) (side-effect-free error-free))
             '(declare (side-effect-free error-free))))))

(ert-deftest ert-test-pel-elcode-properties-of-sexp--quote-non-impacting ()
  "A defun whose only operators beyond pure predicates are `quote' forms
must still be recognized as pure and side-effect-free.
This is the key regression protected by the PR that adds `quote' and
`function' to `pel-elcode-non-impacting-operators'."
  ;; 'text-mode expands to (quote text-mode); quote must not degrade purity.
  (should (equal
           (pel-elcode-properties-of-sexp
            '(defun check-text-mode (mode)
               "Return t when MODE is text-mode."
               (eq mode 'text-mode)))
           (if pel-emacs-28-or-later-p
               '(declare (pure t) (side-effect-free error-free))
             '(declare (side-effect-free error-free)))))

  ;; Multiple quoted atoms — still pure
  (should (equal
           (pel-elcode-properties-of-sexp
            '(defun one-of-three (x)
               (or (eq x 'a) (eq x 'b) (eq x 'c))))
           (if pel-emacs-28-or-later-p
               '(declare (pure t) (side-effect-free error-free))
             '(declare (side-effect-free error-free))))))

(ert-deftest ert-test-pel-elcode-properties-of-sexp--function-non-impacting ()
  "A defun that uses #\\='fn (i.e. the `function' special form) to pass a
pure function reference must not have its purity degraded."
  ;; mapcar with a pure function reference
  ;; Note: mapcar itself is not pure/sef-free in standard Emacs Lisp,
  ;; so this test confirms `function' is filtered before properties are computed.
  ;; We use only pure operators so the result depends solely on them.
  (should (equal
           (pel-elcode-properties-of-sexp
            '(defun check-val (val)
               (and (not (eq val nil))
                    (symbolp val)
                    (functionp #'identity))))
           ;; functionp is side-effect-free; #'identity adds `function' which
           ;; must be filtered; `and', `not' are non-impacting.
           ;; Result depends on what Emacs reports for `functionp'.
           ;; We at minimum verify the call does not signal an error.
           (if pel-emacs-30-or-later-p
               '(declare (side-effect-free error-free))
             ;; Prior to Emacs 30, functionp was not declared pure nor side-effect-free
             nil))))

(ert-deftest ert-test-pel-elcode-properties-of-sexp--defsubst ()
  "`defsubst' is treated the same as `defun'."
  (should (equal
           (pel-elcode-properties-of-sexp
            '(defsubst pel-expression-p (val)
               (and (not (eq val t))
                    (not (eq val nil))
                    (or (symbolp val)
                        (consp val)))))
           (if pel-emacs-28-or-later-p
               '(declare (pure t) (side-effect-free error-free))
             '(declare (side-effect-free error-free))))))

(ert-deftest ert-test-pel-elcode-properties-of-sexp--only-non-impacting ()
  "A defun whose body contains *only* non-impacting operators (flow control)
and pure predicates must yield a full pure+side-effect-free result."
  (should (equal
           (pel-elcode-properties-of-sexp
            '(defun f (x y)
               (if (and (symbolp x) (symbolp y))
                   (or (eq x y) (eq x nil))
                 nil)))
           (if pel-emacs-28-or-later-p
               '(declare (pure t) (side-effect-free error-free))
             '(declare (side-effect-free error-free))))))

;; ---------------------------------------------------------------------------
;;* `pel-elcode-properties-of-sexp-at-point'
;;  ========================================


(ert-deftest ert-test-pel-elcode-properties-of-sexp-at-point ()
  "Ensure at-point API delegates consistently to `pel-elcode-properties-of-sexp'."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun pel-expression-p (val)\n"
            "  (declare (pure t) (side-effect-free error-free))\n"
            "  (and (not (eq val t))\n"
            "       (not (eq val nil))\n"
            "       (or (symbolp val) (consp val))))")
    (goto-char (point-min))
    (let ((expected (if pel-emacs-28-or-later-p
                        '(declare (pure t) (side-effect-free error-free))
                      '(declare (side-effect-free error-free)))))
      (should (equal (pel-elcode-properties-of-sexp-at-point) expected)))))

(ert-deftest ert-test-pel-elcode-properties-of-sexp-at-point--explicit-pos ()
  "The optional POS argument moves point before reading."
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; Insert two sexps; we want to read the second one by position.
    (insert "42\n"
            "(defun f (x) (symbolp x))")
    ;; Find the start of the second sexp
    (goto-char (point-min))
    (forward-line 1)
    (let ((pos (point)))
      (goto-char (point-min))           ; move away from target
      (should (equal (pel-elcode-properties-of-sexp-at-point pos)
                     (if pel-emacs-28-or-later-p
                         '(declare (pure t) (side-effect-free error-free))
                       '(declare (side-effect-free error-free))))))))

(ert-deftest ert-test-pel-elcode-properties-of-sexp-at-point--impure ()
  "At-point correctly returns nil for a side-effecting defun."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun f () (message \"hi\"))")
    (goto-char (point-min))
    (should-not (pel-elcode-properties-of-sexp-at-point))))

(ert-deftest ert-test-pel-elcode-properties-of-sexp-at-point--quote ()
  "At-point: `quote' forms must not degrade purity (PR regression guard)."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun check-mode (mode) (eq mode 'text-mode))")
    (goto-char (point-min))
    (should (equal (pel-elcode-properties-of-sexp-at-point)
                   (if pel-emacs-28-or-later-p
                       '(declare (pure t) (side-effect-free error-free))
                     '(declare (side-effect-free error-free)))))))

;; ---------------------------------------------------------------------------
;;* `pel-elcode-print-properties-of-sexp-at-point'
;;  ==============================================

(ert-deftest ert-test-pel-elcode-print-properties-of-sexp-at-point--nil ()
  "When no properties apply: kill-ring is unchanged and message shows nil."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun f () (message \"hi\"))")
    (goto-char (point-min))
    (let ((kill-ring nil)
          (captured-message nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq captured-message (apply #'format fmt args)))))
        (pel-elcode-print-properties-of-sexp-at-point))
      ;; `kill-new' must NOT have been called when props is nil
      (should-not kill-ring)
      ;; But message must still be called with "nil"
      (should (equal captured-message "nil")))))

(ert-deftest ert-test-pel-elcode-print-properties-of-sexp-at-point--pure ()
  "When properties exist: result is pushed to kill-ring and shown via message."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun pel-expression-p (val)\n"
            "  (declare (pure t) (side-effect-free error-free))\n"
            "  (and (not (eq val t))\n"
            "       (not (eq val nil))\n"
            "       (or (symbolp val) (consp val))))")
    (goto-char (point-min))
    (let ((kill-ring nil)
          (captured-message nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq captured-message (apply #'format fmt args)))))
        (pel-elcode-print-properties-of-sexp-at-point))
      (let ((expected (if pel-emacs-28-or-later-p
                          '(declare (pure t) (side-effect-free error-free))
                        '(declare (side-effect-free error-free)))))
        ;; Result is pushed into the kill ring
        (should (equal (car kill-ring) (format "%S" expected)))
        ;; And displayed via message
        (should (equal captured-message (format "%S" expected)))))))

(ert-deftest ert-test-pel-elcode-print-properties-of-sexp-at-point--quote ()
  "PR regression: `quote' forms must not degrade purity in the printed output."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun check-mode (mode) (eq mode 'text-mode))")
    (goto-char (point-min))
    (let ((kill-ring nil)
          (captured-message nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq captured-message (apply #'format fmt args)))))
        (pel-elcode-print-properties-of-sexp-at-point))
      ;; The printed message should be in kill ring
      (should (equal captured-message
                     (car kill-ring)))
      ;; and should be the following:
      (should (equal (car kill-ring)
                     (if pel-emacs-28-or-later-p
                         "(declare (pure t) (side-effect-free error-free))"
                       "(declare (side-effect-free error-free))"))))))

(ert-deftest ert-test-pel-elcode-print-properties-of-sexp-at-point--side-effect-free-only ()
  "A side-effect-free (non-error-free) defun: correct kill-ring content."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun pel-user-option-p (symbol)\n"
            "  (and (custom-variable-p symbol)\n"
            "       (eq t (compare-strings \"pel-use-\" nil nil\n"
            "                              (symbol-name symbol) 0 8))))")
    (goto-char (point-min))
    (let ((kill-ring nil))
      (cl-letf (((symbol-function 'message) #'ignore))
        (pel-elcode-print-properties-of-sexp-at-point))
      (should (equal (car kill-ring)
                     "(declare (side-effect-free t))")))))

;; ---------------------------------------------------------------------------
;;* `pel-elcode-operators-in-sexp-at-point'
;;  ========================================

(ert-deftest ert-test-pel-elcode-operators-in-sexp-at-point--basic ()
  "At-point variant returns same operators as the sexp-based API."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun f (x) (symbolp x))")
    (goto-char (point-min))
    (should (equal (pel-elcode-operators-in-sexp-at-point)
                   '(defun symbolp)))))

(ert-deftest ert-test-pel-elcode-operators-in-sexp-at-point--explicit-pos ()
  "The optional POS argument moves point before reading the sexp."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "42\n"
            "(defun f (x) (+ x 1))")
    (goto-char (point-min))
    (forward-line 1)
    (let ((pos (point)))
      (goto-char (point-min))           ; move away from target
      (should (equal (pel-elcode-operators-in-sexp-at-point pos)
                     '(defun +))))))

(ert-deftest ert-test-pel-elcode-operators-in-sexp-at-point--quote-filtered ()
  "`quote' is extracted by operators-in but shows up in the result since
filtering is done at the properties level, not here."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun check-mode (mode) (eq mode 'text-mode))")
    (goto-char (point-min))
    ;; quote IS in the operator list here; it is filtered by properties-of-sexp
    (should (equal (pel-elcode-operators-in-sexp-at-point)
                   '(defun eq quote)))))

(ert-deftest ert-test-pel-elcode-operators-in-sexp-at-point--dolist-local-setq ()
  "At-point: local setq inside dolist is filtered."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun sum-list (lst)\n"
            "  (let ((acc 0))\n"
            "    (dolist (x lst acc)\n"
            "      (setq acc (+ acc x)))))")
    (goto-char (point-min))
    (should (equal (pel-elcode-operators-in-sexp-at-point)
                   '(defun let dolist +)))))

;; ---------------------------------------------------------------------------
;;* Local `setq' filtering — explicit tests
;;  ----------------------------------------

(ert-deftest ert-test-pel-elcode-operators-in--setq-on-defun-arg ()
  "`setq' targeting a defun argument (locally bound) must be filtered out."
  (should-not (memq 'setq
                    (pel-elcode-operators-in
                     '(defun normalize (x)
                        (setq x (abs x))
                        x))))
  ;; Only defun and abs should remain
  (should (equal (pel-elcode-operators-in
                  '(defun normalize (x)
                     (setq x (abs x))
                     x))
                 '(defun abs))))

(ert-deftest ert-test-pel-elcode-operators-in--setq-on-let-local ()
  "`setq' targeting a `let'-bound variable must be filtered."
  (should (equal
           (pel-elcode-operators-in
            '(defun f (n)
               (let ((acc 0))
                 (setq acc (+ acc n)))))
           '(defun let +))))            ; setq is gone

(ert-deftest ert-test-pel-elcode-operators-in--setq-on-let*-local ()
  "`setq' targeting a `let*'-bound variable must be filtered."
  (should (equal
           (pel-elcode-operators-in
            '(defun f (n)
               (let* ((acc 0))
                 (setq acc (* acc n)))))
           '(defun let* *))))           ; setq is gone

(ert-deftest ert-test-pel-elcode-operators-in--setq-on-dotimes-var ()
  "The `dotimes' iteration variable is locally bound; `setq' on it is filtered."
  (should (equal
           (pel-elcode-operators-in
            '(defun f (n)
               (let ((acc 0))
                 (dotimes (i n acc)
                   (setq acc (+ acc i))))))
           '(defun let dotimes +))))    ; setq filtered, i is local

(ert-deftest ert-test-pel-elcode-operators-in--setq-on-dolist-var ()
  "The `dolist' iteration variable is locally bound; `setq' on it is filtered.
(The accumulator `acc' is bound by `let', also local.)"
  (should (equal
           (pel-elcode-operators-in
            '(defun sum-list (lst)
               (let ((acc 0))
                 (dolist (x lst acc)
                   (setq acc (+ acc x))))))
           '(defun let dolist +))))    ; setq filtered

(ert-deftest ert-test-pel-elcode-operators-in--setq-on-global ()
  "`setq' targeting a non-local (global) variable must be KEPT."
  (should (memq 'setq
                (pel-elcode-operators-in
                 '(defun f (x)
                    (setq some-global-counter x)))))
  ;; Exact operators: defun, setq
  (should (equal (pel-elcode-operators-in
                  '(defun f (x)
                     (setq some-global-counter x)))
                 '(defun setq))))

(ert-deftest ert-test-pel-elcode-operators-in--setq-mixed-local-global ()
  "A `setq' with at least one non-local target must be KEPT (not filtered)."
  ;; `acc' is local (let-bound), but `global-flag' is not → setq stays
  (should (memq 'setq
                (pel-elcode-operators-in
                 '(defun f (lst)
                    (let ((acc 0))
                      (setq acc 1
                            global-flag t)))))))

(ert-deftest ert-test-pel-elcode-operators-in--setq-multiple-locals ()
  "A `setq' with ALL targets locally bound is fully filtered."
  ;; Both `a' and `b' are let-bound
  (should-not (memq 'setq
                    (pel-elcode-operators-in
                     '(defun f (n)
                        (let ((a 0)
                              (b 0))
                          (setq a (+ n 1)
                                b (* n 2))
                          (+ a b)))))))

;; ---------------------------------------------------------------------------
;;* Unknown macro expansion
;;  -----------------------

(ert-deftest ert-test-pel-elcode-operators-in--unknown-macro-expanded ()
  "An unknown macro (not in `pel-elcode-structural-forms' or
`pel-elcode-non-impacting-operators') is expanded with `macroexpand-1'.
Its internal operators become visible.  `pcase' is the canonical example.

We assert membership rather than exact equality because pcase's expansion
is Emacs-version-dependent."
  (let ((ops (pel-elcode-operators-in
              '(defun classify (x)
                 (pcase x
                   ((pred symbolp) 'sym)
                   ((pred numberp) 'num)
                   (_ 'other))))))
    ;; defun itself must be present
    (should (memq 'defun ops))
    ;; Predicates supplied by the user must survive expansion
    (should (memq 'symbolp ops))
    (should (memq 'numberp ops))
    ;; pcase itself must NOT appear: it was replaced by its expansion's head
    (should-not (memq 'pcase ops))))

(ert-deftest ert-test-pel-elcode-operators-in--non-impacting-macro-not-expanded ()
  "`dolist' is in both `pel-elcode-non-impacting-operators' AND
`pel-elcode-structural-forms': it must NOT be expanded.
Its implementation-detail operators (`car', `cdr', internal `setq' for
the loop variable) must stay hidden."
  (let ((ops (pel-elcode-operators-in
              '(defun f (lst)
                 (dolist (x lst)
                   (+ x 1))))))
    ;; dolist IS in the result (as itself, not expanded away)
    (should (memq 'dolist ops))
    ;; Internal macro operators must NOT appear
    (should-not (memq 'car  ops))
    (should-not (memq 'cdr  ops))
    (should-not (memq 'while ops))))

(ert-deftest ert-test-pel-elcode-operators-in--dotimes-not-expanded ()
  "`dotimes' is listed in both constants and must not be expanded."
  (let ((ops (pel-elcode-operators-in
              '(defun f (n)
                 (dotimes (i n)
                   (+ i 1))))))
    (should (memq 'dotimes ops))
    (should-not (memq 'while ops))
    (should-not (memq 'car   ops))))

;; ---------------------------------------------------------------------------
;;* `pel-elcode-properties-of-sexp' — dolist/dotimes accumulator pattern
;;  ---------------------------------------------------------------------

(ert-deftest ert-test-pel-elcode-properties-of-sexp--dolist-accumulator-pure ()
  "A defun using dolist with a local accumulator mutated by setq must be
rated pure + side-effect-free once local-setq filtering removes `setq'.
This is the key end-to-end regression test for the new local-var tracking."
  ;; After filtering: operators are defun, let, dolist, +, *
  ;; let/dolist filtered as non-impacting; defun removed; +/* are pure/sef-free.
  (should (equal
           (pel-elcode-properties-of-sexp
            '(defun sum-squares (lst)
               "Return the sum of squares of numbers in LST."
               (let ((acc 0))
                 (dolist (x lst acc)
                   (setq acc (+ acc (* x x)))))))
           (if pel-emacs-28-or-later-p
               '(declare (pure t) (side-effect-free t))
             '(declare (side-effect-free t))))))

(ert-deftest ert-test-pel-elcode-properties-of-sexp--dotimes-accumulator-pure ()
  "Same pattern using `dotimes': local setq is filtered → pure+sef-free."
  (should (equal
           (pel-elcode-properties-of-sexp
            '(defun count-squares (n)
               "Return sum of squares 0..N-1."
               (let ((acc 0))
                 (dotimes (i n acc)
                   (setq acc (+ acc (* i i)))))))
           (if pel-emacs-28-or-later-p
               '(declare (pure t) (side-effect-free t))
             '(declare (side-effect-free t))))))

(ert-deftest ert-test-pel-elcode-properties-of-sexp--global-setq-impure ()
  "A defun that uses `setq' on a non-local binding must NOT be rated pure."
  (should-not
   (pel-elcode-properties-of-sexp
    '(defun f (x)
       (setq some-global x)
       x))))

(ert-deftest ert-test-pel-elcode-properties-of-sexp--setq-on-arg-pure ()
  "A defun that only `setq's its own argument (local) must be rated pure+sef-free."
  (should (equal
           (pel-elcode-properties-of-sexp
            '(defun normalize (x)
               (setq x (abs x))
               x))
           (if pel-emacs-28-or-later-p
               '(declare (pure t) (side-effect-free t))
             '(declare (side-effect-free t))))))

;;; --------------------------------------------------------------------------
(provide 'pel-elcode-test)

;;; pel-elcode-test.el ends here
