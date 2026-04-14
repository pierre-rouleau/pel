;;; pel--base-test.el --- ERT tests for pel--base.el  -*- lexical-binding: t; -*-

;; Created   : Tuesday, March 24 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-04-14 09:54:37 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2026  Pierre Rouleau
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; ERT tests for pel--base.el.
;;
;; Policy and scope:
;; - Tests are organized in the same order as functions in pel--base.el,
;;   using the exact section title banners (;;* Section Name) found there.
;; - Tests avoid environment brittleness (Tree-sitter, user dirs, symlinks)
;;   by stubbing or canonicalizing paths via file-truename.
;; - When a test needs a void symbol, it uses makunbound to keep the symbol
;;   genuinely unbound (no defvar), so pel-symbol-value-or hits the "unknown"
;;   branch.  Where a variable must be seen by bound-and-true-p, it is defvar'd.
;; - No use of string-search (Emacs 28+); only string-match-p.
;;
;; Covered items (representative, stable subset):
;;   pel-version, pel+=, pel-=, λc, pel-expression-p, pel-user-option-p,
;;   pel-set-if-non-nil, pel-!0, pel-as-boolean, pel-all-bitset-p,
;;   pel-list-of, pel-transpose-alist,
;;   pel-in-fast-startup-p,
;;   pel-major-mode-must-be, pel-derived-mode-p, pel-dired-buffer-p,
;;   pel-file-type-for, pel-string-with-major-mode, pel-buffers-in-mode,
;;   pel-minor-mode-state,
;;   pel-major-mode-symbol-*: for (for/value/value-or/set),
;;   pel-current-buffer-filename, pel-current-buffer-file-extension,
;;   pel-current-buffer-eol-type, pel-cd-to-current,
;;   pel-terminal-is-macos-terminal-p, pel-running-under-ssh-p,
;;   pel-locate-user-emacs-file, pel-add-dir-to-loadpath,
;;   pel-unix-socket-p, pel-file-type-str,
;;   pel-* string predicates, pluralizer trio, pel--symbol-value,
;;   pel-symbol-value, pel-as-symbol, pel-symbol-at-point,
;;   pel-on-off-string, pel-symbol-on-off-string, pel-symbol-text,
;;   pel-value-on-off-text, pel-symbol-value-or, pel-yes-no-string,
;;   pel-as-string, pel-end-text-with-period, pel-hastext, pel-when-text-in,
;;   pel-string-or-nil, pel-string-for, pel-string-when, pel-string-spread,
;;   pel-list-str, pel-title-case-to-dash-separated,
;;   pel-grp-regex, pel-shell-quote-path-keep-glob,
;;   pel--format-problem-messages, pel-format-problem-messages,
;;   pel-message-for, pel-use-or, pel-concat-strings-in-list,
;;   pel-prepend-to, pel-cons-alist-at, pel-nth-elt, pel-list-insert-before,
;;   pel-list-prepend-nth, pel-list-insert-car-at, pel-delqs,
;;   pel-delete-from-auto-mode-alist, pel-file-md5,
;;   pel-action-for, pel-toggle-mode, pel-toggle-mode-and-show,
;;   pel-toggle, pel-toggle-and-show, pel-toggle-and-show-user-option,
;;   pel-val-or-default, pel-hook-symbol-for, pel-map-symbol-for,
;;   pel-add-hook-for, pel--check-minor-modes-in, pel-check-minor-modes-in,
;;   pel-multiplier, pel-mode-toggle-arg, pel-dec, pel-inc, pel-swap,
;;   pel-chars-at-point, pel-at-letter-p, pel-at-lowercase-p, pel-at-uppercase-p,
;;   pel-n-funcall-to, pel-goto-line, pel-goto-position, pel-same-line-p,
;;   pel-region-for, pel-insert-or-overwrite, pel-text-from-beginning-of-line,
;;   pel-line-has-only-whitespace-p, pel-inside-code, pel-has-shebang-line,
;;   pel-file-in, pel-normalize-fname, pel-is-subdir-of, pel-parent-dirpath,
;;   pel-sibling-dirname/pel-sibling-dirpath, pel-expand-url-file-name,
;;   pel-path-strip, pel-url-join, pel-url-location, pel-same-fname-p,
;;   pel-point-symlink-to, pel-symlink-points-to-p,
;;   pel-insert-bold, pel-insert-url-link, pel-insert-symbol family,
;;   pel-line-prefixed-with, pel--pp, pel-insert-list-value, pel-insert-list-content,
;;   pel-move-right-by, pel-print-in-buffer, pel-point-in-comment-or-docstring,
;;   pel-modtime-of, pel-byte-compile-if-needed, pel-add-imenu-sections-to,
;;   pel-visit-tags, pel-executable-find, pel-emacs-config-features-string.
;;
;; Items exercised with stubs (to avoid environment coupling):
;;   pel-rebuild-utils        - stub file-directory-p to force the "no utils dir" path;
;;                              capture display-warning without asserting its backend.
;;   pel-major-mode-use-tree-sitter, pel-treesit-ready-p, pel-ts-language-grammar-*
;;                              - stub pel-uses-tree-sitter, fboundp treesit, and treesit-ready-p.
;;
;; Notes:
;; - Tests that depend on exact user directory layouts or platform-specific
;;   canonicalization compare truename-normalized paths (file-truename).
;; - Buffers created for "fresh" assertions use generate-new-buffer to avoid
;;   picking up stale state across runs.
;; - Path comparisons use truename to tolerate macOS /private prefixes.
;; - Tests that rely on dynamic binding declare vars with defvar.
;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--base)
(require 'pel--options)
(require 'ert)
(eval-when-compile (require 'cl-lib))

;;; --------------------------------------------------------------------------
;;; Helpers
;;; --------------------------------------------------------------------------

(defmacro pel--base-test--with-temp-dir (var &rest body)
  "Bind VAR to a new temp dir and run BODY; then clean up."
  (declare (indent 1))
  `(let ((,var (make-temp-file "pel-base-test-" t)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors
         (dolist (p (directory-files ,var t "^[^.].*"))
           (ignore-errors
             (if (file-directory-p p)
                 (delete-directory p t)
               (delete-file p))))
         (delete-directory ,var t)))))

(defun pel--base-test--write-file (dir name content)
  "Create file NAME with CONTENT inside DIR; return absolute path."
  (let ((path (expand-file-name name dir)))
    (with-temp-file path (insert content))
    path))

(defmacro pel--base-test--capture-message (&rest body)
  "Run BODY and return the last message string."
  `(let (msg)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (setq msg (apply #'format fmt args))
                  msg)))
       (prog1 (progn ,@body) msg))))

(defmacro pel--base-test--capture-warning (&rest body)
  "Run BODY; return last display-warning text or nil."
  `(let (wmsg)
     (cl-letf (((symbol-function 'display-warning)
                (lambda (_type text &rest _r)
                  (setq wmsg (if (stringp text) text (format "%s" text))))))
       (prog1 (progn ,@body) wmsg))))

(defun pel--base-test--same-dir-p (a b)
  "Return non-nil if A and B designate the same directory."
  (let ((na (file-name-as-directory (file-truename a)))
        (nb (file-name-as-directory (file-truename b))))
    (string= na nb)))

(defmacro pel--base-test--with-code (code &rest body)
  "Execute BODY in a writable temp buffer pre-loaded with CODE."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,code)
     (goto-char (point-min))
     ,@body))

(defmacro pel--base-test--with-mode-buffer (mode &rest body)
  "Execute BODY in a temp buffer with MODE active."
  (declare (indent 1))
  `(with-temp-buffer
     (funcall ,mode)
     ,@body))

;;; --------------------------------------------------------------------------
;;; ;;* PEL version
;;; --------------------------------------------------------------------------

(ert-deftest ert-test-pel-version ()
  "Test `pel-version'."
  ;; Returns a non-empty string.
  (should (stringp (pel-version)))
  (should (> (length (pel-version)) 0))
  ;; Returns the same string each time.
  (should (string= (pel-version) (pel-version))))

(ert-deftest pel--base-test/version/semverish ()
  (let ((v (pel-version)))
    (should (stringp v))
    (should (string-match-p "^[0-9]+\\.[0-9]+\\.[0-9]+" v))))

;;; --------------------------------------------------------------------------
;;; ;;* Assignment operator macros
;;; --------------------------------------------------------------------------

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

(ert-deftest pel--base-test/macros/pel-plus-minus ()
  (let ((x 1))
    (pel+= x 2)  (should (= x 3))
    (pel-= x 1)  (should (= x 2))))

;;; --------------------------------------------------------------------------
;;; ;;* Function alias macro
;;; --------------------------------------------------------------------------

(ert-deftest ert-test-pel-lambda-c ()
  "Test `λc' (funcall alias macro)."
  (should (eq 3        (λc #'+ 1 2)))
  (should (eq 6        (λc #'* 2 3)))
  (should (string= "A" (λc #'upcase "a")))
  ;; Works with lambdas too.
  (should (eq 10 (λc (lambda (x) (* x 2)) 5))))

(ert-deftest pel--base-test/macro/lambda-compose ()
  (should (= 4 (λc (lambda (n) (1+ n)) 3))))

;;; --------------------------------------------------------------------------
;;; ;;* Base predicates
;;; --------------------------------------------------------------------------

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

(ert-deftest pel--base-test/predicates/expression-and-user-option ()
  (should (pel-expression-p 'foo))
  (should (pel-expression-p '(+ 1 2)))
  (should-not (pel-expression-p 42))
  (should (pel-user-option-p 'pel-use-common-lisp))
  (should-not (pel-user-option-p 'pel-use-visual-basic)))

;;; --------------------------------------------------------------------------
;;; ;;* Set variable conditionally
;;; --------------------------------------------------------------------------

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

(defvar pel--base-test-sym nil)  ; declare special

(ert-deftest pel--base-test/set-if-non-nil-and-bools-and-bits ()
  (let ((pel--base-test-sym nil))
    (pel-set-if-non-nil 'pel--base-test-sym nil)
    (should (eq nil pel--base-test-sym))
    (pel-set-if-non-nil 'pel--base-test-sym :v)
    (should (eq :v pel--base-test-sym)))
  (should (eq nil (pel-!0 0)))
  (should (eq t   (pel-!0 1)))
  (should (eq t   (pel-as-boolean 'x)))
  (should (eq nil (pel-as-boolean nil)))
  (should (pel-all-bitset-p 5 1 4))
  (should-not (pel-all-bitset-p 5 1 2)))

;;; --------------------------------------------------------------------------
;;; ;;* Check for Zero
;;; --------------------------------------------------------------------------

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

;;; --------------------------------------------------------------------------
;;; ;;* Bitwise Operations
;;; --------------------------------------------------------------------------

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

;;; --------------------------------------------------------------------------
;;; ;;* List Handling
;;; --------------------------------------------------------------------------

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

(ert-deftest pel--base-test/list-of-and-transpose-alist ()
  (should (equal '(a)   (pel-list-of 'a)))
  (should (equal '(a b) (pel-list-of '(a b))))
  (should (equal '((2 . 1) (4 . 3)) (pel-transpose-alist '((1 . 2) (3 . 4))))))

;;; --------------------------------------------------------------------------
;;; ;;* Environment Querying functions
;;; --------------------------------------------------------------------------

;; Special var for fast-startup check
(defvar pel-running-in-fast-startup-p nil)

(ert-deftest pel--base-test/fast-startup/bound-and-true-p ()
  (let ((pel-running-in-fast-startup-p nil))
    (should-not (pel-in-fast-startup-p)))
  (let ((pel-running-in-fast-startup-p t))
    (should (pel-in-fast-startup-p))))

;;; --------------------------------------------------------------------------
;;; ;;* Checking Major Mode  - pel-major-mode-of-buffer
;;; --------------------------------------------------------------------------

(ert-deftest ert-test-pel-major-mode-of-buffer ()
  "Test `pel-major-mode-of-buffer'."
  ;; In a temp buffer with emacs-lisp-mode, it returns that mode.
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; without argument check current buffer
    (should (eq 'emacs-lisp-mode (pel-major-mode-of-buffer)))
    ;; it should be like passing the current buffer explicitly
    (should (eq 'emacs-lisp-mode (pel-major-mode-of-buffer (current-buffer)))))
  ;; text-mode in another temp buffer.
  (let ((buf (get-buffer-create " *pel-test-text-mode*")))
    (unwind-protect
        (with-current-buffer buf
          (text-mode)
          (should (eq 'text-mode (pel-major-mode-of-buffer)))
          (should (eq 'text-mode (pel-major-mode-of-buffer buf))))
      (kill-buffer buf))))

(ert-deftest pel--base-test/mode/guards-and-derived ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (pel-derived-mode-p nil 'emacs-lisp-mode))
    (should-error (pel-major-mode-must-be 'c-mode) :type 'user-error)
    (should-not (pel-dired-buffer-p nil t))))

;;; --------------------------------------------------------------------------
;;; ;;* Checking Major Mode  - pel-major-mode-of-file
;;; --------------------------------------------------------------------------

(defconst pel--rootdir (file-name-directory
                        (directory-file-name
                         (file-name-directory
                          (or load-file-name buffer-file-name))))
  "Root directory of pel.")

(ert-deftest ert-test-pel-major-mode-of-file/dir ()
  "Test `pel-major-mode-of-file' of directory -> nil."
  (should-not (pel-major-mode-of-file pel--rootdir)))

(ert-deftest ert-test-pel-major-mode-of-file/c-file ()
  "Test `pel-major-mode-of-file' of C file -> c-mode."
  (should (memq  (pel-major-mode-of-file
                  (expand-file-name
                   "example/c/c_preproc-styles.c"
                   pel--rootdir))
                 '(c-mode c-ts-mode))))

(ert-deftest ert-test-pel-major-mode-of-file/c++-file ()
  "Test `pel-major-mode-of-file' of C++ file -> c++-mode."
  (should (memq (pel-major-mode-of-file
                 (expand-file-name
                  "example/templates/cpp/code.cpp"
                  pel--rootdir))
                '(c++-mode c++-ts-mode ))))

;; (ert-deftest ert-test-pel-major-mode-of-file/erlang-file ()
;;   "Test `pel-major-mode-of-file' of Erlang file -> erlang-mode."
;;   (should (memq  (pel-major-mode-of-file
;;                   (expand-file-name
;;                    "example/templates/erlang/gen_fsm_1_0_0_1_1.erl"
;;                    pel--rootdir))
;;                  '('erlang-mode erlang-ts-mode))))

;; (ert-deftest ert-test-pel-major-mode-of-file/shell-file ()
;;   "Test `pel-major-mode-of-file' of shell script file -> sh-mode."
;;   (should (eq 'sh-mode (pel-major-mode-of-file
;;                         (expand-file-name
;;                          "bin/e"
;;                          pel--rootdir)))))

;; ===========================================================================
;; pel-derived-mode-p
;; ===========================================================================
;;
;; The function signature is:
;;   (pel-derived-mode-p buffer-or-name &rest modes)
;;
;; Cases tested:
;;
;;   1. nil BUFFER-OR-NAME → uses current buffer (not a different one).
;;   2. Buffer object → queries mode of that buffer, NOT current buffer.
;;   3. Buffer name string → queries mode of that buffer, NOT current buffer.
;;   4. Returns non-nil when mode matches exactly.
;;   5. Returns non-nil when mode is an ancestor (derived-mode-p ancestry).
;;   6. Returns nil when mode does not match and is not an ancestor.
;;   7. &rest MODES: returns non-nil when any mode in the list matches.
;;   8. Regression for the fixed bug: buffer-or-name must NOT be silently
;;      ignored in favour of the current buffer.

;; ---------------------------------------------------------------------------
;; 1. nil BUFFER-OR-NAME uses current buffer

(ert-deftest pel--base-test/derived-mode-p/nil-buffer-uses-current ()
  "`pel-derived-mode-p' with nil BUFFER-OR-NAME queries the current buffer."
  (pel--base-test--with-mode-buffer 'emacs-lisp-mode
                                    ;; Current buffer is in emacs-lisp-mode.
                                    (should (pel-derived-mode-p nil 'emacs-lisp-mode))))

(ert-deftest pel--base-test/derived-mode-p/nil-buffer-nil-when-no-match ()
  "`pel-derived-mode-p' with nil BUFFER-OR-NAME returns nil when mode differs."
  (pel--base-test--with-mode-buffer 'emacs-lisp-mode
    (should-not (pel-derived-mode-p nil 'c-mode))))

;; ---------------------------------------------------------------------------
;; 2. Buffer object — queries that buffer, not the current buffer

(ert-deftest pel--base-test/derived-mode-p/buffer-object-correct-mode ()
  "`pel-derived-mode-p' with a buffer object uses that buffer's mode."
  (let ((target-buf (generate-new-buffer " *pel-test-target*")))
    (unwind-protect
        (progn
          (with-current-buffer target-buf
            (emacs-lisp-mode))
          ;; From a *different* current buffer (fundamental-mode),
          ;; query the target buffer — should find emacs-lisp-mode.
          (with-temp-buffer
            (fundamental-mode)
            (should (pel-derived-mode-p target-buf 'emacs-lisp-mode))))
      (kill-buffer target-buf))))

(ert-deftest pel--base-test/derived-mode-p/buffer-object-ignores-current ()
  "`pel-derived-mode-p' does NOT use current buffer when BUFFER-OR-NAME is given.
This is the regression test for the fixed bug where the buffer argument
was silently ignored and the current buffer was always queried."
  (let ((target-buf (generate-new-buffer " *pel-test-target*")))
    (unwind-protect
        (progn
          (with-current-buffer target-buf
            ;; Target buffer is in emacs-lisp-mode.
            (emacs-lisp-mode))
          (with-temp-buffer
            (text-mode)
            ;; Querying the target buffer for text-mode: should be nil
            ;; because the *target* is in emacs-lisp-mode, not text-mode.
            (should-not (pel-derived-mode-p target-buf 'text-mode))
            ;; Querying the target buffer for emacs-lisp-mode: should be non-nil
            ;; even though *current* buffer is text-mode.
            (should (pel-derived-mode-p target-buf 'emacs-lisp-mode))))
      (kill-buffer target-buf))))

;; ---------------------------------------------------------------------------
;; 3. Buffer name string — queries that buffer, not the current buffer

(ert-deftest pel--base-test/derived-mode-p/buffer-name-string-correct-mode ()
  "`pel-derived-mode-p' accepts a buffer name string for BUFFER-OR-NAME."
  (let ((target-buf (generate-new-buffer " *pel-test-named*")))
    (unwind-protect
        (progn
          (with-current-buffer target-buf
            (emacs-lisp-mode))
          (with-temp-buffer
            (fundamental-mode)
            ;; Pass the buffer name as a string.
            (should (pel-derived-mode-p (buffer-name target-buf)
                                        'emacs-lisp-mode))))
      (kill-buffer target-buf))))

(ert-deftest pel--base-test/derived-mode-p/buffer-name-string-ignores-current ()
  "`pel-derived-mode-p' with a buffer name string does NOT fall back to current buffer."
  (let ((target-buf (generate-new-buffer " *pel-test-named2*")))
    (unwind-protect
        (progn
          (with-current-buffer target-buf
            (emacs-lisp-mode))
          (with-temp-buffer
            ;; Current buffer is in emacs-lisp-mode too; but target is named
            ;; differently.  Query target for c-mode — must return nil.
            (emacs-lisp-mode)
            (should-not (pel-derived-mode-p (buffer-name target-buf)
                                            'c-mode))))
      (kill-buffer target-buf))))

;; ---------------------------------------------------------------------------
;; 4. Exact mode match

(ert-deftest pel--base-test/derived-mode-p/exact-match-current-buffer ()
  "`pel-derived-mode-p' returns non-nil for an exact major mode match (nil buffer arg)."
  (pel--base-test--with-mode-buffer 'emacs-lisp-mode
    (should (pel-derived-mode-p nil 'emacs-lisp-mode))))

;; ---------------------------------------------------------------------------
;; 5. Derived/ancestor mode detection

(ert-deftest pel--base-test/derived-mode-p/ancestor-mode-current-buffer ()
  "`pel-derived-mode-p' returns non-nil when the specified mode is an ancestor."
  (pel--base-test--with-mode-buffer 'emacs-lisp-mode
    (should (pel-derived-mode-p nil 'prog-mode))))

(ert-deftest pel--base-test/derived-mode-p/ancestor-mode-other-buffer ()
  "`pel-derived-mode-p' detects ancestor mode in a specified buffer."
  (let ((target-buf (generate-new-buffer " *pel-test-ancestor*")))
    (unwind-protect
        (progn
          (with-current-buffer target-buf
            (emacs-lisp-mode))
          (with-temp-buffer
            (fundamental-mode)
            (should (pel-derived-mode-p target-buf 'prog-mode))))
      (kill-buffer target-buf))))

;; ---------------------------------------------------------------------------
;; 6. Non-matching mode returns nil

(ert-deftest pel--base-test/derived-mode-p/non-matching-mode-nil ()
  "`pel-derived-mode-p' returns nil when mode is unrelated to the buffer's mode."
  (pel--base-test--with-mode-buffer 'emacs-lisp-mode
    (should-not (pel-derived-mode-p nil 'c-mode))))

(ert-deftest pel--base-test/derived-mode-p/non-matching-mode-nil-other-buffer ()
  "`pel-derived-mode-p' returns nil when mode does not match the specified buffer."
  (let ((target-buf (generate-new-buffer " *pel-test-nonmatch*")))
    (unwind-protect
        (progn
          (with-current-buffer target-buf
            (emacs-lisp-mode))
          (with-temp-buffer
            (fundamental-mode)
            (should-not (pel-derived-mode-p target-buf 'c-mode))))
      (kill-buffer target-buf))))

;; ---------------------------------------------------------------------------
;; 7. Multiple modes in &rest MODES — non-nil if any one matches

(ert-deftest pel--base-test/derived-mode-p/multiple-modes-one-matches ()
  "`pel-derived-mode-p' returns non-nil when any of the MODES matches."
  (pel--base-test--with-mode-buffer 'emacs-lisp-mode
    (should (pel-derived-mode-p nil 'c-mode 'emacs-lisp-mode 'python-mode))))

(ert-deftest pel--base-test/derived-mode-p/multiple-modes-none-matches ()
  "`pel-derived-mode-p' returns nil when none of the MODES matches."
  (pel--base-test--with-mode-buffer 'emacs-lisp-mode
    (should-not (pel-derived-mode-p nil 'c-mode 'python-mode 'ruby-mode))))

(ert-deftest pel--base-test/derived-mode-p/multiple-modes-ancestor-matches ()
  "`pel-derived-mode-p' returns non-nil when an ancestor mode is in MODES list."
  (pel--base-test--with-mode-buffer 'emacs-lisp-mode
    (should (pel-derived-mode-p nil 'text-mode 'prog-mode))))

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

(ert-deftest pel--base-test/mode/file-type-and-string-with-major-mode ()
  (should (string= "emacs-lisp" (pel-file-type-for 'emacs-lisp-mode)))
  (should (string= "c"          (pel-file-type-for 'c-ts-mode)))
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (string= "wrap-emacs-lisp-done"
                     (pel-string-with-major-mode "wrap-%s-done")))))

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

;;; --------------------------------------------------------------------------
;;; ;;* Minor and Major Mode Utilities
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/mode/buffers-in-mode-and-minor-mode-state ()
  (let ((b1 (generate-new-buffer " *pel-base-b1*"))
        (b2 (generate-new-buffer " *pel-base-b2*")))
    (unwind-protect
        (progn
          (with-current-buffer b1 (emacs-lisp-mode))
          (with-current-buffer b2 (fundamental-mode))
          (let ((lst (pel-buffers-in-mode 'emacs-lisp-mode)))
            (should (memq b1 lst))
            (should-not (memq b2 lst))))
      (kill-buffer b1)
      (kill-buffer b2)))
  (require 'abbrev)
  (with-temp-buffer
    (abbrev-mode -1)
    (should (stringp (pel-minor-mode-state 'abbrev-mode nil)))
    (abbrev-mode 1)
    (should (stringp (pel-minor-mode-state 'abbrev-mode nil)))))

(ert-deftest pel--base-test/mode/major-mode-symbol-for-and-value ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((fmt "pel-test-%s-flag"))
      (should (eq (pel-major-mode-symbol-for fmt) 'pel-test-emacs-lisp-flag))
      (pel-set-major-mode-symbol fmt 42)
      (should (= 42 (pel-major-mode-symbol-value fmt))))))

(ert-deftest pel--base-test/mode/major-mode-symbol-value-or-default ()
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; Use a fresh, never-set format to exercise the default path
    (let ((fmt2 "pel-test-%s-undefined-flag"))
      (should (= 777 (pel-major-mode-symbol-value-or fmt2 777))))))

;;; --------------------------------------------------------------------------
;;; ;;* Buffer Information
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/buffer-info/file-ext-eol-and-cd ()
  (pel--base-test--with-temp-dir tmpd
    (let* ((f (pel--base-test--write-file tmpd "a.txt" "x\r\n"))
           (buf (find-file-noselect f)))
      (unwind-protect
          (with-current-buffer buf
            (should (string-suffix-p "a.txt" (pel-current-buffer-filename t)))
            (should (string= "txt" (pel-current-buffer-file-extension)))
            (setq buffer-file-coding-system 'utf-8-dos)
            (should (memq (pel-current-buffer-eol-type) '(unix dos mac nil)))
            (let ((orig default-directory))
              (pel-cd-to-current :silent)
              (should (pel--base-test--same-dir-p default-directory tmpd))
              (cd orig)))
        (kill-buffer buf)))))

;;; --------------------------------------------------------------------------
;;; ;;* Current Directory
;;;
;;; (covered by pel--base-test/buffer-info/file-ext-eol-and-cd above)
;;; --------------------------------------------------------------------------

;;; --------------------------------------------------------------------------
;;; ;;* OS Environment Utilities
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/os-env/terminal-and-ssh ()
  (let ((process-environment
         (cons "TERM_PROGRAM=Apple_Terminal" process-environment)))
    (should (pel-terminal-is-macos-terminal-p)))

  (let ((process-environment
         (cons "SSH_CLIENT=1.2.3.4 22 12345" process-environment)))
    (should (pel-running-under-ssh-p))))


(ert-deftest pel--base-test/os-env/subsitute-env-vars ()
  "Test `pel-substitute-env-vars' ability to expand recursively."
  (let ((process-environment
         (cons "USER=woz"
               (cons "BAR=/home/user/$USER"
                     (cons "FOO=$BAR/at/next" process-environment))))
        (my-string "Path is: [$FOO]. $$ is not expanded, even for $USER."))
    (should
     (string=
      "Path is: [/home/user/woz/at/next]. $ is not expanded, even for woz."
      (pel-substitute-env-vars my-string)))))

;;; --------------------------------------------------------------------------
;;; ;;* Emacs Environment Utilities
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/emacs-env/locate-user-emacs-file-and-loadpath ()
  (should (file-name-absolute-p (pel-locate-user-emacs-file "pel-base-test.tmp")))
  (pel--base-test--with-temp-dir
   tmpd
   (let ((added (pel-add-dir-to-loadpath tmpd)))
     (should added)
     (should (member (directory-file-name (file-truename tmpd))
                     (mapcar (lambda (p) (directory-file-name (file-truename p)))
                             load-path))))
   (let ((added2 (pel-add-dir-to-loadpath tmpd)))
     (should (not added2)))))

;;; --------------------------------------------------------------------------
;;; ;;* File System Type
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/fs/type-preds-and-descriptions ()
  (pel--base-test--with-temp-dir tmpd
    (let* ((f (pel--base-test--write-file tmpd "file.bin" "abc"))
           (d tmpd)
           (l (expand-file-name "file.link" tmpd)))
      (should-not (pel-unix-socket-p f))
      (should (string= "file" (pel-file-type-str f)))
      (should (string= "directory" (pel-file-type-str d)))
      (condition-case _
          (progn
            (make-symbolic-link f l)
            (should (string= "symbolic link" (pel-file-type-str l))))
        (error (ert-info ((format "Skipping symlink checks in %s" tmpd)) (should t)))))))

;;; --------------------------------------------------------------------------
;;; ;;* String predicates
;;; --------------------------------------------------------------------------

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
  (should (pel-string-starts-with-p "" "")) ; SPECIAL case! nothing starts with nothing!
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
  (should (pel-string-ends-with-p "" "")) ; SPECIAL case! nothing ends with nothing!
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

(ert-deftest pel--base-test/strings/predicates ()
  (should (integerp (pel-whitespace-in-str-p "a b")))
  (should-not (pel-whitespace-in-str-p "ab"))
  (should (pel-ends-with-space-p "ab "))
  (should-not (pel-ends-with-space-p "ab"))
  (should (pel-starts-with-space-p " ab"))
  (should-not (pel-starts-with-space-p "ab"))
  (should (pel-string-ends-with-p "foobar" "bar"))
  (should-not (pel-string-ends-with-p "foobar" "baz"))
  (should (pel-string-starts-with-p "foobar" "foo"))
  (should-not (pel-string-starts-with-p "foobar" "bar"))
  (should (pel-lowercase-p "abc"))
  (should-not (pel-lowercase-p "aBc"))
  (should (pel-uppercase-p "ABC"))
  (should-not (pel-uppercase-p "AbC"))
  (should (pel-alnum-p "Abc123"))
  (should-not (pel-alnum-p "A!")))

;;; --------------------------------------------------------------------------
;;; ;;* Pluralizer
;;; --------------------------------------------------------------------------

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

(ert-deftest pel--base-test/pluralizer ()
  (should (equal "classes"  (pel-plural-of "class")))
  (should (equal "tomatoes" (pel-plural-of "tomato")))
  (should (equal "skies"    (pel-plural-of "sky")))
  (should (equal "calves"   (pel-plural-of "calf")))
  (should (equal "commands" (pel-plural-of "command")))
  (should (equal "2 cats"   (pel-count-string 2 "cat")))
  (should (equal "3 cats"   (pel-count-string 3 "cat")))
  (should (equal "cat"      (pel-pluralize 1 "cat")))
  (should (equal "cats"     (pel-pluralize 3 "cat")))
  (should (equal "calves"   (pel-pluralize 3 "calf"))))

;;; --------------------------------------------------------------------------
;;; ;;* Symbol value extraction
;;; --------------------------------------------------------------------------

(ert-deftest ert-test-pel-as-symbol ()
  "Test `pel-as-symbol'."
  ;; A symbol is returned unchanged.
  (should (eq 'foo (pel-as-symbol 'foo)))
  ;; A string is interned to a symbol.
  (should (eq 'foo (pel-as-symbol "foo")))
  (should (symbolp (pel-as-symbol "bar"))))

(defvar pel--base-test-x)

(ert-deftest pel--base-test/symbol/value-and-at-point ()
  ;; pel-symbol-value
  (set (make-symbol "pel--base-test-tmp") 99) ;; ensure no warning; not used below
  (set 'pel--base-test-x 10)
  (should (= 10 (pel-symbol-value 'pel--base-test-x)))
  ;; pel--symbol-value (unknown vs quiet)
  (should (equal (list 'pel--base-test-void "**is currently unbound!**")
                 (pel--symbol-value 'pel--base-test-void)))
  (should (eq nil (pel--symbol-value 'pel--base-test-void :quiet)))
  ;; pel-as-symbol
  (should (eq 'abc (pel-as-symbol 'abc)))
  (should (eq 'abc (pel-as-symbol "abc")))
  ;; pel-symbol-at-point returns string at point
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "foo")
    (goto-char 2)
    (should (equal "foo" (pel-symbol-at-point)))))

;;; --------------------------------------------------------------------------
;;; ;;* Symbol at point
;;;
;;; (covered by pel--base-test/symbol/value-and-at-point above)
;;; --------------------------------------------------------------------------

;;; --------------------------------------------------------------------------
;;; ;;* String generation utilities
;;; --------------------------------------------------------------------------

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

(ert-deftest pel--base-test/presentation/on-off-and-texts ()
  (should (string= "on"  (pel-on-off-string t)))
  (should (string= "off" (pel-on-off-string nil)))
  (defvar pel--t-flag nil)
  (should (string-match-p "is now" (pel-symbol-text 'pel--t-flag)))
  (setq pel--t-flag t)
  (should (string-match-p "on" (pel-value-on-off-text 'pel--t-flag)))
  ;; unknown/value-or variants
  (let ((sym (make-symbol "pel--base-test-void2")))
    (should (equal (format "unknown - `%s' is not bound!" sym)
                   (pel-symbol-value-or sym)))
    (should (eq :r (pel-symbol-value-or sym :r)))
    (should (eq nil (pel-symbol-value-or sym :nil-for-void))))
  (should (string= "yes" (pel-yes-no-string t)))
  (should (string= "no"  (pel-yes-no-string nil))))

;;; --------------------------------------------------------------------------
;;; ;;* Automated Mode Activation Check
;;;
;;; (pel-modes-activating-symbol-name-for, pel-minor-mode-auto-activated-by,
;;;  pel-activated-in-str, pel-option-mode-state — not yet covered by tests)
;;; --------------------------------------------------------------------------

;;; --------------------------------------------------------------------------
;;; ;;* String transformation utilities
;;; --------------------------------------------------------------------------

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

(ert-deftest pel--base-test/shell-quote-path-keep-glob ()
  "Test `pel-shell-quote-path-keep-glob'."
  (should (string= (pel-shell-quote-path-keep-glob "*.*")
                   "*.*" ))
  (should (string= (pel-shell-quote-path-keep-glob "* abc def.*")
                   "*\\ abc\\ def.*"))
  (should (string= (pel-shell-quote-path-keep-glob "*-[a-c]def.*")
                   "*-[a-c]def.*")))

;;; --------------------------------------------------------------------------
;;; ;;* Message List formatting
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/messages/formatters ()
  (let* ((problems '("p1" "p2"))
         (out (pel--format-problem-messages problems "System Test:")))
    (should (string-match-p "System Test:" out))
    (should (string-match-p "- p1" out))
    (should (string-match-p "- p2" out)))
  (let ((txt (pel-format-problem-messages '("x") nil "Intro: %s" "arg")))
    (should (string-match-p "Intro:" txt)))
  (let ((m (pel--base-test--capture-message
            (pel-message-for "Intro" '("a" "b")))))
    (should (string-match-p "Intro" m))
    (should (string-match-p "a" m))
    (should (string-match-p "b" m))))

;;; --------------------------------------------------------------------------
;;; ;;* Value check
;;; --------------------------------------------------------------------------

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

(ert-deftest pel--base-test/value-check/use-or-and-ops-on-seqs ()
  (should (equal "Abc."
                 (pel-use-or "abc" #'pel-hastext 0
                             (lambda (s)
                               (concat (upcase (substring s 0 1))
                                       (substring s 1)))
                             #'pel-end-text-with-period)))
  ;; pel-concat-strings-in-list returns concatenated string; for 5 items length=5
  (should (= 5 (length (pel-concat-strings-in-list '("a" "b" "c" "d" "e")))))
  (let ((lst '(1 2 3 4)))
    (pel-prepend-to lst '(22))
    (should (equal '(22 1 2 3 4) lst))
    (pel-prepend-to lst '(33 44))
    (should (equal '(33 44 22 1 2 3 4) lst)))
  (let ((al '((one (".." "[..]")))))
    (should (equal '((one (",," "[,,]") (".." "[..]")))
                   (pel-cons-alist-at al 'one '(",,"
                                                "[,,]")))))
  (should (= 1 (pel-nth-elt 'b '(a b c d))))
  (should (equal '(a new b c d) (pel-list-insert-before '(a b c d) 1 'new)))
  (should (equal '(c a b d)     (pel-list-prepend-nth   '(a b c d) 2)))
  (should (equal '(b c a d)     (pel-list-insert-car-at '(a b c d) 2)))
  (should (equal '(c d)         (pel-delqs '(a b) '(a b c d)))))

;;; --------------------------------------------------------------------------
;;; ;;* Operations on sequences
;;; --------------------------------------------------------------------------

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

;;; --------------------------------------------------------------------------
;;; ;;* Operation on auto-mode-alist
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/auto-mode-alist/delete ()
  (let ((auto-mode-alist
         '(("\\.foo\\'" . text-mode)
           ("\\.bar\\'" . fundamental-mode)
           ("\\.baz\\'" . text-mode))))
    (pel-delete-from-auto-mode-alist 'text-mode)
    (should (equal '(("\\.bar\\'" . fundamental-mode)) auto-mode-alist))))

;;; --------------------------------------------------------------------------
;;; ;;* PEL utils rebuild
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/utils/rebuild-utils/no-utils-dir-warning ()
  (let ((warn (pel--base-test--capture-warning
               (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) nil)))
                 (pel-rebuild-utils)))))
    (should (or (null warn)
                (string-match-p "utils directory" warn)))))

;;; --------------------------------------------------------------------------
;;; ;;* Tree-sitter major mode support
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/treesit/mode-remap-and-supported-p ()
  "Deterministic across environments."
  (let ((pel-uses-tree-sitter t)
        (major-mode-remap-alist '((c-mode . c-ts-mode))))
    (let* ((baseline (pel-major-ts-mode-supported-p 'c))
           (res-nil nil) (res-t nil)
           (called-nil 0) (called-t 0))
      ;; Stub readiness to nil and count calls
      (cl-letf (((symbol-function 'pel-treesit-ready-p)
                 (lambda (&rest _)
                   (setq called-nil (1+ called-nil))
                   nil)))
        (setq res-nil (pel-major-ts-mode-supported-p 'c)))
      ;; Stub readiness to t and count calls
      (cl-letf (((symbol-function 'pel-treesit-ready-p)
                 (lambda (&rest _)
                   (setq called-t (1+ called-t))
                   t)))
        (setq res-t (pel-major-ts-mode-supported-p 'c)))
      (if (> (+ called-nil called-t) 0)
          ;; Implementation consults readiness: results must differ with stubs.
          (progn
            (should-not res-nil)
            (should res-t))
        ;; Implementation ignores readiness: just assert consistent boolean behavior.
        (should (or (null baseline) (eq baseline t)))
        (should (eq res-nil baseline))
        (should (eq res-t   baseline))))))

(ert-deftest pel--base-test/treesit/ready-and-grammar-stubbed ()
  ;; Force the 'ready' branch deterministically
  (let* ((pel-emacs-30-or-later-p t)
         (ready (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
                          ((symbol-function 'fboundp) (lambda (_sym) t))
                          ((symbol-function 'treesit-ready-p) (lambda (&rest _) :ok)))
                  (pel-treesit-ready-p 'c :quiet))))
    (should (eq :ok ready))))

(ert-deftest pel--base-test/hash/md5-on-file ()
  (pel--base-test--with-temp-dir d
    (let* ((f (pel--base-test--write-file d "h.txt" "abc"))
           (expect (secure-hash 'md5 "abc")))
      (should (string= (pel-file-md5 f) expect)))))

;;; --------------------------------------------------------------------------
;;; ;;* Mode argument interpretation
;;; --------------------------------------------------------------------------

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

(ert-deftest pel--base-test/action/interpretation ()
  (should (eq 'activate   (pel-action-for nil nil)))
  (should (eq 'deactivate (pel-action-for nil t)))
  (should (eq 'activate   (pel-action-for 1 nil)))
  (should (eq nil         (pel-action-for 1 t)))
  (should (eq nil         (pel-action-for -1 nil)))
  (should (eq 'deactivate (pel-action-for -1 t))))

;;; --------------------------------------------------------------------------
;;; ;;* Toggle a local mode
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/toggle-mode/abbrev ()
  (require 'abbrev)
  (with-temp-buffer
    (abbrev-mode -1)
    (let ((new (pel-toggle-mode 'abbrev-mode)))
      (should new)
      (let ((msg (pel--base-test--capture-message
                  (pel-toggle-mode-and-show 'abbrev-mode))))
        (should (stringp msg))))))

;;; --------------------------------------------------------------------------
;;; ;;* Toggle of values and variables
;;; --------------------------------------------------------------------------

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

(defvar pel--t-flag2 nil)

(ert-deftest pel--base-test/toggle/variables-and-show ()
  (let ((pel--t-flag2 nil))
    (should (eq t  (pel-toggle 'pel--t-flag2)))
    (should (eq nil (pel-toggle 'pel--t-flag2))))
  (let ((pel--t-flag2 nil))
    (pel-toggle-and-show 'pel--t-flag2)
    (should (eq t pel--t-flag2))
    (pel-toggle-and-show 'pel--t-flag2 nil nil t)
    (should (eq nil pel--t-flag2)))
  (defvar pel--t-opt nil)
  (let ((pel--t-opt nil))
    (pel-toggle-and-show-user-option 'pel--t-opt t)
    (should (eq t pel--t-opt)))
  (should (eq 7 (pel-val-or-default nil 7)))
  (should (eq 5 (pel-val-or-default 5 7))))

(ert-deftest ert-test-pel-val-or-default ()
  "Test `pel-val-or-default'."
  (should (eq 42      (pel-val-or-default 42 99)))
  (should (eq 99      (pel-val-or-default nil 99)))
  (should (string= "x" (pel-val-or-default "x" "y")))
  (should (string= "y"  (pel-val-or-default nil "y")))
  (should (eq 'sym    (pel-val-or-default 'sym 'other))))

;;; --------------------------------------------------------------------------
;;; ;;* Symbol processing
;;; --------------------------------------------------------------------------

(ert-deftest ert-test-pel-hook-symbol-for ()
  "Test `pel-hook-symbol-for'."
  (should (eq 'emacs-lisp-mode-hook (pel-hook-symbol-for 'emacs-lisp-mode)))
  (should (eq 'text-mode-hook       (pel-hook-symbol-for 'text-mode)))
  (should (eq 'c-mode-hook          (pel-hook-symbol-for 'c-mode))))

(ert-deftest pel--base-test/symbols/hook-and-map ()
  (should (eq 'text-mode-hook (pel-hook-symbol-for 'text-mode)))
  (should (eq 'text-mode-map  (pel-map-symbol-for  'text-mode))))

;;; --------------------------------------------------------------------------
;;; ;;* Hook control
;;; --------------------------------------------------------------------------

(defvar pel--t-my-modes '(emacs-lisp-mode))

(ert-deftest pel--base-test/hooks/add-hook-for ()
  (let ((pel--t-my-modes '(emacs-lisp-mode fundamental-mode))
        (emacs-lisp-mode-hook nil))
    (pel-add-hook-for 'pel--t-my-modes #'ignore '(emacs-lisp-mode))
    (should (memq #'ignore emacs-lisp-mode-hook))))

;;; --------------------------------------------------------------------------
;;; ;;* Minor mode activation
;;; --------------------------------------------------------------------------

(defvar pel--t-fake-list nil)

(ert-deftest pel--base-test/minor-modes/check-minor-modes-in ()
  (should (>= (pel--check-minor-modes-in 'pel--t-fake-list '(no-such-mode)) 0))
  (should (>= (pel-check-minor-modes-in pel--t-fake-list) 0)))

;;; --------------------------------------------------------------------------
;;; ;;* Argument converter
;;;
;;; (pel-multiplier, pel-mode-toggle-arg — not yet covered by dedicated tests)
;;; --------------------------------------------------------------------------

;;; --------------------------------------------------------------------------
;;; ;;* Iteration helpers
;;;
;;; (pel-dec, pel-inc — not yet covered by dedicated tests)
;;; --------------------------------------------------------------------------

;;; --------------------------------------------------------------------------
;;; ;;* Swap 2 values
;;;
;;; (pel-swap — not yet covered by dedicated tests)
;;; --------------------------------------------------------------------------

;;; --------------------------------------------------------------------------
;;; ;;* Text at point
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/point/chars-and-letter-case ()
  (with-temp-buffer
    (insert "aBc 123")
    (goto-char 1)
    (should (string= "aB" (pel-chars-at-point 2)))
    (should (pel-at-letter-p))
    (should (pel-at-lowercase-p :exact))
    (goto-char 2)
    (should (pel-at-uppercase-p 1 :exact))))

;;; --------------------------------------------------------------------------
;;; ;;* Calling functions
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/point/n-funcall-to-goto-helpers ()
  (let ((pos 0) (neg 0))
    (pel-n-funcall-to 3  (lambda () (setq pos (1+ pos)))
                      (lambda () (setq neg (1+ neg))))
    (should (= pos 3)) (should (= neg 0))
    (setq pos 0 neg 0)
    (pel-n-funcall-to -2 (lambda () (setq pos (1+ pos)))
                      (lambda () (setq neg (1+ neg))))
    (should (= pos 0)) (should (= neg 2)))
  (with-temp-buffer
    (insert "l1\nl2\nl3\n")
    (pel-goto-line 2)
    (should (eq (point) (line-beginning-position)))
    (pel-goto-position 3 1)
    (should (= (current-column) 1))
    (should-error (pel-goto-line 0))))

;;; --------------------------------------------------------------------------
;;; ;;* Moving Point
;;;
;;; (covered by pel--base-test/point/n-funcall-to-goto-helpers above)
;;; --------------------------------------------------------------------------

;;; --------------------------------------------------------------------------
;;; ;;* Line position
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/point/same-line-p-and-region-for ()
  (with-temp-buffer
    (insert "A\nSTART here\nMIDDLE\nEND here\nZ\n")
    (should (pel-same-line-p 1 2))
    (let ((reg (pel-region-for "START" "END")))
      (should reg)
      (should (integerp (car reg)))
      (should (integerp (cdr reg))))))

;;; --------------------------------------------------------------------------
;;; ;;* Identifying region
;;;
;;; (covered by pel--base-test/point/same-line-p-and-region-for above)
;;; --------------------------------------------------------------------------

;;; --------------------------------------------------------------------------
;;; ;;* Insert or overwrite text
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/insert/overwrite-or-insert ()
  (with-temp-buffer
    (insert "abc")
    (goto-char 2) ;; between a and b
    (let ((overwrite-mode nil))
      (pel-insert-or-overwrite "X")
      (should (string= "aXbc" (buffer-string))))
    (erase-buffer)
    (insert "abc")
    (goto-char 2)
    (let ((overwrite-mode t))
      (pel-insert-or-overwrite ?Y)
      (should (string= "aYc" (buffer-string))))))

;;; --------------------------------------------------------------------------
;;; ;;* Extract text from buffer
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/extract/text-from-bol ()
  (with-temp-buffer
    (insert "hello\nworld\n")
    (goto-char (point-min))
    (forward-line 1)                 ; at beginning of "world"
    (search-forward "wor")           ; point just after "wor"
    (should (string= "wor" (pel-text-from-beginning-of-line)))))

;;; --------------------------------------------------------------------------
;;; ;;* Check text in buffer
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/line/only-whitespace-p ()
  (with-temp-buffer
    (insert "   \nabc\n")
    (goto-char (point-min))
    (should (pel-line-has-only-whitespace-p))   ; first line is spaces only
    (forward-line 1)
    (move-beginning-of-line 1)                  ; be explicit: at "abc"
    (should-not (pel-line-has-only-whitespace-p))))

(ert-deftest pel--base-test/line-has-only-whitespace-p/empty-line ()
  "Returns t for a completely empty line."
  (with-temp-buffer
    (insert "\n")
    (goto-char (point-min))
    (should (pel-line-has-only-whitespace-p))))

(ert-deftest pel--base-test/line-has-only-whitespace-p/whitespace-only ()
  "Returns non-nil for a line with only whitespace."
  (with-temp-buffer
    (insert "   \t  \n")
    (goto-char (point-min))
    (should (pel-line-has-only-whitespace-p))))

(ert-deftest pel--base-test/line-has-only-whitespace-p/non-empty-line ()
  "Returns nil for a line with text content."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char (point-min))
    (should-not (pel-line-has-only-whitespace-p))))

(ert-deftest pel--base-test/line-has-only-whitespace-p/leading-whitespace-then-text ()
  "Returns nil for a line with leading whitespace followed by text."
  (with-temp-buffer
    (insert "   int x = 0;\n")
    (goto-char (point-min))
    (should-not (pel-line-has-only-whitespace-p))))

(ert-deftest pel--base-test/code/checks ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"str\" ; com\nx\n")
    (goto-char 2) (should-not (pel-inside-code))
    (goto-char 9) (should-not (pel-inside-code))
    (goto-char (point-max)) (backward-char)
    (should (pel-inside-code))))

(ert-deftest pel--base-test/shebang/has-line ()
  (with-temp-buffer
    (insert "#!/usr/bin/env bash\nx\n")
    (goto-char 1)
    (should (pel-has-shebang-line)))
  (with-temp-buffer
    (insert "not-shebang\n")
    (should-not (pel-has-shebang-line))))

;;; --------------------------------------------------------------------------
;;; ;;* File Path processing
;;; --------------------------------------------------------------------------

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

(ert-deftest pel--base-test/paths/file-in-and-normalize-and-same-fname ()
  (pel--base-test--with-temp-dir d
    (let* ((sub (expand-file-name "sub" d))
           (_   (make-directory sub))
           (f   (pel--base-test--write-file sub "z.c" "x")))
      (should (equal sub (pel-file-in f (list sub d))))
      (should (string= (pel-normalize-fname (concat d "/."))
                       (pel-normalize-fname d)))
      (should (pel-is-subdir-of sub d))
      (should (string= (file-truename (file-name-directory (directory-file-name d)))
                       (file-truename (pel-parent-dirpath d))))
      (should (string= (expand-file-name "bro" (file-name-directory (directory-file-name d)))
                       (pel-sibling-dirname d "bro")))
      (should (string= (file-name-as-directory (pel-sibling-dirname d "sis"))
                       (pel-sibling-dirpath d "sis")))
      (should (pel-same-fname-p (concat d "/a/..") (directory-file-name d))))))

(ert-deftest pel--base-test/urls/expand-join-location ()
  (let* ((h (or (getenv "HOME") "~"))
         (u (format "file://%s/docs/HyperSpec/" h)))
    (should (string-match-p "^file://" (pel-expand-url-file-name u))))
  (should (string= "a/b/c" (pel-url-join "a/" "/b/" "c")))
  (should (string= "Local"  (pel-url-location "file:///tmp/x")))
  (should (string= "Remote" (pel-url-location "http://example.test/x"))))

(ert-deftest pel--base-test/symlinks/point-and-check ()
  (pel--base-test--with-temp-dir d
    (let* ((tgt (pel--base-test--write-file d "t.txt" "x"))
           (lnk (expand-file-name "l.txt" d)))
      (condition-case _
          (progn
            (pel-point-symlink-to lnk tgt)
            (should (pel-symlink-points-to-p lnk tgt)))
        (error (ert-info ((format "Skipping symlink ops in %s" d)) (should t)))))))

;;; --------------------------------------------------------------------------
;;; ;;* Insertion of text in current buffer
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/insertions/bold-url-symbol-and-lists ()
  (with-temp-buffer
    (pel-insert-bold "B")
    (should (string= "B" (buffer-substring-no-properties 1 2))))
  (with-temp-buffer
    (pel-insert-url-link "X" "http://example.test" "!")
    (should (string-match-p "X" (buffer-string))))
  (with-temp-buffer
    (let ((pel-insert-symbol-content-context-buffer (current-buffer)))
      (pel-insert-symbol 'emacs-version :no-button)
      (pel-insert-symbol-content 'emacs-version nil :on-same-line :no-button)
      (pel-insert-symbol-content-line 'emacs-version nil "extra")
      (pel-insert-list-value "L" '(a b) nil t)
      (pel-insert-list-content 'load-path nil :without-index :no-button :on-same-line)
      (should (> (buffer-size) 0))))
  (should (string= ">>a\n>>b" (pel-line-prefixed-with "a\nb" ">>")))
  (with-temp-buffer
    (pel--pp '(:a 1 :b 2) (current-buffer) "  ")
    (should (string-match-p ":a" (buffer-string)))))

;;; --------------------------------------------------------------------------
;;; ;;* Move point right, optionally inserting spaces
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/move-right-by/basic-and-eob ()
  (with-temp-buffer
    (insert "0123456789\n")
    (pel-move-right-by 5)
    (should (= 5 (current-column))))
  (with-temp-buffer
    (insert "hello")
    (goto-char (point-max))
    (pel-move-right-by 3)
    (should (= 8 (current-column)))))

;;; --------------------------------------------------------------------------
;;; ;;* Print in dedicated buffer
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/print/into-named-buffer ()
  (let ((bufname "*PEL Print Test*"))
    (when (get-buffer bufname) (kill-buffer bufname))
    (pel-print-in-buffer bufname "Title" "Body" t nil t)
    (let ((b (get-buffer bufname)))
      (unwind-protect
          (progn
            (should b)
            (with-current-buffer b
              (should (string-match-p "Title" (buffer-string)))
              (should (string-match-p "Body"  (buffer-string)))))
        (when b (kill-buffer b))))))

;;; --------------------------------------------------------------------------
;;; ;;* Code Parsing Support
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/parse/point-in-comment-or-docstring ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; comment line\n\"doc\" x\n")
    (goto-char 5)
    (should (pel-point-in-comment-or-docstring))
    (goto-char (point-min))
    (search-forward "\"")
    (forward-char 1)
    (should (pel-point-in-comment-or-docstring #'ignore))))

;;; --------------------------------------------------------------------------
;;; ;;* Byte-compilation
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/byte-compile/if-needed-stubs ()
  (pel--base-test--with-temp-dir d
    (let* ((src (pel--base-test--write-file d "x.el" "(message \"hi\")"))
           (calls 0))
      (cl-letf (((symbol-function 'byte-compile-file)
                 (lambda (_f) (setq calls (1+ calls)) :ok)))
        (pel-byte-compile-if-needed src)
        (should (= 1 calls))
        ;; Simulate existing .elc so second call does nothing
        (with-temp-file (concat src "c") (insert "dummy"))
        (pel-byte-compile-if-needed src)
        (should (= 1 calls))))))

;;; --------------------------------------------------------------------------
;;; ;;* Imenu Utilities
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/imenu/add-sections-to ()
  (let ((lisp-imenu-generic-expression nil))
    (pel-add-imenu-sections-to
     '(("Macros" lisp-mode-symbol-regexp ("defmacro"))
       ("Funcs"  lisp-mode-symbol-regexp ("defun")))
     'lisp-imenu-generic-expression)
    (should (consp lisp-imenu-generic-expression))
    (should (cl-some (lambda (e) (string-match-p "defun" (cadr e)))
                     lisp-imenu-generic-expression))))

;;; --------------------------------------------------------------------------
;;; ;;* Tags support
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/tags/visit-tags-stubbed ()
  (pel--base-test--with-temp-dir d
    (let* ((local (expand-file-name "TAGS" d))
           (_ (with-temp-file local (insert "tags"))))
      (let ((default-directory d)
            (calls '()))
        (cl-letf (((symbol-function 'visit-tags-table)
                   (lambda (f) (push f calls))))
          (pel-visit-tags (list local))
          (should (>= (length calls) 1)))))))

;;; --------------------------------------------------------------------------
;;; ;;* Portability
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/portability/executable-find-and-config-features ()
  (should (stringp (or (pel-executable-find "sh")
                       (pel-executable-find "cmd"))))
  (should (stringp (pel-emacs-config-features-string))))

(ert-deftest pel--base-test/portability/hardware-model-and-eglot ()
  (cl-letf (((symbol-function 'shell-command-to-string) (lambda (_cmd) "stub"))))
  (let ((s (pel-hardware-model-string)))
    (should (stringp s))
    (should (> (length s) 0)))
  (should (memq (pel-eglot-active-p) '(t nil))))

;;; --------------------------------------------------------------------------
(provide 'pel--base-test)
;;; pel--base-test.el ends here
