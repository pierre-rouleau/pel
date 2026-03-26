;;; pel--base-test.el --- ERT tests for pel--base.el  -*- lexical-binding: t; -*-

;; Created   : Tuesday, March 24 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-26 15:05:53 EDT, updated by Pierre Rouleau>

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
;; - Tests are organized in the same order as functions in pel--base.el (by
;;   coarse sections, matching the file’s banners).
;; - Tests avoid environment brittleness (Tree‑sitter, user dirs, symlinks)
;;   by stubbing or canonicalizing paths via file-truename.
;; - When a test needs a void symbol, it uses makunbound to keep the symbol
;;   genuinely unbound (no defvar), so pel-symbol-value-or hits the “unknown”
;;   branch. Where a variable must be seen by bound-and-true-p, it is defvar’d.
;; - No use of string-search (Emacs 28+); only string-match-p.
;; - No `#b` binary integer literals; only decimal integers.
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
;;   pel-grp-regex, pel--format-problem-messages, pel-format-problem-messages,
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
;;   pel-rebuild-utils        - stub file-directory-p to force the “no utils dir” path;
;;                              capture display-warning without asserting its backend.
;;   pel-major-mode-use-tree-sitter, pel-treesit-ready-p, pel-ts-language-grammar-*
;;                              - stub pel-uses-tree-sitter, fboundp treesit, and treesit-ready-p.
;;
;; Notes:
;; - Tests that depend on exact user directory layouts or platform‑specific
;;   canonicalization compare truename-normalized paths (file-truename).
;; - Buffers created for “fresh” assertions use generate-new-buffer to avoid
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
;;; 1) Version, macros, predicates, small utilities
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/version/semverish ()
  (let ((v (pel-version)))
    (should (stringp v))
    (should (string-match-p "^[0-9]+\\.[0-9]+\\.[0-9]+" v))))

(ert-deftest pel--base-test/macros/pel-plus-minus ()
  (let ((x 1))
    (pel+= x 2)  (should (= x 3))
    (pel-= x 1)  (should (= x 2))))

(ert-deftest pel--base-test/macro/lambda-compose ()
  (should (= 4 (λc (lambda (n) (1+ n)) 3))))

(ert-deftest pel--base-test/predicates/expression-and-user-option ()
  (should (pel-expression-p 'foo))
  (should (pel-expression-p '(+ 1 2)))
  (should-not (pel-expression-p 42))
  (should (pel-user-option-p 'pel-use-common-lisp))
  (should-not (pel-user-option-p 'pel-use-visual-basic)))

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

;; Special var for fast-startup check
(defvar pel-running-in-fast-startup-p nil)
(ert-deftest pel--base-test/fast-startup/bound-and-true-p ()
  (let ((pel-running-in-fast-startup-p nil))
    (should-not (pel-in-fast-startup-p)))
  (let ((pel-running-in-fast-startup-p t))
    (should (pel-in-fast-startup-p))))

(ert-deftest pel--base-test/list-of-and-transpose-alist ()
  (should (equal '(a)   (pel-list-of 'a)))
  (should (equal '(a b) (pel-list-of '(a b))))
  (should (equal '((2 . 1) (4 . 3)) (pel-transpose-alist '((1 . 2) (3 . 4))))))

;;; --------------------------------------------------------------------------
;;; 2) Major/minor mode, buffer info, directories
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/mode/guards-and-derived ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (pel-derived-mode-p nil 'emacs-lisp-mode))
    (should-error (pel-major-mode-must-be 'c-mode) :type 'user-error)
    (should-not (pel-dired-buffer-p nil t))))

(ert-deftest pel--base-test/mode/file-type-and-string-with-major-mode ()
  (should (string= "emacs-lisp" (pel-file-type-for 'emacs-lisp-mode)))
  (should (string= "c"          (pel-file-type-for 'c-ts-mode)))
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (string= "wrap-emacs-lisp-done"
                     (pel-string-with-major-mode "wrap-%s-done")))))

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

;; Major-mode-bound symbols
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
          ;; Current buffer is in text-mode; we query target-buf for c-mode.
          ;; Should be nil because target is emacs-lisp-mode, not c-mode.
          ;; With the OLD (buggy) code this would have checked current buffer
          ;; (text-mode) and also returned nil, masking the bug; but checking
          ;; emacs-lisp-mode vs. emacs-lisp-mode for the target confirms the
          ;; fix works in both directions.
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
;;
;; emacs-lisp-mode is derived from prog-mode (Emacs >= 24).
;; c-mode is also derived from prog-mode.

(ert-deftest pel--base-test/derived-mode-p/ancestor-mode-current-buffer ()
  "`pel-derived-mode-p' returns non-nil when the specified mode is an ancestor.
`emacs-lisp-mode' is derived from `prog-mode'."
  (pel--base-test--with-mode-buffer 'emacs-lisp-mode
    (should (pel-derived-mode-p nil 'prog-mode))))

(ert-deftest pel--base-test/derived-mode-p/ancestor-mode-other-buffer ()
  "`pel-derived-mode-p' detects ancestor mode in a specified buffer."
  (let ((target-buf (generate-new-buffer " *pel-test-ancestor*")))
    (unwind-protect
        (progn
          (with-current-buffer target-buf
            (emacs-lisp-mode))
          ;; From a fundamental-mode buffer, ask about prog-mode ancestry of target.
          (with-temp-buffer
            (fundamental-mode)
            (should (pel-derived-mode-p target-buf 'prog-mode))))
      (kill-buffer target-buf))))

;; ---------------------------------------------------------------------------
;; 6. Non-matching mode returns nil

(ert-deftest pel--base-test/derived-mode-p/non-matching-mode-nil ()
  "`pel-derived-mode-p' returns nil when mode is unrelated to the buffer's mode."
  (pel--base-test--with-mode-buffer 'emacs-lisp-mode
    ;; emacs-lisp-mode is not derived from c-mode.
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
    ;; prog-mode is an ancestor of emacs-lisp-mode.
    (should (pel-derived-mode-p nil 'text-mode 'prog-mode))))

;;; --------------------------------------------------------------------------
;;; 3) OS/Env and Emacs environment helpers
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/os-env/terminal-and-ssh ()
  (let ((process-environment (cons "TERM_PROGRAM=Apple_Terminal" process-environment)))
    (should (pel-terminal-is-macos-terminal-p)))
  (let ((process-environment (cons "SSH_CLIENT=1.2.3.4 22 12345" process-environment)))
    (should (pel-running-under-ssh-p))))

(ert-deftest pel--base-test/emacs-env/locate-user-emacs-file-and-loadpath ()
  (should (file-name-absolute-p (pel-locate-user-emacs-file "pel-base-test.tmp")))
  (pel--base-test--with-temp-dir tmpd
    (let ((added (pel-add-dir-to-loadpath tmpd)))
      (should added)
      (should (member (directory-file-name (file-truename tmpd))
                      (mapcar (lambda (p) (directory-file-name (file-truename p)))
                              load-path))))
    (let ((added2 (pel-add-dir-to-loadpath tmpd)))
      (should (not added2)))))

;;; --------------------------------------------------------------------------
;;; 4) File system type helpers
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
;;; 5) String predicates and transformations
;;; --------------------------------------------------------------------------

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

(ert-deftest pel--base-test/pluralizer ()
  (should (equal "classes"  (pel-plural-of "class")))
  (should (equal "tomatoes" (pel-plural-of "tomato")))
  (should (equal "skies"    (pel-plural-of "sky")))
  (should (equal "calves"   (pel-plural-of "calf")))
  (should (equal "commands" (pel-plural-of "command")))
  (should (equal "2 cats" (pel-count-string 2 "cat")))
  (should (equal "cat"     (pel-pluralize 1 "cat")))
  (should (equal "cats"    (pel-pluralize 3 "cat"))))

;;; --------------------------------------------------------------------------
;;; 6) Symbol value helpers and presentation
;;; --------------------------------------------------------------------------

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
;;; 7) Message/format helpers and list utilities
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

(ert-deftest pel--base-test/auto-mode-alist/delete ()
  (let ((auto-mode-alist
         '(("\\.foo\\'" . text-mode)
           ("\\.bar\\'" . fundamental-mode)
           ("\\.baz\\'" . text-mode))))
    (pel-delete-from-auto-mode-alist 'text-mode)
    (should (equal '(("\\.bar\\'" . fundamental-mode)) auto-mode-alist))))

;;; --------------------------------------------------------------------------
;;; 8) Build utils and Tree‑sitter (stubbed where needed)
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/utils/rebuild-utils/no-utils-dir-warning ()
  (let ((warn (pel--base-test--capture-warning
               (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) nil)))
                 (pel-rebuild-utils)))))
    (should (or (null warn)
                (string-match-p "utils directory" warn)))))

(ert-deftest pel--base-test/treesit/mode-remap-and-supported-p ()
  "Deterministic across environments. If the implementation consults
`pel-treesit-ready-p', verify that readiness influences the result;
otherwise verify consistent boolean results with a remap present."
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
;;; 9) Actions, toggles, symbols/hooks
;;; --------------------------------------------------------------------------

(ert-deftest pel--base-test/action/interpretation ()
  (should (eq 'activate   (pel-action-for nil nil)))
  (should (eq 'deactivate (pel-action-for nil t)))
  (should (eq 'activate   (pel-action-for 1 nil)))
  (should (eq nil         (pel-action-for 1 t)))
  (should (eq nil         (pel-action-for -1 nil)))
  (should (eq 'deactivate (pel-action-for -1 t))))

(ert-deftest pel--base-test/toggle-mode/abbrev ()
  (require 'abbrev)
  (with-temp-buffer
    (abbrev-mode -1)
    (let ((new (pel-toggle-mode 'abbrev-mode)))
      (should new)
      (let ((msg (pel--base-test--capture-message
                  (pel-toggle-mode-and-show 'abbrev-mode))))
        (should (stringp msg))))))

;; Use defvar for dynamic binding tests
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

(ert-deftest pel--base-test/symbols/hook-and-map ()
  (should (eq 'text-mode-hook (pel-hook-symbol-for 'text-mode)))
  (should (eq 'text-mode-map  (pel-map-symbol-for  'text-mode))))

(defvar pel--t-my-modes '(emacs-lisp-mode))
(ert-deftest pel--base-test/hooks/add-hook-for ()
  (let ((pel--t-my-modes '(emacs-lisp-mode fundamental-mode))
        (emacs-lisp-mode-hook nil))
    (pel-add-hook-for 'pel--t-my-modes #'ignore '(emacs-lisp-mode))
    (should (memq #'ignore emacs-lisp-mode-hook))))

(defvar pel--t-fake-list nil)
(ert-deftest pel--base-test/minor-modes/check-minor-modes-in ()
  (should (>= (pel--check-minor-modes-in 'pel--t-fake-list '(no-such-mode)) 0))
  (should (>= (pel-check-minor-modes-in pel--t-fake-list) 0)))

;;; --------------------------------------------------------------------------
;;; 10) Text at point and navigation
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

(ert-deftest pel--base-test/point/same-line-p-and-region-for ()
  (with-temp-buffer
    (insert "A\nSTART here\nMIDDLE\nEND here\nZ\n")
    (should (pel-same-line-p 1 2))
    (let ((reg (pel-region-for "START" "END")))
      (should reg)
      (should (integerp (car reg)))
      (should (integerp (cdr reg))))))

;;; --------------------------------------------------------------------------
;;; 11) Insert/overwrite, extract text, shebang, code, whitespace
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

(ert-deftest pel--base-test/extract/text-from-bol ()
  (with-temp-buffer
    (insert "hello\nworld\n")
    (goto-char (point-min))
    (forward-line 1)                 ; at beginning of "world"
    (search-forward "wor")           ; point just after "wor"
    (should (string= "wor" (pel-text-from-beginning-of-line)))))

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

(ert-deftest pel--base-test/line/only-whitespace-p ()
  (with-temp-buffer
    (insert "   \nabc\n")
    (goto-char (point-min))
    (should (pel-line-has-only-whitespace-p))   ; first line is spaces only
    (forward-line 1)
    (move-beginning-of-line 1)                  ; be explicit: at "abc"
    (should-not (pel-line-has-only-whitespace-p))))

;;; --------------------------------------------------------------------------
;;; Tests for `pel-line-has-only-whitespace-p'
;;; --------------------------------------------------------------------------

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

;;; --------------------------------------------------------------------------
;;; 12) File paths and URLs
;;; --------------------------------------------------------------------------

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
;;; 13) Insertion helpers, pretty-print, movement, print, parse, byte-compile, imenu, TAGS, portability
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

;; pel-move-right-by — core behavior and invariants
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

(ert-deftest pel--base-test/imenu/add-sections-to ()
  (let ((lisp-imenu-generic-expression nil))
    (pel-add-imenu-sections-to
     '(("Macros" lisp-mode-symbol-regexp ("defmacro"))
       ("Funcs"  lisp-mode-symbol-regexp ("defun")))
     'lisp-imenu-generic-expression)
    (should (consp lisp-imenu-generic-expression))
    (should (cl-some (lambda (e) (string-match-p "defun" (cadr e)))
                     lisp-imenu-generic-expression))))

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
