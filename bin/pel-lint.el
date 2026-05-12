;;; pel-lint.el --- Lint PEL code  -*- lexical-binding: t; -*-

;; Created   : Monday, May 11 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-05-12 17:45:32 EDT, updated by Pierre Rouleau>

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
;; Usage (run from the PEL root directory):
;;
;;   emacs -Q --batch -l bin/pel-lint.el --eval "(pel-lint-main)"
;;   emacs -Q --batch -l bin/pel-lint.el --eval "(pel-lint-main)" -- PEL-DIR KEYS-FILE MACROS-FILE INSTALL-FILE AUTOLOAD-FILE
;;
;; Key Prefix Check
;; ================
;; Checks that every pel-setup-major-mode call either has an explicit
;; key-prefix: clause, or that its inferred pel:for-<target> symbol is
;; consistent with the key-vector assignments in pel--keys-macros.el.
;;
;; A mismatch is flagged as an error only when ALL of the following are true:
;;   1. The co-alias mode name also appears as a pel-setup-major-mode target.
;;   2. Neither mode is a Tree-Sitter variant of the other (name + "-ts").
;;   3. The inferred pel:for-<target> symbol is NOT defined anywhere in
;;      pel_keys.el via define-pel-global-prefix.

;; Mode Dispatching/Fixer Check
;; ============================
;; Three checks are performed:
;;
;;   F1. Every feature listed in `pel--ts-mode-with-fixer' has a
;;       corresponding  pel--<feature>-fixer  defun in a pel-*.el file.
;;
;;   F2. Every pel--*-fixer defun found in any pel-*.el file has its
;;       base feature symbol listed in `pel--ts-mode-with-fixer'.
;;       (Catches the case where the fixer is written but never wired.)
;;
;;   F3. Every feature listed in `pel--ts-mode-with-fixer' actually
;;       appears as a feature argument to `pel-eval-after-load' in
;;       pel_keys.el — either directly, inferred from a
;;       `pel-setup-major-mode' :same-for-ts / :independent-ts / :ts-only
;;       call, or inferred from a `pel-config-major-mode' :same-for-ts
;;       usage inside an existing `pel-eval-after-load' block.
;;       A mismatch means the fixer can never be called.
;;
;; Autoload-registration consistency checks
;; =========================================
;; Four checks are performed:
;;
;;   A1. Every symbol listed in a (pel-autoload FNAME for: ...) or
;;       (pel-autoload-function FNAME for: ...) call in pel-autoload.el
;;       has a matching definition (defun/defsubst/cl-defun/
;;       define-derived-mode/define-minor-mode/define-global-minor-mode/
;;       define-generic-mode) in FNAME.el.
;;       Catches typos such as double-dashes or renamed functions where
;;       the autoload entry was not updated.
;;
;;   A2. The source file FNAME.el named in each autoload entry exists and
;;       is readable in PEL-DIR.
;;       Catches stale entries left behind when a file is renamed or removed.
;;
;;   A3. A symbol is interactive (a command or mode) but is registered under
;;       `pel-autoload-function' (which is for non-interactive functions).
;;       → Should be moved to a `pel-autoload' entry.
;;
;;   A4. A symbol is non-interactive but is registered under `pel-autoload'
;;       (which is for interactive commands).
;;       → Should be moved to a `pel-autoload-function' entry.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

(eval-and-compile
  ;; Make the PEL root directory (one level above this bin/ directory)
  ;; available on the load-path so that PEL source files can be required
  ;; with (require 'pel--base), (require 'pel--macros), etc.
  (let ((pel-root (expand-file-name
                   ".."
                   (file-name-directory
                    (or load-file-name
                        (and (boundp 'byte-compile-current-file)
                             byte-compile-current-file)
                        buffer-file-name)))))
    (unless (member pel-root load-path)
      (push pel-root load-path))))

(require 'pel--base)           ; use: `pel+='
(require 'cl-lib)
(require 'subr-x)              ; use: `string-trim'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;* Utility Functions
;; ==================

(defun pel-lint/ts-variant-pair-p (a b)
  "Return non-nil when A and B are a Tree-Sitter variant pair.
That is: one of the two strings equals the other with \"-ts\" appended."
  (or (string-equal (concat a "-ts") b)
      (string-equal (concat b "-ts") a)))

;; ---------------------------------------------------------------------------
;;* Key Prefix Linting
;;  ==================

;; ---------------------------------------------------------------------------
;;** Step 1 – Parse pel--keys-macros.el

(defun pel-lint/parse-macros-file (filename)
  "Parse FILENAME (pel--keys-macros.el).
Return a cons cell (MODE-TO-VEC . VEC-TO-MODES) where:
  MODE-TO-VEC  is a hash table: mode-name string  -> key-vector string
  VEC-TO-MODES is a hash table: key-vector string -> list of mode-name strings"
  (let ((mode-to-vec  (make-hash-table :test #'equal))
        (vec-to-modes (make-hash-table :test #'equal)))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      ;; Match:  "modename"   [f11 ...]
      (while (re-search-forward
              "\"\\([a-zA-Z0-9_-]+\\)\"[[:space:]]+\\(\\[f11[^]]*\\]\\)"
              nil t)
        (let ((mode-name (match-string-no-properties 1))
              (key-vec   (string-trim (match-string-no-properties 2))))
          (puthash mode-name key-vec mode-to-vec)
          (let ((existing (gethash key-vec vec-to-modes '())))
            (puthash key-vec (append existing (list mode-name)) vec-to-modes)))))
    (cons mode-to-vec vec-to-modes)))

;; ---------------------------------------------------------------------------
;;** Step 2 – Parse pel_keys.el

(defun pel-lint/parse-keys-file (filename)
  "Parse FILENAME (pel_keys.el).
Return a cons cell (DEFINED-PREFIXES . TARGET-INFO) where:

DEFINED-PREFIXES is a hash table (used as a set) of all pel:for-XXX
symbols created by define-pel-global-prefix calls.

TARGET-INFO is a hash table: target-name string -> t (has explicit key-prefix:)
                                               -> nil (no explicit key-prefix:)"
  (let ((defined-prefixes (make-hash-table :test #'equal))
        (target-info      (make-hash-table :test #'equal)))
    (with-temp-buffer
      (insert-file-contents filename)
      ;; --- Collect define-pel-global-prefix definitions ---
      (goto-char (point-min))
      (while (re-search-forward
              "(define-pel-global-prefix[[:space:]]+\\(pel:[^[:space:]()\n]+\\)"
              nil t)
        (puthash (match-string-no-properties 1) t defined-prefixes))
      ;; --- Collect pel-setup-major-mode targets + key-prefix: presence ---
      (goto-char (point-min))
      (while (re-search-forward
              (concat "(pel-setup-major-mode[[:space:]]+"
                      "\\([^[:space:]\n]+\\)"
                      "[[:space:]]+:\\(?:no-ts\\|same-for-ts\\|ts-only\\)")
              nil t)
        (let* ((target     (match-string-no-properties 1))
               (body-start (match-end 0))
               ;; Body ends just before the next pel-setup-major-mode call.
               (body-end (save-excursion
                           (goto-char (match-beginning 0))
                           (condition-case nil
                               (progn (forward-sexp 1) (point))
                             (scan-error (point-max)))))
               (body       (buffer-substring-no-properties body-start body-end))
               (has-kp     (if (string-match-p "key-prefix:" body) t nil)))
          (puthash target has-kp target-info))))
    (cons defined-prefixes target-info)))

;; ---------------------------------------------------------------------------
;;** Step 3 – Cross-validate

(defun pel-lint/validate-key-prefixes (macros-file keys-file)
  "Return a list of error strings describing key-prefix mismatches.
Returns nil when everything is consistent.
MACROS-FILE is the path to pel--keys-macros.el.
KEYS-FILE is the path to pel_keys.el."
  (let* ((macros-result    (pel-lint/parse-macros-file macros-file))
         (mode-to-vec      (car macros-result))
         (vec-to-modes     (cdr macros-result))
         (keys-result      (pel-lint/parse-keys-file keys-file))
         (defined-prefixes (car keys-result))
         (target-info      (cdr keys-result))
         ;; Build a set of actual pel-setup-major-mode targets for fast lookup.
         (actual-targets   (let ((ht (make-hash-table :test #'equal)))
                             (maphash (lambda (k _) (puthash k t ht))
                                      target-info)
                             ht))
         (errors           '()))
    (maphash
     (lambda (target has-explicit-kp)
       (unless has-explicit-kp
         (let ((inferred-sym (concat "pel:for-" target)))
           ;; If pel:for-<target> IS defined, this target owns that prefix
           ;; canonically — no error.
           (unless (gethash inferred-sym defined-prefixes)
             (let* ((vec      (gethash target mode-to-vec))
                    (aliases  (when vec (gethash vec vec-to-modes '()))))
               (when aliases
                 ;; Keep only co-aliases that are also actual targets
                 ;; and are not TS variants of the current target.
                 (let ((conflicting
                        (cl-remove-if
                         (lambda (a)
                           (or (equal a target)
                               (not (gethash a actual-targets))
                               (pel-lint/ts-variant-pair-p target a)
                               ;; Exclude co-aliases that already own their
                               ;; inferred prefix via
                               ;; define-pel-global-prefix: they are the
                               ;; canonical owner, so no conflict exists.
                               (gethash (concat "pel:for-" a) defined-prefixes)))
                         aliases)))
                   (when conflicting
                     (push
                      (format
                       (concat
                        "  pel-setup-major-mode '%s':\n"
                        "    Inferred prefix 'pel:for-%s' is not defined via\n"
                        "    define-pel-global-prefix, but mode(s) %s share\n"
                        "    the same key-vector %S.\n"
                        "    → Add 'key-prefix: pel:for-%s' to the\n"
                        "      pel-setup-major-mode call for '%s' "
                        "(or whichever prefix is primary).")
                       target target conflicting vec
                       (car conflicting) target)
                      errors)))))))))
     target-info)
    (nreverse errors)))

;; ---------------------------------------------------------------------------
;;** Key Prefix Main

(defun pel-lint-key-prefixes (keys-file macros-file)
  "Run key prefix linting in KEYS-FILE and MACROS-FILE.  Return error count."
  (message "--------------------------------------------")
  (message "PEL key-prefix consistency validator")
  (message "  Macros file : %s" macros-file)
  (message "  Keys file   : %s" keys-file)
  (let ((errors (pel-lint/validate-key-prefixes macros-file keys-file)))
    (if errors
        (progn
          (message "\nERRORS — key-prefix alias mismatches found:")
          (dolist (e errors)
            (message "\n%s" e)))
      (message "OK — no key-prefix alias mismatches found."))
    (length errors)))

;; ---------------------------------------------------------------------------
;;* Mode Dispatcher/Fixer Linting
;;  =============================

;;** Step F1 – Extract pel--ts-mode-with-fixer list from pel--install.el

(defun pel-lint/parse-fixer-list (install-file)
  "Return list of feature-name from `pel--ts-mode-with-fixer' in INSTALL-FILE.
Return a list of feature-name strings.
Uses the Emacs Lisp reader: comments are automatically ignored."
  (with-temp-buffer
    (insert-file-contents install-file)
    (goto-char (point-min))
    (let (result)
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              ;; Looking for: (defconst pel--ts-mode-with-fixer '(sym ...) "doc")
              (when (and (listp form)
                         (eq (car  form) 'defconst)
                         (eq (cadr form) 'pel--ts-mode-with-fixer))
                (let ((val (nth 2 form))) ; third element is the value expression
                  ;; value is (quote (ada-ts-mode dart-ts-mode ...))
                  (when (and (listp val)
                             (eq (car val) 'quote)
                             (listp (cadr val)))
                    (setq result
                          (mapcar #'symbol-name (cadr val))))))))
        (end-of-file nil))
      result)))

;; ---------------------------------------------------------------------------
;;** Step F2 – Find all pel--*-fixer defuns in pel-*.el source files

(defun pel-lint/parse-el-files-for-fixers (pel-dir)
  "Return alist (FEATURE-NAME . FILENAME) for every `pel--<f>-fixer' defun found.
Scans all pel-[a-z]*.el files in PEL-DIR using the Lisp reader."
  (let (result)
    (dolist (fpath (directory-files
                    pel-dir t "\\`pel-[a-z][a-z0-9-]*\\.el\\'"))
      (let ((fname (file-name-nondirectory fpath)))
        (with-temp-buffer
          (insert-file-contents fpath)
          (goto-char (point-min))
          (condition-case nil
            (while t
              (let ((form (read (current-buffer))))
                ;; Looking for: (defun pel--<feature>-fixer () ...)
                (when (and (consp form)
                           (memq (car form) '(defun defsubst))
                           (symbolp (cadr form)))
                  (let ((fn-name (symbol-name (cadr form))))
                    (when (string-match "\\`pel--\\(.*\\)-fixer\\'" fn-name)
                      (push (cons (match-string 1 fn-name) fname)
                            result))))))
            (end-of-file nil)))))
    (nreverse result)))

;; ---------------------------------------------------------------------------
;;** Step F3 – Collect all feature symbols reachable via pel-eval-after-load

(defun pel-lint/parse-keys-for-eval-after-load-features (keys-file)
  "Return hash-table of feature-name strings reachable via `pel-eval-after-load'.
Uses the Emacs Lisp reader to parse KEYS-FILE; comments and strings are
automatically ignored.  Handles:
  - (pel-eval-after-load FEATURE ...)
  - (pel-eval-after-load (F1 F2 ...) ...)
  - (pel-setup-major-mode TARGET :same-for-ts|:independent-ts|:ts-only ...)
  - (pel-config-major-mode TARGET ... :same-for-ts|:independent-ts|:ts-only ...)"
  (let ((features (make-hash-table :test #'equal)))
    (cl-labels
        ((note (sym)
           (puthash (symbol-name sym) t features))
         (ts-option-p (kw)
           (memq kw '(:same-for-ts :independent-ts :ts-only)))
         (walk-list (tail)
           ;; Safe list traversal: stops at non-cons cdr (handles dotted pairs).
           (while (consp tail)
             (walk (car tail))
             (setq tail (cdr tail))))
         (walk (form)
           (when (consp form)          ; consp excludes nil and non-list atoms
             (cond
              ;; (pel-eval-after-load FEATURE ...) — single symbol
              ((and (eq (car form) 'pel-eval-after-load)
                    (symbolp (cadr form)))
               (note (cadr form))
               (walk-list (cddr form)))
              ;; (pel-eval-after-load (F1 F2 ...) ...) — list of symbols
              ((and (eq (car form) 'pel-eval-after-load)
                    (listp (cadr form)))
               (let ((feat-tail (cadr form)))
                 (while (consp feat-tail)
                   (when (symbolp (car feat-tail))
                     (note (car feat-tail)))
                   (setq feat-tail (cdr feat-tail))))
               (walk-list (cddr form)))
              ;; (pel-setup-major-mode TARGET OPTION ...)
              ((eq (car form) 'pel-setup-major-mode)
               (let ((target (cadr form)))
                 (when (symbolp target)
                   (let ((args (cddr form)))
                     (while args
                       (when (ts-option-p (car args))
                         (note (intern
                                (concat (symbol-name target) "-ts-mode")))
                         (unless (eq (car args) :ts-only)
                           (note target)))
                       (setq args (cdr args))))))
               (walk-list (cddr form)))
              ;; (pel-config-major-mode TARGET KEYVAR OPTION ...)
              ;; Note: KEYVAR may contain ':' (e.g. pel:for-js); the reader
              ;; handles this transparently.
              ((eq (car form) 'pel-config-major-mode)
               (let ((target (cadr form)))
                 (when (symbolp target)
                   (let ((args (cddr form)))
                     (while args
                       (when (ts-option-p (car args))
                         (note (intern
                                (concat (symbol-name target) "-ts-mode")))
                         (unless (eq (car args) :ts-only)
                           (note target)))
                       (setq args (cdr args))))))
               (walk-list (cddr form)))
              ;; Any other cons: recurse into sub-forms safely.
              ;; Using walk-list (while consp) rather than mapc so that
              ;; improper/dotted-pair lists like ("a" . "α") don't crash.
              (t
               (walk-list form))))))
      (with-temp-buffer
        (insert-file-contents keys-file)
        (goto-char (point-min))
        (condition-case nil
          (while t
            (walk (read (current-buffer))))
          (end-of-file nil))))
    features))

;; ---------------------------------------------------------------------------
;;** Step F4 – Cross-validate

(defun pel-lint/validate-fixer-registrations (install-file keys-file pel-dir)
  "Return list of error strings for fixer-registration inconsistencies.

Three checks are performed (see file commentary for full description).
INSTALL-FILE is path to pel--install.el.
KEYS-FILE    is path to pel_keys.el.
PEL-DIR      is the directory containing the pel-*.el source files."
  (let* ((fixer-list      (pel-lint/parse-fixer-list install-file))
         (defined-fixers  (pel-lint/parse-el-files-for-fixers pel-dir))
         (active-feats    (pel-lint/parse-keys-for-eval-after-load-features
                           keys-file))
         ;; Fast-lookup sets
         (fixer-list-ht   (let ((ht (make-hash-table :test #'equal)))
                            (dolist (f fixer-list) (puthash f t ht))
                            ht))
         (defined-ht      (let ((ht (make-hash-table :test #'equal)))
                            (dolist (pair defined-fixers)
                              (puthash (car pair) (cdr pair) ht))
                            ht))
         (errors '()))
    ;; -- Check F1: every fixer-list entry has a defined function ----------
    (dolist (feature fixer-list)
      (unless (gethash feature defined-ht)
        (push
         (format
          (concat "  [F1] pel--ts-mode-with-fixer lists '%s' but no\n"
                  "       'pel--%s-fixer' defun was found in any pel-*.el file.\n"
                  "    → Define (defun pel--%s-fixer () ...) in pel-%s.el\n"
                  "      and register it with pel-autoload-function in\n"
                  "      pel-autoload.el.")
          feature feature feature
          (replace-regexp-in-string "-ts\\(-mode\\)?\\'" "" feature))
         errors)))
    ;; -- Check F2: every defined fixer function has its feature in list --
    (maphash
     (lambda (feature src-file)
       (unless (gethash feature fixer-list-ht)
         (push
          (format
           (concat "  [F2] 'pel--%s-fixer' is defined in %s but\n"
                   "       '%s' is NOT listed in pel--ts-mode-with-fixer\n"
                   "       in pel--install.el.  The fixer will never be called.\n"
                   "    → Add '%s' to the pel--ts-mode-with-fixer constant.")
           feature src-file feature feature)
          errors)))
     defined-ht)
    ;; -- Check F3: every fixer-list entry is reachable -------------------
    (dolist (feature fixer-list)
      (unless (gethash feature active-feats)
        (push
         (format
          (concat "  [F3] 'pel--%s-fixer' can never be triggered:\n"
                  "       '%s' is in pel--ts-mode-with-fixer but does not\n"
                  "       appear as a feature argument to pel-eval-after-load\n"
                  "       (directly or via pel-setup/config-major-mode) in\n"
                  "       pel_keys.el.\n"
                  "    → Either correct the feature name in the constant\n"
                  "      (e.g. 'erlang-ts-mode' → 'erlang-ts') or add an\n"
                  "      explicit (pel-eval-after-load %s ...) call.")
          feature feature feature)
         errors)))
    (nreverse errors)))


;; ---------------------------------------------------------------------------
;;** Fixer-check main entry point

(defun pel-lint-fixers (pel-dir keys-file install-file)
  "Run fixer-registration consistency checks; Return error count."
  (message "--------------------------------------------")
  (message "PEL fixer-registration consistency validator")
  (message "  Install file : %s" install-file)
  (message "  Keys file    : %s" keys-file)
  (message "  PEL source   : %s" pel-dir)
  (let ((errors (pel-lint/validate-fixer-registrations
                 install-file keys-file pel-dir)))
    (if errors
        (progn
          (message "\nERRORS — fixer-registration inconsistencies found:")
          (dolist (e errors) (message "\n%s" e)))
      (message "OK — no fixer-registration inconsistencies found."))
    (length errors)))

;; ---------------------------------------------------------------------------
;;* Autoload Checker
;;  ================

;; ---------------------------------------------------------------------------
;;** Step A1/A2 – Parse pel-autoload.el for (pel-autoload[−function] ...) calls

(defun pel-lint/parse-autoload-entries (autoload-file)
  "Parse AUTOLOAD-FILE; return list of (MACRO-NAME FILENAME SYMBOL-LIST) triples.
MACRO-NAME is the symbol `pel-autoload' or `pel-autoload-function' — this
distinguishes interactive commands from non-interactive functions.
FILENAME is the bare string from each call (e.g. \"pel-ada\").
SYMBOL-LIST is the list of function symbols registered.
Uses the Lisp reader; comments, strings and dotted pairs are handled safely.
Forms nested inside `when', `if', `cond', etc. are traversed recursively."
  (let (result)
    (cl-labels
        ((walk-list (tail)
           ;; Safe traversal: stops at non-cons cdr (handles dotted pairs).
           (while (consp tail)
             (walk (car tail))
             (setq tail (cdr tail))))
         (walk (form)
           (when (consp form)
             (cond
              ;; (pel-autoload        FNAME for: SYM ...) or
              ;; (pel-autoload-function FNAME for: SYM ...)
              ((and (memq (car form) '(pel-autoload pel-autoload-function))
                    (stringp (cadr form)))
               (let* ((macro-name (car form))
                      (fname      (cadr form))
                      ;; caddr is the literal `for:' keyword — skip it
                      (sym-tail   (cdddr form))
                      (sym-list   nil))
                 (while (consp sym-tail)
                   (when (symbolp (car sym-tail))
                     (push (car sym-tail) sym-list))
                   (setq sym-tail (cdr sym-tail)))
                 (when sym-list
                   (push (list macro-name fname (nreverse sym-list))
                         result))))
              ;; Any other cons: recurse into all sub-forms.
              (t
               (walk-list form))))))
      (with-temp-buffer
        (insert-file-contents autoload-file)
        (goto-char (point-min))
        (condition-case nil
          (while t (walk (read (current-buffer))))
          (end-of-file nil))))
    (nreverse result)))

;; ---------------------------------------------------------------------------
;;** Step A1 helper – Collect definition names and interactivity from a source file

(defun pel-lint/collect-defuns-in-file (filepath)
  "Return hash-table mapping function-name strings to their interactivity.
The value stored for each function is:
  t   — the function is interactive (a command or a mode function).
  nil — the function exists but has no interactive declaration.
Use `:absent' as the DEFAULT argument to `gethash' to distinguish a
non-interactive function (nil value) from a missing one (`:absent').

Recognized definition forms:
  `defun', `defsubst', `cl-defun'  — interactive if first non-docstring
    body form is (interactive ...).
  `define-derived-mode', `define-minor-mode',
  `define-global-minor-mode', `define-generic-mode'  — always interactive.

Traverses nested forms so definitions inside `when', `unless', etc. are found.
Uses the Lisp reader; comments and strings are automatically skipped."
  (let ((defuns (make-hash-table :test #'equal)))
    (cl-labels
        ((interactive-body-p (body)
           ;; BODY is cdddr of the defun: ([DOCSTRING] BODYFORM...).
           ;; Skip an optional leading docstring AND an optional leading
           ;; (declare ...) form, then check whether the first remaining
           ;; form is (interactive ...).
           (let ((forms body))
             (when (and (consp forms) (stringp (car forms)))
               (setq forms (cdr forms)))
             (when (and (consp forms)
                        (consp (car forms))
                        (eq (caar forms) 'declare))
               (setq forms (cdr forms)))
             (and (consp forms)
                  (consp (car forms))
                  (eq (caar forms) 'interactive))))
         (walk-list (tail)
           (while (consp tail)
             (walk (car tail))
             (setq tail (cdr tail))))
         (walk (form)
           (when (consp form)
             (cond
              ;; defun / defsubst / cl-defun — interactivity from body
              ((and (memq (car form) '(defun defsubst cl-defun))
                    (symbolp (cadr form)))
               (puthash (symbol-name (cadr form))
                        (interactive-body-p (cdddr form))
                        defuns)
               ;; Recurse into body to catch any nested definitions.
               (walk-list (cdddr form)))
              ;; Mode-defining macros — always produce an interactive command.
              ;; define-derived-mode : (define-derived-mode MODE PARENT "Name" ...)
              ;; define-minor-mode   : (define-minor-mode MODE ...)
              ;; define-global-minor-mode, define-generic-mode: same pattern.
              ((and (memq (car form) '(define-derived-mode
                                       define-minor-mode
                                       define-global-minor-mode
                                       define-generic-mode))
                    (symbolp (cadr form)))
               (puthash (symbol-name (cadr form)) t defuns))
              ;; Any other cons: recurse into sub-forms.
              (t
               (walk-list form))))))
      (with-temp-buffer
        (insert-file-contents filepath)
        (goto-char (point-min))
        (condition-case nil
          (while t (walk (read (current-buffer))))
          (end-of-file nil))))
    defuns))

;; ---------------------------------------------------------------------------
;;** Cross-validate

(defun pel-lint/validate-autoload-registrations (autoload-file pel-dir)
  "Return list of error strings for autoload-registration inconsistencies.

AUTOLOAD-FILE is the path to pel-autoload.el.
PEL-DIR is the directory containing the pel-*.el source files.

Four checks are performed (see file commentary for full description):
A1 — listed symbol has no matching definition in the named source file.
A2 — named source file does not exist in PEL-DIR.
A3 — listed symbol is interactive but registered under `pel-autoload-function'.
A4 — listed symbol is non-interactive but registered under `pel-autoload'."
  (let ((entries (pel-lint/parse-autoload-entries autoload-file))
        errors)
    (dolist (entry entries)
      (let* ((macro-name (nth 0 entry))
             (fname      (nth 1 entry))
             (symbols    (nth 2 entry))
             (filepath   (expand-file-name (concat fname ".el") pel-dir)))
        ;; -- Check A2: source file must exist and be readable ---------------
        (if (not (file-readable-p filepath))
            (push
             (format
              (concat "  [A2] pel-autoload.el references \"%s\" but\n"
                      "       %s\n"
                      "       does not exist or is not readable.\n"
                      "    → Remove or correct the autoload entry in\n"
                      "      pel-autoload.el.")
              fname filepath)
             errors)
          ;; -- Checks A1 / A3 / A4: inspect each listed symbol --------------
          (let ((defuns (pel-lint/collect-defuns-in-file filepath)))
            (dolist (sym symbols)
              (let* ((sym-name      (symbol-name sym))
                     ;; :absent sentinel distinguishes nil (non-interactive)
                     ;; from a truly missing function.
                     (interactive-p (gethash sym-name defuns :absent)))
                (cond
                 ;; A1: function not defined in the named file at all
                 ((eq interactive-p :absent)
                  (push
                   (format
                    (concat "  [A1] pel-autoload.el lists '%s'\n"
                            "       for file \"%s\", but no matching definition\n"
                            "       (defun / defsubst / cl-defun /\n"
                            "        define-derived-mode / define-minor-mode /\n"
                            "        define-global-minor-mode / define-generic-mode)\n"
                            "       named '%s' was found in\n"
                            "       %s.\n"
                            "    → Check for a typo in pel-autoload.el or a\n"
                            "      missing/renamed definition in %s.el.")
                    sym-name fname sym-name filepath fname)
                   errors))
                 ;; A3: interactive symbol listed under pel-autoload-function
                 ((and (eq macro-name 'pel-autoload-function)
                       interactive-p)
                  (push
                   (format
                    (concat "  [A3] '%s' in \"%s\" is an interactive command\n"
                            "       but is registered under `pel-autoload-function'.\n"
                            "    → Move '%s' to a `pel-autoload' entry in\n"
                            "      pel-autoload.el.")
                    sym-name fname sym-name)
                   errors))
                 ;; A4: non-interactive symbol listed under pel-autoload
                 ((and (eq macro-name 'pel-autoload)
                       (not interactive-p))
                  (push
                   (format
                    (concat "  [A4] '%s' in \"%s\" is a non-interactive function\n"
                            "       but is registered under `pel-autoload'.\n"
                            "    → Move '%s' to a `pel-autoload-function' entry in\n"
                            "      pel-autoload.el.")
                    sym-name fname sym-name)
                   errors)))))))))
    (nreverse errors)))

;; ---------------------------------------------------------------------------
;;** Autoload-check entry point

(defun pel-lint-autoloads (autoload-file pel-dir)
  "Run autoload-registration consistency checks.  Return error count."
  (message "--------------------------------------------")
  (message "PEL autoload-registration consistency validator")
  (message "  Autoload file : %s" autoload-file)
  (message "  PEL source    : %s" pel-dir)
  (let ((errors (pel-lint/validate-autoload-registrations
                 autoload-file pel-dir)))
    (if errors
        (progn
          (message "\nERRORS — autoload-registration inconsistencies found:")
          (dolist (e errors) (message "\n%s" e)))
      (message "OK — no autoload-registration inconsistencies found."))
    (length errors)))

;; ---------------------------------------------------------------------------
;;* Top Level Script Main
;;  =====================

(defun pel-lint-main ()
  "Execute all linters.  Exit Emacs with status 0 if all ok, 1 otherwise."
  (let* ((extra-args  command-line-args-left) ; args that follow the script name
         (pel-dir      (or (nth 0 extra-args) "."))
         (keys-file    (or (nth 1 extra-args) "pel_keys.el"))
         (macros-file  (or (nth 2 extra-args) "pel--keys-macros.el"))
         (install-file (or (nth 3 extra-args) "pel--install.el"))
         (autoload-file (or (nth 4 extra-args) "pel-autoload.el"))
         (errors 0))

    ;; Prevent Emacs from attempting to process remaining args itself.
    (setq command-line-args-left nil)

    ;; Check presence of files that must be parsed
    (unless (file-directory-p pel-dir)
      (message "ERROR: Not a directory: %s" pel-dir)
      (kill-emacs 2))
    (dolist (f (list keys-file macros-file install-file autoload-file))
      (unless (file-readable-p f)
        (message "ERROR: Cannot read: %s" f)
        (kill-emacs 2)))

    ;; Proceed: run linters
    (pel+= errors (pel-lint-key-prefixes keys-file macros-file))
    (message "")
    (pel+= errors (pel-lint-fixers pel-dir keys-file install-file))
    (message "")
    (pel+= errors (pel-lint-autoloads autoload-file pel-dir))
    (message "")
    (if (= errors 0)
        (kill-emacs 0)
      (kill-emacs 1))))

;; ---------------------------------------------------------------------------
(provide 'pel-lint)
;;; pel-lint.el ends here
