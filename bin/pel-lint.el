;;; pel-lint.el --- Lint PEL code  -*- lexical-binding: t; -*-

;; Created   : Monday, May 11 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-05-12 10:38:04 EDT, updated by Pierre Rouleau>

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
;;   emacs -Q --batch -l bin/pel-lint.el --eval "(pel-lint-main)" -- MACROS-FILE KEYS-FILE
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
;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
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
  "Run key prefix linting in KEYS-FILE and MACROS_FILE.  Return error count."
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
  "Return list of feature-name from pel--ts-mode-with-fixer in INSTALL-FILE."
  (let ((modes '()))
    (with-temp-buffer
      (insert-file-contents install-file)
      (goto-char (point-min))
      (when (re-search-forward
             "(defconst[[:space:]]+pel--ts-mode-with-fixer[[:space:]]+'("
             nil t)
        (let ((list-end (save-excursion
                          (goto-char (1- (point))) ; back to opening '('
                          (forward-sexp 1)
                          (point))))
          (while (re-search-forward "\\([a-z][a-z0-9-]+\\)" list-end t)
            (push (match-string-no-properties 1) modes)))))
    (nreverse modes)))

;; ---------------------------------------------------------------------------
;;** Step F2 – Find all pel--*-fixer defuns in pel-*.el source files

(defun pel-lint/parse-el-files-for-fixers (pel-dir)
  "Return alist of (FEATURE-NAME . FILENAME) for every pel--%s-fixer defun found.
FEATURE-NAME is the string between \"pel--\" and \"-fixer\" in the function
name — it must exactly equal an entry in `pel--ts-mode-with-fixer' for the
fixer to be triggered.  Searches all pel-[a-z]*.el files in PEL-DIR."
  (let ((result '()))
    (dolist (fpath (directory-files pel-dir t "\\`pel-[a-z][a-z0-9-]*\\.el\\'"))
      (let ((fname (file-name-nondirectory fpath)))
        (with-temp-buffer
          (insert-file-contents fpath)
          (goto-char (point-min))
          (while (re-search-forward
                  (concat "(defun[[:space:]]+pel--"
                          "\\([a-z][a-z0-9-]+\\)"
                          "-fixer[[:space:]]*()")
                  nil t)
            (push (cons (match-string-no-properties 1) fname) result)))))
    (nreverse result)))

;; ---------------------------------------------------------------------------
;;** Step F3 – Collect all feature symbols reachable via pel-eval-after-load

(defun pel-lint/parse-keys-for-eval-after-load-features (keys-file)
  "Return hash-table of feature-name strings reachable by pel-eval-after-load.
Collects from KEYS-FILE (pel_keys.el):
  1. Single-feature  (pel-eval-after-load FEATURE ...)  calls.
  2. Multi-feature   (pel-eval-after-load (F1 F2 ...) ...)  calls.
  3. Features inferred from
     (pel-setup-major-mode TARGET :same-for-ts|:independent-ts|:ts-only ...)
     which the macro expands to pel-eval-after-load with TARGET-ts-mode.
  4. Features inferred from
     (pel-config-major-mode TARGET ... :same-for-ts|:independent-ts ...)
     inside an existing pel-eval-after-load block."
  (let ((features (make-hash-table :test #'equal)))
    (with-temp-buffer
      (insert-file-contents keys-file)
      ;; 1. Direct: (pel-eval-after-load single-feature ...)
      (goto-char (point-min))
      (while (re-search-forward
              "(pel-eval-after-load[[:space:]]+\\([a-z][a-z0-9-]+\\)"
              nil t)
        (puthash (match-string-no-properties 1) t features))
      ;; 2. Direct: (pel-eval-after-load (feat1 feat2 ...) ...)
      (goto-char (point-min))
      (while (re-search-forward
              "(pel-eval-after-load[[:space:]]+(\\([^)\n]+\\))"
              nil t)
        (dolist (tok (split-string (match-string-no-properties 1)))
          (when (string-match-p "\\`[a-z][a-z0-9-]*\\'" tok)
            (puthash tok t features))))
      ;; 3. Infer <target>-ts-mode from pel-setup-major-mode ts options
      (goto-char (point-min))
      (while (re-search-forward
              (concat "(pel-setup-major-mode[[:space:]]+"
                      "\\([a-z][a-z0-9+_-]+\\)"
                      "[[:space:]]+:"
                      "\\(same-for-ts\\|independent-ts\\|ts-only\\)")
              nil t)
        (let ((target (match-string-no-properties 1))
              (option (match-string-no-properties 2)))
          (puthash (concat target "-ts-mode") t features)
          (unless (string-equal option "ts-only")
            (puthash (concat target "-mode") t features))))
      ;; 4. Infer <target>-ts-mode from pel-config-major-mode ts options
      ;;    (used inside pel-eval-after-load blocks, e.g. for JS)
      (goto-char (point-min))
      (while (re-search-forward
              (concat "(pel-config-major-mode[[:space:]]+"
                      "\\([a-z][a-z0-9+_-]+\\)"
                      "\\(?:[[:space:]]+[^:)]*\\)"
                      ":\\(same-for-ts\\|independent-ts\\|ts-only\\)")
              nil t)
        (let ((target (match-string-no-properties 1))
              (option (match-string-no-properties 2)))
          (puthash (concat target "-ts-mode") t features)
          (unless (string-equal option "ts-only")
            (puthash (concat target "-mode") t features)))))
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
          (replace-regexp-in-string "-ts-mode\\'" "" feature))
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
;;* Top Level Script Main
;;  =====================

(defun pel-lint-main ()
  "Execute all linters.  Exit Emacs with status 0 if all ok, 1 otherwise."
  (let* ((extra-args  command-line-args-left) ; args that follow the script name
         (pel-dir      (or (nth 0 extra-args) "."))
         (keys-file    (or (nth 1 extra-args) "pel_keys.el"))
         (macros-file  (or (nth 2 extra-args) "pel--keys-macros.el"))
         (install-file (or (nth 3 extra-args) "pel--install.el"))
         (errors 0))

    ;; Prevent Emacs from attempting to process remaining args itself.
    (setq command-line-args-left nil)

    ;; Check presence of files that must be parsed
    (unless (file-directory-p pel-dir)
      (message "ERROR: Not a directory: %s" pel-dir)
      (kill-emacs 2))
    (dolist (f (list keys-file macros-file install-file))
      (unless (file-readable-p f)
        (message "ERROR: Cannot read: %s" f)
        (kill-emacs 2)))

    ;; Proceed: run linters
    (setq errors (+ errors (pel-lint-key-prefixes keys-file macros-file)))
    (setq errors (+ errors (pel-lint-fixers pel-dir keys-file install-file)))
    (if (= errors 0)
        (kill-emacs 0)
      (kill-emacs 1))))

;; ---------------------------------------------------------------------------
(provide 'pel-lint)
;;; pel-lint.el ends here
