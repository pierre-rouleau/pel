;;; pel-lint.el --- Lint PEL code  -*- lexical-binding: t; -*-

;; Created   : Monday, May 11 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-05-11 13:35:07 EDT, updated by Pierre Rouleau>

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
;; Checks that every pel-setup-major-mode call either has an explicit
;; key-prefix: clause, or that its inferred pel:for-<target> symbol is
;; consistent with the key-vector assignments in pel--keys-macros.el.
;;
;; A mismatch is flagged as an error only when ALL of the following are true:
;;   1. The co-alias mode name also appears as a pel-setup-major-mode target.
;;   2. Neither mode is a Tree-Sitter variant of the other (name + "-ts").
;;   3. The inferred pel:for-<target> symbol is NOT defined anywhere in
;;      pel_keys.el via define-pel-global-prefix.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'cl-lib)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-lint/ts-variant-pair-p (a b)
  "Return non-nil when A and B are a Tree-Sitter variant pair.
That is: one of the two strings equals the other with \"-ts\" appended."
  (or (string-equal (concat a "-ts") b)
      (string-equal (concat b "-ts") a)))

;; ---------------------------------------------------------------------------
;;; Step 1 – Parse pel--keys-macros.el

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
;;; Step 2 – Parse pel_keys.el

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
               (body-end   (save-excursion
                             (if (re-search-forward
                                  "(pel-setup-major-mode" nil t)
                                 (match-beginning 0)
                               (point-max))))
               (body       (buffer-substring-no-properties body-start body-end))
               (has-kp     (if (string-match-p "key-prefix:" body) t nil)))
          (puthash target has-kp target-info))))
    (cons defined-prefixes target-info)))

;; ---------------------------------------------------------------------------
;;; Step 3 – Cross-validate

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
                               (pel-lint/ts-variant-pair-p target a)))
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
;;; Main

(defun pel-lint-main ()
  (let* ((extra-args  command-line-args-left) ; args that follow the script name
         (macros-file (or (nth 0 extra-args) "pel--keys-macros.el"))
         (keys-file   (or (nth 1 extra-args) "pel_keys.el")))
    ;; Prevent Emacs from attempting to process remaining args itself.
    (setq command-line-args-left nil)

    (message "PEL key-prefix consistency validator")
    (message "  Macros file : %s" macros-file)
    (message "  Keys file   : %s" keys-file)

    (unless (file-readable-p macros-file)
      (message "ERROR: Cannot read: %s" macros-file)
      (kill-emacs 2))
    (unless (file-readable-p keys-file)
      (message "ERROR: Cannot read: %s" keys-file)
      (kill-emacs 2))

    (let ((errors (pel-lint/validate-key-prefixes macros-file keys-file)))
      (if errors
          (progn
            (message "\nERRORS — key-prefix alias mismatches found:")
            (dolist (e errors)
              (message "\n%s" e))
            (kill-emacs 1))
        (message "OK — no key-prefix alias mismatches found.")
        (kill-emacs 0)))))

;; ---------------------------------------------------------------------------
;;; pel-lint.el ends here
