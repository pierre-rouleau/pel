;;; pel-lint-declarations.el --- Lint missing defun declare properties  -*- lexical-binding: t; -*-

;; Created   : Wednesday, June 03 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>

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

;; ---------------------------------------------------------------------------
;;; Commentary:
;;
;; Command-line usage, from the directory containing the Emacs Lisp files
;; to inspect:
;;
;;   emacs -Q --batch -L . -l bin/pel-lint-declarations.el --eval "(pel-lint-declarations-main)"
;;
;; The linter scans all *.el files in the current directory, non-recursively.
;;
;; It stops at the first `defun' form for which `pel-elcode-properties-of-sexp'
;; can infer one or more of:
;;
;;   - (pure t)
;;   - (side-effect-free t)
;;   - (side-effect-free error-free)
;;
;; but the corresponding declaration is missing from the function.
;;
;; Exit status:
;;
;;   0  no diagnostic found
;;   1  first missing declaration found, or unreadable/malformed input

;;; Code:

(eval-and-compile
  ;; Make the PEL root directory available on `load-path' when this file is
  ;; loaded from bin/.
  (let ((pel-root (expand-file-name
                   ".."
                   (file-name-directory
                    (or load-file-name
                        (and (boundp 'byte-compile-current-file)
                             byte-compile-current-file)
                        buffer-file-name)))))
    (unless (member pel-root load-path)
      (push pel-root load-path))))

(require 'cl-lib)
(require 'lisp-mode)
(require 'elisp-mode)
(eval-and-compile
  (let ((load-prefer-newer t))
    (require 'pel-elcode)))
(require 'byte-opt)     ; use: `pure' and `side-effect-free' symbol properties
;;                             on built-in functions (not, eq, symbolp, consp, …)

;; ---------------------------------------------------------------------------
;; Utilities
;; ---------

(defun pel-lint-declarations--unexpected-props (present allowed)
  "Return properties from PRESENT that are not in ALLOWED, preserving order."
  (let ((unexpected '()))
    (dolist (prop present (nreverse unexpected))
      (unless (memq prop allowed)
        (push prop unexpected)))))

(defun pel-lint-declarations--invalid-property-clauses-in-declare-form
    (declare-form)
  "Return invalid PEL property clauses from DECLARE-FORM.

Only clauses related to `pure' and `side-effect-free' are checked.
Other declaration clauses, such as `indent', `debug' or `obsolete', are
ignored by this linter."
  (let ((invalid '()))
    (dolist (clause (cdr-safe declare-form))
      (pcase clause
        (`(pure t)
         nil)
        (`(pure ,_)
         (push clause invalid))
        (`(side-effect-free t)
         nil)
        (`(side-effect-free error-free)
         nil)
        (`(side-effect-free ,_)
         (push clause invalid))
        (_
         nil)))
    (nreverse invalid)))

(defun pel-lint-declarations--invalid-property-clauses-in-defun-declare
    (defun-form)
  "Return invalid PEL property clauses in DEFUN-FORM's leading declarations."
  (let ((body (cdddr defun-form))
        (invalid '()))
    ;; Skip optional docstring.
    (when (stringp (car-safe body))
      (setq body (cdr body)))

    ;; Inspect one or more leading declare forms.
    (while (and (consp body)
                (consp (car body))
                (eq (caar body) 'declare))
      (setq invalid
            (append invalid
                    (pel-lint-declarations--invalid-property-clauses-in-declare-form
                     (car body))))
      (setq body (cdr body)))

    invalid))

;; ---------------------------------------------------------------------------
;; Property normalization

(defun pel-lint-declarations--props-in-declare-form (declare-form)
  "Return normalized property symbols present in DECLARE-FORM.

The returned list may contain the symbols:
  `pure', `side-effect-free', `error-free'."
  (let ((props '()))
    (dolist (clause (cdr-safe declare-form))
      (pcase clause
        (`(pure ,_)
         (cl-pushnew 'pure props))
        (`(side-effect-free error-free)
         (cl-pushnew 'side-effect-free props)
         (cl-pushnew 'error-free props))
        (`(side-effect-free ,_)
         (cl-pushnew 'side-effect-free props))))
    (nreverse props)))

(defun pel-lint-declarations--props-in-defun-declare (defun-form)
  "Return normalized declaration properties already present in DEFUN-FORM.

Only declarations in the standard defun declaration position are considered:
after the arglist and optional docstring, before the function body."
  (let ((body (cdddr defun-form))
        (props '()))
    ;; Skip optional docstring.
    (when (stringp (car-safe body))
      (setq body (cdr body)))

    ;; Collect one or more leading declare forms, defensively.  Normally there
    ;; should be at most one, but accepting several makes the checker tolerant.
    (while (and (consp body)
                (consp (car body))
                (eq (caar body) 'declare))
      (dolist (prop (pel-lint-declarations--props-in-declare-form (car body)))
        (cl-pushnew prop props))
      (setq body (cdr body)))

    (nreverse props)))

(defun pel-lint-declarations--props-in-recommended-declare (declare-form)
  "Return normalized property symbols recommended by DECLARE-FORM."
  (pel-lint-declarations--props-in-declare-form declare-form))

(defun pel-lint-declarations--missing-props (required present)
  "Return properties from REQUIRED that are not PRESENT, preserving order."
  (let ((missing '()))
    (dolist (prop required (nreverse missing))
      (unless (memq prop present)
        (push prop missing)))))

(defun pel-lint-declarations--props-string (props)
  "Return a readable string for property symbol list PROPS."
  (mapconcat #'symbol-name props ", "))

;; ---------------------------------------------------------------------------
;; Defun detection

(defun pel-lint-declarations--defun-form-p (form)
  "Return non-nil when FORM is a top-level `defun' form."
  (and (consp form)
       (eq (car form) 'defun)
       (symbolp (cadr form))
       (consp (cddr form))))

(defun pel-lint-declarations--diagnostic-for-defun (filename line defun-form)
  "Return a diagnostic string for DEFUN-FORM, or nil.

FILENAME and LINE identify the location of DEFUN-FORM.

This reports, in order of priority:

1. malformed known declaration clauses, such as `(pure nil)';
2. invalid / over-strong declared properties;
3. missing inferred declared properties."
  (let* ((recommended
          (pel-elcode-properties-of-sexp defun-form))
         (required-props
          (if recommended
              (pel-lint-declarations--props-in-recommended-declare recommended)
            nil))
         (present-props
          (pel-lint-declarations--props-in-defun-declare defun-form))
         (invalid-clauses
          (pel-lint-declarations--invalid-property-clauses-in-defun-declare
           defun-form))
         (unexpected-props
          (pel-lint-declarations--unexpected-props present-props
                                                   required-props))
         (missing-props
          (pel-lint-declarations--missing-props required-props
                                                present-props)))
    (cond

     ;; First: malformed declarations for known PEL properties.
     (invalid-clauses
      (format
       "%s:%d: function `%s' has invalid declaration clause%s: %S"
       filename
       line
       (cadr defun-form)
       (if (cdr invalid-clauses) "s" "")
       invalid-clauses))

     ;; Second: declarations that are syntactically valid but semantically
     ;; stronger than what `pel-elcode-properties-of-sexp' can justify.
     (unexpected-props
      (format
       (concat "%s:%d: function `%s' has invalid declaration propert%s: %s; "
               "inferred declaration is %S")
       filename
       line
       (cadr defun-form)
       (if (cdr unexpected-props) "ies" "y")
       (pel-lint-declarations--props-string unexpected-props)
       recommended))

     ;; Third: properties that could be declared but are missing.
     (missing-props
      (format
       (concat "%s:%d: function `%s' can use declaration %S, "
               "but is missing: %s")
       filename
       line
       (cadr defun-form)
       recommended
       (pel-lint-declarations--props-string missing-props)))

     (t
      nil))))

;; ---------------------------------------------------------------------------
;; File scanning

;; (defun pel-lint-declarations--first-diagnostic-in-file (filename)
;;   "Return first missing-declaration diagnostic in FILENAME, or nil."
;;   (with-temp-buffer
;;     (insert-file-contents filename)
;;     (goto-char (point-min))
;;     (catch 'diagnostic
;;       (condition-case err
;;           (while t
;;             ;; Skip whitespace and comments so line number points at the form.
;;             (forward-comment (point-max))
;;             (when (eobp)
;;               (throw 'diagnostic nil))
;;             (let* ((start-pos (point))
;;                    (line      (line-number-at-pos start-pos))
;;                    (form      (read (current-buffer))))
;;               (when (pel-lint-declarations--defun-form-p form)
;;                 (let ((diagnostic
;;                        (pel-lint-declarations--diagnostic-for-defun
;;                         filename line form)))
;;                   (when diagnostic
;;                     (throw 'diagnostic diagnostic))))))
;;         (end-of-file
;;          nil)
;;         (error
;;          (format "%s:%d: read error: %s"
;;                  filename
;;                  (line-number-at-pos)
;;                  (error-message-string err)))))))


(defun pel-lint-declarations--diagnostics-in-file (filename)
  "Return all declaration diagnostics found in FILENAME.

If a read error occurs, return diagnostics found before the read error plus
one read-error diagnostic.  The file cannot be parsed reliably after such an
error, so scanning continues with the next file."
  (let ((diagnostics '()))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (emacs-lisp-mode)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (condition-case err
            (while t
              ;; Use the Emacs Lisp syntax table so `forward-comment' skips
              ;; semicolon comments before line number capture.
              (forward-comment (point-max))
              (if (eobp)
                  (signal 'end-of-file nil)
                (let* ((line (line-number-at-pos))
                       (form (read (current-buffer))))
                  (when (pel-lint-declarations--defun-form-p form)
                    (let ((diagnostic
                           (pel-lint-declarations--diagnostic-for-defun
                            filename line form)))
                      (when diagnostic
                        (push diagnostic diagnostics)))))))
          (end-of-file
           nil)
          (error
           (push (format "%s:%d: read error: %s"
                         filename
                         (line-number-at-pos)
                         (error-message-string err))
                 diagnostics)))))
    (nreverse diagnostics)))

(defun pel-lint-declarations--elisp-files-in-current-directory ()
  "Return sorted list of Emacs Lisp files in `default-directory'."
  (sort (directory-files default-directory
                         :full
                         "\\.el\\'")
        #'string<))

;; (defun pel-lint-declarations-first-diagnostic ()
;;   "Return first missing-declaration diagnostic in current directory, or nil.
;;
;; Only `*.el' files directly under `default-directory' are scanned."
;;   (catch 'diagnostic
;;     (dolist (filename (pel-lint-declarations--elisp-files-in-current-directory))
;;       (let ((diagnostic
;;              (pel-lint-declarations--first-diagnostic-in-file filename)))
;;         (when diagnostic
;;           (throw 'diagnostic diagnostic))))
;;     nil))

(defun pel-lint-declarations-diagnostics ()
  "Return all declaration diagnostics in current directory.

Only `*.el' files directly under `default-directory' are scanned."
  (let ((diagnostics '()))
    (dolist (filename (pel-lint-declarations--elisp-files-in-current-directory))
      (setq diagnostics
            (append diagnostics
                    (pel-lint-declarations--diagnostics-in-file filename))))
    diagnostics))

(defun pel-lint-declarations-first-diagnostic ()
  "Return first declaration diagnostic in current directory, or nil."
  (car (pel-lint-declarations-diagnostics)))

;; ---------------------------------------------------------------------------
;; Command-line entry point

;; (defun pel-lint-declarations-main ()
;;   "Run missing declaration linting for all Emacs Lisp files in current directory.
;;
;; Exit with status 1 on the first diagnostic.  Exit with status 0 when no
;; diagnostic is found."
;;   (let ((diagnostic (pel-lint-declarations-first-diagnostic)))
;;     (if diagnostic
;;         (progn
;;           (princ (concat diagnostic "\n") #'external-debugging-output)
;;           (kill-emacs 1))
;;       (kill-emacs 0))))

(defun pel-lint-declarations-main ()
  "Run declaration linting for all Emacs Lisp files in current directory.

Print all diagnostics found.  Exit with status 1 when at least one diagnostic
is found.  Exit with status 0 when no diagnostic is found."
  (let ((diagnostics (pel-lint-declarations-diagnostics)))
    (if diagnostics
        (progn
          (dolist (diagnostic diagnostics)
            (princ (concat diagnostic "\n") #'external-debugging-output))
          (kill-emacs 1))
      (kill-emacs 0))))

;; ---------------------------------------------------------------------------
(provide 'pel-lint-declarations)

;;; pel-lint-declarations.el ends here
