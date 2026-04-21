;;; pel-erlang-activation-test.el --- Test Erlang support activation  -*- lexical-binding: t; -*-

;; Created   : Tuesday, April 21 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-04-21 15:53:46 EDT, updated by Pierre Rouleau>

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
;; This file holds tests to validate PEL's ability to install the packages
;; used for Erlang support and that must be done without network access.
;;
;; The test strategy
;;
;; Tier 1 — Unit “no network” tests (spy the installer)
;;
;;     - Goal: Prove that setting pel-use-erlang t drives the exact
;;             package-install intents, without requiring erlang.el or internet.
;;
;;     - Technique: `cl-letf-override' `pel--ensure-pkg-elpa' (the real installer
;;                  behind `pel-ensure-package-elpa') and `pel-package-installed-p'
;;                  to collect calls;
;;                  - set all unrelated pel-use-* options to nil;
;;                  - set `pel-use-erlang' to t;
;;                  - then load pel_keys.el.
;;                  Assert that the spy captured the expected package symbols.
;;
;;     Also test that fast-startup prevents any install attempts by stubbing
;;     `pel-in-fast-startup-p' to t.
;;
;; Tier 2 — Stub-based functional tests (fully offline)
;;
;;     Goal: Actually exercise pel-erlang.el behavior in batch without the real erlang.el present.
;;     Technique: add a tiny stub in test/stubs/erlang.el that defines:
;;         (defvar erlang-electric-commands nil)
;;         (defun erlang-mode () (interactive))
;;         (provide 'erlang)
;;
;;     Push that stub directory to load-path before requiring
;;     pel-erlang or pel_keys. This lets the code truly run the Erlang-specific
;;     tests (electric period, binary navigation, shell init) in batch,
;;     consistently.


;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--install)  ;; for `pel-ensure-package-elpa' macro references
(require 'ert)

(when noninteractive
  (add-to-list 'load-path (expand-file-name "test/stubs"))
  (require 'erlang)
  (require 'erlang-start))

;; Predeclare relevant defcustoms as special to avoid lexical-binding conflicts
;; when pel--options.el defcustom’s them during (load "pel_keys.el").
(defvar pel-use-erlang)
(defvar pel-use-erlang-ls)
(defvar pel-use-tree-sitter)

;; ---------------------------------------------------------------------------
;;* Tier 1 - Unit “no network” tests (spy the installer)

;; ---------------------------------------------------------------------------
;; Spy harness: collect ensure calls as (PKG . SITE)
;; ---------------------------------------------------------------------------
(defmacro pel-activation-test--with-ensure-spy (&rest body)
  "Execute BODY while spying pel--ensure-pkg-elpa calls into `calls'.
The spy is arity-tolerant and records entries as (PKG . SITE)."
  (declare (indent 0))
  `(let ((calls nil))
     (cl-letf*
         (((symbol-function 'pel--ensure-pkg-elpa)
           (lambda (pkg &optional elpa-site)
             (push (cons pkg elpa-site) calls)
             t))
          ;; Pretend nothing is installed so ensure paths execute.
          ((symbol-function 'pel-package-installed-p) (lambda (_feature) nil))
          ;; Default: not in fast-startup; tests may rebind it locally.
          ((symbol-function 'pel-in-fast-startup-p)   (lambda () nil)))
       ,@body)))

;; ---------------------------------------------------------------------------
;; Save/restore custom variables around BODY, avoiding let-bindings.
;;
;; Each entry in VARS may be:
;;   - SYMBOL           => just save/restore it
;;   - (SYMBOL VALUE)   => save, set to VALUE before BODY, then restore

(defmacro pel-activation-test--with-vars (vars &rest body)
  "Temporarily set and then restore global variables across BODY.

VARS is a list of symbols or (SYMBOL VALUE) pairs.
Symbols are saved and restored; pairs are set before BODY and restored after.
This avoids lexical-binding conflicts that occur when let-binding defcustom
variables."
  (declare (indent 1))
  ;; Validate argument shape early to avoid accidental (pel-use-FOO VAL)
  ;; calls.
  (unless (and (listp vars)
               (cl-every (lambda (e) (or (symbolp e)
                                         (and (consp e)
                                              (symbolp (car e)))))
                         vars))
    (error "pel-activation-test--with-vars: \
expected a list of SYMBOL or (SYMBOL VALUE) pairs, got: %S"
           vars))
  (let* ((syms (mapcar (lambda (e) (if (consp e) (car e) e)) vars))
         (sets (cl-loop for e in vars if (consp e)
                        collect `(set ',(car e) ,(cadr e)))))
    `(let* ((--pel-vars-- ',syms)
            (--pel-saved-- (mapcar (lambda (sym)
                                     (list sym (boundp sym)
                                           (and (boundp sym)
                                                (symbol-value sym))))
                                   --pel-vars--)))
       (unwind-protect
           (progn ,@sets ,@body)
         (dolist (cell --pel-saved--)
           (let ((sym      (nth 0 cell))
                 (wasbound (nth 1 cell))
                 (oldval   (nth 2 cell)))
             (if wasbound
                 (set sym oldval)
               (makunbound sym))))))))

;; ===========================================================================
;; Tests
;; ===========================================================================

(ert-deftest pel-activation/erlang/ensures-core-packages ()
  "When `pel-use-erlang' is t, PEL asks to ensure Erlang packages."
  (pel-activation-test--with-ensure-spy
    (pel-activation-test--with-vars
        ((pel-use-erlang t)
         (pel-use-erlang-ls nil)
         (pel-use-tree-sitter nil))
      (load "pel_keys.el" nil 'nomessage 'nosuffix)
      ;; Be tolerant about SITE arg; assert package intent.
      (should (assoc 'erlang calls)))))

(ert-deftest pel-activation/erlang/tree-sitter_adds_erlang_ts ()
  "When Tree-sitter is enabled, PEL also ensures erlang-ts."
  (pel-activation-test--with-ensure-spy
    (pel-activation-test--with-vars
        ((pel-use-erlang t)
         (pel-use-tree-sitter t)
         (pel-use-erlang-ls nil))
      (load "pel_keys.el" nil 'nomessage 'nosuffix)
      (should (assoc 'erlang    calls))
      (should (assoc 'erlang-ts calls)))))

(ert-deftest pel-activation/erlang/ls_adds_lsp_clients ()
  "When `pel-use-erlang-ls' is t, PEL ensures lsp-mode and lsp-ui."
  (pel-activation-test--with-ensure-spy
    (pel-activation-test--with-vars
        ((pel-use-erlang t)
         (pel-use-erlang-ls t)
         (pel-use-tree-sitter nil))
      (load "pel_keys.el" nil 'nomessage 'nosuffix)
      (should (assoc 'erlang  calls))
      (should (assoc 'lsp-mode calls))
      (should (assoc 'lsp-ui   calls)))))

(ert-deftest pel-activation/erlang/fast-startup_prevents_installs ()
  "In fast-startup mode, no ensure calls occur even if `pel-use-erlang' is t."
  (pel-activation-test--with-ensure-spy
    ;; Rebind fast-startup only for this test.
    (cl-letf (((symbol-function 'pel-in-fast-startup-p) (lambda () t)))
      (pel-activation-test--with-vars
          ((pel-use-erlang t)
           (pel-use-erlang-ls t)
           (pel-use-tree-sitter t))
        (load "pel_keys.el" nil 'nomessage 'nosuffix)
        (should (null calls))))))

;;; --------------------------------------------------------------------------
(provide 'pel-erlang-activation-test)

;;; pel-erlang-activation-test.el ends here
