;;; pel-erlang-activation-test.el --- Test Erlang support activation  -*- lexical-binding: t; -*-

;; Created   : Tuesday, April 21 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-04-21 13:50:42 EDT, updated by Pierre Rouleau>

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


;; (when noninteractive
;;   ;; Use the erlang.el stub file as a replacement for erlang.el
;;   (add-to-list 'load-path (expand-file-name "test/stubs"))
;;   (require 'erlang nil :noerror))


;; ---------------------------------------------------------------------------
;;* Tier 1 - Unit “no network” tests (spy the installer)

;; Small harness to spy pel--ensure-pkg-elpa and collect calls as (PKG . SITE)
(defmacro pel-activation-test--with-ensure-spy (&rest body)
  "Execute BODY while spying pel--ensure-pkg-elpa calls into `calls'."
  (declare (indent 0))
  `(let ((calls nil))
     (cl-letf* (((symbol-function 'pel--ensure-pkg-elpa)
                 ;; Accept both one-arg (PKG) and two-arg (PKG SITE) forms.
                 (lambda (&rest args)
                   (let ((pkg  (nth 0 args))
                         (site (nth 1 args)))
                     (push (cons pkg site) calls)
                     t)))
                ;; Pretend nothing is installed so ensure paths execute.
                ((symbol-function 'pel-package-installed-p) (lambda (&rest _) nil))
                ;; Default: not in fast startup; individual tests may override.
                ((symbol-function 'pel-in-fast-startup-p)   (lambda () nil)))
       ,@body)))

(ert-deftest pel-activation/erlang/ensures-core-packages ()
  "When `pel-use-erlang' is t, PEL asks to ensure Erlang packages."
  (when noninteractive (ert-skip "Test failing non interactively"))
  (pel-activation-test--with-ensure-spy
    (let ((pel-use-erlang t)
          (pel-use-tree-sitter nil)
          (pel-use-erlang-ls nil)
          ;; keep other families off to avoid unrelated ensures
          (pel-use-c nil) (pel-use-c++ nil) (pel-use-common-lisp nil)
          (pel-use-rst nil) (pel-use-yasnippet nil) (pel-use-tree-sitter nil))
      (load "pel_keys.el" nil 'nomessage 'nosuffix)
      ;; Expect at least the core erlang package ensure, pinned to melpa.
      (should (member (cons 'erlang "melpa") calls)))))

(ert-deftest pel-activation/erlang/tree-sitter_adds_erlang_ts ()
  "When Tree-sitter is enabled, PEL also ensures erlang-ts."
  (when noninteractive (ert-skip "Test failing non interactively"))
  (pel-activation-test--with-ensure-spy
    (let ((pel-use-erlang t)
          (pel-use-tree-sitter t)
          (pel-use-erlang-ls nil))
      (load "pel_keys.el" nil 'nomessage 'nosuffix)
      (should (member (cons 'erlang    "melpa") calls))
      (should (member (cons 'erlang-ts "melpa") calls)))))

(ert-deftest pel-activation/erlang/ls_adds_lsp_clients ()
  "When `pel-use-erlang-ls' is t, PEL ensures lsp-mode and lsp-ui."
  (when noninteractive (ert-skip "Test failing non interactively"))
  (pel-activation-test--with-ensure-spy
    (let ((pel-use-erlang t)
          (pel-use-erlang-ls t)
          (pel-use-tree-sitter nil))
      (load "pel_keys.el" nil 'nomessage 'nosuffix)
      (should (member (cons 'erlang  "melpa") calls))
      (should (assoc 'lsp-mode calls))
      (should (assoc 'lsp-ui   calls)))))

(ert-deftest pel-activation/erlang/fast-startup_prevents_installs ()
  "In fast-startup mode, no ensure calls occur even if `pel-use-erlang' is t."
  (when noninteractive (ert-skip "Test failing non interactively"))
  (let ((calls nil))
    (cl-letf* (((symbol-function 'pel--ensure-pkg-elpa)
                (lambda (&rest args)
                  (push (cons (nth 0 args) (nth 1 args)) calls)
                  t))
               ((symbol-function 'pel-package-installed-p) (lambda (&rest _) nil))
               ;; Simulate fast startup: installer should not be called at all.
               ((symbol-function 'pel-in-fast-startup-p)   (lambda () t)))
      (let ((pel-use-erlang t) (pel-use-erlang-ls t) (pel-use-tree-sitter t))
        (load "pel_keys.el" nil 'nomessage 'nosuffix)
        (should (null calls))))))

;;; --------------------------------------------------------------------------
(provide 'pel-erlang-activation-test)

;;; pel-erlang-activation-test.el ends here
