;;; pel-autocomplete-test.el --- ERT tests for pel-autocomplete.el  -*- lexical-binding: t; -*-

;; Created   : Monday, March 23 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-24 16:41:58 EDT, updated by Pierre Rouleau>

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
;; ERT tests for pel-autocomplete.el.
;;
;; Covered items:
;;
;;   pel--auto-complete-mode-p         - predicate for auto-complete-mode
;;   pel--global-auto-complete-mode-p  - predicate for global-auto-complete-mode
;;   pel--company-mode-p               - predicate for company-mode
;;   pel--global-company-mode-p        - predicate for global-company-mode
;;   pel--corfu-mode-p                 - predicate for corfu-mode (with terminal variant)
;;   pel--global-corfu-mode-p          - predicate for global-corfu-mode
;;   pel--autocompletion-tools-selection - builds (char prompt symbol) list
;;   pel--used-auto-completion-tool    - buffer-local variable isolation
;;
;; Items intentionally NOT covered:
;;   - pel--auto-complete-mode-on/off  (calls real minor-mode functions)
;;   - pel--company-mode-on/off        (same)
;;   - pel--corfu-mode-on/off          (same)
;;   - pel-autocomplete--disable       (exercises live mode teardown)
;;   - pel-select-auto-completion      (drives full mode setup/teardown)
;;   - pel-completion-info             (inserts into a live help buffer)
;;   - pel-complete                    (requires active completion system)
;;   - pel-select-auto-complete-tool   (interactive; drives live selection)
;;
;; Notes on test isolation:
;;   - Predicate tests let-bind the underlying mode variable directly instead
;;     of loading the completion package; `bound-and-true-p' returns nil for
;;     an unbound symbol, so the predicates correctly report "off" when their
;;     package is absent.
;;   - `pel--autocompletion-tools-selection' tests let-bind the PEL user
;;     options (defcustom specials) so no package needs to be installed.
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-autocomplete)
(require 'ert)

;; Declare variables from external packages as special/dynamic variables so
;; that `let' bindings are visible to `bound-and-true-p' even when the
;; packages (auto-complete, company, corfu, corfu-terminal) are not installed
;; in the test environment.
(defvar auto-complete-mode        nil)
(defvar global-auto-complete-mode nil)
(defvar company-mode              nil)
(defvar global-company-mode       nil)
(defvar corfu-mode                nil)
(defvar global-corfu-mode         nil)
(defvar corfu-terminal-mode       nil)

;;; --------------------------------------------------------------------------
;;; Code:

;; ===========================================================================
;; pel--auto-complete-mode-p
;; ===========================================================================

(ert-deftest pel-autocomplete-test/auto-complete-mode-p/off-when-nil ()
  "`pel--auto-complete-mode-p' returns nil when `auto-complete-mode' is nil."
  (let ((auto-complete-mode nil))
    (should-not (pel--auto-complete-mode-p))))

(ert-deftest pel-autocomplete-test/auto-complete-mode-p/on-when-t ()
  "`pel--auto-complete-mode-p' returns non-nil when `auto-complete-mode' is t."
  (let ((auto-complete-mode t))
    (should (pel--auto-complete-mode-p))))

;; ===========================================================================
;; pel--global-auto-complete-mode-p
;; ===========================================================================

(ert-deftest pel-autocomplete-test/global-auto-complete-mode-p/off-when-nil ()
  "`pel--global-auto-complete-mode-p' returns nil when the global mode is nil."
  (let ((global-auto-complete-mode nil))
    (should-not (pel--global-auto-complete-mode-p))))

(ert-deftest pel-autocomplete-test/global-auto-complete-mode-p/on-when-t ()
  "`pel--global-auto-complete-mode-p' returns non-nil when the global mode is t."
  (let ((global-auto-complete-mode t))
    (should (pel--global-auto-complete-mode-p))))

;; ===========================================================================
;; pel--company-mode-p
;; ===========================================================================

(ert-deftest pel-autocomplete-test/company-mode-p/off-when-nil ()
  "`pel--company-mode-p' returns nil when `company-mode' is nil."
  (let ((company-mode nil))
    (should-not (pel--company-mode-p))))

(ert-deftest pel-autocomplete-test/company-mode-p/on-when-t ()
  "`pel--company-mode-p' returns non-nil when `company-mode' is t."
  (let ((company-mode t))
    (should (pel--company-mode-p))))

;; ===========================================================================
;; pel--global-company-mode-p
;; ===========================================================================

(ert-deftest pel-autocomplete-test/global-company-mode-p/off-when-nil ()
  "`pel--global-company-mode-p' returns nil when `global-company-mode' is nil."
  (let ((global-company-mode nil))
    (should-not (pel--global-company-mode-p))))

(ert-deftest pel-autocomplete-test/global-company-mode-p/on-when-t ()
  "`pel--global-company-mode-p' returns non-nil when `global-company-mode' is t."
  (let ((global-company-mode t))
    (should (pel--global-company-mode-p))))

;; ===========================================================================
;; pel--corfu-mode-p
;; In graphic sessions (pel-emacs-is-graphic-p non-nil), only corfu-mode
;; is checked.  In terminal sessions with corfu-terminal enabled, both
;; corfu-terminal-mode and corfu-mode must be on.
;; ===========================================================================

(ert-deftest pel-autocomplete-test/corfu-mode-p/graphic-off-when-nil ()
  "`pel--corfu-mode-p' returns nil on graphic display when `corfu-mode' is nil."
  (let ((corfu-mode nil)
        (pel-use-corfu-terminal nil)
        (pel-emacs-is-graphic-p t))
    (should-not (pel--corfu-mode-p))))

(ert-deftest pel-autocomplete-test/corfu-mode-p/graphic-on-when-t ()
  "`pel--corfu-mode-p' returns non-nil on graphic display when `corfu-mode' is t."
  (let ((corfu-mode t)
        (pel-use-corfu-terminal nil)
        (pel-emacs-is-graphic-p t))
    (should (pel--corfu-mode-p))))

(ert-deftest pel-autocomplete-test/corfu-mode-p/terminal-requires-both-modes ()
  "`pel--corfu-mode-p' on terminal with corfu-terminal requires both modes on."
  (let ((pel-use-corfu-terminal t)
        (pel-emacs-is-graphic-p nil))
    ;; Both on → t
    (let ((corfu-mode t)
          (corfu-terminal-mode t))
      (should (pel--corfu-mode-p)))
    ;; Only corfu-mode on → nil
    (let ((corfu-mode t)
          (corfu-terminal-mode nil))
      (should-not (pel--corfu-mode-p)))
    ;; Only corfu-terminal-mode on → nil
    (let ((corfu-mode nil)
          (corfu-terminal-mode t))
      (should-not (pel--corfu-mode-p)))))

;; ===========================================================================
;; pel--global-corfu-mode-p
;; ===========================================================================

(ert-deftest pel-autocomplete-test/global-corfu-mode-p/graphic-off-when-nil ()
  "`pel--global-corfu-mode-p' returns nil on graphic display when mode is nil."
  (let ((global-corfu-mode nil)
        (pel-use-corfu-terminal nil)
        (pel-emacs-is-graphic-p t))
    (should-not (pel--global-corfu-mode-p))))

(ert-deftest pel-autocomplete-test/global-corfu-mode-p/graphic-on-when-t ()
  "`pel--global-corfu-mode-p' returns non-nil on graphic display when mode is t."
  (let ((global-corfu-mode t)
        (pel-use-corfu-terminal nil)
        (pel-emacs-is-graphic-p t))
    (should (pel--global-corfu-mode-p))))

(ert-deftest pel-autocomplete-test/global-corfu-mode-p/terminal-uses-corfu-terminal ()
  "On terminal with corfu-terminal, `pel--global-corfu-mode-p' checks corfu-terminal-mode."
  (let ((pel-use-corfu-terminal t)
        (pel-emacs-is-graphic-p nil))
    (let ((corfu-terminal-mode t))
      (should (pel--global-corfu-mode-p)))
    (let ((corfu-terminal-mode nil))
      (should-not (pel--global-corfu-mode-p)))))

;; ===========================================================================
;; pel--autocompletion-tools-selection
;; ===========================================================================

(ert-deftest pel-autocomplete-test/tools-selection/builtin-always-present ()
  "Emacs built-in (`?e') is always included regardless of other options."
  (let ((pel-use-auto-complete nil)
        (pel-use-company nil)
        (pel-use-corfu nil)
        (pel-emacs-29-or-later-p nil))
    (let ((sel (pel--autocompletion-tools-selection)))
      (should (assoc ?e sel)))))

(ert-deftest pel-autocomplete-test/tools-selection/builtin-only-when-all-off ()
  "When all external tools are disabled, only the built-in entry is returned."
  (let ((pel-use-auto-complete nil)
        (pel-use-company nil)
        (pel-use-corfu nil)
        (pel-emacs-29-or-later-p nil))
    (let ((sel (pel--autocompletion-tools-selection)))
      (should (= (length sel) 1))
      (should (assoc ?e sel)))))

(ert-deftest pel-autocomplete-test/tools-selection/auto-complete-added-when-on ()
  "auto-complete (`?a') appears when `pel-use-auto-complete' is t."
  (let ((pel-use-auto-complete t)
        (pel-use-company nil)
        (pel-use-corfu nil)
        (pel-emacs-29-or-later-p nil))
    (let ((sel (pel--autocompletion-tools-selection)))
      (should (assoc ?a sel))
      (should (= (length sel) 2)))))

(ert-deftest pel-autocomplete-test/tools-selection/company-added-when-on ()
  "company (`?c') appears when `pel-use-company' is t."
  (let ((pel-use-auto-complete nil)
        (pel-use-company t)
        (pel-use-corfu nil)
        (pel-emacs-29-or-later-p nil))
    (let ((sel (pel--autocompletion-tools-selection)))
      (should (assoc ?c sel))
      (should (= (length sel) 2)))))

(ert-deftest pel-autocomplete-test/tools-selection/corfu-added-when-on-and-emacs29 ()
  "corfu (`?u') appears when `pel-use-corfu' is t and Emacs >= 29."
  (let ((pel-use-auto-complete nil)
        (pel-use-company nil)
        (pel-use-corfu t)
        (pel-emacs-29-or-later-p t))
    (let ((sel (pel--autocompletion-tools-selection)))
      (should (assoc ?u sel))
      (should (= (length sel) 2)))))

(ert-deftest pel-autocomplete-test/tools-selection/corfu-absent-when-emacs-older ()
  "corfu must NOT appear when `pel-emacs-29-or-later-p' is nil."
  (let ((pel-use-corfu t)
        (pel-emacs-29-or-later-p nil)
        (pel-use-auto-complete nil)
        (pel-use-company nil))
    (let ((sel (pel--autocompletion-tools-selection)))
      (should-not (assoc ?u sel)))))

(ert-deftest pel-autocomplete-test/tools-selection/all-four-when-all-on ()
  "All four entries appear when every tool is enabled."
  (let ((pel-use-auto-complete t)
        (pel-use-company t)
        (pel-use-corfu t)
        (pel-emacs-29-or-later-p t))
    (let ((sel (pel--autocompletion-tools-selection)))
      (should (= (length sel) 4))
      (should (assoc ?e sel))
      (should (assoc ?a sel))
      (should (assoc ?c sel))
      (should (assoc ?u sel)))))

(ert-deftest pel-autocomplete-test/tools-selection/entries-have-three-elements ()
  "Each entry in the selection list has exactly 3 elements: char, string, symbol."
  (let ((pel-use-auto-complete t)
        (pel-use-company t)
        (pel-use-corfu t)
        (pel-emacs-29-or-later-p t))
    (dolist (entry (pel--autocompletion-tools-selection))
      (should (= (length entry) 3))
      (should (characterp (nth 0 entry)))
      (should (stringp    (nth 1 entry)))
      (should (symbolp    (nth 2 entry))))))

;; ===========================================================================
;; pel--used-auto-completion-tool  (buffer-local variable)
;; ===========================================================================

(ert-deftest pel-autocomplete-test/used-tool/is-buffer-local ()
  "`pel--used-auto-completion-tool' changes independently in each buffer."
  (let ((buf1 (get-buffer-create " *pel-autocomplete-test-1*"))
        (buf2 (get-buffer-create " *pel-autocomplete-test-2*")))
    (unwind-protect
        (progn
          (with-current-buffer buf1
            (setq-local pel--used-auto-completion-tool 'company))
          (with-current-buffer buf2
            (setq-local pel--used-auto-completion-tool 'auto-complete))
          (with-current-buffer buf1
            (should (eq 'company      pel--used-auto-completion-tool)))
          (with-current-buffer buf2
            (should (eq 'auto-complete pel--used-auto-completion-tool))))
      (kill-buffer buf1)
      (kill-buffer buf2))))

(ert-deftest pel-autocomplete-test/used-tool/defaults-to-nil-in-new-buffer ()
  "`pel--used-auto-completion-tool' is nil in a freshly created buffer."
  (let ((buf (generate-new-buffer " *pel-autocomplete-test-fresh*")))
    (unwind-protect
        (with-current-buffer buf
          (should (eq nil pel--used-auto-completion-tool)))
      (kill-buffer buf))))

;;; --------------------------------------------------------------------------
(provide 'pel-autocomplete-test)

;;; pel-autocomplete-test.el ends here
