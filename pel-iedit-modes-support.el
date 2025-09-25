;;; pel-iedit-modes-support.el --- Improve iedit support for modes.  -*- lexical-binding: t; -*-

;; Created   : Monday, April 18 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-09-25 15:42:27 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2022, 2024, 2025  Pierre Rouleau
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
;; This file provides convenience code to enhance iedit-mode support inside
;; the buffers using the following major modes:
;;
;; - `cperl-mode'
;; - `sh-mode'
;; - `tcl-mode'
;; - `ninja-mode'
;;
;; It handles problems related to mis-handling of several syntactic elements
;; for the supported major modes, handling the problem described by the
;; following *reference* : https://github.com/victorhge/iedit/issues/148
;;
;;
;; As identified by the issue 148 referenced above, iedit is normally not able
;; to select the instance of someroot or somepath in a `sh-mode' buffer when the
;; variable is referenced with a $ following a slash as in:
;;
;;       # sh-mode buffer:
;;       someroot=abc
;;       somedir=def
;;       somefile=ghi
;;       export DIRPATH=/$someroot/$somedir/$somefile
;;
;; A similar issue exists with the ':' character.  In sh-mode the ':'
;; character is identified as part of the '_' syntax preventing detection of
;; symbols in strings that are prefixed or followed by the ':' character.
;;
;; To correct the issue for a given mode, call the `pel-iedit-enhance-MODE'
;; function (with MODE replaced by the proper mode name) when the mode starts
;; and iedit is available.  The best way to do that is to schedule its call
;; via a hook.
;;
;; For example, for cperl you could add the function to the cperl mode hook
;; with:
;;        (add-hook 'cperl-mode-hook 'pel-iedit-enhance-cperl)
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
;;; --------------------------------------------------------------------------
;;; Code:
;;

;; -- Perl Editing
;;
;; For something like:
;;  'log4perl.logger.Doxygen::Filter::Perl::_ChangeState'
;; allow selection of the word 'Doxygen' or 'Filter'.

(defun pel--fix-cperl-syntax-for-iedit (function &rest args)
  "Activate iedit extension for the cperl-mode."
  (when (boundp 'cperl-mode-syntax-table)
    ;; Modify the cperl syntax table during text search operation.
    (modify-syntax-entry ?: "." cperl-mode-syntax-table)
    ;; Then execute iedit operation.  Return its value
    (apply function args)))

(defun pel--restore-cperl-syntax (function &rest args)
  "Deactivate iedit extension for the cperl-mode."
  (when (boundp 'cperl-mode-syntax-table)
    ;; Execute iedit operation, remember its result.
    (let ((result (apply function args)))
      ;; Restore cperl syntax table
      (modify-syntax-entry ?: "_" cperl-mode-syntax-table)
      ;; return iedit function result
      result)))

(defun pel-iedit-enhance-cperl ()
  "Improve `cperl-mode' syntax table during iedit search.

Treat the colon character as punctuation to allow changing
only one portion of a Perl symbol separated by \"::\"."
  (advice-add 'iedit-default-occurrence :around #'pel--fix-cperl-syntax-for-iedit)
  (advice-add 'iedit-start :around #'pel--fix-cperl-syntax-for-iedit)
  (advice-add 'iedit-done  :around #'pel--restore-cperl-syntax))


;; ---------------------------------------------------------------------------
;; -- Unix Shell Mode editing
;;
;; Note: code disabled since it seems that iedit now works fine in sh-mode.
;;       Kept commented out for now.

;; (defun pel--fix-sh-syntax-for-iedit (function &rest args)
;;   "Activate iedit extension for the sh-mode."
;;   (when (boundp 'sh-mode-syntax-table)
;;     ;; Modify the sh syntax table during text search operation.
;;     (modify-syntax-entry ?/ "." sh-mode-syntax-table)
;;     (modify-syntax-entry ?: "." sh-mode-syntax-table)
;;     (modify-syntax-entry ?. "." sh-mode-syntax-table)
;;     (modify-syntax-entry ?+ "." sh-mode-syntax-table)
;;
;;     ;; Then execute iedit operation.  Return its value
;;     (apply function args)))
;;
;; (defun pel--restore-sh-syntax (function &rest args)
;;   "Deactivate iedit extension for the sh-mode."
;;   (when (boundp 'sh-mode-syntax-table)
;;     ;; Execute iedit operation, remember its result.
;;     (let ((result (apply function args)))
;;       ;; Restore sh syntax table
;;       (modify-syntax-entry ?/ "_" sh-mode-syntax-table)
;;       (modify-syntax-entry ?: "_" sh-mode-syntax-table)
;;       (modify-syntax-entry ?. "_" sh-mode-syntax-table)
;;       (modify-syntax-entry ?+ "_" sh-mode-syntax-table)
;;
;;       ;; return iedit function result
;;       result)))
;;
;; (defun pel-iedit-enhance-sh ()
;;   "Fix sh syntax table during iedit to allow proper parsing in `sh-mode'."
;;   (advice-add 'iedit-default-occurrence :around #'pel--fix-sh-syntax-for-iedit)
;;   (advice-add 'iedit-start :around #'pel--fix-sh-syntax-for-iedit)
;;   (advice-add 'iedit-done  :around #'pel--restore-sh-syntax))

;; ---------------------------------------------------------------------------
;; -- TCL Editing
;;
;; In tcl-mode buffers,  selecting 'debug' does not match inside '{$debug}'.
;; Fix it my changing the syntax of character '$' to punctuation.

(defun pel--fix-tcl-syntax-for-iedit (function &rest args)
  "Activate iedit extension for the tcl-mode."
  (when (boundp 'tcl-mode-syntax-table)
    ;; Modify the tcl syntax table during text search operation.
    (modify-syntax-entry ?$ "." tcl-mode-syntax-table)
    ;; Then execute iedit operation.  Return its value
    (apply function args)))

(defun pel--restore-tcl-syntax (function &rest args)
  "Deactivate iedit extension for the tcl-mode."
  (when (boundp 'tcl-mode-syntax-table)
    ;; Execute iedit operation, remember its result.
    (let ((result (apply function args)))
      ;; Restore tcl syntax table
      (modify-syntax-entry ?$ "_" tcl-mode-syntax-table)
      ;; return iedit function result
      result)))

(defun pel-iedit-enhance-tcl ()
  "Fix tcl syntax table during iedit to allow proper parsing in `tcl-mode'."
  (advice-add 'iedit-default-occurrence :around #'pel--fix-tcl-syntax-for-iedit)
  (advice-add 'iedit-start :around #'pel--fix-tcl-syntax-for-iedit)
  (advice-add 'iedit-done  :around #'pel--restore-tcl-syntax))

;; ---------------------------------------------------------------------------
;; -- Ninja build editing
;;
(defun pel--fix-ninja-syntax-for-iedit (function &rest args)
  "Activate iedit extension for the ninja-mode."
  (when (boundp 'ninja-mode-syntax-table)
    ;; Modify the ninja syntax table during text search operation.
    ;; The leading ? of variable extension should be a prefix
    (modify-syntax-entry ?$ "'" ninja-mode-syntax-table)
    ;; And the / should be considered a punctuation to allow variable
    ;; to be used inside a path
    (modify-syntax-entry ?/ "." ninja-mode-syntax-table)
    ;; Then execute iedit operation.  Return its value
    (apply function args)))

(defun pel--restore-ninja-syntax (function &rest args)
  "Deactivate iedit extension for the ninja-mode."
  (when (boundp 'ninja-mode-syntax-table)
    ;; Execute iedit operation, remember its result.
    (let ((result (apply function args)))
      ;; Restore ninja syntax table
      (modify-syntax-entry ?$ "w" ninja-mode-syntax-table)
      (modify-syntax-entry ?/ "_" ninja-mode-syntax-table)
      ;; return iedit function result
      result)))

(defun pel-iedit-enhance-ninja ()
  "Fix ninja syntax table during iedit to allow proper parsing in `ninja-mode'."
  (advice-add 'iedit-default-occurrence :around #'pel--fix-ninja-syntax-for-iedit)
  (advice-add 'iedit-start :around #'pel--fix-ninja-syntax-for-iedit)
  (advice-add 'iedit-done  :around #'pel--restore-ninja-syntax))

;;; --------------------------------------------------------------------------
(provide 'pel-iedit-modes-support)

;;; pel-iedit-modes-support.el ends here
