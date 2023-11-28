;;; pel-iedit.el --- Iedit-mode extensions.  -*- lexical-binding: t; -*-

;; Created   : Sunday, November 26 2023.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2023-11-28 15:23:30 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2023  Pierre Rouleau
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
;; iedit mode key mapping change and extensions.

;; This file provides new key mapping to the iedit navigation and toggling;
;; keys that do not clash with often used bindings.  It also provides a quick
;; way to access the iedit customization buffer.  The new bindings are
;; installed by calling `pel-add-keys-to-iedit-mode'.

;; TODO: complete the following:
;;
;; Another goal of this file is to provide ability to use iedit on source code and
;; exclude comments and strings. That feature is experimental and currently
;; does not work (at least on Emacs 26.3).  The theory is that since iedit
;; uses isearch for performing it search we can provide isearch a filter to
;; only search in source code. However, the implementation here is not working
;; properly. It attempts to advice the function identified by the
;; `isearch-filter-predicate' variable to return nil when point is not over
;; code.  The goal was to be able to toggle that search behaviour on or off.
;; The variables and functions are defined but not bound to any key sequence.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
;; (require 'iedit)
(require 'isearch)
(require 'nadvice)
(require 'pel--options)
(require 'pel--syntax-macros)
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defvar-local pel-iedit-only-in-code nil
  "When non-nil iedit only operator on code, not in comment nor strings.")

(defun pel-isearch-in-code (start-pos end-pos)
  "isearch predicate to search inside code only."
  (and
   (pel-inside-code-p start-pos)
   (pel-inside-code-p end-pos)))

(defun pel-iedit-toggle-just-in-code ()
  "Restrict iedit to just code; exclude comment and strings."
  (interactive)
  (if pel-iedit-only-in-code
      (progn
        (remove-function isearch-filter-predicate #'pel-isearch-in-code)
        (setq pel-iedit-only-in-code nil))
    (add-function :around isearch-filter-predicate #'pel-isearch-in-code)
    (setq pel-iedit-only-in-code t)))

(defun pel-customize-iedit ()
  "Customize the iedit group."
  (interactive)
  (customize-group "iedit"))

(defun pel-customize-pel-iedit ()
  "Customize the iedit group."
  (interactive)
  (customize-group "pel-pkg-for-iedit"))

(defun pel--change-iedit-navkeys (map)
  "Re-organize iedit keys"
  (define-key map (kbd "<backtab>")        nil)
  (define-key map (kbd "<S-tab>")          nil)
  (define-key map (kbd "<S-iso-lefttab>")  nil)
  (define-key map (kbd "M-S-<f7>")     'iedit-prev-occurrence)

  ;; Change the iedit-mode navigation keys.
  (define-key map (kbd "M-;")       nil)
  (define-key map (kbd "M-S-<f8>")    'iedit-toggle-selection)

  (define-key map (kbd "TAB")       nil)
  (define-key map (kbd "<tab>")     nil)
  (define-key map (kbd "M-S-<f9>")    'iedit-next-occurrence)

  ;; Add ability to toggle including comments and strings -- FUTURE: currently does not work
  ;; (define-key map (kbd "M-;") #'pel-iedit-toggle-just-in-code)
  )

(defun pel-add-keys-to-iedit-mode ()
  "Add keys that work in terminal mode to iedit-mode key maps."
  (when (boundp 'iedit-lib-keymap)
    (define-key iedit-lib-keymap (kbd "C-c C-a") 'iedit-show/hide-context-lines)
    (define-key iedit-lib-keymap (kbd "C-c C-o") 'iedit-show/hide-occurrence-lines))

  (when (and pel-iedit-use-alternate-keys
             (boundp 'iedit-mode-keymap))
    (let ((map iedit-mode-keymap))
      (pel--change-iedit-navkeys map)))

  (when (boundp 'iedit-mode-occurrence-keymap)
    (let ((map iedit-mode-occurrence-keymap))
      (define-key map (kbd "<f11> <f2>") #'pel-customize-pel-iedit)
      (define-key map (kbd "<f11> <f3>") #'pel-customize-iedit)
      (when pel-iedit-use-alternate-keys
        (pel--change-iedit-navkeys map))

      (define-key map (kbd "<f1> <f2>") 'iedit-help-for-occurrences)
      (define-key map (kbd "M-U")       'pel-redo)
      (define-key map (kbd "<f1> M-c")  'iedit-toggle-case-sensitive)
      (define-key map (kbd "M-c")       'iedit-downcase-occurrences)
      (define-key map (kbd "M-C")       'iedit-upcase-occurrences))))

;;; --------------------------------------------------------------------------
(provide 'pel-iedit)

;;; pel-iedit.el ends here
