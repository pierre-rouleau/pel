;;; pel-iedit.el --- Iedit-mode extensions.  -*- lexical-binding: t; -*-

;; Created   : Sunday, November 26 2023.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-09-24 00:48:55 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2023, 2025  Pierre Rouleau
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

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
;; (require 'iedit)
(require 'isearch)
(require 'nadvice)
(require 'pel--options)
;;; --------------------------------------------------------------------------
;;; Code:
;;

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
  (define-key map (kbd "M-S-<f9>")    'iedit-next-occurrence))

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
