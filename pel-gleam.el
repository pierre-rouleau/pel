;;; pel-gleam.el --- PEL Gleam support extension.  -*- lexical-binding: t; -*-

;; Created   : Monday, October  6 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-20 11:17:36 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2025  Pierre Rouleau
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

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)  ; use: `pel-toggle-and-show-user-option'
;;                           `pel-symbol-value-or'
;;                           `pel-symbol-on-off-string'
(require 'pel--options)
(require 'pel-indent)           ; use `pel-insert-tab-set-width-info'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-gleam-toggle-format-on-buffer-save (&optional globally)
  "Toggle automatic execution of gleam format when saving Gleam files.
By default change behaviour for local buffer only.

When GLOBALLY argument is non-nil, change it for all Gleam buffers in
the current Emacs editing session (the change does not persist across
Emacs sessions).  To modify the global state permanently modify the
customized value of the `gleam-ts-format-on-save' user option."
  (interactive "P")
  (pel-toggle-and-show-user-option 'gleam-ts-format-on-save globally))

;;-pel-autoload
(defun pel-gleam-insert-indent-tab-info ()
  "Insert Gleam indentation and hard tab setup info in current context.
Return `pel-show-indent' capability list."
  (insert (propertize "* Indentation Control:" 'face 'bold))
  (pel-insert-symbol-content-line 'gleam-ts-indent-offset)
  (insert "\n\n")
  ;;
  (insert (propertize "* Hard Tab Control:" 'face 'bold))
  (insert (substitute-command-keys "
 The Gleam designers are pushing for an 2-column indentation
 made of space only. No tabs.
 However, several people find the 2-column indentation too small.
 If this is the case for you, and you work on official Gleam code,
 you could use a temporary use a `tab-width' of 2, tabify the code
 with \\[tabify], then change the `tab-width' to a larger value
 for wider indentation.
 You can disable the format on save while you work on that file.
 When you're done just untabify the code with \\[untabify].
"))
  (pel-insert-symbol-content-line 'pel-gleam-tab-width)
  (pel-insert-symbol-content-line 'tab-width)
  (pel-insert-symbol-content-line 'pel-gleam-use-tabs
                                  nil #'pel-on-off-string)
  (pel-insert-symbol-content-line 'indent-tabs-mode
                                  nil #'pel-on-off-string)
  ;; Return a capability list for `pel-show-indent' or similar callers
  '(supports-set-tab-width))

;;-pel-autoload
(defun pel-gleam-setup-info (&optional append)
  "Display Gleam setup information."
  (interactive "P")
  (let ((pel-insert-symbol-content-context-buffer (current-buffer)))
    (pel-print-in-buffer
     "*pel-gleam-info*"
     "PEL setup for Gleam programming language"
     (lambda ()
       "Print Gleam setup"
       (insert (propertize "* Major Mode Control:" 'face 'bold))
       (insert "
- Note: Gleam is currently only supported by a Tree-Sitter aware mode.")
       (pel-insert-symbol-content 'major-mode nil :on-same-line :no-button
                                  "major mode currently used")
       (when pel-use-tree-sitter
         (insert (format "\n- %s" (pel-ts-language-grammar-status-for
                                   'gleam "\n- "))))
       (insert "\n\n")
       ;;
       (insert (propertize "* Code Formatting Control:" 'face 'bold))
       (pel-insert-symbol-content-line 'gleam-ts-format-on-save
                                       nil
                                       (lambda (v)
                                         (pel-on-off-string
                                          v
                                          "yes, format on save."
                                          "no, save buffer unchanged.")))
       (insert "\n\n")
       (pel-gleam-insert-indent-tab-info)
       (pel-insert-tab-set-width-info))))
  (unless append :clear-buffer)
  :use-help-mode)

;;; --------------------------------------------------------------------------
(provide 'pel-gleam)

;;; pel-gleam.el ends here
