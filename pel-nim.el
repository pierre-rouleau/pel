;;; pel-nim.el --- PEL extra support for Nim.  -*- lexical-binding: t; -*-

;; Created   : Monday, March 19 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-20 15:16:28 EDT, updated by Pierre Rouleau>

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
;; PEL extra support for the Nim programming language.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)        ; use: `pel-has-shebang-line'
(require 'pel--options)     ; use: `pel-nim-shebang-line'
(require 'pel-ccp)          ; use: `pel-delete-line'
(require 'pel-indent)       ; use: `pel-insert-tab-set-width-info'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-nim-insert-indent-tab-info ()
  "Insert Nim indentation and hard tab setup info in current context.
Return `pel-show-indent' capability list."
  (insert (propertize "* Indentation Control:" 'face 'bold))
  (insert "
- Under PEL, Nim indentation level width is controlled entirely by the
  value of the `pel-nim-indent-width' user-option:
  PEL stores its value inside the variables used by the nim-mode
  to ensure consistency.

  If you want to use hard tabs for indentation, you should set the value
  tab-width to the same value of pel-nim-indent-width and then you can
  control the visual rendering of indentation by changing the values of those
  two user-options: the content of the buffer and file does wont change but
  the indentation rendering will.

  Note, however, that other editors may not be able to do the same; the use of
  hard tabs in Nim source code is not required as it is for Go, therefore
  this technique may not as well-spread as it is for Go.
")
  (pel-insert-symbol-content-line 'pel-nim-indent-width)
  (pel-insert-symbol-content-line 'nim-indent-offset)
  (insert "\n\n")
  ;;
  (insert (propertize "* Hard Tab Control:" 'face 'bold))
  (insert "
- The hard tab rendering width is for nim buffer is controlled by
  `pel-nim-tab-width' and stored into `tab-width'.
  These do not control the indentation, just the visual width (in columns)
  that Emacs uses to render a hard tab character.
")
  (pel-insert-symbol-content-line 'pel-nim-tab-width)
  (pel-insert-symbol-content-line 'tab-width)
  (pel-insert-symbol-content-line 'pel-nim-use-tabs
                                  nil #'pel-on-off-string)
  (pel-insert-symbol-content-line 'indent-tabs-mode
                                  nil #'pel-on-off-string)
  ;; Return a capability list for `pel-show-indent' or similar callers
  '(supports-set-tab-width))

;;-pel-autoload
(defun pel-nimscript-insert-indent-tab-info ()
  "Insert Nim Script indentation and hard tab setup info in current context.
Return `pel-show-indent' capability list."
  (pel-nim-insert-indent-tab-info))

;;-pel-autoload
(defun pel-nim-setup-info (&optional append)
  "Display Nim setup information."
  (interactive "P")
  (pel-major-mode-must-be '(nim-mode nimscript-mode))
  (let ((pel-insert-symbol-content-context-buffer (current-buffer)))
    (pel-print-in-buffer
     "*pel-nim-info*"
     "PEL setup for Nim programming language"
     (lambda ()
       "Print Nim setup info."
       (insert (propertize "* Major Mode Control:" 'face 'bold))
       (pel-insert-symbol-content 'major-mode nil :on-same-line :no-button
                                  "major mode currently used")
       (insert "
There is no known Tree-Sitter based Emacs major mode for Nim yet.")
       (insert "\n\n")
       (pel-nim-insert-indent-tab-info)
       (pel-insert-tab-set-width-info))
     (unless append :clear-buffer)
     :use-help-mode)))

;; Nim Shebang Line Control
;; ------------------------
;;
;; Use for extension-less Nim files that use Nim directly.

;;-pel-autoload
(defun pel-nim-insert-shebang-line ()
  "Insert a shebang line corresponding to user-option choice."
  ;; If the user-option specifies to use Emacs default, assume that
  ;; it was already inserted by the `pel-as' command selecting Nim.
  ;; Otherwise assume it's there and must be replaced.
  (save-excursion
    ;; If the file already has a shebang line, replace it by the user-option selected
    (when (pel-has-shebang-line)
      (goto-char (point-min))
      (pel-delete-line))
    (goto-char (point-min))
    (insert pel-nim-shebang-line)
    (insert "\n")))

;;; --------------------------------------------------------------------------
(provide 'pel-nim)

;;; pel-nim.el ends here
