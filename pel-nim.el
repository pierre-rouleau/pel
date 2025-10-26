;;; pel-nim.el --- PEL extra support for Nim.  -*- lexical-binding: t; -*-

;; Created   : Monday, March 19 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-26 15:14:42 EDT, updated by Pierre Rouleau>

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
(require 'pel-indent)       ; use: `pel-indent-insert-control-info',
;;                          ;      `pel-indent-control-context'
;;                          ;      `pel-tab-insert-control-info',
;;                          ;      `pel-tab-control-context'
(require 'pel-modes)        ; use: `pel-insert-minor-mode-activation-info'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-nim-insert-indent-info ()
  "Insert Nim indentation setup info in current context.
Return a list of generic symbols described."
  (insert "
- Under PEL, Nim indentation level width is controlled entirely by the
  value of the `pel-nim-indent-width' user-option:
  PEL stores its value inside the variables used by the nim-mode
  to ensure consistency.

  If you want to use hard tabs for indentation, you should set
  `tab-width' to the same value of `pel-nim-indent-width' and then you
  can control the visual rendering of indentation by changing the values
  of those two user-options: the content of the buffer and file does
  wont change but the indentation rendering will.

  Note, however, that other editors may not be able to do the same; the use of
  hard tabs in Nim source code is not required as it is for Go, therefore
  this technique may not as well-spread as it is for Go.
")
  (pel-insert-symbol-content-line 'pel-nim-indent-width)
  (pel-insert-symbol-content-line 'nim-indent-offset)
  ;; Return the list of generic symbols described here.
  '(indent-description-intro
    pel-MM-indent-width))

;;-pel-autoload
(defun pel-nim-insert-tab-info ()
  "Insert Nim hard tab setup info in current context.
Return a list of generic symbols described."
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
  ;; Return the list of generic symbols described here.
  '(tab-description-intro
    pel-MM-tab-width
    tab-width
    pel-MM-use-tabs
    indent-tabs-mode))

;;-pel-autoload
(defun pel-nimscript-insert-indent-info ()
  "Insert Nim Script indentation setup info in current context.
Return a list of generic symbols described."
  (pel-nim-insert-indent-info))

;;-pel-autoload
(defun pel-nimscript-insert-tab-info ()
  "Insert Nim Script hard tab setup info in current context.
Return a list of generic symbols described."
  (pel-nim-insert-tab-info))

(defun pel--nim-minor-mode-info ()
  "Insert information related to Nim minor modes."
  (insert "
Automatic activation of minor mode is also controlled by the
following user-options:")
  (pel-insert-list-content 'pel-nim-activates-minor-modes
                           nil nil nil :1line))

;;-pel-autoload
(defun pel-nim-setup-info (&optional append)
  "Display Nim setup information."
  (interactive "P")
  (pel-major-mode-must-be '(nim-mode nimscript-mode))
  (let ((pel-insert-symbol-content-context-buffer (current-buffer))
        (current-major-mode major-mode)
        (indent-control-context (pel-indent-control-context))
        (tab-control-context (pel-tab-control-context)))
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
       ;; --
       (pel-insert-minor-mode-activation-info current-major-mode
                                              #'pel--nim-minor-mode-info)
       (insert "\n\n")
       (pel-indent-insert-control-info indent-control-context)
       (pel-tab-insert-control-info tab-control-context))
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
