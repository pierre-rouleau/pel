;;; pel-forth.el --- PEL Forth support extension.  -*- lexical-binding: t; -*-

;; Created   : Friday, October 17 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-26 12:18:58 EDT, updated by Pierre Rouleau>

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
(require 'pel--base)        ; use:
(require 'pel--options)     ; use:
(require 'pel-indent)       ; use: `pel-indent-insert-control-info',
;;                          ;      `pel-indent-control-context'
;;                          ;      `pel-tab-insert-control-info',
;;                          ;      `pel-tab-control-context'
(require 'pel-modes)        ; use: `pel-insert-minor-mode-activation-info'
;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-forth-insert-indent-info ()
  "Insert Forth indentation and hard tab setup info in current context.
Return a list of generic symbols described."
  (insert "
- Under PEL, Forth indentation level width is controlled entirely by the
  value of the `pel-forth-indent-width' user-option:
  PEL stores its value inside the variables used by forth-mode:
  `forth-smie-basic-indent' to ensure consistency.

  If you want to use hard tabs for indentation, you should set
  `tab-width' to the same value of `pel-forth-indent-width' and then you
  can control the visual rendering of indentation by changing the values
  of those two user-options: the content of the buffer and file does
  wont change but the indentation rendering will.

  Note, however, that other editors may not be able to do the same; the use of
  hard tabs in Forth source code is not required as it is for Go, therefore
  this technique may not as well-spread as it is for Go.
")
  (pel-insert-symbol-content-line 'pel-forth-indent-width)
  (pel-insert-symbol-content-line 'forth-smie-basic-indent)
  ;; Return the list of generic symbols described here.
  '(indent-description-info
    pel-MM-indent-width))

;;-pel-autoload
(defun pel-forth-insert-tab-info ()
  "Insert Forth indentation and hard tab setup info in current context.
Return a list of generic symbols described."
  (insert "
- The hard tab rendering width is for forth buffer is controlled by
  `pel-forth-tab-width' and stored into `tab-width'.
  These do not control the indentation, just the visual width (in columns)
  that Emacs uses to render a hard tab character.
")
  (pel-insert-symbol-content-line 'pel-forth-tab-width)
  (pel-insert-symbol-content-line 'pel-forth-use-tabs
                                  nil #'pel-on-off-string)
  ;; Return the list of generic symbols described here.
  '(tab-description-intro
    pel-MM-tab-width
    pel-MM-use-tabs))

(defun pel--forth-minor-mode-info ()
  "Insert information related to Forth minor modes."
  (insert "
Automatic activation of minor mode is also controlled by the
following user-options:")
  (pel-insert-list-content 'pel-forth-activates-minor-modes
                           nil nil nil :1line))

;;-pel-autoload
(defun pel-forth-setup-info (&optional append)
  "Display Forth setup information."
  (interactive "P")
  (pel-major-mode-must-be 'forth-mode)
  (let ((pel-insert-symbol-content-context-buffer (current-buffer))
        (current-major-mode major-mode)
        (indent-control-context (pel-indent-control-context))
        (tab-control-context (pel-tab-control-context)))
    (pel-print-in-buffer
     "*pel-forth-info*"
     "PEL setup for Forth programming language"
     (lambda ()
       "Print Forth setup info."
       (insert (propertize "* Major Mode Control:" 'face 'bold))
       (pel-insert-symbol-content 'major-mode nil :on-same-line :no-button
                                  "major mode currently used")
       (insert "
There is no known Tree-Sitter based Emacs major mode for Forth yet.")
       (insert "\n\n")
       ;; --
       (pel-insert-minor-mode-activation-info current-major-mode
                                              #'pel--forth-minor-mode-info)
       (insert "\n\n")
       (pel-indent-insert-control-info indent-control-context)
       (pel-tab-insert-control-info tab-control-context))
     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-forth)

;;; pel-forth.el ends here
