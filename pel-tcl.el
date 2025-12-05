;;; pel-tcl.el --- PEL extra support for Tcl.  -*- lexical-binding: t; -*-

;; Created   : Monday, March 17 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-12-05 17:01:17 EST, updated by Pierre Rouleau>

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
;; PEL extra support for the Tcl programming language.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)        ; use: `pel-has-shebang-line'
(require 'pel--options)     ; use: `pel-tcl-shebang-line'
(require 'pel-ccp)          ; use: `pel-delete-line'
(require 'pel-indent)       ; use: `pel-indent-insert-control-info',
;;                          ;      `pel-indent-control-context'
;;                          ;      `pel-tab-insert-control-info',
;;                          ;      `pel-tab-control-context'
(require 'pel-modes)        ; use: `pel-insert-minor-mode-activation-info'
;;                          ;      `pel-active-minor-modes'
;;                          ;      `pel-insert-list-of-minor-modes'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-tcl-insert-indent-info ()
  "Insert Tcl indentation setup info in current context.
Return a list of generic symbols described."
  (insert "
- Under PEL, Tcl indentation level width is controlled entirely by the
  value of the `pel-tcl-indent-width' user-option:
  PEL stores its value inside the variables used by the tcl-mode
  to ensure consistency.  It does, however, not change the value
  of tcl-continued-indent-level.
  You probably want to ensure they all have the same values.

  If you want to use hard tabs for indentation, you should set
  `tab-width' to the same value of `pel-tcl-indent-width' and then you
  can control the visual rendering of indentation by changing the values
  of those two user-options: the content of the buffer and file does
  wont change but the indentation rendering will.

  Note, however, that other editors may not be able to do the same; the use of
  hard tabs in Tcl source code is not required as it is for Go, therefore
  this technique may not as well-spread as it is for Go.
")
  (pel-insert-symbol-content-line 'pel-tcl-indent-width)
  (pel-insert-symbol-content-line 'tcl-indent-level)
  (pel-insert-symbol-content-line 'tcl-continued-indent-level)
  ;; Return the list of generic symbols described here.
  '(indent-description-intro
    pel-MM-indent-width
    tcl-indent-level
    tcl-continued-indent-level))

;;-pel-autoload
(defun pel-tcl-insert-tab-info ()
  "Insert Tcl hard tab setup info in current context.
Return a list of generic symbols described."
  (insert "
- The hard tab rendering width is for tcl buffer is controlled by
  `pel-tcl-tab-width' and stored into `tab-width'.
  These do not control the indentation, just the visual width (in columns)
  that Emacs uses to render a hard tab character.
")
  (pel-insert-symbol-content-line 'pel-tcl-tab-width)
  (pel-insert-symbol-content-line 'tab-width)
  (pel-insert-symbol-content-line 'pel-tcl-use-tabs
                                  nil #'pel-on-off-string)
  (pel-insert-symbol-content-line 'indent-tabs-mode
                                  nil #'pel-on-off-string)
  ;; Return the list of generic symbols described here.
  '(tab-description-intro
    pel-MM-tab-width
    tab-width
    pel-MM-use-tabs
    indent-tabs-mode))

(defun pel--tcl-minor-mode-info ()
  "Insert information related to Tcl minor modes."
  (insert "
Automatic activation of minor mode is also controlled by the
following user-options:")
  (pel-insert-list-content 'pel-tcl-activates-minor-modes
                           nil nil nil :1line))

;;-pel-autoload
(defun pel-tcl-setup-info (&optional append)
  "Display Tcl setup information."
  (interactive "P")
  (pel-major-mode-must-be '(tcl-mode))
  (let ((pel-insert-symbol-content-context-buffer (current-buffer))
        (current-major-mode major-mode)
        (active-modes (pel-active-minor-modes))
        (indent-control-context (pel-indent-control-context))
        (tab-control-context (pel-tab-control-context)))
    (pel-print-in-buffer
     "*pel-tcl-info*"
     "PEL setup for Tcl programming language"
     (lambda ()
       "Print Tcl setup info."
       (insert (propertize "* Major Mode Control:" 'face 'bold))
       (pel-insert-symbol-content 'major-mode nil :on-same-line nil
                                  "major mode currently used")
       (insert "
There is no known Tree-Sitter based Emacs major mode for Tcl yet.")
       ;; -- List of minor modes
       (pel-insert-list-of-minor-modes active-modes)
       (insert "\n\n")
       (pel-insert-minor-mode-activation-info current-major-mode
                                              #'pel--tcl-minor-mode-info)
       ;; --
       (insert "\n\n")
       (pel-indent-insert-control-info indent-control-context)
       (pel-tab-insert-control-info tab-control-context))
     (unless append :clear-buffer)
     :use-help-mode)))

;; Tcl Shebang Line Control
;; ------------------------
;;
;; Use for extension-less Tcl files that use Tcl directly.

;;-pel-autoload
(defun pel-tcl-insert-shebang-line ()
  "Insert a shebang line corresponding to user-option choice."
  ;; If the user-option specifies to use Emacs default, assume that
  ;; it was already inserted by the `pel-as' command selecting Tcl.
  ;; Otherwise assume it's there and must be replaced.
  (save-excursion
    ;; If the file already has a shebang line, replace it by the user-option selected
    (when (pel-has-shebang-line)
      (goto-char (point-min))
      (pel-delete-line))
    (goto-char (point-min))
    (insert pel-tcl-shebang-line)
    (insert "\n")))

;;-pel-autoload
(defun pel-tcl-expect-insert-shebang-line ()
  "Insert a shebang line corresponding to user-option choice."
  ;; If the user-option specifies to use Emacs default, assume that
  ;; it was already inserted by the `pel-as' command selecting Tcl expect.
  ;; Otherwise assume it's there and must be replaced.
  (save-excursion
    ;; If the file already has a shebang line, replace it by the user-option selected
    (when (pel-has-shebang-line)
      (goto-char (point-min))
      (pel-delete-line))
    (goto-char (point-min))
    (insert pel-tcl-expect-shebang-line)
    (insert "\n")))

;;; --------------------------------------------------------------------------
(provide 'pel-tcl)

;;; pel-tcl.el ends here
