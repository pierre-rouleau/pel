;;; pel-js.el --- PEL Javascript support extension.  -*- lexical-binding: t; -*-

;; Created   : Monday, October 20 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-12-05 16:34:38 EST, updated by Pierre Rouleau>

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
;;                          ;      `pel-active-minor-modes'
;;                          ;      `pel-insert-list-of-minor-modes'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-js-mode ()
  "Major mode dispatcher for editing Javascript source text.
Uses `js-mode' or `js-ts-mode' depending on what is available
and required by `pel-use-js'."
  (interactive)
  (cond
   ;; When `pel-use-js` is t or 'with-js2-minor, use the built-in `js-mode'.
   ((memq pel-use-js '(t
                       with-js2-minor))
    (when (fboundp 'js-mode)
      (js-mode)))

   ;; When `pel-use-js' is `with-tree-sitter or 'with-ts-js2-minor,
   ;; use `js-ts-mode' (which is also built-in Emacs).
   ((memq pel-use-js '(with-tree-sitter
                       with-ts-js2-minor))
    (if (and (pel-treesit-ready-p 'javascript)
             (fboundp 'js-ts-mode))
        (js-ts-mode)
      (display-warning 'pel-js-with-tree-sitter
                       (format "Can't use js-ts-mode: %s"
                               (if (pel-treesit-ready-p 'js)
                                   "error loading js-ts-mode"
                                 "no grammar for javascript")))
      (if (fboundp 'js-mode)
          (js-mode)
        (user-error
         "Can't use `js-ts-mode' nor `js-mode': check installation!"))))))

;;-pel-autoload
(defun pel--js-ts-mode-fixer ()
  "Remove `js-ts-mode' entries from `auto-mode-alist'.
It removes what entered when `js-ts-mode' loads."
  ;; There are several file extensions for Javascript and the `js-ts-mode'
  ;; adds several entries (entries for .js, .zon).
  ;; Delete them all from auto-mode-alist.
  (setq auto-mode-alist
        (rassq-delete-all 'js-ts-mode auto-mode-alist))
  ;; Also prevent the hijacking from `js2-mode' and `js3-mode'
  ;; to allow the user to use these mode temporarily.
  (setq auto-mode-alist
        (rassq-delete-all 'js2-mode auto-mode-alist))
  (setq auto-mode-alist
        (rassq-delete-all 'js3-mode auto-mode-alist)))


;;-pel-autoload
(defun pel-js-mode-used-text (use-js)
  "Description of what USE-JS specifies for major mode.
USE-JS should be set to `pel-use-js' value used in current buffer."
  (cond
   ((eq use-js t)
    "use js-mode from js-mode.el.")
   ((eq use-js 'js2-mode)
    "use js2-mode from external js2-mode.el")
   ((eq use-js 'with-tree-sitter)
    "use js-ts-mode tree-sitter aware mode.")
   ((eq use-js 'with-js2-minor)
    "\
use js-mode from js-mode.el with js2-minor-mode from external js2-mode.el")
   ((eq use-js 'with-ts-js2-minor)
    "\
use js-ts-mode tree-sitter aware mode with js2-minor-mode from js2-mode.el")
   ((eq use-js 'js3-mode)
    "use js3-mode from external js3-mode.el")
   (t "Invalid! Use t, js2-mode or with-tree-sitter")))

;; ---------------------------------------------------------------------------

;;-pel-autoload
(defun pel-js-insert-indent-info ()
  "Insert Javascript indentation used setup in current context.
Return a list of generic symbols described."
  (insert "
- Under PEL, Javascript indentation level width is controlled entirely by the
  value of the `pel-js-indent-width' user-option:
  PEL stores its value inside the variables used by the js-mode and
  js-ts-mode to ensure consistency.

  If you want to use hard tabs for indentation, you should set
  `tab-width' to the same value of `pel-js-indent-width' and then you
  can control the visual rendering of indentation by changing the values
  of those two user-options: the content of the buffer and file does
  wont change but the indentation rendering will.

  Note, however, that other editors may not be able to do the same; the use of
  hard tabs in Javascript source code is not required as it is for Go, therefore
  this technique may not as well-spread as it is for Go.
")
  (pel-insert-symbol-content-line 'pel-js-indent-width)
  (pel-insert-symbol-content-line 'js-curly-indent-offset)
  (pel-insert-symbol-content-line 'js-expr-indent-offset)
  (pel-insert-symbol-content-line 'js-indent-level)
  (pel-insert-symbol-content-line 'js-jsx-attribute-offset)
  (pel-insert-symbol-content-line 'js-jsx-indent-level)
  (pel-insert-symbol-content-line 'js-paren-indent-offset)
  (pel-insert-symbol-content-line 'js-square-indent-offset)
  (pel-insert-symbol-content-line 'js-switch-indent-offset)
  (pel-insert-symbol-content-line 'js3-indent-level)
  ;; Return the list of generic symbols described here.
  '(indent-description-info
    pel-MM-indent-width
    js-curly-indent-offset
    js-expr-indent-offset
    js-indent-level
    js-jsx-attribute-offset
    js-jsx-indent-level
    js-paren-indent-offset
    js-square-indent-offset
    js-switch-indent-offset
    js3-indent-level))

(defun pel-js-insert-tab-info ()
  "Insert Javascript hard tab setup used in current context.
Return a list of generic symbols described."
  (insert "
- The hard tab rendering width is for js buffer is controlled by
  `pel-js-tab-width' and stored into `tab-width'.
  These do not control the indentation, just the visual width (in
  columns) that Emacs uses to render a hard tab character.
  Whether hard tabs are used in js buffer is controlled by
  `pel-js-use-tabs' and stored inside `indent-tabs-mode'.
")
  (pel-insert-symbol-content-line 'pel-js-tab-width)
  (pel-insert-symbol-content-line 'pel-js-use-tabs
                                  nil #'pel-on-off-string)
  ;; Return the list of generic symbols described here.
  '(tab-description-intro
    pel-MM-tab-width
    pel-MM-use-tabs))

(defun pel--js-minor-mode-info ()
  "Insert information related to Javascript minor modes."
  (insert (substitute-command-keys "
 - You can use the `js2-minor-mode' with `js-mode' or `js-ts-mode'.
   The minor mode is automatically activated when the `pel-use-js'
   user option is set to with-js2-minor or with-ts-js2-minor.
 - When enabled, toggle `js2-minor-mode' with \\[js2-minor-mode].
 - The development js2 commands and features are activated by the
   `pel-js2-activates-development-mode' user-option.
"))
  (pel-insert-symbol-content-line 'pel-js2-activates-development-mode)
  (insert "
Automatic activation of minor mode is also controlled by the
following user-options:")
  (pel-insert-list-content 'pel-js-activates-minor-modes
                           nil nil nil :1line)
  (pel-insert-list-content 'pel-js2-activates-minor-modes
                           nil nil nil :1line)
  (pel-insert-list-content 'pel-js3-activates-minor-modes
                           nil nil nil :1line))

;;-pel-autoload
(defun pel-js-setup-info (&optional append)
  "Display Javascript setup information."
  (interactive "P")
  (pel-major-mode-must-be '(js-mode js-ts-mode js2-mode js3-mode))
  (let ((pel-insert-symbol-content-context-buffer (current-buffer))
        (current-major-mode major-mode)
        (active-modes (pel-active-minor-modes))
        (indent-control-context (pel-indent-control-context))
        (tab-control-context (pel-tab-control-context)))
    (pel-print-in-buffer
     "*pel-js-info*"
     "PEL setup for Javascript programming language"
     (lambda ()
       "Print Javascript setup info."
       (insert (propertize "* Major Mode Control:" 'face 'bold))
       (pel-insert-symbol-content 'major-mode nil :on-same-line :no-button
                                  "major mode currently used")
       (when pel-use-tree-sitter
         (insert (format "\n- %s" (pel-ts-language-grammar-status-for
                                   'js "\n- "))))
       (pel-insert-symbol-content-line 'pel-use-js nil
                                       (function pel-js-mode-used-text))
       ;; -- List of minor modes
       (pel-insert-list-of-minor-modes active-modes)
       (insert "\n\n")
       (pel-insert-minor-mode-activation-info current-major-mode
                                              #'pel--js-minor-mode-info)
       ;; --
       (insert "\n\n")
       (pel-indent-insert-control-info indent-control-context)
       (pel-tab-insert-control-info tab-control-context))
     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-js)

;;; pel-js.el ends here
