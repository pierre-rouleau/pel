;;; pel-lua.el --- PEL extra support for Lua.  -*- lexical-binding: t; -*-

;; Created   : Monday, March 19 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-12-05 16:46:43 EST, updated by Pierre Rouleau>

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
;; PEL extra support for the Lua programming language.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)        ; use: `pel-has-shebang-line'
(require 'pel--options)     ; use: `pel-lua-shebang-line'
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
(defun pel-lua-mode ()
  "Major mode dispatcher for editing Lua source text.
Uses `lua-mode' or `lua-ts-mode' depending on what is available
and required by `pel-use-lua'."
  (interactive)
  (cond
   ;; When `pel-use-lua` is t, PEL has downloaded and installed lua-mode.el
   ;; that provides the `lua-mode'.  Use that.
   ((eq pel-use-lua t)
    (when (fboundp 'lua-mode)
      (lua-mode)))

   ;; The `lua-ts-mode' is not built-in Emacs
   ((eq pel-use-lua 'with-tree-sitter)
    (if (and (pel-treesit-ready-p 'lua)
             (require 'lua-ts-mode nil :noerror)
             (fboundp 'lua-ts-mode))
        (lua-ts-mode)
      (display-warning 'pel-lua-with-tree-sitter
                       (format "Can't use lua-ts-mode: %s"
                               (if (pel-treesit-ready-p 'lua)
                                   "error loading lua-ts-mode"
                                 "no grammar for lua")))

      (if (fboundp 'lua-mode)
          (lua-mode)
        (user-error
         "Can't use `lua-ts-mode' nor `lua-mode': check installation!"))))))

;;-pel-autoload
(defun pel-lua-repl ()
  "Start the Lua REPL.
The actual REPL command used depends on whether `lua-ts-mode' is available and
the selection made by `pel-lua-repl-used'."
  (interactive)
  (if (and pel-use-tree-sitter
           (eq pel-lua-repl-used 'use-lua-ts-mode-repl-when-available)
           (fboundp 'lua-ts-inferior-lua))
      (call-interactively (function lua-ts-inferior-lua))
    (if (fboundp 'lua-start-process)
        (call-interactively (function lua-start-process))
      (error "lua-start-process is not bound."))))


;;-pel-autoload
(defun pel--lua-ts-mode-fixer ()
  "Remove `lua-ts-mode' entries from `auto-mode-alist'.
It removes what entered when `lua-ts-mode' loads."
  ;; There are several file extensions for Lua and the lua-ts-mode
  ;; adds several entries (entries for .lua).
  ;; Delete them all from auto-mode-alist.
  (setq auto-mode-alist
        (rassq-delete-all 'lua-ts-mode auto-mode-alist)))

;; --

;;-pel-autoload
(defun pel-lua-mode-used-text (use-lua)
  "Description of what USE-LUA specifies for major mode.
USE-LUA should be set to `pel-use-lua' value used in current buffer."
  (cond
   ((eq use-lua t)
    "use lua-mode from lua-mode.el.")
   ((eq use-lua 'with-tree-sitter)
    "use lua-ts-mode tree-sitter aware mode.")
   (t "Invalid! Use t or with-tree-sitter")))

;;-pel-autoload
(defun pel-lua-insert-indent-info ()
  "Insert Lua indentation setup info in current context.
Return a list of generic symbols described."
  (insert "
- Under PEL, Lua indentation level width is controlled entirely by the
  value of the `pel-lua-indent-width' user-option:
  PEL stores its value inside the variables used by the lua-mode and
  lua-ts-mode to ensure consistency.

  If you want to use hard tabs for indentation, you should set
  `tab-width' to the same value of `pel-lua-indent-width' and then you
  can control the visual rendering of indentation by changing the values
  of those two user-options: the content of the buffer and file does
  wont change but the indentation rendering will.

  Note, however, that other editors may not be able to do the same; the use of
  hard tabs in Lua source code is not required as it is for Go, therefore
  this technique may not as well-spread as it is for Go.
")
  (pel-insert-symbol-content-line 'pel-lua-indent-width)
  (pel-insert-symbol-content-line 'lua-indent-level)
  (pel-insert-symbol-content-line 'lua-ts-indent-offset)
  ;; Return the list of generic symbols described here.
  '(indent-description-info
    pel-MM-indent-width
    lua-indent-level
    lua-ts-indent-offset))

;;-pel-autoload
(defun pel-lua-insert-tab-info ()
  "Insert Lua hard tab setup info in current context.
Return a list of generic symbols described."
  (insert "
- The hard tab rendering width is for lua buffer is controlled by
  `pel-lua-tab-width' and stored into `tab-width'.
  These do not control the indentation, just the visual width (in columns)
  that Emacs uses to render a hard tab character.
")
  (pel-insert-symbol-content-line 'pel-lua-tab-width)
  (pel-insert-symbol-content-line 'tab-width)
  (pel-insert-symbol-content-line 'pel-lua-use-tabs
                                  nil #'pel-on-off-string)
  (pel-insert-symbol-content-line 'indent-tabs-mode
                                  nil #'pel-on-off-string)
  ;; Return the list of generic symbols described here.
  '(tab-description-intro
    pel-MM-tab-width
    tab-width
    pel-MM-use-tabs
    indent-tabs-mode))

(defun pel--lua-minor-mode-info ()
  "Insert information related to Lua minor modes."
  (insert "
Automatic activation of minor mode is also controlled by the
following user-options:")
  (pel-insert-list-content 'pel-lua-activates-minor-modes
                           nil nil nil :1line))

;;-pel-autoload
(defun pel-lua-setup-info (&optional append)
  "Display Lua setup information."
  (interactive "P")
  (pel-major-mode-must-be '(lua-mode lua-ts-mode))
  (let ((pel-insert-symbol-content-context-buffer (current-buffer))
        (current-major-mode major-mode)
        (active-modes (pel-active-minor-modes))
       (indent-control-context (pel-indent-control-context))
        (tab-control-context (pel-tab-control-context)))
    (pel-print-in-buffer
     "*pel-lua-info*"
     "PEL setup for Lua programming language"
     (lambda ()
       "Print Lua setup info."
       (insert (propertize "* Major Mode Control:" 'face 'bold))
       (pel-insert-symbol-content 'major-mode nil :on-same-line nil
                                  "major mode currently used")
       (when pel-use-tree-sitter
         (insert (format "\n- %s" (pel-ts-language-grammar-status-for
                                   'lua "\n- "))))
       (pel-insert-symbol-content-line 'pel-use-lua nil
                                       (function pel-lua-mode-used-text))
       ;; -- List of minor modes
       (pel-insert-list-of-minor-modes active-modes)
       (insert "\n\n")
       (pel-insert-minor-mode-activation-info current-major-mode
                                              #'pel--lua-minor-mode-info)
       ;; --
       (insert "\n\n")
       (pel-indent-insert-control-info indent-control-context)
       (pel-tab-insert-control-info tab-control-context))
     (unless append :clear-buffer)
     :use-help-mode)))

;; Lua Shebang Line Control
;; ------------------------
;;
;; Use for extension-less Lua files that use Lua directly.

(defun pel-lua-insert-shebang-line ()
  "Insert a shebang line corresponding to user-option choice."
  ;; If the user-option specifies to use Emacs default, assume that
  ;; it was already inserted by the `pel-as' command selecting Lua.
  ;; Otherwise assume it's there and must be replaced.
  (save-excursion
    ;; If the file already has a shebang line, replace it by the user-option selected
    (when (pel-has-shebang-line)
      (goto-char (point-min))
      (pel-delete-line))
    (goto-char (point-min))
    (insert pel-lua-shebang-line)
    (insert "\n")))

;;; --------------------------------------------------------------------------
(provide 'pel-lua)

;;; pel-lua.el ends here
