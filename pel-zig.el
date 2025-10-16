;;; pel-zig.el --- PEL Zig support extension.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, October 14 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-16 09:55:51 EDT, updated by Pierre Rouleau>

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

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-zig-mode ()
  "Major mode dispatcher for editing Zig source text.
Uses `zig-mode' or `zig-ts-mode' depending on what is available
and required by `pel-use-zig'."
  (interactive)
  (cond
   ;; When `pel-use-zig` is t, PEL has downloaded and installed zig-mode.el
   ;; that provides the `zig-mode'.  Use that.
   ((eq pel-use-zig t)
    (when (fboundp 'zig-mode)
      (zig-mode)))

   ;; The `zig-ts-mode' is not built-in Emacs
   ((eq pel-use-zig 'with-tree-sitter)
    (if (and (pel-treesit-ready-p 'zig)
             (require 'zig-ts-mode nil :noerror)
             (fboundp 'zig-ts-mode))
        (zig-ts-mode)
      (display-warning 'pel-zig-with-tree-sitter
                       (format "Can't use zig-ts-mode: %s"
                               (if (pel-treesit-ready-p 'zig)
                                   "error loading zig-ts-mode"
                                 "no grammar for zig")))

      (if (fboundp 'zig-mode)
          (zig-mode)
        (user-error
         "Can't use `zig-ts-mode' nor `zig-mode': check installation!"))))))

;;-pel-autoload
(defun pel--zig-ts-mode-fixer ()
  "Remove `zig-ts-mode' entries from `auto-mode-alist'.
It removes what entered when `zig-ts-mode' loads."
  ;; There are several file extensions for Zig and the zig-ts-mode
  ;; adds several entries (entries for .zig, .zon).
  ;; Delete them all from auto-mode-alist.
  (setq auto-mode-alist
        (rassq-delete-all 'zig-ts-mode auto-mode-alist)))

;; --

;;-pel-autoload
(defun pel-zig-mode-used-text (use-zig)
  "Description of what USE-ZIG specifies for major mode.
USE-ZIG should be set to `pel-use-zig' value used in current buffer."
  (cond
   ((eq use-zig t)
    "use zig-mode from zig-mode.el.")
   ((eq use-zig 'with-tree-sitter)
    "use zig-ts-mode tree-sitter aware mode.")
   (t "Invalid! Use t or with-tree-sitter")))

;;-pel-autoload
(defun pel-zig-setup-info (&optional append)
  "Display Zig setup information."
  (interactive "P")
  (pel-major-mode-must-be '(zig-mode zig-ts-mode))
  (let ((pel-insert-symbol-content-context-buffer (current-buffer)))
    (pel-print-in-buffer
     "*pel-zig-info*"
     "PEL setup for Zig programming language"
     (lambda ()
       "Print Zig setup info."
       (insert (propertize "* Major Mode Control:" 'face 'bold))
       (pel-insert-symbol-content-line 'major-mode nil
                                       "major mode currently used.")
       (when pel-use-tree-sitter
         (insert (format "\n- %s" (pel-ts-language-grammar-status-for
                                   'zig "\n- "))))
       (pel-insert-symbol-content-line 'pel-use-zig nil
                                       (function pel-zig-mode-used-text))
       (insert "\n\n")
       ;;
       (insert (propertize "* Code Formatting Control:" 'face 'bold))
       (pel-insert-symbol-content-line 'zig-format-on-save)
       ;;
       (insert "\n\n")
       (insert (propertize "* Indentation Control:" 'face 'bold))
       (insert "
- Under PEL, Zig indentation level width is controlled entirely by the
  value of the pel-zig-indent-width user-option:
  PEL stores its value inside the variables used by the zig-mode and
  zig-ts-mode to ensure consistency.
- The hard tab rendering width is for zig buffer is controlled by
  pel-zig-tab-width and stored into tab-width.  These do not control the
  indentation, just the visual width (in columns) that Emacs uses to render a
  hard tab character.

  If you want to use hard tabs for indentation, you should set the value
  tab-width to the same value of pel-zig-indent-width and then you can
  control the visual rendering of indentation by changing the values of those
  two user-options: the content of the buffer and file does wont change but
  the indentation rendering will.

  Note, however, that other editors may not be able to do the same; the use of
  hard tabs in Zig source code is not required as it is for Go, therefore
  this technique may not as well-spread as it is for Go.
")
       (pel-insert-symbol-content-line 'pel-zig-indent-width)
       (pel-insert-symbol-content-line 'zig-indent-offset)
       (pel-insert-symbol-content-line 'zig-ts-indent-offset)
       (pel-insert-symbol-content-line 'pel-zig-tab-width)
       (pel-insert-symbol-content-line 'tab-width))
     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-zig)

;;; pel-zig.el ends here
