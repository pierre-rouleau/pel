;;; pel-elixir.el --- PEL Elixir support extension.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, October 14 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-20 09:31:54 EDT, updated by Pierre Rouleau>

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
(require 'pel-indent)       ; use `pel-insert-tab-set-width-info'
;;; --------------------------------------------------------------------------
;;; Code:
;;


;;-pel-autoload
(defun pel-elixir-mode ()
  "Major mode dispatcher for editing Elixir source text.
Uses `elixir-mode' or `elixir-ts-mode' depending on what is available
and required by `pel-use-elixir'."
  (interactive)
  (cond
   ;; When `pel-use-elixir` is t, PEL has downloaded and installed
   ;; elixir-mode.el that ;; provides the `elixir-mode'.  Use that.
   ((eq pel-use-elixir t)
    (when (fboundp 'elixir-mode)
      (elixir-mode)))

   ;; The `elixir-ts-mode' is built-in Emacs
   ((eq pel-use-elixir 'with-tree-sitter)
    (if (and (pel-treesit-ready-p 'elixir)
             (require 'elixir-ts-mode nil :noerror)
             (fboundp 'elixir-ts-mode))
        (elixir-ts-mode)
      (display-warning 'pel-elixir-with-tree-sitter
                       (format "Can't use elixir-ts-mode: %s"
                               (if (pel-treesit-ready-p 'elixir)
                                   "error loading elixir-ts-mode"
                                 "no grammar for elixir")))

      (if (fboundp 'elixir-mode)
          (elixir-mode)
        (user-error
         "Can't use `elixir-ts-mode' nor `elixir-mode': check installation!"))))))

;;-pel-autoload
(defun pel--elixir-ts-mode-fixer ()
  "Remove `elixir-ts-mode' entries from `auto-mode-alist'.
It removes what entered when `elixir-ts-mode' loads."
  ;; There are several file extensions for Elixir and the elixir-ts-mode
  ;; adds several entries (entries for .elixir, .ex, .exs, mix.lock).
  ;; Delete them all from auto-mode-alist.
  (setq auto-mode-alist
        (rassq-delete-all 'elixir-ts-mode auto-mode-alist)))

;; --

;;-pel-autoload
(defun pel-elixir-mode-used-text (use-elixir)
  "Description of what USE-ELIXIR specifies for major mode.
USE-ELIXIR should be set to `pel-use-elixir' value used in current buffer."
  (cond
   ((eq use-elixir t)
    "use elixir-mode from elixir-mode.el.")
   ((eq use-elixir 'with-tree-sitter)
    "use elixir-ts-mode tree-sitter aware mode.")
   (t "Invalid! Use t or with-tree-sitter")))

;;-pel-autoload
(defun pel-elixir-insert-indent-tab-info ()
  "Insert Elixir indentation and hard tab setup info in current context.
Return `pel-show-indent' capability list."
  (insert (propertize "* Indentation Control:" 'face 'bold))
  (insert "
- Under PEL, Elixir indentation level width is controlled entirely by the
  value of the pel-elixir-indent-width user-option:
  PEL stores its value inside the variables used by the elixir-mode and
  elixir-ts-mode to ensure consistency.
- The hard tab rendering width is for elixir buffer is controlled by
  pel-elixir-tab-width and stored into tab-width.  These do not control the
  indentation, just the visual width (in columns) that Emacs uses to render a
  hard tab character.

  If you want to use hard tabs for indentation, you should set the value
  tab-width to the same value of pel-elixir-indent-width and then you can
  control the visual rendering of indentation by changing the values of those
  two user-options: the content of the buffer and file does wont change but
  the indentation rendering will.

  Note, however, that other editors may not be able to do the same; the use of
  hard tabs in Elixir source code is not required as it is for Go, therefore
  this technique may not as well-spread as it is for Go.
")
  (pel-insert-symbol-content-line 'pel-elixir-indent-width)
  (pel-insert-symbol-content-line 'elixir-basic-offset)
  (pel-insert-symbol-content-line 'elixir-match-label-offset)
  (pel-insert-symbol-content-line 'elixir-ts-indent-offset)
  (insert "\n\n")
  ;;
  (insert (propertize "* Hard Tab Control:" 'face 'bold))
  (pel-insert-symbol-content-line 'pel-elixir-tab-width)
  (pel-insert-symbol-content-line 'tab-width)
  ;; Return a capability list for `pel-show-indent' or similar callers
  '(supports-set-tab-width))

;;-pel-autoload
(defun pel-elixir-setup-info (&optional append)
  "Display Elixir setup information."
  (interactive "P")
  (pel-major-mode-must-be '(elixir-mode elixir-ts-mode))
  (let ((pel-insert-symbol-content-context-buffer (current-buffer)))
    (pel-print-in-buffer
     "*pel-elixir-info*"
     "PEL setup for Elixir programming language"
     (lambda ()
       "Print Elixir setup info."
       (insert (propertize "* Major Mode Control:" 'face 'bold))
       (pel-insert-symbol-content 'major-mode nil :on-same-line :no-button
                                  "major mode currently used")
       (when pel-use-tree-sitter
         (insert (format "\n- %s" (pel-ts-language-grammar-status-for
                                   'elixir "\n- "))))
       (pel-insert-symbol-content-line 'pel-use-elixir nil
                                       (function pel-elixir-mode-used-text))
       (insert "\n\n")
       (pel-elixir-insert-indent-tab-info)
       (pel-insert-tab-set-width-info))
     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-elixir)

;;; pel-elixir.el ends here
