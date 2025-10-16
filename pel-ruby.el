;;; pel-ruby.el --- PEL extra support for Ruby.  -*- lexical-binding: t; -*-

;; Created   : Monday, March 19 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-16 09:58:35 EDT, updated by Pierre Rouleau>

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
;; PEL extra support for the Ruby programming language.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)              ; use: `pel-has-shebang-line'
(require 'pel--options)           ; use: `pel-ruby-shebang-line'
(require 'pel-ccp)                ; use: `pel-delete-line'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-ruby-mode ()
  "Major mode dispatcher for editing Ruby source text.
Uses `ruby-mode' or `ruby-ts-mode' depending on what is available
and required by `pel-use-ruby'."
  (interactive)
  (cond
   ;; When `pel-use-ruby` is t, PEL uses Emacs built-in `ruby-mode'.
   ;; Use that.
   ((eq pel-use-ruby t)
    (when (fboundp 'ruby-mode)
      (ruby-mode)))

   ;; The `ruby-ts-mode' is also built-in Emacs
   ((eq pel-use-ruby 'with-tree-sitter)
    (if (and (pel-treesit-ready-p 'ruby)
             (require 'ruby-ts-mode nil :noerror)
             (fboundp 'ruby-ts-mode))
        (ruby-ts-mode)
      (display-warning 'pel-ruby-with-tree-sitter
                       (format "Can't use ruby-ts-mode: %s"
                               (if (pel-treesit-ready-p 'ruby)
                                   "error loading ruby-ts-mode"
                                 "no grammar for ruby")))

      (if (fboundp 'ruby-mode)
          (ruby-mode)
        (user-error
         "Can't use `ruby-ts-mode' nor `ruby-mode': check installation!"))))))

;;-pel-autoload
(defun pel--ruby-ts-mode-fixer ()
  "Remove `ruby-ts-mode' entries from `auto-mode-alist'.
It removes what entered when `ruby-ts-mode' loads."
  ;; There are several file extensions for Ruby and the ruby-ts-mode
  ;; adds several entries (entries for all Ruby files).
  ;; Delete them all from auto-mode-alist.
  (setq auto-mode-alist
        (rassq-delete-all 'ruby-ts-mode auto-mode-alist)))

;; --

;;-pel-autoload
(defun pel-ruby-mode-used-text (use-ruby)
  "Description of what USE-RUBY specifies for major mode.
USE-RUBY should be set to `pel-use-ruby' value used in current buffer."
  (cond
   ((eq use-ruby t)
    "use ruby-mode from ruby-mode.el.")
   ((eq use-ruby 'with-tree-sitter)
    "use ruby-ts-mode tree-sitter aware mode.")
   (t "Invalid! Use t or with-tree-sitter")))

;;-pel-autoload
(defun pel-ruby-setup-info (&optional append)
  "Display Ruby setup information."
  (interactive "P")
  (let ((pel-insert-symbol-content-context-buffer (current-buffer)))
    (pel-print-in-buffer
     "*pel-ruby-info*"
     "PEL setup for Ruby programming language"
     (lambda ()
       "Print Ruby setup info."
       (insert (propertize "* Major Mode Control:" 'face 'bold))
       (pel-insert-symbol-content-line 'major-mode nil
                                       "major mode currently used.")
       (when pel-use-tree-sitter
         (insert (format "\n- %s" (pel-ts-language-grammar-status-for
                                   'ruby "\n- "))))
       (pel-insert-symbol-content-line 'pel-use-ruby nil
                                       (function pel-ruby-mode-used-text))
       (insert "\n\n")
       ;;
       (insert (propertize "* Indentation Control:" 'face 'bold))
       (insert "
- Under PEL, Ruby indentation level width is controlled entirely by the
  value of the pel-ruby-indent-width user-option:
  PEL stores its value inside the variables used by the ruby-mode and
  ruby-ts-mode to ensure consistency.
- The hard tab rendering width is for ruby buffer is controlled by
  pel-ruby-tab-width and stored into tab-width.  These do not control the
  indentation, just the visual width (in columns) that Emacs uses to render a
  hard tab character.

  If you want to use hard tabs for indentation, you should set the value
  tab-width to the same value of pel-ruby-indent-width and then you can
  control the visual rendering of indentation by changing the values of those
  two user-options: the content of the buffer and file does wont change but
  the indentation rendering will.

  Note, however, that other editors may not be able to do the same; the use of
  hard tabs in Ruby source code is not required as it is for Go, therefore
  this technique may not as well-spread as it is for Go.
")
       (pel-insert-symbol-content-line 'pel-ruby-indent-width)
       (pel-insert-symbol-content-line 'ruby-indent-level)
       (pel-insert-symbol-content-line 'pel-ruby-tab-width)
       (pel-insert-symbol-content-line 'tab-width))
     (unless append :clear-buffer)
     :use-help-mode)))

;; Ruby Shebang Line Control
;; ------------------------
;;
;; Use for extension-less Ruby files that use Ruby directly.

(defun pel-ruby-insert-shebang-line ()
  "Insert a shebang line corresponding to user-option choice."
  ;; If the user-option specifies to use Emacs default, assume that
  ;; it was already inserted by the `pel-as' command selecting Ruby.
  ;; Otherwise assume it's there and must be replaced.
  (save-excursion
    ;; If the file already has a shebang line, replace it by the user-option selected
    (when (pel-has-shebang-line)
      (goto-char (point-min))
      (pel-delete-line))
    (goto-char (point-min))
    (insert pel-ruby-shebang-line)
    (insert "\n")))

;;; --------------------------------------------------------------------------
(provide 'pel-ruby)

;;; pel-ruby.el ends here
