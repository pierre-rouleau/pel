;;; pel-rust.el --- Rust programming language support.  -*- lexical-binding: t; -*-

;; Created   : Sunday, October 12 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-26 15:16:48 EDT, updated by Pierre Rouleau>

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
(require 'pel--base)        ; use: `pel-set-tab-width', `pel-treesit-ready-p'
(require 'pel--options)     ; use: `pel-go-run-gofmt-on-buffer-save'
(require 'pel-indent)       ; use: `pel-indent-insert-control-info',
;;                          ;      `pel-indent-control-context'
;;                          ;      `pel-tab-insert-control-info',
;;                          ;      `pel-tab-control-context'
(require 'pel-modes)        ; use: `pel-insert-minor-mode-activation-info'
;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-rust-mode ()
  "Major mode dispatcher for editing Rust source text.
Uses `rust-mode' or `rust-ts-mode' depending on what is available
and required by `pel-use-rust'."
  (interactive)
  (cond
   ;; When `pel-use-rust` is t, PEL has downloaded and installed rust-mode.el that
   ;; provides the `rust-mode'.  Use that.
   ((eq pel-use-rust t)
    (when (fboundp 'rust-mode)
      (rust-mode)))

   ;; The `rust-ts-mode' is built-in Emacs
   ((eq pel-use-rust 'with-tree-sitter)
    (if (and (pel-treesit-ready-p 'rust)
             (require 'rust-ts-mode nil :noerror)
             (fboundp 'rust-ts-mode))
        (rust-ts-mode)
      (display-warning 'pel-rust-with-tree-sitter
                       (format "Can't use rust-ts-mode: %s"
                               (if (pel-treesit-ready-p 'rust)
                                   "error loading rust-ts-mode"
                                 "no grammar for rust")))

      (if (fboundp 'rust-mode)
          (rust-mode)
        (user-error
         "Can't use `rust-ts-mode' nor `rust-mode': check installation!"))))))

;;-pel-autoload`
(defun pel--rust-ts-mode-fixer ()
  "Remove `rust-ts-mode' entries from `auto-mode-alist'.
It removes what entered when `rust-ts-mode' loads."
  (setq auto-mode-alist
        (rassq-delete-all 'rust-ts-mode auto-mode-alist)))

;;-pel-autoload
(defun pel-rust-mode-used-text (use-rust)
  "Return a description of what USE-RUST (`pel-use-rust') specifies for major mode."
  (cond
   ((eq use-rust t)
    "use rust-mode from rust-mode.el.")
   ((eq use-rust 'with-tree-sitter)
    "use rust-ts-mode tree-sitter aware mode.")
   (t "Invalid! Use t or with-tree-sitter")))

;;-pel-autoload
(defun pel-rust-insert-indent-info ()
  "Insert Rust indentation setup info in current context.
Return a list of generic symbols described."
  (insert "
- Under PEL, Rust indentation level width is controlled entirely by the
  value of the `rust-indent-offset' user-option:
  PEL stores its value inside the variables used by the rust-mode and
  rust-ts-mode to ensure consistency.

  If you want to use hard tabs for indentation, you should set
  `tab-width' to the same value of `pel-rust-indent-width' and then you
  can control the visual rendering of indentation by changing the values
  of those two user-options: the content of the buffer and file does
  wont change but the indentation rendering will.

  Note, however, that other editors may not be able to do the same; the use of
  hard tabs in Rust source code is not required as it is for Go, therefore
  this technique may not as well-spread as it is for Go.
")
  (pel-insert-symbol-content-line 'rust-indent-offset)
  (pel-insert-symbol-content-line 'rust-ts-mode-indent-offset)
  ;; Return the list of generic symbols described here.
  '(indent-description-intro))

;;-pel-autoload
(defun pel-rust-insert-tab-info ()
  "Insert Rust hard tab setup info in current context.
Return a list of generic symbols described."
  (insert "
- The hard tab rendering width is for rust buffer is controlled by
  `pel-rust-tab-width' and stored into `tab-width'.
  These do not control the indentation, just the visual width (in columns)
  that Emacs uses to render a hard tab character.
")
  (pel-insert-symbol-content-line 'pel-rust-tab-width)
  (pel-insert-symbol-content-line 'tab-width)
  (pel-insert-symbol-content-line 'pel-rust-use-tabs
                                  nil #'pel-on-off-string)
  (pel-insert-symbol-content-line 'indent-tabs-mode
                                  nil #'pel-on-off-string)
  ;; Return the list of generic symbols described here.
  '(tab-description-intro
    pel-MM-tab-width
    tab-width
    pel-MM-use-tabs
    indent-tabs-mode))

(defun pel--rust-minor-mode-info ()
  "Insert information related to Rust minor modes."
  (insert "
Automatic activation of minor mode is also controlled by the
following user-options:")
  (pel-insert-list-content 'pel-rust-activates-minor-modes
                           nil nil nil :1line))
;;-pel-autoload
(defun pel-rust-setup-info (&optional append)
  "Display Rust setup information."
  (interactive "P")
  (pel-major-mode-must-be '(rust-mode rust-ts-mode))
  (let ((pel-insert-symbol-content-context-buffer (current-buffer))
        (current-major-mode major-mode)
        (indent-control-context (pel-indent-control-context))
        (tab-control-context (pel-tab-control-context)))
    (pel-print-in-buffer
     "*pel-rust-info*"
     "PEL setup for Rust programming language"
     (lambda ()
       "Print Rust setup info."
       ;;
       (insert (propertize "* Major Mode Control:" 'face 'bold))
       (pel-insert-symbol-content 'major-mode nil :on-same-line :no-button
                                  "major mode currently used")
       (when pel-use-tree-sitter
         (insert (format "\n- %s" (pel-ts-language-grammar-status-for
                                   'rust "\n- "))))
       (pel-insert-symbol-content-line 'pel-use-rust nil
                                       (function pel-rust-mode-used-text))
       (insert "\n\n")
       ;;
       (insert (propertize "* Rust Packages:" 'face 'bold))
       (dolist (s '(pel-use-rust-mode
                    pel-use-rustic
                    pel-use-cargo
                    pel-use-emacs-racer
                    pel-use-flycheck-rust))
         (pel-insert-symbol-content-line s))
       (insert "\n\n")
       ;; --
       (pel-insert-minor-mode-activation-info current-major-mode
                                              #'pel--rust-minor-mode-info)
       (insert "\n\n")
       (pel-indent-insert-control-info indent-control-context)
       (pel-tab-insert-control-info tab-control-context))
     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-rust)

;;; pel-rust.el ends here
