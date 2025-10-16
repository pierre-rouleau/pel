;;; pel-rust.el --- Rust programming language support.  -*- lexical-binding: t; -*-

;; Created   : Sunday, October 12 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-16 08:32:11 EDT, updated by Pierre Rouleau>

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
(defun pel-rust-setup-info (&optional append)
  "Display Rust setup information."
  (interactive "P")
  (pel-major-mode-must-be '(rust-mode rust-ts-mode))
  (let ((pel-insert-symbol-content-context-buffer (current-buffer)))
    (pel-print-in-buffer
     "*pel-rust-info*"
     "PEL setup for Rust programming language"
     (lambda ()
       "Print Rust setup info."
       ;;
       (insert (propertize "* Major Mode Control:" 'face 'bold))
       (pel-insert-symbol-content-line 'major-mode nil "major mode currently used.")
       (when pel-use-tree-sitter
         (insert (format "\n- %s" (pel-ts-language-grammar-status-for 'rust))))
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
       ;;
       (insert (propertize "* Indentation Control:" 'face 'bold))
       (pel-insert-symbol-content-line 'rust-indent-offset)
       (pel-insert-symbol-content-line 'tab-width))
     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-rust)

;;; pel-rust.el ends here
