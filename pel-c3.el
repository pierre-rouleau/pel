;;; pel-c3.el --- Support for the C3 Programming language.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, December 23 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-12-29 17:32:01 EST, updated by Pierre Rouleau>

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
(require 'pel--base)         ; use: `pel-insert-list-content',
;;                           ;      `pel-insert-symbol-content-context-buffer'
(require 'pel-modes)         ; use: `pel-insert-minor-mode-activation-info'
(require 'pel-prompt)        ; use: `pel-select-from'
(require 'cl-macs)           ; use: `cl-case'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defvar-local pel-c3-comment-style 'slash)

(defun pel-set-c3-comment-style (c3-comment-style)
  "Set C3 comment style in buffer to C3-COMMENT-STYLE."
  (cl-case c3-comment-style
    (slash
     (setq-local comment-start "// ")
     (setq-local comment-end   "")
     (setq-local pel-c3-comment-style c3-comment-style))
    (star
     (setq-local comment-start "/* ")
     (setq-local comment-end   " */")
     (setq-local pel-c3-comment-style c3-comment-style))
    (docstring
     (setq-local comment-start "<* ")
     (setq-local comment-end   " *>")
     (setq-local pel-c3-comment-style c3-comment-style))
    (t
     (error "Invalid c3-comment-style: %s" c3-comment-style)))
  (message "Now commenting with: %s %s" comment-start comment-end))

;;-pel-autoload
(defun pel-select-c3-comment-style ()
  "Select C3 comment style.
Select from the following C3 comment styles:
- //
- /* */
- <* *>"
  (interactive)
  (let ((selection '((?/ "//" slash)
                     (?* "/* */" star)
                     (?< "<* *>" docstring))))
    (pel-select-from "C3 comment style"
                     selection
                     pel-c3-comment-style
                     #'pel-set-c3-comment-style
                     nil)))

;;-pel-autoload
(defun pel-c3-indent-tab-info (&optional append)
  "Display C3 Indentation and hard tab control information."
  (interactive "P")
  (pel-major-mode-must-be 'c3-ts-mode)
  (let ((indent-control-context (pel-indent-control-context))
        (tab-control-context (pel-tab-control-context)))
    (pel-print-in-buffer
     "*pel-indent-info*"
     "Indentation Width Control and Space/Tab Insertion Rendering"
     (lambda ()
       (pel-indent-insert-control-info indent-control-context)
       (pel-tab-insert-control-info tab-control-context))
     (unless append :clear-buffer)
     :use-help-mode)))


(defun pel--c3-minor-mode-info ()
  "Insert information related to C3 minor modes."
  (insert "
Automatic activation of minor mode is also controlled by the
following user-options:")
  (pel-insert-list-content 'pel-c3-activates-minor-modes
                           nil nil nil :1line))

;;-pel-autoload
(defun pel-c3-setup-info (&optional append)
  "Display C3 setup information."
  (interactive "P")
  (pel-major-mode-must-be 'c3-ts-mode)
  (let ((pel-insert-symbol-content-context-buffer (current-buffer))
        (current-major-mode major-mode)
        (active-modes (pel-active-minor-modes))
        (indent-control-context (pel-indent-control-context))
        (tab-control-context (pel-tab-control-context)))
    (pel-print-in-buffer
     "*pel-c3-info*"
     "PEL setup for C3 programming language"
     (lambda ()
       "Print C3 setup"
       (insert (propertize "* Major Mode Control:" 'face 'bold))
       (insert "
- Note: C3 is currently only supported by a Tree-Sitter aware mode.")
       (pel-insert-symbol-content 'major-mode nil :on-same-line nil
                                  "major mode currently used")
       (when pel-use-tree-sitter
         (insert (format "\n- %s" (pel-ts-language-grammar-status-for
                                   'c3 "\n- "))))
       ;; -- List of minor modes
       (pel-insert-list-of-minor-modes active-modes)
       (insert "\n\n")
       (pel-insert-minor-mode-activation-info current-major-mode
                                              #'pel--c3-minor-mode-info)
       ;; --
       (insert "\n\n")
       (pel-indent-insert-control-info indent-control-context)
       (pel-tab-insert-control-info tab-control-context))
     (unless append :clear-buffer)
     :use-help-mode)))

;; ---------------------------------------------------------------------------
;;* C3 Compilation
;;  --------------

;;-pel-autoload
(defun pel-c3-compile ()
  "Static check current C3 file, show errors in `compilation-mode' buffer."
  (interactive)
  ;; Save modified buffer
  (when (and (buffer-modified-p)
           (buffer-file-name))
    (save-buffer)
    (message "Saving: %s" (buffer-name)))
  ;; Compile with compilation-shell-minor-mode to handle the ANSI escape
  ;; sequences generated by the compiler.
  (compile (format "c3c compile %s"
                   (shell-quote-argument (buffer-file-name)))
           t))

;;; --------------------------------------------------------------------------
  (provide 'pel-c3)

;;; pel-c3.el ends here
