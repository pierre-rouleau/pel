;;; pel-ada.el --- PEL Ada support extension.  -*- lexical-binding: t; -*-

;; Created   : Friday, October 17 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-26 08:33:16 EDT, updated by Pierre Rouleau>

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
(defun pel-ada-mode ()
  "Major mode dispatcher for editing Ada source text.
Uses `ada-mode' or `ada-ts-mode' depending on what is available
and required by `pel-use-ada'."
  (interactive)
  (cond
   ;; When `pel-use-ada` is t, PEL has downloaded and installed ada-mode.el
   ;; that provides the `ada-mode'.  Use that.
   ((eq pel-use-ada t)
    (when (fboundp 'ada-mode)
      (ada-mode)))

   ;; The `ada-ts-mode' is not built-in Emacs
   ((eq pel-use-ada 'with-tree-sitter)
    (if (and (pel-treesit-ready-p 'ada)
             (require 'ada-ts-mode nil :noerror)
             (fboundp 'ada-ts-mode))
        (ada-ts-mode)
      (display-warning 'pel-ada-with-tree-sitter
                       (format "Can't use ada-ts-mode: %s"
                               (if (pel-treesit-ready-p 'ada)
                                   "error loading ada-ts-mode"
                                 "no grammar for ada")))
      (if (fboundp 'ada-mode)
          (ada-mode)
        (user-error
         "Can't use `ada-ts-mode' nor `ada-mode': check installation!"))))))

;;-pel-autoload
(defun pel--ada-ts-mode-fixer ()
  "Remove `ada-ts-mode' entries from `auto-mode-alist'.
It removes what entered when `ada-ts-mode' loads."
  ;; There are several file extensions for Ada and the ada-ts-mode
  ;; adds several entries (entries for .ada, .zon).
  ;; Delete them all from auto-mode-alist.
  (setq auto-mode-alist
        (rassq-delete-all 'ada-ts-mode auto-mode-alist)))

;; ---------------------------------------------------------------------------
;;-pel-autoload
(defun pel-ada-mode-used-text (use-ada)
  "Description of what USE-ADA specifies for major mode.
USE-ADA should be set to `pel-use-ada' value used in current buffer."
  (cond
   ((eq use-ada t)
    "use ada-mode from ada-mode.el.")
   ((eq use-ada 'with-tree-sitter)
    "use ada-ts-mode tree-sitter aware mode.")
   (t "Invalid! Use t or with-tree-sitter")))


;;-pel-autoload
(defun pel-ada-insert-indent-info ()
  "Insert Ada indentation setup used in current context.
Return a list of generic symbols described."
  (insert "
- Ada indentation under `ada-mode' is controlled by a LSP back-end,
  and under `ada-ts-mode' it is either controlled by a LSP back-end or
  by the `ada-ts-mode' tree-sitter grammar control with specific concepts
  controlled by 9 customizable user-options listed below.
  Using this last method is the most flexible, works very well and is the
  recommended way;  depending on a LSP back-end means that the indentation is
  essentially done by code reformatting and you may have less control over it.
")
  (pel-insert-symbol-content-line 'ada-ts-mode-indent-backend)
  (pel-insert-symbol-content-line 'ada-ts-mode-indent-strategy)
  (pel-insert-symbol-content-line 'ada-ts-mode-indent-offset)
  (pel-insert-symbol-content-line 'ada-ts-mode-indent-when-offset)
  (pel-insert-symbol-content-line 'ada-ts-mode-indent-broken-offset)
  (pel-insert-symbol-content-line 'ada-ts-mode-indent-exp-item-offset)
  (pel-insert-symbol-content-line 'ada-ts-mode-indent-subprogram-is-offset)
  (pel-insert-symbol-content-line 'ada-ts-mode-indent-record-offset)
  (pel-insert-symbol-content-line 'ada-ts-mode-indent-label-offset)
  ;; Return the list of generic symbols described here.
  '(indent-description-info))

;;-pel-autoload
(defun pel-ada-insert-tab-info ()
  "Insert Ada hard tab setup used in current context.
Return a list of generic symbols described."
  (insert "
- The hard tab rendering width is for ada buffer is controlled by
  `pel-ada-tab-width' and stored into `tab-width'.
  These do not control the indentation, just the visual width (in
  columns) that Emacs uses to render a hard tab character.
  Whether hard tabs are used in ada buffer is controlled by
  `pel-ada-use-tabs' and stored inside `indent-tabs-mode'.
")
  (pel-insert-symbol-content-line 'pel-ada-tab-width)
  (pel-insert-symbol-content-line 'pel-ada-use-tabs
                                  nil #'pel-on-off-string)
    ;; Return the list of generic symbols described here.
  '(tab-description-intro
    pel-MM-tab-width
    pel-MM-use-tabs))

(defun pel--ada-minor-mode-info ()
  "Insert information related to Ada minor modes."
  (insert "
Automatic activation of minor mode is also controlled by the
following user-options:")
  (pel-insert-list-content 'pel-ada-activates-minor-modes
                           nil nil nil :1line))

;;-pel-autoload
(defun pel-ada-setup-info (&optional append)
  "Display Ada setup information."
  (interactive "P")
  (pel-major-mode-must-be '(ada-mode ada-ts-mode))
  (let ((pel-insert-symbol-content-context-buffer (current-buffer))
        (current-major-mode major-mode)
        (indent-control-context (pel-indent-control-context))
        (tab-control-context (pel-tab-control-context)))
    (pel-print-in-buffer
     "*pel-ada-info*"
     "PEL setup for Ada programming language"
     (lambda ()
       "Print Ada setup info."
       (insert (propertize "* Major Mode Control:" 'face 'bold))
       (pel-insert-symbol-content 'major-mode nil :on-same-line :no-button
                                  "major mode currently used")
       (when pel-use-tree-sitter
         (insert (format "\n- %s" (pel-ts-language-grammar-status-for
                                   'ada "\n- "))))
       (pel-insert-symbol-content-line 'pel-use-ada nil
                                       (function pel-ada-mode-used-text))
       (when pel-use-tree-sitter
         (insert "\n  The quality of `ada-ts-mode' is high; using it is recommended!")
         (unless (eq pel-use-ada 'with-tree-sitter)
           (insert "\n Unless you have a specific reason to not use it, you should.")))
       (insert "\n\n")
       ;; --
       (pel-insert-minor-mode-activation-info current-major-mode
                                              #'pel--ada-minor-mode-info)
       (insert "\n\n")
       (pel-indent-insert-control-info indent-control-context)
       (pel-tab-insert-control-info tab-control-context))
     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-ada)

;;; pel-ada.el ends here
