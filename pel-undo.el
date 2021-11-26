;;; pel-undo.el --- PEL undo/redo management  -*-lexical-binding: t; -*-

;; Copyright (C) 2020, 2021  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

;; This file is part of the PEL package
;; This file is not part of GNU Emacs.

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

;; -----------------------------------------------------------------------------
;;; Commentary:
;;
;; This provides undo and refun functions that are aware of the undo-tree-mode
;; and behave like their undo-tree-mode equival;ent when the undo-tree-mode is
;; active or behave like the standard Emacs undo when undo-tree-mode is not
;; used.
;;
;; Having these functions simplify the management of undo-tree-mode when we need
;; to change the undo and redo key bindings.
;;
;; Note that we load undo-tree right away.  That's done because pel-undo is only
;; loaded when pel-use-undo-tree is t so when the undo-tree package is required.
;; However since we don't want to cause error if it is not present, the require
;; statement is written to not cause error and the functions are used only if
;; it is available.

(require 'pel--options)
(require 'undo-tree nil :noerror)
;;; Code

;;-pel-autoload
(defun pel-undo (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric ARG serves as a repeat count.

Uses function `undo-tree-undo' when both variable `pel-use-undo-tree'
and variable `undo-tree-mode' are t,
otherwise use the standard Emacs function `undo'.

In Transient Mark mode when the mark is active, only undo changes
within the current region. Similarly, when not in Transient Mark
mode, just C-u as an argument limits undo to
changes within the current region."
  (interactive "*P")
  (if (and pel-use-undo-tree
           (boundp 'undo-tree-mode)
           undo-tree-mode
           (fboundp 'undo-tree-undo))
      (undo-tree-undo arg)
    (undo arg)))

;;-pel-autoload
(defun pel-redo (&optional arg)
  "Redo some previously undone changes.
Repeat this command to redo more changes.
A numeric ARG serves as a repeat count.

Uses function `undo-tree-redo' when both variable `pel-use-undo-tree'
and variable `undo-tree-mode' are t,
otherwise use the standard Emacs function `undo'.

In Transient Mark mode when the mark is active, only redo changes
within the current region. Similarly, when not in Transient Mark
mode, just C-u as an argument limits redo to
changes within the current region."
  (interactive "*P")
  (if (and pel-use-undo-tree
           (boundp 'undo-tree-mode)
           undo-tree-mode
           (fboundp 'undo-tree-redo))
      (undo-tree-redo arg)
    (undo arg)))

;; -----------------------------------------------------------------------------
(provide 'pel-undo)

;;; pel-undo.el ends here
