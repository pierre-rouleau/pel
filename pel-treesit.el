;;; pel-treesit.el --- PEL Tree-Sitter support extensions.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, October  7 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-07 17:19:53 EDT, updated by Pierre Rouleau>

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
(require 'pel--keys-macros)              ; use: `pel-help-open-pdf'
;;                                       ;      `pel--customize-group'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; pel-autoload
(defun pel-treesit-help (&optional open-github-page-p)
  "Open the local PEL Tree-Sitter PDF.
If OPEN-GITHUB-PAGE-P is non nil, open the corresponding GitHub web page
instead."
  (interactive "P")
  (pel-help-open-pdf "treesit" open-github-page-p))

;; pel-autoload
(defun pel-treesit-customize (&optional other-window)
  "Open the treesit customize group in current or OTHER_WINDOW."
  (interactive "P")
  (pel--customize-group 'pel-pkg-for-tree-sitter other-window))

;; pel-autoload
(defun pel-treesit-emacs-customize (&optional other-window)
  "Open the treesit customize group in current or OTHER_WINDOW."
  (interactive "P")
  (pel--customize-group 'treesit other-window))


;;; --------------------------------------------------------------------------
(provide 'pel-treesit)

;;; pel-treesit.el ends here
