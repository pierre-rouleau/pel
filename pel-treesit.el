;;; pel-treesit.el --- PEL Tree-Sitter support extensions.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, October  7 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-11-20 11:20:19 EST, updated by Pierre Rouleau>

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
;;  PEL support for Tree-Sitter and the ability to dynamically toggle the
;;  major mode from a classic major mode to a Tree-Sitter based major mode,
;;  something Emacs is not normally doing.
;;
;;  The `pel-treesit-toggle-mode' toggles the classic and tree-sitter major
;;  modes.  For this to work extra work is performed by PEL for the modes that
;;  are supported.
;;
;;  * `pel-treesit-toggle-mode'
;;    - `pel--mode-for'
;;      -d: `pel-treesit-mode-assoc-alist'
;;
;;
;;  The file also hold PEL F12 help commands:
;;
;;  * `pel-treesit-help'
;;  * `pel-treesit-customize'
;;  * `pel-treesit-emacs-customize'
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)
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

(defconst pel-treesit-mode-assoc-alist
  '((go-dot-mod-ts-mode . go-mod-ts-mode)
    (go-mod-mode . go-dot-mod-mode))
  "Map expected mode name with non-conventional implemented mode name.")


;; --
(defun pel--mode-for (conventional-mode)
  "Return the real mode symbol for CONVENTIONAL-MODE.
- If CONVENTIONAL-MODE is bound, return that,
- otherwise if an alternative is identified inside
  `pel-treesit-mode-assoc-alist', return that,
- otherwise, nothing is bound and return nil."
  (if (fboundp conventional-mode)
      conventional-mode
    (if-let (alternate-mode (alist-get conventional-mode
                                       pel-treesit-mode-assoc-alist))
        (when (fboundp alternate-mode)
          alternate-mode)
      nil)))

;; pel-autoload
(defun pel-treesit-toggle-mode ()
  "Toggle the major mode between classic mode and tree-sitter based mode.
Signals a user-error if the other mode is not available."
  (interactive)
  (let ((current-mode-str (symbol-name major-mode))
        (current-mode-name (pel-file-type-for major-mode)))
    (if (string-match "-ts-mode" current-mode-str)
        ;; currently using a tree-sitter mode.
        (let* ((classic-mode-str (format "%s-mode" current-mode-name))
               (classic-mode (pel--mode-for (intern classic-mode-str))))
          (if classic-mode
              (progn
                (call-interactively classic-mode)
                (message "Switched to classic-mode: %s" classic-mode-str))
            (user-error "Classic major mode `%s' is not loaded! \
Does it exist? Is it installed?"
                        classic-mode-str)))
      ;;
      ;; currently using a classic mode
      (let* ((ts-mode-str (format "%s-ts-mode" current-mode-name))
             (ts-mode (pel--mode-for (intern ts-mode-str))))
        (if ts-mode
            (progn
              (call-interactively ts-mode)
              (message "Switched to Tree-Sitter based mode: %s" ts-mode-str))
          (user-error "Tree-Sitter based major mode `%s' is not loaded! \
Does it exist? Is it installed?"
                      ts-mode-str))))))

;;; --------------------------------------------------------------------------
(provide 'pel-treesit)

;;; pel-treesit.el ends here
