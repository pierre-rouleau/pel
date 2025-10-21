;;; pel-treesit.el --- PEL Tree-Sitter support extensions.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, October  7 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-21 11:29:10 EDT, updated by Pierre Rouleau>

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

;; [:todo 2025-10-19, by Pierre Rouleau: reduce size of the following code.]
;; pel-autoload
(defun pel-treesit-toggle-mode ()
  "Toggle the major mode between classic mode and tree-sitter based mode.
Signals a user-error if the other mode is not available."
  (interactive)
  (let ((current-mode-name (symbol-name major-mode)))
    (if (string-match "-ts-mode" current-mode-name)
        ;; currently using a tree-sitter mode.
        (let* ((classic-mode-name (format "%s-mode"
                                          (substring current-mode-name 0 -8)))
               (classic-mode (intern classic-mode-name)))
          (if (fboundp classic-mode)
              (progn
                (call-interactively classic-mode)
                (message "Switched to classic-mode: %s" classic-mode-name))
            (let ((alternate-mode (alist-get classic-mode
                                             pel-treesit-mode-assoc-alist)))
              (if (and alternate-mode
                       (fboundp alternate-mode))
                  (progn
                    (call-interactively alternate-mode)
                    (message "Switched to Tree-Sitter based mode: %s"
                             (symbol-name alternate-mode)))
                (user-error "\
Classic major mode `%s' is not loaded! Does it exist? Is it installed?"
                            classic-mode-name)))))
      ;; currently using a classic mode
      (let* ((ts-mode-name (format "%s-ts-mode"
                                   (substring current-mode-name 0 -5)))
             (ts-mode (intern ts-mode-name)))
        (if (fboundp ts-mode)
            (progn
              (call-interactively ts-mode)
              (message "Switched to Tree-Sitter based mode: %s" ts-mode-name))
          (let ((alternate-ts-mode (alist-get ts-mode
                                              pel-treesit-mode-assoc-alist)))
            (if (and alternate-ts-mode
                     (fboundp alternate-ts-mode))
                (progn
                  (call-interactively alternate-ts-mode)
                  (message "Switched to Tree-Sitter based mode: %s"
                           (symbol-name alternate-ts-mode)))
              (user-error "\
Tree-Sitter based major mode `%s' is not loaded! Does it exist? Is it installed?"
                          ts-mode-name))))))))

;;; --------------------------------------------------------------------------
(provide 'pel-treesit)

;;; pel-treesit.el ends here
