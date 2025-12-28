;;; pel-treesit.el --- PEL Tree-Sitter support extensions.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, October  7 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-12-28 16:04:22 EST, updated by Pierre Rouleau>

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
;;  The file hold PEL F12 help commands:
;;
;;  * `pel-treesit-help'
;;  * `pel-treesit-customize'
;;  * `pel-treesit-emacs-customize'

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

;;  The last group of functions implement a quick check on the validity of the
;;  Tree-Sitter Language Grammar directory setup:
;;
;;  * `pel-treesit-check-setup'
;;    - `pel--check-dpath'
;;      - `pel-check-dirpath'
;;      - `pel--check-symlink'
;;    - `pel--check-fname'
;;      - `pel--treesit-format'
;;        -d `pel--treesit-path-lenght'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: `pel-os-lib-file-extension'
(require 'pel--keys-macros)             ; use: `pel-help-open-pdf'
;;                                      ;      `pel--customize-group'
(require 'pel-prompt)                   ; use: `pel-select-symbol-from'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;* PEL F12 Commands
;;  ----------------

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
  (pel--customize-group
   (pel-select-symbol-from "Select group" '(treesit
                                            combobulate))
   other-window))

(defconst pel-treesit-mode-assoc-alist
  '((go-dot-mod-ts-mode . go-mod-ts-mode)
    (go-mod-mode . go-dot-mod-mode))
  "Map expected mode name with non-conventional implemented mode name.")


;; ---------------------------------------------------------------------------
;;* Toggle between Classic and Tree-Sitter Mode
;;  -------------------------------------------

(defun pel--mode-for (conventional-mode)
  "Return the real mode symbol for CONVENTIONAL-MODE.
- If CONVENTIONAL-MODE is bound, return that,
- otherwise if an alternative is identified inside
  `pel-treesit-mode-assoc-alist', return that,
- otherwise, nothing is bound and return nil."
  (if (fboundp conventional-mode)
      conventional-mode
    (let ((alternate-mode (alist-get conventional-mode
                                     pel-treesit-mode-assoc-alist)))
      (when (fboundp alternate-mode)
        alternate-mode))))

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

;; ---------------------------------------------------------------------------
;;* Check Tree-Sitter Setup
;;  -----------------------

(defvar pel--treesit-path-lenght 60
  "Minimum width of the file paths printed.")

(defun pel--treesit-format (format-str path)
  "Return adjusted FORMAT-STR to hold PATH.

Replace \"PATH\" with a `format' width specifier that is wide enough for the
largest of all PATH seen in the Emacs session."
  (when (> (length path) pel--treesit-path-lenght)
    (setq pel--treesit-path-lenght (length path)))
  (replace-regexp-in-string "PATH" (format "%%-%ds" pel--treesit-path-lenght)
                            format-str :fixedcase :literal))

(defun pel--check-symlink (path &optional parent)
  "Check validity of  symbolic link PATH.
The optional PARENT identifies the parent directory.
Issue a warning if there are any problem.
Return 1 if error, 0 if OK."
  (if (file-directory-p path)
      0
    (display-warning 'tree-sitter-setup
                     (format (pel--treesit-format "%sPATH ->-- x --> %s" path)
                             (if parent
                                 (format "Inside %s: " parent)
                               "")
                             path (file-symlink-p path)))
    1))

(defun pel--check-fname (fpath &optional parent)
  "Check validity of FPATH file path.
The optional PARENT identifies the parent directory.
Issue a warning if there are any problem.
Return 1 if error, 0 if OK."
  (if (file-exists-p fpath)
      0
    (if (file-symlink-p fpath)
        (display-warning 'tree-sitter-setup
                         (format (pel--treesit-format "%sPATH ->-- x --> %s" fpath)
                                 (if parent
                                     (format "Inside %s: " parent)
                                   "")
                                 fpath
                                 (file-symlink-p fpath)))
      (display-warning 'tree-sitter-setup
                       (format "%s%s: does not exists!"
                               (if parent
                                   (format "Inside %s: " parent)
                                 "")
                               fpath)))
    1))

(defun pel-check-dirpath (dpath &optional parent)
  "Check validity of directory DPATH.
The optional PARENT identifies the parent directory.
Issue a warning if there are any problem.
Return 1 if error, 0 if OK."
  (if (file-directory-p dpath)
      0
    (display-warning 'tree-sitter-setup
                     (format "%s%s: does not exists!"
                             (if parent
                                 (format "Inside %s: " parent)
                               "")
                             dpath))
    1))

(defun pel--check-dpath (dpath &optional parent)
  "Check the validity of the directory path DPATH.
The optional PARENT identifies the parent directory.
Issue a warning if there are any problems.
Return 1 if error, 0 if OK."
  (if (file-directory-p dpath)
      (if (file-symlink-p dpath)
          (pel--check-symlink dpath parent)
        (pel-check-dirpath dpath parent))
    (if (file-symlink-p dpath)
        (pel--check-symlink dpath parent)
      (pel-check-dirpath dpath parent))))

(defun pel-treesit-check-setup ()
  "Check the Emacs Tree-Sitter environment and report problems."
  (interactive)
  (if (boundp 'treesit-extra-load-path)
      (let ((err-count 0))
        (dolist (dpath (append treesit-extra-load-path
                               (list (concat user-emacs-directory "tree-sitter"))))
          (setq err-count (+ err-count ))
          (if (eq (pel--check-dpath dpath) 0)
              ;; valid dpath: check it's content.
              (dolist (fname (directory-files dpath :full-names
                                              (format "\\.%s\\'" pel-os-lib-file-extension)))
                (setq err-count (+ err-count (pel--check-fname fname))))
            ;; invalid dpath
            (setq err-count (1+ err-count ))))
        (if (eq err-count 0)
            (message "Tree-Sitter directory settings appears OK")
          (message "Detected %d errors in Tree-Sitter directory settings!"
                   err-count)))
    (user-error "This Emacs does not support Tree-Sitter")))

;;; --------------------------------------------------------------------------
(provide 'pel-treesit)

;;; pel-treesit.el ends here
