;;; pel-go.el --- Go programming language support.  -*- lexical-binding: t; -*-

;; Created   : Friday, January 29 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-11 15:17:45 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2025  Pierre Rouleau
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
(require 'pel--base)                    ; use: pel-set-tab-width
(require 'pel--options)                 ; use: pel-go-run-gofmt-on-buffer-save
;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-go-mode ()
  "Major mode dispatcher for editing Go source text.
Uses `go-mode' or `go-ts-mode' depending on what is available
and required by `pel-use-go'."
  (cond
   ;; When `pel-use-go` is t, PEL has downloaded and installed `go-mode'
   ((eq pel-use-go t)
    (when (fboundp 'go-mode)
      (go-mode)))

   ;; The `go-ts-mode' is built-in Emacs
   ((eq pel-use-go 'with-tree-sitter)
    (require 'go-ts-mode)
    (when (fboundp 'go-ts-mode)
      (go-ts-mode)))))

;;-pel-autoload
(defun pel--go-ts-mode-fixer ()
  "Remove `go-ts-mode' entries from `auto-mode-alist'.
It removes what entered when `go-ts-mode' loads."
  (setq auto-mode-alist
        (delete '("\\.go\\'" . go-ts-mode) auto-mode-alist)))
;; --

;; (defvar pel-go-run-gofmt-on-buffer-save pel-go-run-gofmt-on-buffer-save
;;   "Modifiable setting of the corresponding user-option.")

;;-pel-autoload
(defun pel-go-gofmt-on-buffer-save ()
  "Hook call - run gofmt on buffer save if required.

The user-option variable `pel-go-run-gofmt-on-buffer-save'
determines if gofmt is ran.

Does not force the loading of go support when another type of file is saved."
  (when (and pel-go-run-gofmt-on-buffer-save
             (eq major-mode 'go-mode)
             (fboundp 'gofmt))
    (gofmt)))

;;-pel-autoload
(defun pel-go-toggle-gofmt-on-buffer-save (&optional globally)
  "Toggle automatic run of gofmt when saving Go files.
By default change behaviour for local buffer only.
When GLOBALLY argument is non-nil, change it for all Go buffers for the current
Emacs editing session (the change does not persist across Emacs sessions).
To modify the global state permanently modify the customized value of the
`pel-go-toggle-gofmt-on-buffer-save' user option via the `pel-pkg-for-go'
group customize buffer."
  (interactive "P")
  (pel-toggle-and-show-user-option 'pel-go-run-gofmt-on-buffer-save globally))

;; --

;;-pel-autoload
(defun pel-go-setup-info ()
  "Display Go setup information."
  (interactive)
  (message "\
Tab-width for this buffer: %d
Runs gofmt on buffer save: %s"
           tab-width
           (pel-symbol-on-off-string 'pel-go-run-gofmt-on-buffer-save
                                     "yes" "no")))

;; --

;;-pel-autoload
(defun pel-go-toggle-syntax-checker ()
  "Toggle the syntax checker mode on/off.
The syntax checker activated or deactivated is either flycheck
or flymake, as selected by the user-option variable
`pel-use-goflymake'."
  (interactive)
  (pel-toggle-syntax-check-mode 'pel-use-goflymake))

;;; --------------------------------------------------------------------------
(provide 'pel-go)

;;; pel-go.el ends here
