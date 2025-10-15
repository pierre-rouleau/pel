;;; pel-go.el --- Go programming language support.  -*- lexical-binding: t; -*-

;; Created   : Friday, January 29 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-15 15:32:20 EDT, updated by Pierre Rouleau>

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
(require 'pel--base)        ; use: `pel-set-tab-width', `pel-treesit-ready-p'
(require 'pel--options)     ; use:

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-go-mode ()
  "Major mode dispatcher for editing Go source text.
Uses `go-mode' or `go-ts-mode' depending on what is available
and required by `pel-use-go'."
  (cond
   ;; When `pel-use-go` is t, PEL has downloaded and installed go-mode.el that
   ;; provides the `go-mode'.  Use that.
   ((eq pel-use-go t)
    (when (fboundp 'go-mode)
      (go-mode)))

   ;; The `go-ts-mode' is built-in Emacs
   ((eq pel-use-go 'with-tree-sitter)
    (if (and (pel-treesit-ready-p 'go)
             (require 'go-ts-mode nil :noerror)
             (fboundp 'go-ts-mode))
        (go-ts-mode)
      (display-warning 'pel-go-with-tree-sitter
                       (format "Can't use go-ts-mode: %s"
                               (if (pel-treesit-ready-p 'go)
                                   "error loading go-ts-mode"
                                 "no grammar for go")))

      (if (fboundp 'go-mode)
          (go-mode)
        (user-error
         "Can't use `go-ts-mode' nor `go-mode': check installation!"))))))

;;-pel-autoload
(defun pel-go-dot-mod-mode ()
  "Major mode dispatcher for editing Go source text.
Uses `go-dot-mod-mode' or `go-mod-ts-mode' depending on what is available
and required by `pel-use-go'."
  (cond
   ;; When `pel-use-go` is t, PEL has downloaded and installed go-mode.el
   ;; which provides the `go-dot-mod-mode'.  Use that.
   ((eq pel-use-go t)
    (when (fboundp 'go-dot-mod-mode)
      (go-dot-mod-mode)))

   ;; The `go-dot-ts-mode' is provided by the go-ts-mode.el which is
   ;; built-in Emacs but the grammar must also be present.
   ((eq pel-use-go 'with-tree-sitter)
    (if (and (pel-treesit-ready-p 'go)
             (require 'go-ts-mode nil :noerror)
             (fboundp 'go-mod-ts-mode))
        (go-mod-ts-mode)
      (display-warning 'pel-go-with-tree-sitter
                       (format "Can't use go-ts-mode: %s"
                               (if (pel-treesit-ready-p 'go)
                                   "error loading go-ts-mode"
                                 "no grammar for go")))
      (if (fboundp 'go-dot-mod-mode)
          (go-dot-mod-mode)
        (user-error
         "Can't use `go-dot-ts-mode' nor `go-dot-mod-mode':\
 check installation!"))))))


;;-pel-autoload`
(defun pel--go-ts-mode-fixer ()
  "Remove `go-ts-mode' entries from `auto-mode-alist'.
It removes what entered when `go-ts-mode' loads."
  (setq auto-mode-alist
        (rassq-delete-all 'go-ts-mode auto-mode-alist))
  (setq auto-mode-alist
        (rassq-delete-all 'go-mod-ts-mode auto-mode-alist)))

;; --

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
(defun pel-go-mode-used-text (use-go)
  "Return a description of what USE-GO (`pel-use-go') specifies for major mode."
  (cond
   ((eq use-go t)
    "use go-mode and go-dot-mod-mode from go-mode.el.")
   ((eq use-go 'with-tree-sitter)
    "use go-ts-mode and go-mod-ts-mode tree-sitter aware modes.")
   (t "Invalid! Use t or with-tree-sitter")))

;;-pel-autoload
(defun pel-go-setup-info (&optional append)
  "Display Go setup information."
  (interactive "P")
  (pel-major-mode-must-be '(go-mode
                            go-ts-mode
                            go-dot-mod-mode
                            go-mod-ts-mode))
  (let ((pel-insert-symbol-content-context-buffer (current-buffer)))
    (pel-print-in-buffer
     "*pel-go-info*"
     "PEL setup for Go programming language"
     (lambda ()
       "Print Go setup info."
       (insert (propertize "* Major Mode Control:" 'face 'bold))
       (pel-insert-symbol-content-line 'major-mode nil "major mode currently used.")
       (when pel-use-tree-sitter
         (insert (format "\n- %s" (pel-ts-language-grammar-status-for 'go))))
       (pel-insert-symbol-content-line 'pel-use-go nil
                                       (function pel-go-mode-used-text))
       (insert "\n\n")
       ;;
       (insert (propertize "* Code Formatting Control:" 'face 'bold))
       (pel-insert-symbol-content-line 'pel-go-run-gofmt-on-buffer-save
                                       nil
                                       (lambda (v)
                                         (pel-on-off-string
                                          v
                                          "yes, format on save."
                                          "no, save buffer unchanged.")))
       (pel-insert-symbol-content-line 'pel-use-goflymake)
       (insert "\n\n")
       ;;
       (insert (propertize "* Indentation Control:" 'face 'bold))
       (insert "
- Under PEL, Go indentation level width is controlled by pel-go-tab-width:
  PEL stores its value in tab-width and go-ts-mode-indent-offset when
  a Go buffer is opened in go-mode and in go-ts-mode.
  Without PEL, indentation width is controlled either by either user-option
  depending of the major mode used.  That can lead to confusion,
  a confusion that PEL avoids.

  You can temporarily change the one used by the major mode you are using to
  increase or decrease the visual indentation spacing.  The best way to do
  that is to use something like:
  -  M-: (setq-local tab-width 8) for go-mode
  -  M-: (setq-local go-ts-mode-indent-offset 8) for go-ts-mode.

  Changing pel-go-tab-width will only affect the rendering of the next Go
  buffer(s) you open.

  Under normal circumstances (if you do nothing) the 3 user-options should
  have the exact same value, imposed by the value of pel-go-tab-width when
  the Go buffer is opened.
")
       (pel-insert-symbol-content-line 'pel-go-tab-width nil
                                       "\
corresponds to rendered indentation width. \
Changing it has no impact on buffer/file content!")
       (pel-insert-symbol-content-line 'tab-width nil
                                       "used by go-mode.")
       (pel-insert-symbol-content-line 'go-ts-mode-indent-offset nil
                                       "used by go-ts-mode."))
     (unless append :clear-buffer)
     :use-help-mode)))

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
