;;; pel-as.el --- Set mode of fundamental-mode buffer.  -*- lexical-binding: t; -*-

;; Created   : Friday, March 14 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-03-19 14:43:15 EDT, updated by Pierre Rouleau>

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
;; This implements the `pel-as' command.  It is used to interactively set the
;; major mode of buffers that are in `fundamental-mode'.  Emacs often open
;; files without extensions in `fundamental-mode' when the file is first
;; created.  The `pel-as' command helps set up the mode and the file shebang
;; when one is required.  It supports several types of files.
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel-prompt)       ; use: `pel-prompt-with-completion'
(require 'cl-lib)           ; use: `cl-case'
(require 'sh-script)        ; use: `shell-script-mode', `sh-set-shell',
;;                          ;      `sh-ancestor-alist'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defconst pel--sh-modes (append (mapcar (lambda (e) (car e))
                                        sh-ancestor-alist)
                                (when (eq system-type 'gnu/linux)
                                  '(csh ksh)))
  "List of potential shells script modes.")

;; [:todo 2025-03-17, by Pierre Rouleau: Tie this to pel-skels-generic-first-line]
(defconst pel--all-modes (append pel--sh-modes
                                 '(config
                                   d
                                   lua
                                   nim
                                   perl pike python
                                   ruby
                                   expect tcl))
  "List of modes for languages that support shebang lines.")



(defun pel--as-sh (shell-type)
  "Activate the sh-mode for the specified SHELL-TYPE."
  (shell-script-mode)
  (condition-case err
      (sh-set-shell shell-type nil :insert-shebang)
    (error
     (user-error "%s: using native shell instead.
Write the shebang line manually. Install %s otherwise."
                 (cadr err) shell-type))))

(defun pel-as (&optional force)
  "Interactively select major mode for buffer.

This command is mostly used to set the major mode of a buffer in
`fundamental-mode', when the PEL key binding is available for it.
Select one of the major modes from the provided list.
After being used once in a buffer the major mode is selected and the PEL key
binding will not be available when PEL supports the major mode.

This sets the major mode to the one selected and also adds the
appropriate shebang line to the very first line of the file if one is
necessary for the type of file.

If after using it once, you want to change the major mode using this
command you have 2 choices:

- Force the major mode back to `fundamental-mode' explicitly with M-x,
  and use the key binding again.
- Invoke the command with M-x directly, by name, passing any argument to FORCE
  its execution (because it will normally not allow it once a major mode is
  set.
"
  (interactive "P")
  (unless (or (eq major-mode 'fundamental-mode) force)
    (let ((pel-use-symbol (pel-major-mode-symbol-for "pel-use-%s"))
          (pel-use-value  (pel-major-mode-symbol-value-or "pel-use-%s" "UNDEF")))
      (user-error "Major-mode already selected. Force with optional arg.%s"
                  (if (string-equal pel-use-value "UNDEF")
                      ""
                    (if pel-use-value
                        ""
                      (format "\n%s is not set. Did you forget to activate it?"
                              pel-use-symbol))))))
  (let ((mode (pel-prompt-with-completion
               "Major mode: "
               (mapcar (function symbol-name) pel--all-modes)
               'pel--all-modes)))
    (cond
     ((member mode (mapcar (function symbol-name) pel--sh-modes))
      (pel--as-sh mode))

     ((equal mode "config")
      (conf-mode))

     ((equal mode "d")
      (require 'd-mode)
      (declare-function d-mode "d-mode")
      (d-mode)
      (require 'pel-d)
      (declare-function pel-d-insert-shebang-line "pel-d")
      (pel-d-insert-shebang-line))

     ((member mode '("expect" "tcl"))
      (require 'tcl)
      (declare-function tcl-mode "tcl")
      (tcl-mode)
      (require 'pel-tcl)
      (declare-function pel-tcl-insert-shebang-line "pel-tcl")
      (declare-function pel-tcl-expect-insert-shebang-line "pel-tcl")
      (if (equal mode "tcl")
          (pel-tcl-insert-shebang-line)
        (pel-tcl-expect-insert-shebang-line))
      )

     ((equal mode "nim")
      (require 'nim-mode)
      (declare-function nim-mode "nim-mode")
      (nim-mode)
      (require 'pel-nim)
      (declare-function pel-nim-insert-shebang-line "pel-nim")
      (pel-nim-insert-shebang-line))

     ((equal mode "perl")
      (require 'cperl-mode)
      (cperl-mode)
      (require 'pel-perl)
      (declare-function pel-perl-insert-shebang-line "pel-perl")
      (pel-perl-insert-shebang-line))

     ((equal mode "pike")
      (pike-mode)
      (require 'pel-pike)
      (declare-function pel-pike-insert-shebang-line "pel-pike")
      (pel-pike-insert-shebang-line))

     ((equal mode "python")
      (if (and pel-use-tree-sitter
               (fboundp 'python-ts-mode))
          (python-ts-mode)
        (python-mode))
      (require 'pel-python)
      (declare-function pel-python-insert-shebang-line "pel-python")
      (pel-python-insert-shebang-line))

     ((equal mode "ruby")
      (require 'ruby-mode)
      (declare-function ruby-mode "ruby-mode")
      (ruby-mode)
      (require 'pel-ruby)
      (declare-function pel-ruby-insert-shebang-line "pel-ruby")
      (pel-ruby-insert-shebang-line))

     (t (user-error "Currently unsupported: %s" mode)))))

;;; --------------------------------------------------------------------------
(provide 'pel-as)

;;; pel-as.el ends here
