;;; pel-as.el --- Set mode of fundamental-mode buffer.  -*- lexical-binding: t; -*-

;; Created   : Friday, March 14 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-03-14 16:54:52 EDT, updated by Pierre Rouleau>

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
;; When the `pel-


;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel-prompt)                   ; for: `pel-prompt-with-completion'
(require 'cl-lib)                       ; for: `cl-case'
(require 'sh-script)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defconst pel-as-modes '(bash csh ksh sh zsh
                              config
                              d
                              lua
                              nim
                              perl pike python
                              ruby
                              expect tcl
                              zig)
  "List of modes for languages that support shebang lines.")



(defun pel--as-sh (shell-type)
  "Activate the sh-mode for the specified SHELL-TYPE."
  (shell-script-mode)
  (sh-set-shell shell-type nil :insert-shebang))

(defun pel-as (&optional force)
  "Interactively select major mode for buffer.

This command is mostly used to set the major mode of a buffer in
`fundamental-mode', when the PEL key binding is available for it.
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
    (user-error "Major-mode already selected. Force with optional arg."))
  (let ((mode (pel-prompt-with-completion
               "Major mode: "
               (mapcar (function symbol-name) pel-as-modes)
               'pel-as-modes)))
    (cond
     ((member mode '("bash" "csh" "ksh" "sh" "zsh"))
      (pel--as-sh mode))
     ((equal mode "config")
      (conf-mode))
     (t (user-error "Currently unsupported: %s" mode)))))

;;; --------------------------------------------------------------------------
(provide 'pel-as)

;;; pel-as.el ends here
