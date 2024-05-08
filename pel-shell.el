;;; pel-shell.el --- PEL shell utilities.  -*- lexical-binding: t; -*-

;; Created   : Thursday, March 10 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2024-05-07 23:22:55 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2022, 2024  Pierre Rouleau
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
;; This file provides commands to improve upon the built-in shell command and
;; provide extra commands for the shell-mode.
;;
;; The `pel-shell' improves upon the built-in shell interactive command,
;; making it behave in a less surprising way: it opens in the current window
;; unless an existing '*shell*' buffer already exists.  The other inferior
;; process commands provided with Emacs (term, ansi-term and ielm) already do
;; that so this provides a less surprising behaviour.
;;
;; The `pel-shell-previous-prompt' and `pel-shell-next-prompt' commands move
;; point to the previous and next prompt line respectively.  They both use the
;; value of the `pel-shell-prompt-line-regexp' user-option to perform the
;; regular expression search that identifies the beginning of a prompt line.
;;
;; I might improve this in the future by adding the ability to detect the
;; prompt regexp, but for now that will do.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--options)                ; use: `pel-shell-prompt-line-regexp'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-shell ()
  "Open a shell in the current window.

If a *shell* buffer is already showing in one of the windows move
point to it.

This command uses the built-in shell command, making it open inside
the current window and not the other window like the built-in shell does.

The behaviour of pel-shell is the same as other inferior process
commands like term, ansi-term and ielm."
  (interactive)
  (let ((shell-buffer (get-buffer "*shell*")))
    (unless shell-buffer
      (setq shell-buffer (generate-new-buffer "*shell*"))
      (switch-to-buffer shell-buffer))  ; user interactive request. OK to call.
    (shell shell-buffer)))

;;-pel-autoload
(defun pel-shell-previous-prompt (n)
  "Move point to the previous prompt line at command.

It uses the `pel-shell-prompt-line-regexp' user-option to perform
the search."
  (interactive "p")
  (if (< n 0)
      (pel-shell-next-prompt (abs n))
    (dotimes (_ n)
      (forward-line -1)
      (if (re-search-backward pel-shell-prompt-line-regexp nil :noerror)
          (if  (re-search-forward pel-shell-prompt-line-regexp nil :noerror)
              (right-char 1)
            (user-error "No prompt found above point. Searching for %s" pel-shell-prompt-line-regexp))
        (user-error "No prompt found above point. Searching for %s" pel-shell-prompt-line-regexp)))))

;;-pel-autoload
(defun pel-shell-next-prompt (n)
  "Move point to the previous prompt line at command.

It uses the `pel-shell-prompt-line-regexp' user-option to perform
the search."
  (interactive "p")
  (if (< n 0)
      (pel-shell-previous-prompt (abs n))
    (dotimes (_ n)
      (right-char 1)
      (if (re-search-forward pel-shell-prompt-line-regexp nil :noerror)
          (right-char 1)
        (left-char 1)
        (user-error "No prompt found below point. Searching for %s" pel-shell-prompt-line-regexp)))))

;;-pel-autoload
(defun pel-shell-show-cfg (&optional append)
  "Print shell configuration information in specialized buffer.

Clear previous buffer content unless optional APPEND argument is non-nil,
in which case it appends to the previous report.
Variables names are also links to customization."
  (interactive "P")
  (let ((pel-insert-symbol-content-context-buffer (current-buffer)))
    (pel-print-in-buffer
     (pel-string-with-major-mode "*pel-%s-info*")
     (pel-string-with-major-mode "*%s Control")
     (lambda ()
       (pel-insert-symbol-content-line 'comint-process-echoes)
       (pel-insert-symbol-content-line 'pel-shell-prompt-line-regexp)
       (pel-insert-symbol-content-line 'pel-shell-activates-minor-modes))
     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-shell)

;;; pel-shell.el ends here
