;;; pel-shell.el --- PEL shell utilities.  -*- lexical-binding: t; -*-

;; Created   : Thursday, March 10 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-03-10 17:45:54, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2022  Pierre Rouleau
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
;; This improves upon the built-in shell interactive command, making it behave
;; in a less surprising way: it opens in the current window unless an existing
;; '*shell*' buffer already exists.  The other inferior process commands
;; provided with Emacs (term, ansi-term and ielm) already do that so this
;; provides a less surprising behaviour.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

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
      (switch-to-buffer shell-buffer))
    (shell shell-buffer)))

;;; --------------------------------------------------------------------------
(provide 'pel-shell)

;;; pel-shell.el ends here
