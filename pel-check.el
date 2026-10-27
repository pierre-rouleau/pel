;;; pel-check.el --- PEL early check for dual environment  -*- lexical-binding: t; -*-

;; Created   : Tuesday, October 27 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-10-27 15:06:39 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2026  Pierre Rouleau
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
;; This small file holds only one function, `pel-check-dual-environment'
;; invoked by pel_keys when Emacs starts and is running in graphics mode to
;; check if PEL supports a dual customization for terminal and graphics mode.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)      ; use: `pel-emacs-27-or-later-p'
(require 'pel--options)   ; use: `pel-support-dual-environment'

(declare-function pel-setup-check-dual-environment "pel-setup" ())

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-check-dual-environment ()
  "Check dual-environment flags; delegate mismatches to setup.

Compare each bound initialization flag with
`pel-support-dual-environment'.  On Emacs 27 and later, also
check the early-init flag.  Ignore unbound flags, as
`pel-setup-check-dual-environment' does.

If the flags agree, return nil without loading `pel-setup'.
Otherwise, load `pel-setup' and call its check to retain the
existing warning and automatic correction behavior.  Return
the result of that check."
  (let ((symbols '(pel-init-support-dual-environment-p))
        (is-ok t))
    (when pel-emacs-27-or-later-p
      (push 'pel-early-init-support-dual-environment-p symbols))
    (dolist (symbol symbols)
      (when (and (boundp symbol)
                 (not (eq pel-support-dual-environment
                          (symbol-value symbol))))
        (setq is-ok nil)))
    (unless is-ok
      ;; Load the setup dependencies only when a mismatch needs handling.
      (require 'pel-setup)
      (pel-setup-check-dual-environment))))

;;; --------------------------------------------------------------------------
(provide 'pel-check)

;;; pel-check.el ends here
