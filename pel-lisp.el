;;; pel-lisp.el --- PEL Lisp Editing Utilities -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2023  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

;; This file is part of the PEL package
;; This file is not part of GNU Emacs.

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

;; -----------------------------------------------------------------------------
;;; Commentary:
;;

;;; Code:

(require 'pel--base)                ; use: `pel-current-buffer-filename'
(require 'elisp-mode)               ; use: `emacs-lisp-byte-compile-and-load'

;;-pel-autoload
(defun pel-toggle-lisp-modes ()
  "Toggle buffer's LISP mode: `lisp-interaction-mode' <-> `emacs-lisp-mode'."
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode) (lisp-interaction-mode))
        ((eq major-mode 'lisp-interaction-mode) (emacs-lisp-mode))
        (t (error "Current buffer not in a LISP-compatible mode"))))


;;-pel-autoload
(defun pel-byte-compile-file-and-load ()
  "Byte compile and load the current elisp file.

Prompt if applied to an unsaved buffer to save to a file.
Issue an error if applied on a buffer not visiting a file.
Returns:
- nil if errors detected (listed in the *Compile-log* buffer)
- non-nil if there were no error:
  - no-byte-compile : if file was current and there was no need to compile it.
  - t               : if file was compiled and no error were detected."
  (interactive)
  (call-interactively (function emacs-lisp-byte-compile-and-load)))

;;-pel-autoload
(defun pel-lint-elisp-file ()
  "Run lint on Emacs Lisp file in current buffer."
  (interactive)
  (require 'elint)
  (elint-file (pel-current-buffer-filename)))

;; -----------------------------------------------------------------------------
(provide 'pel-lisp)

;;; pel-lisp.el ends here
