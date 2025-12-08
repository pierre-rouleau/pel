;;; pel-eldoc.el --- PEL eldoc support.  -*- lexical-binding: t; -*-

;; Created   : Monday, December  8 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-12-08 08:42:11 EST, updated by Pierre Rouleau>

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
(require 'pel--base)       ; set: `pel-insert-symbol-content-context-buffer'
;;                         ; use:  `pel-print-in-buffer', `pel-insert-bold'
;;                         ;       `pel-insert-symbol-content'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-eldoc-setup-info (&optional append)
  "Print eldoc setup information for the current buffer.
The buffer name is *pel-eldoc-info*.
If APPEND is non-nil, append to the buffer."
  (interactive "P")
  (let ((pel-insert-symbol-content-context-buffer (current-buffer)))
    (pel-print-in-buffer
     "*pel-eldoc-info*"
     "Eldoc setup control"
     (lambda ()
       "Print eldoc setup information."
       (pel-insert-bold "* Mode Control:")
       (pel-insert-symbol-content 'global-eldoc-mode nil :on-same-line)
       (pel-insert-symbol-content 'eldoc-mode nil :on-same-line)
       (pel-insert-bold "\n* Control variables:")
       (pel-insert-symbol-content-line 'eldoc-idle-delay)
       (pel-insert-symbol-content-line 'eldoc-print-after-edit)
       (pel-insert-symbol-content-line 'eldoc-echo-area-use-multiline-p )
       (pel-insert-symbol-content-line 'eldoc-echo-area-display-truncation-message)
       (pel-insert-symbol-content-line 'eldoc-echo-area-prefer-doc-buffer)
       (pel-insert-symbol-content-line 'eldoc-documentation-strategy)
       (pel-insert-symbol-content-line 'eldoc-documentation-functions))
     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-eldoc)

;;; pel-eldoc.el ends here
