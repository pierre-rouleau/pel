;;; pel-kbmacros.el --- PEL Keyboard Macro Utilities. -*-lexical-binding: t-*-

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
;; Protected keyboard-macro definitions:
;;
;; By default Emacs does not prevent overwriting of a keyboard macro defined
;; with the <f3> key.  The following functions prevent that.  The <f3> key
;; is rebound to pel-kmacro-start-macro-or-insert-counter which prompts
;; if the macro has already been defined.
;;
;; Use `pel-forget-recorded-keyboard-macro' to prevent the prompt
;; on the next definition.  You can also use the negative sign or
;; numeric 0 prefix when recording the macro to prevent the prompt.

;; -----------------------------------------------------------------------------
;;; Code:
(require 'pel--options)     ; use: pel-kbmacro-prompts

(defvar pel--keyboard-macro-defined
  nil
  "Identify whether a keyboard macro is already defined:
- t  :  one is already defined,
- nil:  none defined.")

;;-pel-autoload
(defun pel-kmacro-start-macro-or-insert-counter (arg)
  "Record keyboard macro but prompt if one already exists.
Insert macro counter during definition or execution.
.
To prevent prompt, pass a numeric 0 to ARG or just a sign.
This protects `kmacro-start-macro-or-insert-counter' recorded macro from
being overwritten by mistake, saving you from having to pop the kmacro ring."
  (interactive "P")
  (unless
      (and (not (or defining-kbd-macro executing-kbd-macro arg))
           pel--keyboard-macro-defined
           pel-kbmacro-prompts
           (not (yes-or-no-p "Macro already recorded. Overwrite it? ")))
    (progn
      (setq pel--keyboard-macro-defined t)
      (kmacro-start-macro-or-insert-counter arg))))

;;-pel-autoload
(defun pel-forget-recorded-keyboard-macro ()
  "Forget that a keyboard macro was recorded by F3.
Does not delete the macro from the keyboard macro ring.
Reset `pel--keyboard-macro-defined'."
  (interactive)
  (setq pel--keyboard-macro-defined nil))

;; prevention of byte-compiler warning with defvar declaration
(defvar kmacro-ring)
(defvar last-kb-macro)
(defvar kmacro-counter)
(defvar kmacro-counter-format)
;;-pel-autoload
(defun pel-kmacro-ring-show-status (&optional print-in-buffer)
  "Show the `kmacro-ring' status information.

Print a message unless PRINT-IN-BUFFER is non-nil in which case
it prints the information in a dedicated buffer."
  (interactive "P")
  (require 'kmacro)
  (let ((msg (format "kmacro-ring:

- kbmacro-ring size    : %d
- kbmacro-ring content : %S
- last-kbd-macro       : %S
- kmacro-counter       : %s
- kmacro-counter-format: %S"
                     (length kmacro-ring)
                     kmacro-ring
                     last-kbd-macro
                     kmacro-counter
                     (replace-regexp-in-string "%" "%%" kmacro-counter-format))))
    (if print-in-buffer
        (pel-print-in-buffer "*kmacro-ring Status*" "Keyboard Macro Status" msg)
      (message msg))))

;; -----------------------------------------------------------------------------
(provide 'pel-kbmacros)

;;; pel-kbmacros.el ends here
