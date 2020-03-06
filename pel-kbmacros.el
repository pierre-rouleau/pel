;;; pel-kbmacros.el --- PEL Keyboard Macro Utilities.

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau.swd@gmail.com>

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
(require 'pel-options)

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

;; -----------------------------------------------------------------------------
(provide 'pel-kbmacros)

;;; pel-kbmacros.el ends here
