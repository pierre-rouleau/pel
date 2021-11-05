;;; pel-spell-iedit.el --- Flyspell/IEdit key binding conflict checker.  -*- lexical-binding: t; -*-

;; Created   : Friday, November  5 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-11-05 11:35:18, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021  Pierre Rouleau
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
;;
;; flyspell binds the key identified by `flyspell-auto-correct-binding' to the
;; command `flyspell-auto-correct-previous-word' .
;;
;; By default the key is (kbd "C-;").
;;
;; This creates the following 2 problems:
;;
;; 1) that key is not available in terminal mode for such a useful command.
;;
;; 2) iedit-mode default key is also (kbd "C-;") as identified by its user
;;    option `iedit-toggle-key-default'.  iedit-mode checks if something else
;;    is mapping it but flyspell is activated lazily per mode, so iedit check
;;    never detects the conflicting binding.  There's no easy automatic
;;    solution for this, aside from checking for the conflict here and
;;    changing one of the user options.  Remember that user option variables
;;    are only available when their relative feature (file) has been loaded,
;;    which means that the check must be done when flyspell-mode is activated.
;;
;;    The code below perform the check when the modes are activated.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defvar flyspell-auto-correct-binding)  ; declared in flyspell as dynamic.
;;                                      ; Done here to prevent compiler warnings

;;-pel-autoload
(defun pel-spell-iedit-check-conflict ()
  "Check for key binding conflict between flyspell and iedit.
Warn user if necessary."
  (when (and (boundp 'iedit-toggle-key-default)
             (boundp 'flyspell-auto-correct-binding)
             (string= (key-description iedit-toggle-key-default)
                      (key-description flyspell-auto-correct-binding)))
    (display-warning
     'pel-keys
     (format "Both iedit and flyspell bind functions to \"%s\"!\n\
To use this key, change the key selected in one of the following \n\
user options:\n\
- `iedit-toggle-key-default'
- `flyspell-auto-correct-binding'

Then save your changes."
             (key-description flyspell-auto-correct-binding)))))

;;; --------------------------------------------------------------------------
(provide 'pel-spell-iedit)

;;; pel-spell-iedit.el ends here
