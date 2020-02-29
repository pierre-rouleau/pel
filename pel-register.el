;;; pel-register.el --- PEL Register Management Utilities

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

;;; Code:
(require 'pel-base)                     ; use: (pel-current-buffer-filename)
(require 'kmacro)

;; --
;; local utilities
(defun pel--proceed-with-register (register)
  "Return t if the REGISTER is unused or overwriting is allowed."
  (not (and (get-register register)
            (not (yes-or-no-p
                  (format "Register %c already used. Proceed anyway? "
                          register))))))

;; --

;;-pel-autoload
(defun pel-filename-to-register (register)
  "Store full path of current buffer file name in specified REGISTER.
Prompts if the register is already used."
  (interactive (list
                (register-read-with-preview
                 "Current filename to register: ")))
  (when (pel--proceed-with-register register)
      (set-register register (cons 'file (pel-current-buffer-filename)))))

;;-pel-autoload
(defun pel-point-to-register (register &optional arg)
  "Store current point to specified REGISTER safely.
With the prefix argument (ARG), store current frame configuration.
Calls `point-to-register' but only after prompting if the specified
register is already used."
  (interactive (list
                (register-read-with-preview
                 "Point to register: ")
		     current-prefix-arg))
  (when (pel--proceed-with-register register)
    (point-to-register register)))

;;-pel-autoload
(defun pel-copy-to-register (register start end &optional delete-flag region)
  "Copy marked region's START<->END text to specified REGISTER safely.
With prefix arg, delete as well.
Calls `copy-to-register' but only after prompting if specified
register is already used."
  (interactive (list (register-read-with-preview "Copy to register: ")
                     (region-beginning)
                     (region-end)
                     current-prefix-arg
                     t))
  (when (pel--proceed-with-register register)
    (copy-to-register register start end delete-flag region)))

;;-pel-autoload
(defun pel-copy-rectangle-to-register (register start end &optional delete-flag)
  "Copy rectangular START<->END region into specified REGISTER safely.
With prefix arg, delete as well.
Calls `copy-rectangle-to-register' but only after prompting if specified
register is already used."
  (interactive (list (register-read-with-preview
                      "Copy rectangle to register: ")
                     (region-beginning)
                     (region-end)
                     current-prefix-arg))
  (when (pel--proceed-with-register register)
    (copy-rectangle-to-register register start end delete-flag)))

;;-pel-autoload
(defun pel-window-configuration-to-register (register)
  "Store the window configuration of the selected frame in REGISTER, safely.
Run `window-configuration-to-register' but only after prompting if specified
register is already used."
  (interactive (list (register-read-with-preview "Window to register: ")))
  (when (pel--proceed-with-register register)
    (window-configuration-to-register register)))

;;-pel-autoload
(defun pel-frameset-to-register (register)
  "Store current frame set in specified REGISTER, safely.
Calls `frameset-to-register' but only after prompting if specified
register is already used."
  (interactive (list (register-read-with-preview "Frameset to register: ")))
  (when (pel--proceed-with-register register)
    (frameset-to-register register)))

;;-pel-autoload
(defun pel-number-to-register (number register)
  "Store NUMBER in specified REGISTER, safely.
Calls `number-to-register' but only after prompting if specified
register is already used."
  (interactive (list current-prefix-arg
                     (register-read-with-preview "Number to register: ")))
  (when (pel--proceed-with-register register)
    (number-to-register number register)))

;;-pel-autoload
(defun pel-kmacro-to-register (register)
  "Store last recorder keyboard macro to specified REGISTER, safely.
Calls `kmacro-to-register' but only after prompting if specified
register is already used."
  (interactive "c")
  (when (pel--proceed-with-register register)
    (kmacro-to-register register)))

;; -----------------------------------------------------------------------------
(provide 'pel-register)

;;; pel-register.el ends here
