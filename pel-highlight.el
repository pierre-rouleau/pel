;;; pel-highlight.el --- PEL highlight management utilities.

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
(require 'hl-line)
(require 'pel-base)

;;-pel-autoload
(defun pel-set-highlight-color (colorname)
  "Select (or prompt) for COLORNAME and use it as current line highlight."
  (interactive "sEnter color name: ")
  (progn
    (set-face-background 'highlight colorname)
    (set-face-foreground 'highlight nil)
    (set-face-underline 'highlight nil)))

;;-pel-autoload
(defun pel-toggle-hl-line-sticky ()
  "Toggle current line highlight to all windows or just the current one.
Toggles the value of `hl-line-sticky-flag' between t and nil."
  (interactive)
  (progn
    (eval-when-compile
	  (defvar hl-line-mode)) ; prevent warning about ref to free variable.
    (setq hl-line-sticky-flag (not hl-line-sticky-flag))
    (message "Current line highlighting %s done in %s."
             (if hl-line-mode
                 "is now"
               "(currently disabled) will be")
             (if hl-line-sticky-flag
                 "all windows showing current buffer"
               "current buffer only"))))

;;-pel-autoload
(defun pel-toggle-show-trailing-whitespace ()
  "Toggle highlight of the trailing whitespaces in current buffer."
  (interactive)
  (pel-toggle-and-show 'show-trailing-whitespace))

;;-pel-autoload
(defun pel-toggle-indicate-empty-lines ()
  "Toggle highlight of empty lines."
  (interactive)
  (pel-toggle-and-show 'indicate-empty-lines))

;;-pel-autoload
(defun pel-toggle-indent-tabs-mode (&optional arg)
  "Toggle use of hard tabs or spaces for indentation in current buffer.
If ARG is positive set to use hard tabs, otherwise force use of spaces only.
Beep on each change to warn user of the change and display new value."
  (interactive "P")
  (beep :dont-break-macro)
  (let ((on-string  "t: using hard TABs for indentation in current buffer")
        (off-string "nil: using SPACEs only for indentation in current buffer"))
  (if (null arg)
      ;; toggle
      (pel-toggle-and-show 'indent-tabs-mode on-string off-string)
    ;; set value according to arg
    (if (< (prefix-numeric-value arg) 0)
        ;; negative; use space -> set to nil
        (setq indent-tabs-mode nil)
      (setq indent-tabs-mode t))
    (message "%s" (pel-symbol-text indent-tabs-mode on-string off-string)))))

;; -----------------------------------------------------------------------------
(provide 'pel-highlight)

;;; pel-highlight.el ends here
