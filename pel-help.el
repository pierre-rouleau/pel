;;; pel-help.el --- PEL extra help utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2023, 2024, 2025  Pierre Rouleau

;; Author: Pierre Rouleau (concat "prouleau" "001" "@" "gmail" ".com")

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
;; Contains a small set of help providing utility commands.
;;

;; -----------------------------------------------------------------------------
;;; Dependency
(require 'which-func)         ; use: `which-function'  -- part of Emacs.
(require 'cl-extra)           ; use `cl-some'
(require 'help-fns)           ; use `help--symbol-completion-table'
(require 'pel--base)
;;; Code:

;;-pel-autoload
(defun pel-show-kill-ring ()
  "Display content of `kill-ring' in *Help* buffer.
Simple shortcut to invoke `describe-variable' on the `kill-ring' variable."
  (interactive)
  (describe-variable 'kill-ring))

;;-pel-autoload
(defun pel-show-major-mode ()
  "Display the symbol of the current major mode."
  (interactive)
  (message "Major mode: %S" major-mode))


;;-pel-autoload
(defun pel-show-function (&optional insert-it)
  "Display the name of the current function at point in mini-buffer.

Also insert it at point when optional INSERT-IT argument is non-nil."
  (interactive "P")
  (let ((fct-name (which-function)))
    (when (and fct-name insert-it)
      (insert fct-name))
    (message "%s" (or fct-name "Point is not inside a function definition."))))


(defun pel--format-prompt (prompt default &rest format-args)
  "Format PROMPT with DEFAULT.
Proxy form `format-prompt' supporting Emacs before 28."
  (if (fboundp 'format-prompt)
      (format-prompt prompt default format-args)
    ;; On older Emacs (before 28), `format-prompt', from minibuffer, did not
    ;; exist: here's a copy of it's implementation from Emacs 30
    (concat
     (if (null format-args)
         (substitute-command-keys prompt)
       (apply #'format (substitute-command-keys prompt) format-args))
     (and default
          (or (not (stringp default))
              (length> default 0))
          (format (substitute-command-keys minibuffer-default-prompt-format)
                  (if (consp default)
                      (car default)
                    default)))
     ": ")))

;;-pel-autoload
(defun pel-show-symbol (symbol)
  "Print a message with the SYMBOL name and value."
  ;; Interactive code taken from the `describe-symbol' from
  ;; Emacs built-in help-fns.el
  (interactive
   (let* ((v-or-f (symbol-at-point))
          (found (when v-or-f
                   (cl-some (lambda (x)
                              (funcall (nth 1 x) v-or-f))
                            describe-symbol-backends)))
          (v-or-f (if found v-or-f (function-called-at-point)))
          (found (or found v-or-f))
          (enable-recursive-minibuffers t)
          (val (completing-read (pel--format-prompt "Describe symbol"
                                                    (and found v-or-f))
				                #'help--symbol-completion-table
				                (lambda (vv)
                                  (cl-some (lambda (x) (funcall (nth 1 x) vv))
                                           describe-symbol-backends))
				                t nil nil
				                (if found (symbol-name v-or-f)))))
     (list (if (equal val "")
	           (or v-or-f "") (intern val)))))
  (message "%s := %s" symbol (symbol-value symbol)))

;; -----------------------------------------------------------------------------
(provide 'pel-help)

;;; pel-help.el ends here
