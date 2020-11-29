;;; pel-elisp.el --- Emacs Lisp support.  -*- lexical-binding: t; -*-

;; Created   : Friday, November 27 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2020-11-29 16:11:58, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020  Pierre Rouleau
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
;; The code here provide navigation functions for Emacs Lisp source code that
;; circumvent the problems present in Emacs lisp.el functions: `end-of-defun'
;; and `beginning-of-defun':
;;
;; - These functions treat Lisp statements inside a string as if it is real
;;   Lisp code: they do not ignore Lisp code inside strings.  This can be
;;   annoying when you write Lisp code that generate Lisp code and you place
;;   an opening parenthesis on the first column inside a string.
;; - The standard Emacs function will not navigate across or to the beginning
;;   or end of incomplete/unbalanced Lisp forms.
;; - The standard functions do not stop at the first or last form in the file:
;;   they stop at the first character and last character of a file.
;;
;; This file provides two alternative implementations:
;; - `pel-elisp-beginning-of-defun' as a replacement for `beginning-of-defun',
;; - `pel-elisp-end-of-defun' as a replacement for `end-of-defun'.
;;
;; The function overcome the first problem: they do not stop at defun forms
;; inside a string.  Unfortunately the current implementation does not resolve
;; the second and third problems described above.
;;
;; Use `pel-elisp-toggle-motion-defuns' to toggle the defun navigation
;; functions.  It calls `pel-elisp-activate-motion-defuns' to activate the
;; functions and `pel-elisp-restore-emacs-motion-defuns' to restore Emacs
;; defaults.
;;
;; BUGS:  An important one: when the PEL functions are used, checkdoc hangs!
;;        I am currently checking why that is.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel--bod ()
  "Raw search to the beginning of a defun.
Move point if one is found.
Return t if one found, nil otherwise."
  (condition-case nil
      (progn
        (re-search-backward "^\\s(")
        (goto-char (1- (match-end 0)))
        t)
    (search-failed nil)))

(defun pel-elisp--bod (arg)
  "Move backward to ARG beginning of defun.
ARG must be positive.
Return t if found, nil otherwise."
  (let ((found t)
        (continue t))
    (while (and continue (> arg 0))
      (setq arg (1- arg))
      (if (pel--bod)
          (let ((old-pos nil))
            (while (and continue
                        (eq (get-char-property (point) 'face)
                            'font-lock-string-face)
                        (not (eq (point) old-pos)))
              (setq old-pos (point))
              (backward-char 1)
              (unless (pel--bod)
                (setq found nil)
                (setq continue nil))))
        (setq found nil)
        (setq continue nil)))
    found))

(defun pel-elisp-beginning-of-defun (&optional arg)
  "Move backward to the beginning of Elisp defun, ignoring strings.

Positive ARG means move backward to the ARGth beginning of defun.
Negative ARG means move forward to the ARGth following beginning of defun.
No ARG is equivalent to ARG set to 1.

Return t if one found, nil otherwise.
Supports shift marking."
  (interactive "^p")
  ;; (message "pel-elisp-beginning-of-defun @ %d" (point))
  (unless arg (setq arg 1))
  (if (<  arg 0)
    ;; arg is negative
      (when (not (eobp))
        (let ((origin (point)))
          ;; first try: move to next beginning
          (pel-elisp-end-of-defun (- arg))
          (pel-elisp--bod 1)
          ;; check if we moved forward
          (when (<= (point) origin)
            ;; if we did not move forwards try again 1 more end forward
            (pel-elisp-end-of-defun (1+ (- arg)))
            (pel-elisp--bod 1)
            )))
    (pel-elisp--bod arg)))

;--

(defun pel--eod ()
  "Raw search forward to end of defun."
  (forward-sexp 1)
  t)

(defun pel-elisp-end-of-defun (&optional arg)
  "Move forward to the end of current or next defun, ignoring strings.
No ARG is equivalent to ARG set to 1.

Return t if one found, nil otherwise.
Supports shift marking."
  (interactive "^p")
  ;; (message "pel-elisp-end-of-defun @ %d" (point))
  (unless arg (setq arg 1))
  (let ((found t)
        (continue t))
    (while (and continue (> arg 0))
      (setq arg (1- arg))
      (if (pel--eod)
          (let ((old-pos nil))
            (while (and continue
                        (eq (get-char-property (point) 'face)
                            'font-lock-string-face)
                        (not (eq (point) old-pos)))
              (setq old-pos (point))
              (forward-char 1)
              (unless (pel--eod)
                (setq found nil)
                (setq continue nil))))
        (setq found nil)
        (setq continue nil))
      (forward-char 1))
    found))

;; ---------------------------------------------------------------------------

(defvar pel-elisp--functions-are-used nil
  "Identifies whether the pel-elisp navigation functions are used.
- If nil, they are not used.
-If non-nil:
  - `pel-elisp-beginning-of-defun' and `pel-elisp-end-of-defun' are used.
  - the value is previous values of:
    (cons `beginning-of-defun-function' `end-of-defun-function')")

;;-pel-autoload
(defun pel-elisp-activate-motion-defuns (&optional verbose)
  "Activate the pel-elisp motion defuns.
Display a message when VERBOSE is non-nil."
  (if pel-elisp--functions-are-used
      (display-warning
       'pel-elisp
       (format "Trying to activate pel-elisp navigation functions \
when they are already being used!")
       :warning)
    (setq pel-elisp--functions-are-used
          (cons beginning-of-defun-function
                end-of-defun-function))
    (setq beginning-of-defun-function (function pel-elisp-beginning-of-defun))
    (setq end-of-defun-function       (function pel-elisp-end-of-defun))
    (when (or (called-interactively-p 'interactive)
              verbose)
      (message "Now using PEL pel-elisp-beginning-of-defun \
and pel-elisp-end-of-defun
CAUTION ⚠️ : This causes checkdoc to stop checking!"))))

(defun pel-elisp-restore-emacs-motion-defuns (&optional verbose)
  "Deactivate the pel-elisp motion defuns.
Restore what was used by Emacs before the call to
`pel-elisp-activate-motion-defuns'.
Display a message when VERBOSE is non-nil."
  (if (not pel-elisp--functions-are-used)
      (display-warning
       'pel-elisp
       (format "Trying to restore Emacs navigation functions \
when they are already being used!")
       :warning)
    (setq beginning-of-defun-function (car pel-elisp--functions-are-used))
    (setq end-of-defun-function       (cdr pel-elisp--functions-are-used))
    (setq pel-elisp--functions-are-used nil)
    (when (or (called-interactively-p 'interactive)
              verbose)
      (message "Now using Emacs beginning-of-defun and end-of-defun"))))

;;-pel-autoload
(defun pel-elisp-toggle-motion-defuns ()
  "Change the function moving to the beginning and end of defuns.

If the standard Emacs functions are used, start using the PEL ones,
or vice-versa.
Emacs standard functions are:
- `beginning-of-defun' and
- `end-of-defun'.
PEL functions are:
- `pel-elisp-beginning-of-defun' and
- `pel-elisp-end-of-defun'."
  (interactive)
  (let ((show-new-state (called-interactively-p 'interactive)))
    (if pel-elisp--functions-are-used
        (pel-elisp-restore-emacs-motion-defuns show-new-state)
      (pel-elisp-activate-motion-defuns show-new-state))))

;;; --------------------------------------------------------------------------
(provide 'pel-elisp)

;;; pel-elisp.el ends here
