;;; pel-elisp.el --- Emacs Lisp support.  -*- lexical-binding: t; -*-

;; Created   : Friday, November 27 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-01-11 15:45:26, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2021  Pierre Rouleau
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
;; This only contains a function to change the behaviour of the Emacs Lisp
;; defun navigation functions like `beginning-of-defun' and `end-of-defun'.
;;
;; By default these stop at what looks like a defun form located inside a
;; string.  But the behaviour is controlled by the
;; `open-paren-in-column-0-is-defun-start' user option.
;;
;; This file provides the command function
;; `pel-toggle-paren-in-column-0-is-defun-start' to toggle the user option and
;; the behaviour of the navigation functions.

;; Credits:  Thanks to Andreas Röhler to mention the Standard Emacs variable
;;           `open-paren-in-column-0-is-defun-start'! That allowed me to
;;           replace a large amount of code with a simple toggle command and
;;           that handles the problem I had with checkdoc as well!

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: pel-toggle-and-show
(require 'pel--options)                 ; use: pel-elisp-target-forms
(require 'pel-prompt)                   ; use: pel-select-from
(eval-when-compile (require 'subr-x))   ; use: string-join

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-toggle-paren-in-column-0-is-defun-start ()
  "Toggle interpretation of a paren in column 0 and display new behaviour.
Toggle the value of `open-paren-in-column-0-is-defun-start'."
  (interactive)
  (pel-toggle-and-show
   'open-paren-in-column-0-is-defun-start
   "t, meaning: interpret all '(' in column 0 as a start of defun."
   "nil, meaning: a '(' in column 0 is no longer always interpreted
    as a defun start."))

;;-pel-autoload
(defun pel-elisp-set-navigate-target-form (&optional globally)
  "Select form navigation behaviour.
Select the behaviour of the following navigation functions:
- `pel-elisp-beginning-of-next-form' and
- `pel-elisp-beginning-of-previous-form'.

This modifies the value of the `pel-elisp-target-forms' user-option only for
the current buffer unless the GLOBALLY argument is non-nil, in which case it
modifies the behaviour for all buffers. The change in behaviour does not
persist across Emacs sessions.  If you want your change to persist, modify
the value of the `pel-elisp-target-forms' user-option and save it."
  (interactive "P")
  (let ((selection
         (pel-select-from
          "Move to:"
          '(
            (?0 "All-top level"
                all-top-level-forms)
            (?1 "Top-level defun"
                top-level-defun-forms)
            (?2 "All defun"
                defun-forms)
            (?3 "All functions, macros definitions"
                all-defun-defmacro-defsubst-forms)
            (?4 "All functions, macros & eieio definitions"
                all-functions-macros-eieio-def-forms)
            (?5 "All of the above and all variable definitions"
                all-functions-variables-def-forms)
            (?6 "User specified forms" user-specified))
          pel-elisp-target-forms)))
    (unless globally
      (with-current-buffer (current-buffer)
        (unless (local-variable-p 'pel-elisp-target-forms)
          (make-local-variable 'pel-elisp-target-forms))
        (when (eq selection 'user-specified)
          (unless (local-variable-p 'pel-elisp-user-specified-targets)
            (make-local-variable 'pel-elisp-user-specified-targets)))))
    (setq pel-elisp-target-forms selection)
    (message "For navigation is now using %s for %s"
             selection
             (if globally "all non configured buffers"
               (current-buffer)))))

(defun pel--elisp-form-regexp-for (forms)
  "Return a regexp to search for specified FORMS.
FORMS is a list of strings, each string is a form to search for."
  (concat "(\\("
          (string-join
           (mapcar (lambda (form) (format "\\(%s\\)" form))
                   forms)
           "\\|")
           "\\) +'?\\(\\_<.+\\_>\\) *"))

(defun pel--navigate-target-regxp (&optional target)
  "Return the regxp to search for the definition forms.
As identified by the `pel-elisp-target-forms' variable or TARGET if specified.
TARGET, like `pel-elisp-target-forms' can be one of the following values:
- 'all-top-level-forms
- 'top-level-defun-forms
- 'defun-forms
- 'all-defun-defmacro-defsubst-forms
- 'all-functions-macros-eieio-def-forms
- 'all-functions-variables-def-forms
- a list of strings."
  (let* (;; Protect against pel-elisp-target-forms being nil
         (pel-elisp-target-forms (or pel-elisp-target-forms
                                     'all-top-level-forms))
         ;; If target is specified use it
         (target (or target pel-elisp-target-forms))
         ;; Searching only top-level forms?
         (top-level-only (memq target '(all-top-level-forms
                                        top-level-defun-forms)))
         ;; Compute regexp for specified criteria
         (regexp
          (cond
           ;; 0
           ((eq target 'all-top-level-forms)
            "(")
           ;; 1
           ((eq target 'top-level-defun-forms)
            (pel--elisp-form-regexp-for '("defun")))
           ;; 2
           ((eq target 'defun-forms)
            (pel--elisp-form-regexp-for '("defun")))
           ;; 3
           ((eq target 'all-defun-defmacro-defsubst-forms)
            (pel--elisp-form-regexp-for '("defun"
                                          "defsubst"
                                          "defmacro"
                                          "defalias"
                                          "defadvice")))
           ;; 4
           ((eq target 'all-functions-macros-eieio-def-forms)
            (pel--elisp-form-regexp-for '("defun"
                                          "defsubst"
                                          "defmacro"
                                          "defalias"
                                          "defadvice"
                                          "defclass"
                                          "defmethod"
                                          "defgeneric")))
           ;; 5
           ((eq target 'all-functions-variables-def-forms)
            (pel--elisp-form-regexp-for '("defun"
                                          "defsubst"
                                          "defmacro"
                                          "defalias"
                                          "defadvice"
                                          "defclass"
                                          "defmethod"
                                          "defgeneric"
                                          "defvar"
                                          "defvaralias"
                                          "defvar-local"
                                          "defvar-mode-local"
                                          "defconst"
                                          "defconst-mode-local"
                                          "defface"
                                          "deftheme"
                                          "defcustom"
                                          "defgroup")))
           ;; 6 : user specified string (may contain nil)
           ((eq target 'user-specified)
            (pel--elisp-form-regexp-for (remove nil pel-elisp-user-specified-targets)))
           (t
            (error
             "Invalid search criteria: pel-elisp-target-forms=%S, target=%S"
             pel-elisp-target-forms
             target)))))
    ;; return regexp for top-level or all specified form(s)
    (if top-level-only
        (concat "^" regexp)
      (concat "^[ \t]*" regexp))))

;;-pel-autoload
(defun pel-elisp-beginning-of-next-form
    (&optional n target silent dont-push-mark)
  "Move point forward to the beginning of next N top-level form.

The search is controlled by the value of `pel-elisp-target-forms' user
option.  That value can be changed for the current session, for all
buffers or only for the current buffer by the command
`pel-elisp-set-navigate-target-form'.
It can also be specified by the TARGET argument: specify one of the
symbols valid for `pel-elisp-target-forms'.

If no valid form is found, don't move point, issue an error describing
the failure unless SILENT is non-nil, in which case the function returns nil
on error and non-nil on success.

On success, the function push original position on the mark ring unless
DONT-PUSH-MARK is non-nil.

The function support shift-marking."
  (interactive "^p")
  (let ((pel-elisp-target-forms (or target pel-elisp-target-forms)))
    (if (< n 0)
        (pel-elisp-beginning-of-previous-form (abs n))
      ;; move point past the regexp (n times)
      (condition-case err
          (let ((start-pos (point)))
            (dotimes (_ n)
              (when (eq (following-char) 40) ; if following char is '('
                (right-char 1))  ; make sure point is past the open '('
              (re-search-forward (pel--navigate-target-regxp)))
            ;; move point on the opening paren
            (back-to-indentation)
            (unless dont-push-mark
              (push-mark start-pos))
            ;; On success, return t, nil on failure if silent
            t)
        (search-failed
         (unless silent
           (user-error "No form found: %s" err)))))))


;;-pel-autoload
(defun pel-elisp-beginning-of-next-defun (&optional n)
  "Move point to the beginning of next N defun form - at any level."
  (interactive "^p")
  (pel-elisp-beginning-of-next-form n 'defun-forms))

;;-pel-autoload
(defun pel-elisp-beginning-of-previous-form
    (&optional n target silent dont-push-mark)
  "Move point backward to the beginning of previous N top-level form.

The search is controlled by value of `pel-elisp-target-forms' user
option.  That value can be changed for the current session, for all
buffers or only for the current buffer by the command
`pel-elisp-set-navigate-target-form'.

It can also be specified by the TARGET argument: specify one of the
symbols valid for `pel-elisp-target-forms'.

If no valid form is found, don't move point, issue an error describing
the failure unless SILENT is non-nil, in which case the function returns nil
on error and non-nil on success.

On success, the function push original position on the mark ring unless
DONT-PUSH-MARK is non-nil.

The function support shift-marking."
  (interactive "^p")
  (let ((pel-elisp-target-forms (or target pel-elisp-target-forms)))
    (if (< n 0)
        (pel-elisp-beginning-of-next-form (abs n))
      ;; move point past the regexp (n times)
      (condition-case err
          (let ((start-pos (point)))
            (dotimes (_ n)
              (re-search-backward (pel--navigate-target-regxp)))
            ;; move point on the opening paren
            (back-to-indentation)
            (unless dont-push-mark
              (push-mark start-pos))
            ;; On success, return t, nil on failure if silent
            t)
        (search-failed
         (unless silent
           (user-error "No form found: %s" err)))))))

(defun pel-elisp-beginning-of-previous-defun (&optional n)
  "Move point to the beginning of previous N defun form - at any level."
  (interactive "^p")
  (pel-elisp-beginning-of-previous-form n 'defun-forms))

;;; --------------------------------------------------------------------------

(provide 'pel-elisp)

;;; pel-elisp.el ends here
