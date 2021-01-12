;;; pel-elisp.el --- Emacs Lisp support.  -*- lexical-binding: t; -*-

;; Created   : Friday, November 27 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-01-12 10:49:17, updated by Pierre Rouleau>

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
;; This file define a set of functions used to provide precise navigation
;; across Emacs Lisp and Lisp definition forms in a way that is more flexible
;; and convenient than the standard Emacs defun navigation functions like
;; `beginning-of-defun' and `end-of-defun'.
;;
;; The standard `beginning-of-defun' and `end-of-defun' only navigate across
;; any top-level form.  They do not discriminate between a defun, a defmacro
;; or even an unless form.  They do not skip forms inside docstrings unless
;; the `open-paren-in-column-0-is-defun-start' is set to nil.
;;
;; The file provides the `pel-toggle-paren-in-column-0-is-defun-start' command
;; to help toggle that user-option.
;;
;; It also provides the following functions that implement a more powerful and
;; flexible navigation command set, providing the user that ability to choose
;; the behaviour of the navigation commands.  The behaviour is controlled by
;; two user-option customized variables:
;;
;;  - `pel-elisp-target-forms' and
;;  - `pel-elisp-user-specified-targets'.
;;
;; With `pel-elisp-target-forms' you can select one of 6 types of targets:
;; - Selection 0 is equivalent to Emacs standard behaviour: any top-level
;;   form.
;; - Selection 1 specifies only top-level defun forms.
;; - Selection 2 specifies any defun forms (but only defun forms).
;; - Selection 3 specifies defun, defmacro, defsubst, defalias and defadvice
;;   forms (at any level).
;; - Selection 4 adds defclass, defmethod and defgeneric forms to what's
;;   available in Selection 3.
;; - Selection 5 adds all variable definition forms to whats available in
;;   selection 4.
;; - Selection 6 is a user-specified set.  They are specified in the list
;;   specified by `pel-elisp-user-specified-targets', giving the user ultimate
;;   control of what can be a target.
;;
;; Note that `pel-elisp-user-specified-targets' user interface is a set of
;; radio buttons controlled list.  The default lists all the forms present in
;; selection 5 with some specialized additions such as support for Hydra
;; forms.  You can add more.  You can also remove some.  But you can disable
;; any, without removing the string by just disabling it with the radio
;; button.  This places a 'nil' in the list which is ignored by the code. It's
;; a convenient way to specify forms that you can quickly activate or
;; de-activate as target.
;;
;; The targets specify how the following 2 commands behave:
;;
;;   - `pel-elisp-beginning-of-next-form'
;;   - `pel-elisp-beginning-of-previous-form'
;;
;; The customizable user-option variables are turned into global or
;; user-buffer local variables in any buffer where you issue the
;; `pel-elisp-user-specified-targets' command.  It then becomes possible to
;; select different behaviour of the navigation commands.  You could for
;; example select to use selection 2 (only defuns at any level) as the
;; persistent and default behaviour for the commands and then modify the
;; behaviour for some of the buffers you are editing.  Or use another default
;; since this specific setting is forced by the following specialized
;; commands:
;;
;;  - `pel-elisp-beginning-of-next-defun'
;;  - `pel-elisp-beginning-of-previous-defun'
;;
;; The commands (*) and function hierarchy of this file look like this:
;;
;; * `pel-toggle-paren-in-column-0-is-defun-start'
;;
;; * `pel-elisp-set-navigate-target-form'
;; *`pel-elisp-beginning-of-next-defun'
;;   * `pel-elisp-beginning-of-next-form'
;; * `pel-elisp-beginning-of-previous-defun'
;;   * `pel-elisp-beginning-of-previous-form'
;;     - `pel--navigate-target-regxp'
;;       - `pel--elisp-form-regexp-for'
;;     - `pel-point-in-comment-or-docstring'
;;
;;
;; Credits:  Thanks to Andreas Röhler to mention the Standard Emacs variable
;;           `open-paren-in-column-0-is-defun-start'! That allowed me to
;;           replace a large amount of code with a simple toggle command and
;;           that handles the problem I had with checkdoc as well!
;;           I also learned the existence and usefulness of parse-partial-sexp
;;           by looking at his code.

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
modifies the behaviour for all buffers.  The change in behaviour does not
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

(defun pel-point-in-comment-or-docstring ()
  "Return position of start of comment or docstring surrounding point.
Return nil when point is outside comment and docstring."
  (nth 8 (parse-partial-sexp (point-min) (point))))

;;-pel-autoload
(defun pel-elisp-beginning-of-next-form
    (&optional n target silent dont-push-mark)
  "Move point forward to the beginning of next N top level form.

The search is controlled by the value of `pel-elisp-target-forms' user
option.  That value can be changed for the current session, for all
buffers or only for the current buffer by the command
`pel-elisp-set-navigate-target-form'.
It can also be specified by the TARGET argument: specify one of the
symbols valid for `pel-elisp-target-forms'.

The function skips over forms inside docstrings.

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
              (while
                  (progn
                    (when (eq (following-char) 40) ; if following char is '('
                      (right-char 1))  ; make sure point is past the open '('
                    (re-search-forward (pel--navigate-target-regxp))
                    (pel-point-in-comment-or-docstring))))
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
  "Move point to the beginning of next N defun form - at any level.
Skip over forms located inside docstrings."
  (interactive "^p")
  (pel-elisp-beginning-of-next-form n 'defun-forms))

;;-pel-autoload
(defun pel-elisp-beginning-of-previous-form
    (&optional n target silent dont-push-mark)
  "Move point backward to the beginning of previous N top level form.

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
              (while
                  (progn
                    (re-search-backward (pel--navigate-target-regxp))
                    (pel-point-in-comment-or-docstring))))
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
  "Move point to the beginning of previous N defun form - at any level.
Skip over forms located inside docstrings."
  (interactive "^p")
  (pel-elisp-beginning-of-previous-form n 'defun-forms))

;;; --------------------------------------------------------------------------

(provide 'pel-elisp)

;;; pel-elisp.el ends here
