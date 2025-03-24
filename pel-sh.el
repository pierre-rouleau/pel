;;; pel-sh.el --- PEL shell script support.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, June  7 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-03-24 17:48:07 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2022, 2024, 2025  Pierre Rouleau
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
;; This file provides extra logic to use inside the `sh-mode' major mode.
;;
;; Available functions:
;;
;; - Naïve Quoting functions:
;;   - `pel-sh-double-quote-word'
;;   - `pel-sh-single-quote-word'
;;   - `pel-sh-backtick-quote-word'
;;      - `pel--quote-word'
;;
;; - Shell script source code Enhancement tools:
;;   - `pel-sh-fix-sc2006'
;;
;; - iMenu sh-mode enhancement
;;   - `pel-toggle-accept-hyphen'
;;
;; - Navigate to next/previous function definition
;;   - `pel-sh-next-function'
;;   - `pel-sh-prev-function'
;;
;; Specialized text insertion
;;   - `pel-sh-add-sh-local'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: `pel-toggle-and-show',
                                        ; `pel-current-buffer-file-extension'

(require 'sh-script)                    ; use: `sh-ancestor-alist'
(require 'minibuffer)                   ; use: `format-prompt'

;; ---------------------------------------------------------------------------
(defvar minibuffer-default-prompt-format) ;; prevent byte-compile warnings
                                          ;; caused by conditional defcustom
(unless pel-emacs-28-or-later-p
  ;; format_prompt was incorporated into minibuffer.el in Emacs 28.1,
  ;; written by Stephan Monnier as part of Emacs.
  ;; PEL supports Emacs versions prior to this, so the following is
  ;; a copy of the code part of minibuffer 28.1

  (defcustom minibuffer-default-prompt-format " (default %s)"
    "Format string used to output \"default\" values.
When prompting for input, there will often be a default value,
leading to prompts like \"Number of articles (default 50): \".
The \"default\" part of that prompt is controlled by this
variable, and can be set to, for instance, \" [%s]\" if you want
a shorter displayed prompt, or \"\", if you don't want to display
the default at all.

This variable is used by the `format-prompt' function."
    :group 'minibuffer                  ; added
    :version "28.1"
    :type 'string)

  (defun format-prompt (prompt default &rest format-args)
    "Format PROMPT with DEFAULT according to `minibuffer-default-prompt-format'.
If FORMAT-ARGS is nil, PROMPT is used as a plain string.  If
FORMAT-ARGS is non-nil, PROMPT is used as a format control
string, and FORMAT-ARGS are the arguments to be substituted into
it.  See `format' for details.

Both PROMPT and `minibuffer-default-prompt-format' are run
through `substitute-command-keys' (which see).  In particular,
this means that single quotes may be displayed by equivalent
characters, according to the capabilities of the terminal.

If DEFAULT is a list, the first element is used as the default.
If not, the element is used as is.

If DEFAULT is nil or an empty string, no \"default value\" string
is included in the return value."
    (concat
     (if (null format-args)
         (substitute-command-keys prompt)
       (apply #'format (substitute-command-keys prompt) format-args))
     (and default
          (or (not (stringp default))
              (> (length default) 0))
          (format (substitute-command-keys minibuffer-default-prompt-format)
                  (if (consp default)
                      (car default)
                    default)))
     ": ")))

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel--quote-word (quote-char)
  (if (use-region-p)
      (let ((begin-pos (region-beginning))
            (end-pos (region-end)))
        (goto-char begin-pos)
        (insert quote-char)
        (goto-char (+ end-pos 1))
        (insert quote-char))
    (insert quote-char)
    (forward-word)
    (insert quote-char)))

;;-pel-autoload
(defun pel-sh-double-quote-word ()
  "Surround word at point or selected area with double quotes."
  (interactive "*")
  (pel--quote-word ?\"))

;;-pel-autoload
(defun pel-sh-single-quote-word ()
  "Surround word at point or selected area with single quotes."
  (interactive "*")
  (pel--quote-word ?\'))

;;-pel-autoload
(defun pel-sh-backtick-quote-word ()
  "Surround word at point or selected area with backtick characters."
  (interactive "*")
  (pel--quote-word ?`))

;; ---------------------------------------------------------------------------
;; Source Code Enhancement Tools
;; -----------------------------

;;-pel-autoload
(defun pel-sh-fix-sc2006 ()
  "Replace a back-ticked string with $() as requested by shellcheck SC2006."
  (interactive "*")
  (let ((found-pos (re-search-forward "`\\(.+\\)`" )))
    (when found-pos
      (replace-match "$(\\1)" :fixedcase))))

;; ---------------------------------------------------------------------------
;; Improve iMenu for sh-mode
;; -------------------------
;;
;; POSIX-compliant function names cannot use the dash (also know as hyphen)
;; ('-', 0x2d) ASCII character.
;;
;; According to Stéphane Chazelas, a known expert of shell scripting:
;;
;; "... The following shells are known to support hyphens in function names:
;;
;;     - pdksh and derivatives, bash, zsh
;;     - some ash derivatives such as the sh of FreeBSD (since 2010) or NetBSD (since
;;     - 2016).
;;     - busybox sh when the selected shell at compile time is hush instead of ash.
;;     - csh and tcsh (in their aliases, those shells have no function support).
;;     - rc and derivatives (again with a radically different syntax)
;;     - fish
;;
;;     The following shells are known to explictly not support hyphens
;;     in function names:
;;
;;     - the Bourne shell and derivatives such as ksh88 and bosh
;;     - ksh93, yash, the original ash and some of its derivatives
;;       (busybox ash (the default choice for sh), dash)
;; "
;;
;; Because of that, the Emacs sh-mode code sets the `imenu-generic-expression'
;; variable to no accept the hyphen characters inside the function names and
;; the value of that variable is set to:
;;
;; ((nil "^\\s-*function\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*\\(?:()\\)?" 1)
;;  (nil "^\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*()" 1))
;;
;; The command `pel-toggle-accept-hyphen' toggle acceptance of hyphen inside
;; function names, but not inside variable names. It is often useful to
;; implement user-facing shell commands that use hyphens, as they are easier
;; to type than underscores.  Use this command when writing scripts meant to
;; be executed by shells that accept them.

(defvar-local pel--accept-hyphen nil
  "Accept hyphen and period in function names when non-nil.")

;;-pel-autoload
(defun pel-toggle-accept-hyphen ()
  "Toggle acceptance of hyphen and period in shell function names."
  (interactive)
  ;; Backup the original value of imenu-generic-expression inside pel--accept-hyphen
  ;; when pel--accept-hyphen is not nil and imenu-generic-expression is
  ;; modified.
  (require 'imenu)
  (let ((original-imenu-generic-expression pel--accept-hyphen))
    (pel-toggle-and-show 'pel--accept-hyphen
                         "Accepts hyphens and periods in function names."
                         "Does NOT accept hyphen nor period in function names.")
    (if pel--accept-hyphen
        (progn
          ;; When the variable has just been toggle to t, store the original
          ;; imenu-generic-expression into it, it remains 'true'
          (setq pel--accept-hyphen imenu-generic-expression)
          ;; Then modify `imenu-generic-expression':
          (setq-local imenu-generic-expression
                '((nil "^\\s-*function\\s-+\\([[:alpha:]_][[:alnum:]._-]*\\)\\s-*\\(?:()\\)?" 1)
                  (nil "^\\s-*\\([[:alpha:]_][[:alnum:]._-]*\\)\\s-*()" 1))))
      ;; When the variable has just been toggle to nil restore the value of
      ;; `imenu-generic-expression', restore it from what was just backed-up.
      (setq-local imenu-generic-expression
                  original-imenu-generic-expression))))

;; ---------------------------------------------------------------------------
;; Navigate to next/previous function definition
;; ---------------------------------------------
;;
;; The POSIX standard does not accept hyphen nor period inside function
;; names, but those may be used in bash and zsh shells, the shells this
;; code supports, so the regexp supports these characters inside the function
;; names.


(defconst pel--sh-function-regexp-strict
  "^\\s-*\\(function\\s-*\\)?\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*()"
  "Regexp for shell script function name that accepts . and -")

(defconst pel--sh-function-regexp
  "^\\s-*\\(function\\s-*\\)?\\([[:alpha:]_][[:alnum:]_.-]*\\)\\s-*()"
  "Regexp for shell script function name that accepts . and -")

;;-pel-autoload
(defun pel-sh-next-function ()
  "Move point to the beginning of next function definition.

By default does not accept hyphen and period in function names.
Execute `pel-toggle-accept-hyphen' to change that."
  (interactive)
  (require 'simple)                     ; use: `back-to-indentation'
  (let ((new-pos))
    (save-excursion
      (move-end-of-line 1)
      (when (re-search-forward (if pel--accept-hyphen
                                   pel--sh-function-regexp
                                 pel--sh-function-regexp-strict)
                               nil :noerror)
        (setq new-pos (point))))
    (if new-pos
        (progn
          (goto-char new-pos)
          (back-to-indentation))
      (user-error "Not finding any function below!"))))

;;-pel-autoload
(defun pel-sh-prev-function ()
  "Move point to the beginning of previous function definition.

By default does not accept hyphen and period in function names.
Execute `pel-toggle-accept-hyphen' to change that."
  (interactive)
  (require 'simple)                     ; use: `back-to-indentation'
  (let ((new-pos))
    (save-excursion
      (beginning-of-line 1)
      (when (re-search-backward (if pel--accept-hyphen
                                    pel--sh-function-regexp
                                  pel--sh-function-regexp-strict)
                                nil :noerror)
        (setq new-pos (point))))
    (if new-pos
        (progn
          (goto-char new-pos)
          (back-to-indentation))
      (user-error "Not finding any function above!"))))

;; ---------------------------------------------------------------------------
;; Specialized text insertion

(declare-function format-prompt "pel-sh") ;; prevent by-compiler warning
                                          ;; cased by conditional defun

(defun pel--shell-name ()
  "Return most expected shell name."
  ;; Some shell script files have an extension that identifies
  ;; the shell scripting language; use that if there's one.
  ;; Otherwise use the major mode unless it is fundamental.
  (or (pel-current-buffer-file-extension)
      (let ((name (pel-string-with-major-mode "%s")))
        (if (string-equal name "fundamental")
            "sh"
          name))))

;;-pel-autoload
(defun pel-sh-add-sh-local (shell-name)
  "Insert a sh-shell file-local variable to end of buffer.

Prompts for a shell name, with tab-completion of supported shell
names.  Defaults to the current major mode shell name."
  (interactive (list (completing-read
                      ;; Extract the list of shell names from
                      ;; `sh-ancestor-alist' and use it as the provided
                      ;; choices.
                      (format-prompt "Shell" (pel--shell-name))
                      (append (mapcar (lambda (e) (symbol-name (car e)))
                                      sh-ancestor-alist)
                              '("csh" "rc" "sh"))
                      nil nil nil nil (pel--shell-name))))
  (save-excursion
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (insert (format "\
# Local Variables:
# sh-shell: %s
# End:
" shell-name))))


;;; --------------------------------------------------------------------------
(provide 'pel-sh)

;;; pel-sh.el ends here
