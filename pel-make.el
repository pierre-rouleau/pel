;;; pel-make.el --- Makefile editing support.  -*- lexical-binding: t; -*-

;; Created   : Friday, January 15 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-07-06 16:03:58 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2022  Pierre Rouleau
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
;; This file provides commands to navigate inside make files.  These
;; complements what the standard Emacs make-mode.el provides.  The provided
;; commands are:
;;
;;  * `pel-make-next-macro'
;;  * `pel-make-previous-macro'
;;
;; These 2 commands navigate across the macro definition statements. They
;; support shift marking and can be used to count the number of make macro
;; statements.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)     ; use: `pel-count-string'
(require 'pel-syntax)    ; use: `pel-syntax-conditional-forward'
;;                       ;      `pel-syntax-conditional-backward'
(require 'make-mode)     ; for nmake support @ end of file
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defconst pel-make-macro-regxp "^[ \t]*\\(\\_<[_[:alpha:]][_[:alnum:]]+\\_>\\) *[?:]*="
  "Regexp used to search make file macro definitions.")

;; --
;;-pel-autoload
(defun pel-make-next-macro (&optional n silent dont-push-mark)
  "Move to the beginning of next N make file macro definition statement.

The function skips over comments.

If no valid form is found, don't move point, issue an error
describing the failure unless SILENT is non-nil, in which case
the function returns nil on error and non-nil on success.
The error message states the number of instanced searched, the
regexp used and the number of instances found.

On success, the function push original position on the mark ring
unless DONT-PUSH-MARK is non-nil.

The command support shift-marking."
  (interactive "^p")
  (if (< n 0)
      (pel-make-previous-macro (abs n))
    (let ((start-pos (point))
          (count 0))
      (condition-case err
          (progn
            (dotimes (_ n)
              (end-of-line) ; don't stay on current definition line if cursor is at bol
              (re-search-forward pel-make-macro-regxp)
              (setq count (1+ count)))
            (back-to-indentation)
            (unless dont-push-mark
                (push-mark start-pos))
            t)
        (search-failed
         ;; restore original position when search failed
         (goto-char start-pos)
         (unless silent
           (user-error "Found only %d of requested %s:\n%s"
                       count
                       (pel-count-string n "macro definition statement")
                       err)))))))

;;-pel-autoload
(defun pel-make-previous-macro (&optional n silent dont-push-mark)
  "Move to the beginning of previous N make file macro definition statement.

The function skips over comments.

If no valid form is found, don't move point, issue an error
describing the failure unless SILENT is non-nil, in which case
the function returns nil on error and non-nil on success.
The error message states the number of instanced searched, the
regexp used and the number of instances found.

On success, the function push original position on the mark ring
unless DONT-PUSH-MARK is non-nil.

The command support shift-marking."
  (interactive "^p")
  (if (< n 0)
      (pel-make-previous-macro (abs n))
    (let ((start-pos (point))
          (count 0))
      (condition-case err
          (progn
            (dotimes (_ n)
              (re-search-backward pel-make-macro-regxp)
              (setq count (1+ count)))
            (back-to-indentation)
            (unless dont-push-mark
                (push-mark start-pos))
            t)
        (search-failed
         ;; restore original position when search failed
         (goto-char start-pos)
         (unless silent
           (user-error "Found only %d of requested %s:\n%s"
                       count
                       (pel-count-string n "macro definition statement")
                       err)))))))

;; ---------------------------------------------------------------------------

;; G1---------------------------------------------------------------------------------------------|
;;                 G2----------------------------------------------------------------------------|
;;                    g3------|
;;                                  G4--------------------------------------------------------|
;;                                     G5---------------------------------------|
;;                                        g6-----|      g7----|      g8------|
;;                 (                                                                             )
;;                    (       )     (                                                         )
;;                                     (                                        )
;;                                        (       )     (      )     (       )
;; "^[[:blank:]]*\\(\\(endif\\)\\|\\(\\(\\(ifdef\\)\\|\\(ifeq\\)\\|\\(ifneq\\)\\)[[:blank:]]\\)\\)"
;;                    |%s     |        |%s                                      |
;;                                  |                                                         |
(defconst pel--make-conditional-regexp-format
  "^[[:blank:]]*\\(\\(%s\\)\\|\\(%s\\)\\)"
  "Regexp to find make conditionals")

(defconst pel--make-if-regexp
  "\\(\\(ifdef\\)\\|\\(ifeq\\)\\|\\(ifneq\\)\\)[[:blank:]]"
  "The Make if statements.")

(defconst pel--make-conditional-group-forward 3
  "Significant matching group when searching end of make conditional.")

(defconst pel--make-conditional-group-backward 4
  "Significant matching group when searching beginning of make conditional.")


(defun pel--after-else-p ()
  "Return t if point is on the same line and after a `else'."
  (let ((original-pos (point))
        (else-pos nil))
    (save-excursion
      (let ((eol-pos (progn
                       (end-of-line nil)
                       (point))))
        (beginning-of-line nil)
        (when (re-search-forward
               "[[:blank:]]?\\(else\\)\\([#[:blank:]].*\\)?$"
               eol-pos
               :no-error)
          ;; store position of the end of `else' in else-pos
          (setq else-pos (nth 3 (match-data))))))
    (and else-pos
         (>= original-pos else-pos))))

;;-pel-autoload
(defun pel-make-forward-conditional (&optional to-else)
  "Move point forward to matching end of make conditional.

If a command prefix TO-ELSE is specified, move point forward
after the matching else statement instead.

On success, push the original position on the mark ring and
return the new position. On error, issue user error on mismatch."
  (interactive "^P")
  (when (pel--after-else-p)
    (beginning-of-line nil)
    (setq to-else nil))

  (pel-syntax-conditional-forward
   (format pel--make-conditional-regexp-format
           (if to-else
               "else"
             "endif")
           pel--make-if-regexp)
   pel--make-conditional-group-forward
   (if to-else
       "else statement"
     "end statement")))

(defun pel-make-backward-conditional (&optional to-else)
  "Move point backward to matching beginning of make conditional.

If a command prefix TO-ELSE is specified, move point backward
after the matching else statement instead.

On success, push the original position on the mark ring and
return the new position. On error, issue user error on mismatch."
  (interactive "^P")
  ;; When point is on the same line and after a `else' statement keyword, then
  ;; to move backward to its matching `if' statement will not work directly
  ;; because point is inside the scope: to handle this situation move to the
  ;; matching `endif' and then move back to it's matching `if'.
  (when (pel--after-else-p)
    (pel-make-forward-conditional)
    (setq to-else nil))

  (pel-syntax-conditional-backward
   (format pel--make-conditional-regexp-format
           "endif"
           (if to-else
               "else"
             pel--make-if-regexp))
   pel--make-conditional-group-backward
   (if to-else
       "else statement"
     "beginning of if statement"))
  ;; When moving back to an else statement, place point after the else
  ;; statement to allow moving backward to the matching if in the next
  ;; command.
  (when to-else
    (end-of-line nil)))

;; ---------------------------------------------------------------------------
;; NMake format support
;; --------------------
;; Credit:
;;  Original code taken from:     https://www.emacswiki.org/emacs/MakefileMode
;;  Author:                       https://www.emacswiki.org/emacs/RolfUnger
;; Updates as per Visual Studio 2019 specs for Nmake

;; TODO: the nmake specs state that the ! must be at the beginning of the line
;; but it can be followed by spaces and tab characters.  Unfortunately the
;; makefile-mode code does not support regexp.
;; Change the code to accept regexp and support the full syntax.
(defconst makefile-nmake-statements
  `("!cmdswitches"   "!CMDSWITCHES"
    "!else"          "!ELSE"
    "!elseif"        "!ELSEIF"
    "!elseifdef"     "!ELSEIFDEF"
    "!endif"         "!ENDIF"
    "!error"         "!ERROR"
    "!if"            "!IF"
    "!ifdef"         "!IFDEF"
    "!ifndef"        "!IFNDEF"
    "!include"       "!INCLUDE"
    "!message"       "!MESSAGE"
    "!undef"         "!UNDEF"
    ,@makefile-statements)
  "List of keywords understood by nmake.")

(defconst makefile-nmake-font-lock-keywords
  (makefile-make-font-lock-keywords
   makefile-var-use-regex
   makefile-nmake-statements
   t))

;;-pel-autoload
(define-derived-mode makefile-nmake-mode makefile-mode "Nmake"
  "An adapted `makefile-mode' that knows about nmake."
  (setq font-lock-defaults
        `(makefile-nmake-font-lock-keywords ,@(cdr font-lock-defaults))))

(define-key makefile-mode-map "\C-c\C-m\C-n" 'makefile-nmake-mode)

;;; --------------------------------------------------------------------------
(provide 'pel-make)

;;; pel-make.el ends here
