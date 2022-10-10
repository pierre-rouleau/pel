;;; pel-c-utils.el --- C/C++ Code Processing utilities.  -*- lexical-binding: t; -*-

;; Created   : Sunday, October  9 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-10-10 10:55:23 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2022  Pierre Rouleau
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
;; This file provides a set of utility functions used to detect and correct
;; C and C++ code that explicitly compares a pointer against NULL and a
;; boolean value against true or false.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--base)
(eval-when-compile
  (require 'subr-x))   ; use: string-join

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-c-search-equal-NULL ()
  "Search for code comparing '== NULL'"
  (interactive)
  (re-search-forward "\\(NULL *==\\)\\|\\(== *NULL\\)"))

(defun pel-c-search-not-equal-NULL ()
  "Search for code comparing '!= NULL'"
  (interactive)
  (re-search-forward "\\(NULL *!=\\)\\|\\(!= *NULL\\)"))

(defun pel-c-search-equal-true ()
  "Search for code comparing '== true'"
  (interactive)
  (re-search-forward
   (string-join '("\\(true *==\\)\\|\\(== *true\\)"
                  "\\(TRUE *==\\)\\|\\(== *TRUE\\)")
                "\\|")))

(defun pel-c-search-not-equal-true ()
  "Search for code comparing '!= true'"
  (interactive)
  (re-search-forward
   (string-join '("\\(true *!=\\)\\|\\(!= *true\\)"
                  "\\(TRUE *!=\\)\\|\\(!= *TRUE\\)")
                "\\|")))

(defun pel-c-search-equal-false ()
  "Search for code comparing '== false'"
  (interactive)
  (re-search-forward
   (string-join '("\\(false *==\\)\\|\\(== *false\\)"
                  "\\(FALSE *==\\)\\|\\(== *FALSE\\)")
                "\\|")))

(defun pel-c-search-not-equal-false ()
  "Search for code comparing '!= false'"
  (interactive)
  (re-search-forward
   (string-join '("\\(false *!=\\)\\|\\(!= *false\\)"
                  "\\(FALSE *!=\\)\\|\\(!= *FALSE\\)")
                "\\|")))

(defun pel-c-search-any-comparison-problem ()
  "Search for any problematic comparing code."
  (interactive)
  (re-search-forward
   (string-join '("\\(NULL *==\\)\\|\\(== *NULL\\)"
                  "\\(NULL *!=\\)\\|\\(!= *NULL\\)"
                  "\\(true *==\\)\\|\\(== *true\\)"
                  "\\(TRUE *==\\)\\|\\(== *TRUE\\)"
                  "\\(true *!=\\)\\|\\(!= *true\\)"
                  "\\(TRUE *!=\\)\\|\\(!= *TRUE\\)"
                  "\\(false *==\\)\\|\\(== *false\\)"
                  "\\(FALSE *==\\)\\|\\(== *FALSE\\)"
                  "\\(false *!=\\)\\|\\(!= *false\\)"
                  "\\(FALSE *!=\\)\\|\\(!= *FALSE\\)")
                "\\|")))

;; ---------------------------------------------------------------------------

(defun pel--c-replace (keywords format-regexp rep-regexp)
  "Replace text identified by FORMAT-REGEXP by REP_REGEXP for all KEYWORDS.

Return number of expression replaced."

  (let ((change-count 0))
    (dolist (keyword keywords)
      (while (progn
               (goto-char (point-min))
               (re-search-forward (format format-regexp keyword)
                                  nil :noerror))
        (replace-match rep-regexp :fixedcase)
        (setq change-count (1+ change-count))))
    change-count))

(defun pel--c-adjusted (regex)
  "Return adjusted regex.

Replace the %s  by what is required for C++ class support.
Return adjusted regexp."
  (format regex (if (eq major-mode 'c++-mode)
                    "\\([_[:alpha:]][_[:alnum:]]*::\\)*"
                  "")))

(defun pel-c-fix-comparison-problems ()
  "Fix C/C++ code that compares pointer explicitly to NULL, false and true.

The regexp used by this command are able to match relatively simple C and C++
expressions including expressions using pointer and references and one nesting
level of functions calls with out without arguments.  However, it does *not*
match everything you can express in C or C++!

Therefore, after using this command check if anything is left.
Also always compare with  the original code to ensure everything is fine!"
  (interactive)

  ;; First implementation: naive/repetitive implementation.
  ;; todo: reduce code repetition by generating the regexp for each of the keywords
  (let ((equal-NULL-count 0)
        (not-equal-NULL-count 0)
        (equal-false-count 0)
        (not-equal-false-count 0)
        (equal-true-count 0)
        (not-equal-true-count 0))
    ;; 1 ------------------------------------------------
    ;; Replace `if (ptr != NULL)' by `if (ptr)'
    (while (progn
             (goto-char (point-min))
             (re-search-forward "\\(NULL *!= *\\)\\|\\( *!= *NULL\\)"
                                nil :noerror))
      (replace-match "" nil :literal)
      (setq not-equal-NULL-count (1+ not-equal-NULL-count)))
    ;; 2 ------------------------------------------------
    ;; Replace `if (ptr == NULL)' by `if (!ptr)'
    (setq equal-NULL-count
          (pel--c-replace '("NULL")
                          (pel--c-adjusted
                           "\\(%s[_[:alpha:]][_[:alnum:]+*>.-]*\\(([_[:alnum:] ,+*>.-]*)\\)*?\\) *== *NULL")
                          "!\\1"))
    ;; 3 ------------------------------------------------
    ;; Replace `if (NULL == ptr)' by `if (!ptr)'
    (pel+= equal-NULL-count
           (pel--c-replace '("NULL")
                           (pel--c-adjusted
                            "%%s *== *\\(%s[_[:alpha:]][_[:alnum:]+*>.-]*\\(([_[:alnum:] ,+*>.-]*)\\)*\\)")
                           "!\\1"))
    ;; 1 ------------------------------------------------
    ;; Replace `if (boolean != false)' by `if (boolean)'
    ;; Replace `if (boolean != False)' by `if (boolean)'
    (setq not-equal-false-count
          (pel--c-replace '("false" "FALSE")
                          (pel--c-adjusted
                           "\\(%s[_[:alpha:]][_[:alnum:]+*>.-]*\\(([_[:alnum:] ,+*>.-]*)\\)*?\\) *!= *%%s")
                          "\\1"))
    ;; 2 ------------------------------------------------
    ;; Replace `if (false != boolean)' by `if (boolean)'
    ;; Replace `if (FALSE != boolean)' by `if (boolean)'
    (pel+= not-equal-false-count
           (pel--c-replace '("false" "FALSE")
                           (pel--c-adjusted
                            "%%s *!= *\\(%s[_[:alpha:]][_[:alnum:]+*>.-]*\\(([_[:alnum:] ,+*>.-]*)\\)*\\)")
                           "\\1"))
    ;; 3 ------------------------------------------------
    ;; Replace `if (boolean == false)' by `if (!boolean)'
    ;; Replace `if (boolean == FALSE)' by `if (!boolean)'
    (setq equal-false-count
          (pel--c-replace '("false" "FALSE")
                          (pel--c-adjusted
                           "\\(%s[_[:alpha:]][_[:alnum:]+*>.-]*\\(([_[:alnum:] ,+*>.-]*)\\)*?\\) *== *%%s")
                          "!\\1"))
    ;; 4 ------------------------------------------------
    ;; Replace `if (false == boolean)' by `if (!boolean)'
    ;; Replace `if (FALSE == boolean)' by `if (!boolean)'
    (pel+= equal-false-count
           (pel--c-replace '("false" "FALSE")
                           (pel--c-adjusted
                            "%%s *== *\\(%s[_[:alpha:]][_[:alnum:]+*>.-]*\\(([_[:alnum:] ,+*>.-]*)\\)*\\)")
                           "!\\1"))
    ;; 1  ------------------------------------------------
    ;; Replace `if (boolean != true)' by `if (!boolean)'
    ;; Replace `if (boolean != TRUE)' by `if (!boolean)'
    (setq not-equal-true-count
          (pel--c-replace '("true" "TRUE")
                          (pel--c-adjusted
                           "\\(%s[_[:alpha:]][_[:alnum:]+*>.-]*\\(([_[:alnum:] ,+*>.-]*)\\)*?\\) *!= *%%s")
                          "!\\1"))
    ;; 2 ------------------------------------------------
    ;; Replace `if (true != boolean)' by `if (!boolean)'
    ;; Replace `if (TRUE != boolean)' by `if (!boolean)'
    (pel+= not-equal-true-count
           (pel--c-replace '("true" "TRUE")
                           (pel--c-adjusted
                            "%%s *!= *\\(%s[_[:alpha:]][_[:alnum:]+*>.-]*\\(([_[:alnum:] ,+*>.-]*)\\)*\\)")
                           "!\\1"))
    ;; 3 ------------------------------------------------
    ;; Replace `if (boolean == true)' by `if (boolean)'
    ;; Replace `if (boolean == TRUE)' by `if (boolean)'
    (setq equal-true-count
          (pel--c-replace '("true" "TRUE")
                          (pel--c-adjusted
                           "\\(%s[_[:alpha:]][_[:alnum:]+*>.-]*\\(([_[:alnum:] ,+*>.-]*)\\)*?\\) *== *%%s")
                          "\\1"))
    ;; 4 ------------------------------------------------
    ;; Replace `if (true == boolean)' by `if (boolean)'
    ;; Replace `if (TRUE == boolean)' by `if (boolean)'
    (pel+= equal-true-count
           (pel--c-replace '("true" "TRUE")
                           (pel--c-adjusted
                            "%%s *== *\\(%s[_[:alpha:]][_[:alnum:]+*>.-]*\\(([_[:alnum:] ,+*>.-]*)\\)*\\)")
                           "\\1"))
    ;; ------------------------------------------------
    ;; Print report of operation
    (message "Fixed: %d '==NULL', %d '!=NULL', %d '==false', %d '!=false',%d '==true', %d '!=true'"
             equal-NULL-count
             not-equal-NULL-count
             equal-false-count
             not-equal-false-count
             equal-true-count
             not-equal-true-count)

    ;; return rep-regexp counts for use in potential caller
    (list equal-NULL-count
          not-equal-NULL-count
          equal-false-count
          not-equal-false-count
          equal-true-count
          not-equal-true-count)))

;;; --------------------------------------------------------------------------
  (provide 'pel-c-utils)

;;; pel-c-utils.el ends here
