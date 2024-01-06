;;; pel-c-utils.el --- C/C++ Code Correction Utilities.  -*- lexical-binding: t; -*-

;; Created   : Sunday, October  9 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2024-01-05 20:05:27 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2022, 2024  Pierre Rouleau
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
(require 'pel--syntax-macros)
(eval-when-compile
  (require 'subr-x))   ; use: string-join
(require 'syntax)     ; syntax always available, even in emacs -Q

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-c-search-equal-NULL ()
  "Search for code comparing '== NULL'"
  (interactive)
  (re-search-forward "\\(NULL[[:blank:]]*==\\)\\|\\(==[[:blank:]]*NULL\\)"))

(defun pel-c-search-not-equal-NULL ()
  "Search for code comparing '!= NULL'"
  (interactive)
  (re-search-forward "\\(NULL[[:blank:]]*!=\\)\\|\\(!=[[:blank:]]*NULL\\)"))

(defun pel-c-search-equal-true ()
  "Search for code comparing '== true'"
  (interactive)
  (re-search-forward
   (string-join '("\\(true[[:blank:]]*==\\)\\|\\(==[[:blank:]]*true\\)"
                  "\\(TRUE[[:blank:]]*==\\)\\|\\(==[[:blank:]]*TRUE\\)")
                "\\|")))

(defun pel-c-search-not-equal-true ()
  "Search for code comparing '!= true'"
  (interactive)
  (re-search-forward
   (string-join '("\\(true[[:blank:]]*!=\\)\\|\\(!=[[:blank:]]*true\\)"
                  "\\(TRUE[[:blank:]]*!=\\)\\|\\(!=[[:blank:]]*TRUE\\)")
                "\\|")))

(defun pel-c-search-equal-false ()
  "Search for code comparing '== false'"
  (interactive)
  (re-search-forward
   (string-join '("\\(false[[:blank:]]*==\\)\\|\\(==[[:blank:]]*false\\)"
                  "\\(FALSE[[:blank:]]*==\\)\\|\\(==[[:blank:]]*FALSE\\)")
                "\\|")))

(defun pel-c-search-not-equal-false ()
  "Search for code comparing '!= false'"
  (interactive)
  (re-search-forward
   (string-join '("\\(false[[:blank:]]*!=\\)\\|\\(!=[[:blank:]]*false\\)"
                  "\\(FALSE[[:blank:]]*!=\\)\\|\\(!=[[:blank:]]*FALSE\\)")
                "\\|")))

(defun pel-c-search-any-comparison-problem ()
  "Search for any problematic comparing code."
  (interactive)
  (re-search-forward
   (string-join '("\\(NULL[[:blank:]]*==\\)\\|\\(==[[:blank:]]*NULL\\)"
                  "\\(NULL[[:blank:]]*!=\\)\\|\\(!=[[:blank:]]*NULL\\)"
                  "\\(true[[:blank:]]*==\\)\\|\\(==[[:blank:]]*true\\)"
                  "\\(TRUE[[:blank:]]*==\\)\\|\\(==[[:blank:]]*TRUE\\)"
                  "\\(true[[:blank:]]*!=\\)\\|\\(!=[[:blank:]]*true\\)"
                  "\\(TRUE[[:blank:]]*!=\\)\\|\\(!=[[:blank:]]*TRUE\\)"
                  "\\(false[[:blank:]]*==\\)\\|\\(==[[:blank:]]*false\\)"
                  "\\(FALSE[[:blank:]]*==\\)\\|\\(==[[:blank:]]*FALSE\\)"
                  "\\(false[[:blank:]]*!=\\)\\|\\(!=[[:blank:]]*false\\)"
                  "\\(FALSE[[:blank:]]*!=\\)\\|\\(!=[[:blank:]]*FALSE\\)")
                "\\|")))

;; ---------------------------------------------------------------------------

(defun pel--c-reformat-cond ()
  "Reformat if statement on current line for 'if(...)' to if '(...)'.

Also reformat:
 'for(...)'   to 'for (...)'
 'while(...)' to 'while (...)'."
  (save-excursion
    (let ((end-pos (progn (end-of-line) (point)))
          (start-pos (progn (beginning-of-line) (point))))
      (narrow-to-region start-pos end-pos)
      (dolist (pair '(("if("    "if (")
                      ("while(" "while (")
                      ("for("   "for (")))
        (beginning-of-line)
        (when (re-search-forward (car pair) nil :noerror)
          (replace-match (cadr pair)) :fixedcase :literally))
      (widen))))


(defun pel---c-replace (regexp rep-regexp)
  "Replace text identified by REGEXP by REP_REGEXP in all buffer.

Return number of changes."
  (let ((change-count 0)
        (orig-pos nil)
        (syntax nil))
    (goto-char (point-min))
    (while (progn
             (setq orig-pos (point))
             (re-search-forward regexp
                                nil :noerror))
      ;; Re-write found expression unless it's in comment or string
      (if (progn
            (setq syntax (syntax-ppss))
            (or (pel--inside-string-p syntax)
                (pel--inside-comment-p syntax)))
          ;; skip expressions found in comment or string
          (right-char 1)
        ;; Found one spot to fix: because the regex does not catch
        ;; everything: fix the line if that needs to be fixed, go
        ;; back and do the search again to now match properly.
        (pel--c-reformat-cond)
        (goto-char orig-pos)
        (when (re-search-forward regexp
                                 nil :noerror)
          (replace-match rep-regexp :fixedcase)
          (setq change-count (1+ change-count)))))
    change-count))

(defun pel--c-replace (keywords format-regexp rep-regexp)
  "Replace text identified by FORMAT-REGEXP by REP_REGEXP for all KEYWORDS.

Return number of expression replaced."

  (let ((change-count 0))
    (dolist (keyword keywords)
      (pel+= change-count
             (pel---c-replace (format format-regexp keyword)
                              rep-regexp)))
    change-count))

(defun pel--c-adjusted (regex)
  "Return adjusted regex.

Replace the '%s' by what is required for C++ class support.
Return adjusted regexp."
  (format regex (if (eq major-mode 'c++-mode)
                    "\\([_[:alpha:]][_[:alnum:]]*::\\)*"
                  "")))

(defun pel-c-fix-comparison-problems ()
  "Fix C/C++ code that compares pointer explicitly to NULL, false and true.

The regexp used by this command are able to match relatively
simple C and C++ expressions including expressions using pointer
and references and one nesting level of functions calls with out
without arguments.  However, it does *not* match everything you
can express in C or C++!

Therefore, after using this command check if any expressions that
should be replaced is left. Also always compare with the original
code to ensure everything is fine!

The best way to feel safe about the modified code is to compile
before and after the change, comparing the generated assembler
code.  There should be NO difference in the generated assembler
code.  With GCC use 'objdump --disassemble' on the generate
object code file to generate the assembler file."
  (interactive "*")
  ;; First implementation: naive/repetitive implementation.
  ;; todo: reduce code repetition by generating the regexp for each of the
  ;; keywords
  (message "Reformatting C/C++ code...")
  (save-excursion
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
               (re-search-forward "\\(NULL[[:blank:]]*!=[[:blank:]]*\\)\\|\\([[:blank:]]*!=[[:blank:]]*NULL\\)"
                                  nil :noerror))
        (replace-match "" nil :literal)
        (setq not-equal-NULL-count (1+ not-equal-NULL-count)))
      ;; 2 ------------------------------------------------
      ;; Replace `if (ptr == NULL)' by `if (!ptr)'
      (setq equal-NULL-count
            (pel--c-replace '("NULL")
                            (pel--c-adjusted
                             "\\(%s[_[:alpha:]][_[:alnum:]+*/>.-]*\\(\\s(.*?\\s)\\)*?\\)[[:blank:]]*==[[:blank:]]*NULL")
                            "!\\1"))
      ;; 3 ------------------------------------------------
      ;; Replace `if (NULL == ptr)' by `if (!ptr)'
      (pel+= equal-NULL-count
             (pel--c-replace '("NULL")
                             (pel--c-adjusted
                              "%%s[[:blank:]]*==[[:blank:]]*\\(%s[_[:alpha:]][_[:alnum:]+*>.-]*\\(\\s(.*?\\s)\\)*\\)")
                             "!\\1"))
      ;; 1 ------------------------------------------------
      ;; Replace `if (boolean != false)' by `if (boolean)'
      ;; Replace `if (boolean != False)' by `if (boolean)'
      (setq not-equal-false-count
            (pel--c-replace '("false" "FALSE")
                            (pel--c-adjusted
                             "\\(%s[_[:alpha:]][_[:alnum:]+*/>.-]*\\(\\s(.*?\\s)\\)*?\\)[[:blank:]]*!=[[:blank:]]*%%s")
                            "\\1"))
      ;; 2 ------------------------------------------------
      ;; Replace `if (false != boolean)' by `if (boolean)'
      ;; Replace `if (FALSE != boolean)' by `if (boolean)'
      (pel+= not-equal-false-count
             (pel--c-replace '("false" "FALSE")
                             (pel--c-adjusted
                              "%%s[[:blank:]]*!=[[:blank:]]*\\(%s[_[:alpha:]][_[:alnum:]+*>.-]*\\(\\s(.*?\\s)\\)*\\)")
                             "\\1"))
      ;; 3 ------------------------------------------------
      ;; Replace `if (boolean == false)' by `if (!boolean)'
      ;; Replace `if (boolean == FALSE)' by `if (!boolean)'
      (setq equal-false-count
            (pel--c-replace '("false" "FALSE")
                            (pel--c-adjusted
                             "\\(%s[_[:alpha:]][_[:alnum:]+*/>.-]*\\(\\s(.*?\\s)\\)*?\\)[[:blank:]]*==[[:blank:]]*%%s")
                            "!\\1"))
      ;; 4 ------------------------------------------------
      ;; Replace `if (false == boolean)' by `if (!boolean)'
      ;; Replace `if (FALSE == boolean)' by `if (!boolean)'
      (pel+= equal-false-count
             (pel--c-replace '("false" "FALSE")
                             (pel--c-adjusted
                              "%%s[[:blank:]]*==[[:blank:]]*\\(%s[_[:alpha:]][_[:alnum:]+*>.-]*\\(\\s(.*?\\s)\\)*\\)")
                             "!\\1"))
      ;; 1  ------------------------------------------------
      ;; Replace `if (boolean != true)' by `if (!boolean)'
      ;; Replace `if (boolean != TRUE)' by `if (!boolean)'
      (setq not-equal-true-count
            (pel--c-replace '("true" "TRUE")
                            (pel--c-adjusted
                             "\\(%s[_[:alpha:]][_[:alnum:]+*/>.-]*\\(\\s(.*?\\s)\\)*?\\)[[:blank:]]*!=[[:blank:]]*%%s")
                            "!\\1"))
      ;; 2 ------------------------------------------------
      ;; Replace `if (true != boolean)' by `if (!boolean)'
      ;; Replace `if (TRUE != boolean)' by `if (!boolean)'
      (pel+= not-equal-true-count
             (pel--c-replace '("true" "TRUE")
                             (pel--c-adjusted
                              "%%s[[:blank:]]*!=[[:blank:]]*\\(%s[_[:alpha:]][_[:alnum:]+*>.-]*\\(\\s(.*?\\s)\\)*\\)")
                             "!\\1"))
      ;; 3 ------------------------------------------------
      ;; Replace `if (boolean == true)' by `if (boolean)'
      ;; Replace `if (boolean == TRUE)' by `if (boolean)'
      (setq equal-true-count
            (pel--c-replace '("true" "TRUE")
                            (pel--c-adjusted
                             "\\(%s[_[:alpha:]][_[:alnum:]+*>.-]*\\(\\s(.*?\\s)\\)*?\\)[[:blank:]]*==[[:blank:]]*%%s")
                            "\\1"))
      ;; 4 ------------------------------------------------
      ;; Replace `if (true == boolean)' by `if (boolean)'
      ;; Replace `if (TRUE == boolean)' by `if (boolean)'
      (pel+= equal-true-count
             (pel--c-replace '("true" "TRUE")
                             (pel--c-adjusted
                              "%%s[[:blank:]]*==[[:blank:]]*\\(%s[_[:alpha:]][_[:alnum:]+*>.-]*\\(\\s(.*?\\s)\\)*\\)")
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
            not-equal-true-count))))

;; ---------------------------------------------------------------------------
(defconst pel-preproc-if-regexp
  ;;  1                  2                               3    <<-- group numbers
  "^\\([[:blank:]]*\\)#\\([[:blank:]]*\\)if[[:blank:]]+\\([[:alpha:]_][[:alnum:]_]+\\)[[:blank:]]*$"
  "Regxp to locate '#if VAR'.")

(defconst pel-preproc-if-eq-regexp-format
  ;;  1                  2                               3    <<-- group numbers
  "^\\([[:blank:]]*\\)#\\([[:blank:]]*\\)if[[:blank:]]+\\([[:alpha:]_][[:alnum:]_]+\\)[[:blank:]]*\\(==\\)[[:blank:]]*\\(%s\\)[[:blank:]]*$"
  "Regexp to locate '#if VAR == 0', '#if VAR == 1'")

(defun pel-c-search-preproc-if ()
  "Search for pre-processor '#if VAR' statement."
  (interactive)
  (re-search-forward pel-preproc-if-regexp))

(defun pel-c-search-preproc-if-set ()
  "Search for pre-processor '#if VAR == 0', '#if VAR ==1' or '!' equivalent."
  (interactive)
  (re-search-forward (format pel-preproc-if-eq-regexp-format "[01]")))

(defun pel-c-fix-preproc-if-problems ()
  "Fix the C/C++ pre-processor #if statements not checking for defined var."
  (interactive "*")
  (message "Reformatting C/C++ C-pre-processing code...")
  (save-excursion
    (let ((changed-count-if 0)
          (changed-count-if-set 0))
      ;; 1: Change '#if VAR' --> '#if (defined(VAR) && (VAR != 0))'
      ;;    keep indentation style
      (pel+= changed-count-if
             (pel---c-replace pel-preproc-if-regexp
                              "\\1#\\2if (defined(\\3) && (\\3 != 0))"))
      ;; 2: Change '#if VAR==0' --> '#if (!defined(VAR) || (VAR==0))'
      ;;    keep indentation style
      (pel+= changed-count-if-set
             (pel---c-replace (format pel-preproc-if-eq-regexp-format "0")
                              "\\1#\\2if (!defined(\\3) || (\\3 == 0))"))
      ;; 3: Change '#if VAR==1' --> '#if (defined(VAR) && (VAR==1))'
      ;;    keep indentation style
      (pel+= changed-count-if-set
             (pel---c-replace (format pel-preproc-if-eq-regexp-format "1")
                              "\\1#\\2if (defined(\\3) && (\\3 == 1))"))
      (message "Fixed: %d '#if VAR', %d '#if VAR == [01]'"
               changed-count-if
               changed-count-if-set)
      ;; return list of change count
      (list changed-count-if changed-count-if-set))))

;;; --------------------------------------------------------------------------
(provide 'pel-c-utils)

;;; pel-c-utils.el ends here
