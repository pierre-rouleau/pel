;;; pel-c-preproc.el --- C Pre-processor specific support.  -*- lexical-binding: t; -*-

;; Created   : Monday, October 10 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2023-01-01 21:46:04 EST, updated by Pierre Rouleau>

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
;; This file implements navigation commands that move across C preprocessor
;; conditional inclusion statements found in C and C++ source code files. It
;; supports nested statements and provides 4 commands.
;;
;; The current implementation does not yet support C++ #elifdef and #elifndef
;; statements, but it supports the C preprocessor statements that were
;; available before.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel-syntax)    ; use: `pel-syntax-conditional-forward'
;;                       ;      `pel-syntax-conditional-backward'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; Navigate across #if|#ifdef|#ifndef  / #elif| #else  / #endif
;; ------------------------------------------------------------

(defconst pel--c-preproc-conditional-regexp
  "^[[:blank:]]*#[[:blank:]]*\\(\\(if\\(\\(def\\)\\|\\(ndef\\)\\)*\\)\\|\\(el\\(se\\|if\\)\\)\\|\\(endif\\)\\)"
  "Regexp to find C preprocessor conditionals.")

(defconst pel--c-preproc-conditional-group-if 2
  "Significant matching group when searching beginning of #if|#ifdef|#ifndef.")

(defconst pel--c-preproc-conditional-else 6
  "Significant matching group when searching #else|#elif conditional.")

(defconst pel--c-preproc-conditional-end 8
  "Significant matching group when searching #endif conditional.")

;; -----
(defun pel--c-preproc-token-from-match (match-result)
  "Return a if, else, end token from search MATCH-RESULT."
  ;; Result seen by using roup test function below.
  ;; #if:     [0-5] all are set: use 5.
  ;;
  ;; #ifdef:  [0-9] all are set: use 5 to identify an if beginning,
  ;;                             8 to identify #ifdef
  ;;
  ;; #ifndef: [0-11] all except 8 and 9 are set.
  ;;                     Use 5 to identify an if beginning.
  ;;                     Use 11 to identify #ifndef
  ;;
  ;; #else:   [0-15] set:0,1,2,3,12,13,14,15
  ;;                 Use 15 to identify else
  ;; #elif:   same as #else
  ;;
  ;; #endif:  [0-17] set:0,1,2,3,16,17:  Use 17 to identify end
  (cond
   ((nth 5 match-result)  'if)
   ((nth 15 match-result) 'else)
   ((nth 17 match-result) 'end)
   (t (error "Invalid search result"))))

(defun pel--c-preproc-token-pos-from-match (match-result)
  "Return token position from search MATCH-RESULT."
  (let ((pos
         (cond
          ((nth 5 match-result)   (nth 0 match-result)) ; #if | #ifdef | #ifndef
          ((nth 15 match-result)  (nth 15 match-result)) ; #else | #elif
          ((nth 17 match-result)  (nth 17 match-result)) ; #endif
          (t (error "Invalid search result")))))
    (message "==> %S from %S" pos match-result)
    pos))

;; (defun roup ()
;;   "Test for development"
;;   (interactive)
;;   (let ((msg-data nil))
;;     (re-search-forward pel--c-preproc-conditional-regexp nil :noerror)
;;     (setq msg-data (match-data))
;;     (message "%s at %S"
;;              (pel--c-preproc-token-from-match msg-data)
;;              msg-data)))


;; -------

;;-pel-autoload
(defun pel-c-preproc-forward-conditional (&optional to-else)
  "Move point forward to matching #endif.

If a command prefix TO-ELSE is specified, move point forward
after the matching else statement instead.

On success, push the original position on the mark ring and
return the new position. On error, issue user error on mismatch."
  (interactive "^P")
  (beginning-of-line nil)
  (pel-syntax-conditional-forward
   pel--c-preproc-conditional-regexp
   (function pel--c-preproc-token-from-match)
   (function pel--c-preproc-token-pos-from-match)
   0
   to-else
   (if to-else
       "#else|#elif"
     "#endif")))

;;-pel-autoload
(defun pel-c-preproc-backward-conditional (&optional to-else)
  "Move point backward to matching #if, #ifdef or #ifndef.

If a command prefix TO-ELSE is specified, move point backward
after the matching else statement instead.

On success, push the original position on the mark ring and
return the new position. On error, issue user error on mismatch."
  (interactive "^P")
  (end-of-line nil)
  (pel-syntax-conditional-backward
   pel--c-preproc-conditional-regexp
   (function pel--c-preproc-token-from-match)
   (function pel--c-preproc-token-pos-from-match)
   0
   to-else
   (if to-else
       "#else|#elif"
     "#if|#ifdef|#ifndef")))

;;-pel-autoload
(defun pel-c-preproc-outward-forward-conditional (&optional nest-count)
  "Move point forward, outward to the end of current #if|ifdef|ifndef block.

By default move 1 nest level outward.  A larger count can be
specified with optional NEST-COUNT numeric argument.

On success, push the original position on the mark ring and
return the new position. On error, issue user error on mismatch."
  (interactive "^p")
  (pel-syntax-conditional-forward
   pel--c-preproc-conditional-regexp
   (function pel--c-preproc-token-from-match)
   (function pel--c-preproc-token-pos-from-match)
   (abs nest-count)
   nil
   "#endif"))

;;-pel-autoload
(defun pel-c-preproc-outward-backward-conditional (&optional nest-count)
  "Move point outward to the beginning of current #if|ifdef|ifndef block.

By default move 1 nest level outward.  A larger count can be
specified with optional NEST-COUNT numeric argument.

On success, push the original position on the mark ring and
return the new position. On error, issue user error on mismatch."
  (interactive "^p")
  (pel-syntax-conditional-backward
   pel--c-preproc-conditional-regexp
   (function pel--c-preproc-token-from-match)
   (function pel--c-preproc-token-pos-from-match)
   (abs nest-count)
   nil
   "#if|#ifdef|#ifndef"))

;; ---------------------------------------------------------------------------

;;-pel-autoload
(defun pel-c-preproc-conditionals-occur (&optional nlines)
  "Show C preprocessor conditional statements inside an occur buffer.

Each line is displayed with NLINES before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
If a region is defined the search is restricted to the region."
  (interactive "^p")
  (occur pel--c-preproc-conditional-regexp
         nlines
         (when (use-region-p)
           (region-bounds))))

;;; --------------------------------------------------------------------------
(provide 'pel-c-preproc)

;;; pel-c-preproc.el ends here
