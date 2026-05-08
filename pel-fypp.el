;;; pel-fypp.el --- Fortran Python Pre-Processor Support  -*- lexical-binding: t; -*-

;; Created   : Friday, May  8 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-05-08 11:09:51 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2026  Pierre Rouleau
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
;; A minimal implementation of fypp major-mode, a major mode for the Fortran
;; Python Pre-Processor.  It essentially just adds syntax highlighting for
;; fypp expressions, using the pre-processor face for all of the FYPP specific
;; characters.
;;
;;
;; Limitation:
;;
;; The current implementation is unfortunately not able to render the enclosed
;; Python code with the Python-mode .
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'f90)
;;; --------------------------------------------------------------------------
;;; Code:
;;

;; The FYPP keywords are:
;;
;;    #:if           Starts a conditional block.
;;    #:elif         Starts an alternative conditional branch.
;;    #:else         Starts the final fallback branch in a conditional block.
;;    #:endif        Closes a conditional (if/elif/else) block.
;;    #:for          Starts a loop for generating repeated code.
;;    #:endfor       Closes a for loop block.
;;    #:set          Defines or updates a preprocessor variable.
;;    #:del          Removes a variable or macro definition.
;;    #:def          Defines a parameterized macro.
;;    #:enddef       Closes a macro definition block.
;;    #:call         Calls a macro with a code block.
;;    #:endcall      Closes a call block.
;;    #:block        Defines a block to be passed as a string argument to a macro.
;;    #:endblock     Closes a block definition.
;;    #:include      Includes the content of another file.
;;    #:mute         Suppresses the output of the enclosed code region.
;;    #:endmute      Closes a mute region.
;;    #:stop         Explicitly stops the  preprocessor, often used with error messages.
;;    #:assert       Asserts a condition for sanity checks.
;;    #:global       Defines a variable in the global scope.
;;
(defconst pel-fypp-keywords
  '("if"
    "elif"
    "else"
    "endif"
    "for"
    "endfor"
    "set"
    "del"
    "def"
    "enddef"
    "call"
    "endcall"
    "block"
    "endblock"
    "include"
    "mute"
    "endmute"
    "stop"
    "assert"
    "global")
  "List of FYPP supported keywords.")

(defconst pel-fypp-keyword-regexp
  (format "^[[:blank:]]*#:[[:blank:]]*%s%s%s[[:space:]\n]+"
          "\\<"
          (rx-to-string
           `(: (or ,@pel-fypp-keywords)))
          "\\>"))


(defconst pel-fypp-opening-inline-keywords
  '("if"
    "elif"
    "for"
    "set"
    "del"
    "def"
    "call"
    "block"
    "include"
    "mute"
    "stop"
    "assert"
    "global")
  "List of FYPP keywords that can start an inline directive block.")

(defconst pel-fypp-opening-inline-block-regexp
  (format "#{[[:blank:]]*%s%s%s[[:blank:]]*"
          "\\<"
          (rx-to-string
           `(: (or ,@pel-fypp-opening-inline-keywords)))
          "\\>"))

(defconst pel-fypp-opening-end-inline-block-regexp "}#"
  "The end part of a inline block start.")

(defconst pel-fypp-closing-inline-keywords
  '("else"
    "endif"
    "endfor"
    "enddef"
    "endcall"
    "endblock"
    "endmute")
  "List of FYPP keywords that can be in a inline closing block.")

(defconst pel-fypp-closing-inline-block-regexp
  (format "#{%s%s%s}#"
          "\\<"
          (rx-to-string
           `(: (or ,@pel-fypp-closing-inline-keywords)))
          "\\>"))

(defconst pel-fypp-comment-regexp
  "^[[:blank:]]*#!"
  "FYPP comment regular expression.")

(define-derived-mode fypp-mode f90-mode "Fypp"
  "Major mode for editing Fypp-preprocessed Fortran code."
  (font-lock-add-keywords
   nil
   (list

    ;; Highlights #:if, #:for, etc.
    (cons pel-fypp-keyword-regexp font-lock-preprocessor-face)

    ;; Highlights the beginning and end of inline directives
    (cons pel-fypp-closing-inline-block-regexp font-lock-preprocessor-face)
    (cons pel-fypp-opening-inline-block-regexp  font-lock-preprocessor-face)
    (cons pel-fypp-opening-end-inline-block-regexp font-lock-preprocessor-face)

    ;; Enclosed Python expressions
    ;; - Highlights the edges of ${expr}$
    (cons "\\${"    font-lock-preprocessor-face)
    (cons "}\\$"    font-lock-preprocessor-face)
    ;; - Highlights line-form $: eval directives
    (cons "^[[:blank:]]*\\$:"  font-lock-preprocessor-face)
    ;; - Highlights the @ at beginning of macros
    (cons "@:" font-lock-preprocessor-face)
    (cons "@{" font-lock-preprocessor-face)
    (cons "}@" font-lock-preprocessor-face)

    ;; Comment
    (cons pel-fypp-comment-regexp font-lock-comment-face)
    )))

;;; -------------------------------------------------------------------------
(provide 'pel-fypp)

;;; pel-fypp.el ends here
