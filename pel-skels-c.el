;;; pel-skels-c.el --- Tempo skeletons for C.  -*- lexical-binding: t; -*-

;; Created   : Monday, August 24 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2020-08-27 18:11:56, updated by Pierre Rouleau>

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

;;; ----------------------------------------------------------------------------
;;; Commentary:
;;
;; This defines tempo skeletons for C code and header files. The format of the
;; inserted text is controlled by the user options inside the `pel-c-code-style'
;; customization group.
;;
;; For header files, the skeleton can insert a include guard that uses a C
;; pre-processor symbol made out of the file base name and a automatically
;; generated UUID.   This provides a super safe include guard code that
;; eliminates the possibility of symbol clash in C pre-processor include guards
;; while creating portable C code.  The inclusion of this safe include guard
;; code is controlled by the variable `pel-c-skel-use-uuid-include-guards', so
;; users that prefer or need to use the less portable ``#pragma once`` can do
;; that.
;;
;; The C code files can be created with several code sections, with or without
;; line separators.
;;
;; The `pel-skels-c-file-header-block' function creates the file header block
;; prompting for the purpose of the file and inserting it if entered. It also
;; inserts the date the file is created, the author information and can insert
;; an automatically updated time stamp and a open source license text.
;;
;; The tempo skeleton templates are currently minimal and do not yet support
;; doxygen formatting and embedded lint specialized comments like I have done in
;; other editing systems in the past. I'd like to support the excellent Gimpel
;; PC-Lint Plus but also other lint and C tooling systems and provide the
;; ability to select which one to use and provide many capabilities to help
;; build robust C and self-documented code. I would also like to provide the
;; ability to insert other type of information to increase the flexibility of
;; this skeleton, but I'll do this later when I spend more time coding in C or
;; if I get requests.
;;

;;; ----------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)       ; use: pel-current-buffer-filename
(require 'pel--options)    ; use: pel-elisp-skel-package-name
(require 'pel--macros)     ; use: pel-append-to
(require 'pel-prompt)      ; use: pel-prompt-purpose-for
(require 'pel-skels)       ; use: pel-skel-create-comment
;;                         ;      pel-skel-author-comment
;;                         ;      pel-skel-time-stamp
(require 'pel-tempo)       ; use: pel-tempo-install-pel-skel
(require 'pel-text-insert) ; use: pel-separator-line
(require 'pel-uuid)        ; use: pel-c-include-guard
(eval-when-compile (require 'subr-x)) ; use: string-trim
;;; ----------------------------------------------------------------------------
;;; Code:
;;

(defun pel-skel-c-separator-line ()
  "Return a section separator line for C if required.
If prohibited (by customization) returns nil.
Otherwise return a string that ends with a newline."
  (when pel-elisp-skel-use-separators
    (concat (pel-separator-line) "\n")))


(defun pel-c-style-comments-strings ()
  "Return a list of 3 strings describing C-style comments.
These 3 strings are:
1- The comment start block.  Something like \"/*\" or \"//\".
   It does *not* end with a space.
2- The intermediate comment block.  Something like \"**\", \"//\",
   or \" *\".
3- The closing comment block.  Something like \"*\" or \"\".
The string returned is selected by the currently active C-style.
The following variables are used:
- variable `c-block-comment-flag'."
  (let* ((c-style         (and (boundp 'c-block-comment-flag)
                               c-block-comment-flag))
         (cb              (if c-style "/*" "//"))
         (cc              (if c-style "**"  "//"))
         (ce              (if c-style "*/"  "")))
    (list cb cc ce)))

(defun pel-skels-c-file-header-block ()
  "Return a tempo list for a C file header block.
The format of the file header block is adjusted for the supported file types:
the C code file and the C header file."
  (let* ((purpose      (pel-prompt-purpose-for "File" 'p))
         (fname        (pel-current-buffer-filename :sans-directory))
         (fn-extension (file-name-extension fname))
         (is-a-header  (string= fn-extension "h"))
         (c-style         (pel-c-style-comments-strings))
         (cb              (nth 0 c-style))
         (cc              (nth 1 c-style))
         (ce              (nth 2 c-style)))
    (list
     'l
     cb " C " (if is-a-header "HEADER" "MODULE") ": "  fname 'n
     cc 'n
     cc " Purpose   : " purpose 'n
     (pel-skel-created-comment cc)
     (pel-skel-author-comment  cc)
     (pel-skel-time-stamp pel-c-skel-insert-file-timestamp cc)
     (when pel-c-skel-with-license
       (list 'l
             cc 'n
             (pel-license-text cc)
             cc 'n))
     ce (pel-when-text-in ce 'n)
     (pel-separator-line) 'n
     (if is-a-header
         (when pel-c-skel-use-uuid-include-guards
           (list
            'l
            (pel-c-include-guard)
            (pel-separator-line) 'n
            'p 'n
            (pel-separator-line) 'n
            "#endif\n"
            ))
       (let ((sk (list 'l)))
         (if pel-c-skel-insert-module-sections
             (progn
               (pel-append-to sk
                              (list
                               cb " Module Description\n"
                               cc " ------------------\n"
                               cc 'n
                               cc " " 'p 'n
                               cc 'n
                               ce 'n (pel-when-text-in ce 'n)))
               (dolist (mtitle pel-c-skel-module-section-titles)
                 (pel-append-to sk
                                (list
                                 (pel-skel-c-separator-line)
                                 cb " " mtitle 'n
                                 cb " " (make-string (length mtitle) ?-) 'n
                                 ce 'n (pel-when-text-in ce 'n)
                                 'p 'n 'n))))
           (pel-append-to sk (list
                              'n
                              'p 'n 'n)))
         (pel-append-to sk (list (pel-skel-c-separator-line))))))))

;; --
;; C function definitions


(defun pel--skels-c-function-def-basic ()
  ""
  )

(defun pel-valid-c-function-name (text)
  "Return the string if it is a valid C function name, nil otherwise.
Replace dash characters with underscores, to simplify typing function
names using underscores."
  (let ((text (replace-regexp-in-string "-" "_" (string-trim text))))
    ;; TODO?  add user options to impose length limits?
    (when (string-match "\\`[a-zA-Z_][a-zA-Z0-9_]*\\'" text)
      text)))

(defun pel--skels-c-function-def-man ()
  "Insert a MAN-style C function definition command block.
This begins with an optional separator line, the name of the function spread
and underlined with its purpose on the same line.
This style is selected when the user option variable
`pel-c-skel-function-define-style' is set to the value man-style.
The comment style is controlled by the CC mode variable `c-block-comment-flag'."
  (let* ((fct-name        (pel-prompt-function (function pel-valid-c-function-name)))
         (purpose         (pel-prompt-purpose-for "Function" 'p))
         (c-style         (pel-c-style-comments-strings))
         (cb              (nth 0 c-style))
         (cc              (nth 1 c-style))
         (ce              (nth 2 c-style))
         (spread-fct-name (pel-string-spread fct-name)))
    (list
     'l
     (pel-skel-c-separator-line)
     cb " " spread-fct-name " ( )     -- " purpose 'n
     cc " " (make-string (+ 4 (length spread-fct-name)) ?^ ) 'n
     cc 'n
     (when pel-c-skel-insert-function-sections
       (let ((sk (list 'l
                       cc " DESCRIPTION\n"
                       cc 'n
                       cc " " fct-name "() " 'p 'n
                       cc 'n
                       cc 'n)))
         (dolist (title pel-c-skel-function-section-titles)
           (pel-append-to sk (list
                              cc " " title 'n
                              cc 'n
                              cc " " 'p 'n
                              cc 'n)))
         (pel-append-to sk (list ce 'n 'n))))
     (if pel-c-skel-function-name-on-first-column
         (list 'l
               'p "void" 'n
               fct-name)
       (list 'l
             'p "void " fct-name))
     "(" 'p ")\n"
     "{" 'n>
     'p 'n
     "}\n\n"
     'p
     )))



(defun pel--skels-c-function-def-doxygen ()
  ""
  )

(defun pel-skels-c-function-definition ()
  "Insert a tempo skeleton for the insertion of a C function definition.
Insert the skeleton selected by the user option variable
`pel-c-skel-function-define-style'."
  (cond ((not pel-c-skel-function-define-style)
         (list 'l 'p))
        ((eq pel-c-skel-function-define-style 'basic-style)
         (pel--skels-c-function-def-basic))
        ((eq pel-c-skel-function-define-style 'man-style)
         (pel--skels-c-function-def-man))
        ((eq pel-c-skel-function-define-style 'doxygen-style)
         (pel--skels-c-function-def-doxygen))
        (t (if (and (load pel-c-skel-function-define-style :noerror)
                    (fboundp 'pel-custom-c-function-block))
               (pel-custom-c-function-block)
             (user-error
              (format
               "Invalid pel-custom-c-function-block defun in file: s!"))))))

;; -----------------------------------------------------------------------------
;; Install Emacs Lisp skeletons

(defvar pel-skels-c-large-header-skel
  '(o
    (pel-skels-c-file-header-block))
  "The skeleton of a C file header block.")

(defvar pel-skels-c-function-definition-skel
  '(o
    (pel-skels-c-function-definition))
  "The skeleton of a C function definition block.")


(defvar pel--c-skels
  '(("File Header" "file-header" pel-skels-c-large-header-skel)
    ("Function"    "function"    pel-skels-c-function-definition-skel))
  "List of Emacs Lisp tempo skeletons.")

(defvar pel--c-skels-keys '(("file-header" . "C-h")
                            ("function"    . "f"))
  "Key mapping for Emacs Lisp skeletons.")

;;-pel-autoload
(defun pel--install-c-skel (key-map)
  "Create the Emacs Lisp skeletons and bind them in the KEY-MAP specified.
This function is meant to be called by the function `pel-init' only."
  (pel-tempo-install-pel-skel
   "c"
   pel--c-skels
   key-map
   pel--c-skels-keys
   "c"))

;;; ----------------------------------------------------------------------------
(provide 'pel-skels-c)

;;; pel-skels-c.el ends here
