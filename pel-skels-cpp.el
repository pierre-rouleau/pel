;;; pel-skels-cpp.el --- Tempo skeletons for C++.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, May 25 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-03-17 15:11:31 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2023, 2025  Pierre Rouleau
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
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)       ; use: pel-swap
(require 'pel--options)    ; use: pel-c++-skel-...
(require 'pel-prompt)      ; use: pel-prompt-purpose-for
(require 'pel-text-insert) ; use: pel-separator-line
(require 'pel-uuid)        ; use: pel-c-include-guard
(require 'pel-tempo)       ; use: pel-tempo-install-pel-skel
(require 'pel-skels)       ; use: pel-skel-create-comment
;;                         ;      pel-skel-author-comment
;;                         ;      pel-skel-time-stamp
;;                         ;      pel-skel-call
(require 'pel-skels-c)     ; use: pel-skels-c-pp-define-skel
;;                         ;      pel-skels-c-pp-include-local-skel
;;                         ;      pel-skels-c-pp-include-global-skel
(eval-when-compile (require 'subr-x)) ; use: string-trim
;;; --------------------------------------------------------------------------
;;; Code:
;;

;; --
;; Utility functions

(defun pel-skel-c++-separator-line ()
  "Return a section separator line for C++ if required.
If prohibited (by customization) returns nil.
Otherwise return a string that ends with a newline."
  (when pel-c++-skel-use-separators
    (concat (pel-separator-line) "\n")))

;; ---------------------------------------------------------------------------
;; File/Module header block

(defconst pel-c++-header-extensions '("h"
                                      "hh"
                                      "HH"
                                      "hpp"
                                      "hxx"
                                      "h++"
                                      "inl"
                                      "icc")
  "List of C++ file extensions seen when in c++-mode.")

(defconst pel-c++-module-special-name-alist
  '(("ii" . "GCC Pre-Processed File"))
  "Describes special implementation extensions.")

(defconst pel-c++-header-special-name-alist
  '(("inl" . "INLINE DEFINITION HEADER")
    ("icc" . "INLINE DEFINITION HEADER"))
  "Describes special header extensions.")

(defun pel-c++-typename (extension alist default)
  "Return name of C++ header type for the C++ EXTENSION string."
  (or
   (cdr (assoc extension alist))
   default))

(defun pel-skels-c++-header-module-block (fname is-a-header cmt-style)
  "Return a tempo list for the comment block inserted at top of the C++ file.
The arguments are:
- FNAME := string.  the name of the current file without path.
- IS-A-HEADER := boolean.  header string if C++ header,  nil otherwise.
- CMT-STYLE := a list of 3 strings: (cb cc ce)
            - cb : comment begin string
            - cc : comment continuation string
            - ce : comment end string."
  (let ((purpose  (pel-prompt-purpose-for "File" 'p))
        (cc       (nth 1 cmt-style))
        (file-type (if is-a-header
                       (pel-c++-typename is-a-header
                                         pel-c++-header-special-name-alist
                                         "HEADER")
                     (pel-c++-typename (file-name-extension fname)
                                       pel-c++-module-special-name-alist
                                       "MODULE"))))
    (list
     'l
     "// C++ " file-type ": "  fname 'n
     "//\n"
     "// Purpose   : " purpose 'n
     (pel-skel-created-comment cc)
     (pel-skel-author-comment  cc)
     (pel-skel-time-stamp pel-c++-skel-insert-file-timestamp cc)
     (pel-skel-copyright-comment pel-c++-skel-with-license cc)
     "//\n")))


(defun pel-skels-c++-file-header-block ()
  "Return a tempo list for a C++ file header block.
The format of the file header block is adjusted for the supported file types:
the C++ code file and the C++ header file.
The file header portion is controlled by the style selected by the
variable `pel-c++-skel-module-header-block-style'."
  (pel-skel-cc-file-header-block pel-c++-skel-module-header-block-style
                                 'pel-skels-c++-header-module-block/custom
                                 pel-c++-skel-hppfile-section-titles
                                 pel-c++-skel-cppfile-section-titles
                                 pel-c++-skel-use-include-guards
                                 (function pel-skels-c++-header-module-block)
                                 (function pel-skel-c++-separator-line)
                                 pel-c++-header-extensions))

;; ---------------------------------------------------------------------------
;; C++ Function skeleton


(defun pel-valid-c++-symbol (text)
  "Return TEXT if it is a valid C function name, nil otherwise.
Replace dash characters with underscores, to simplify typing function
names using underscores."
  (let ((text (replace-regexp-in-string "-" "_" (string-trim text))))
    (when (string-match "\\`[a-zA-Z_][a-zA-Z0-9_]*\\'" text)
      text)))

(defun pel-skels-c++-function-def (&optional name)
  "Insert just the function definition code.
The function NAME can be passed via arguments,
prompt user otherwise.
When NAME is specified the optional separator line is *not* inserted:
it's assumed that another function has already done it."
  (let* ((fct-name   (or name (pel-prompt-function
                               (function pel-valid-c++-symbol))))
         (sk         (list 'l (unless name (pel-skel-c++-separator-line)))))
    (if pel-c++-skel-function-name-on-first-column
        (pel-append-to sk (list
                           'p "void" 'n
                           fct-name))
      (pel-append-to sk (list
                         'p "void " fct-name)))
    (pel-append-to sk (list
                       "(" 'p ")\n"
                       "{" 'n>
                       'p 'n
                       "}\n\n"
                       'p))))

(defun pel-skels-c++-function-def-basic (&optional name purpose)
  "Insert a basic function code template with simple comment block.
The function NAME and PURPOSE can be passed via arguments,
prompt user otherwise."
  (let* ((fct-name   (or name (pel-prompt-function
                               (function pel-valid-c++-symbol))))
         (purpose    (or purpose (pel-prompt-purpose-for "Function" 'p)))
         (cmt-style  (pel-skel-comments-strings))
         (cb         (nth 0 cmt-style))
         (ce         (nth 2 cmt-style)))
    (list
     'l
     (pel-skel-c++-separator-line)
     cb " " fct-name "() -- " purpose 'n
     ce 'n (pel-when-text-in ce 'n)
     (pel-skels-c++-function-def fct-name))))

(defun pel-skels-c++-function-def-man ()
  "Insert a MAN-style C function definition command block.
This begins with an optional separator line, the name of the
function spread and underlined with its purpose on the same line.
This style is selected when the user option variable
`pel-c++-skel-function-define-style' is set to the value man-style.
The comment style is controlled by the CC mode variable
`c-block-comment-flag'."
  (let* ((fct-name        (pel-prompt-function
                           (function pel-valid-c++-symbol)))
         (purpose         (pel-prompt-purpose-for "Function" 'p))
         (cmt-style       (pel-skel-comments-strings))
         (cb              (nth 0 cmt-style))
         (cc              (nth 1 cmt-style))
         (ce              (nth 2 cmt-style))
         (spread-fct-name (pel-string-spread fct-name)))
    (list
     'l
     (pel-skel-c++-separator-line)
     cb " " spread-fct-name " ( )     -- " purpose 'n
     cc " " (make-string (+ 4 (length spread-fct-name)) ?^ ) 'n
     cc 'n
     (when pel-c++-skel-insert-function-sections
       (let ((sk (list 'l
                       cc " DESCRIPTION\n"
                       cc 'n
                       cc " " fct-name "() " 'p 'n
                       cc 'n
                       cc 'n)))
         (dolist (title pel-c++-skel-function-section-titles)
           (pel-append-to sk (list
                              cc " " title 'n
                              cc 'n
                              cc " " 'p 'n
                              cc 'n)))
         (pel-append-to sk (list ce 'n (pel-when-text-in ce 'n)))))
     (pel-skels-c++-function-def fct-name))))

(defun pel-skels-c++-function-definition ()
  "Insert a tempo skeleton for the insertion of a C function definition.
Insert the skeleton selected by the user option variable
`pel-c++-skel-function-define-style'."
  (cond ((not pel-c++-skel-function-define-style)
         (pel-skels-c++-function-def))
        ((eq pel-c++-skel-function-define-style 'basic-style)
         (pel-skels-c++-function-def-basic))
        ((eq pel-c++-skel-function-define-style 'man-style)
         (pel-skels-c++-function-def-man))
        (t (pel-skel-call 'pel-c++-skel-function-define-style
                          'pel-skels-c++-function-def/custom))))

;; ---------------------------------------------------------------------------
;; C++ Class Definitions
;; ---------------------

(defun pel-skels-c++-doc-section-titles (section-titles)
  "Return a C++ documentation block skeleton list of SECTION-TITLES.
SECTION-TITLES may be nil.  In that case the function returns nil."
  (when section-titles
    (let ((cc "// ")
          (sk (list 'l)))
      (dolist (stitle section-titles)
        (pel-append-to
         sk
         (list
          cc stitle ":\n"
          cc " " 'p 'n
          cc 'n
          cc 'n)))
      sk)))

(defun pel-skels-c++-doc-block (class-name &optional purpose)
  "Return a skeleton list for the C++ CLASS-NAME description block."
  (when pel-c++-class-has-doc-block
    (let ((purpose (or purpose (pel-prompt-purpose-for "Class" 'p))))
      (list
       'l
       (format "// Class %s: %s\n//\n" class-name purpose)
       (pel-skels-c++-doc-section-titles pel-c++-class-doc-section-titles)
       'n))))

(defun pel-skels-c++-instrumented-code (code-line)
  "Return skeleton list for CODE-LINE, replace $$ by tempo marker."
  (let ((sk (list 'l)))
    (dolist (code (split-string code-line "\\$\\$") (butlast (reverse sk)))
      (push code sk)
      (push 'p sk))))

(defun pel-skels-c++-code (class-name code-lines)
  "Return a skeleton list of C++ code lines."
  (let ((sk (list 'l)))
    (dolist (code-line code-lines)
      (pel-append-to
       sk
       (list
        'n>
        (pel-skels-c++-instrumented-code
         (replace-regexp-in-string "\\$class-name" class-name code-line)))))
    sk))

(defun pel-skels-c++-member-block (class-name)
  "Return a skeleton list for the C++ CLASS-NAME member block."
  (let ((sk (list 'l))
        access title code-lines)
    (dolist (section pel-c++-class-members-sections)
      (setq access (car section))
      (setq title  (cadr section))
      (setq code-lines (caddr section))
      (when (pel-uppercase-p (substring title 0 1))
        (setq title (format "%s %s" access title)))
      (pel-append-to
       sk
       (list
        'n>
        "// - " (capitalize title) ":" 'n>
        (format "%s:" access)
        (if code-lines
            (pel-skels-c++-code class-name code-lines)
          (list
           'l
           'n>
           'p 'n)))))
    sk))

(defun pel-skels-c++-class-definition (&optional name purpose)
  "Insert a C++ class definition code.
If the class NAME is specified use it, otherwise prompt for it.
Replace dash characters with underscores, to simplify typing function
names using underscores."
  (let* ((class-name (or name (pel-prompt-class
                               (function pel-valid-c++-symbol)))))
    (list
     'l
     (when pel-c++-class-has-doc-block
       (pel-skels-c++-doc-block class-name purpose))
     "class " class-name 'p "\n{"
     (pel-skels-c++-member-block class-name)
     "\n}\n")))

;; ---------------------------------------------------------------------------
;; Install C++ code skeletons

(defvar pel-skels-c++-large-header-skel
  '(o
    (pel-skels-c++-file-header-block))
  "The skeleton of a C file header block.")

(defvar pel-skels-c++-function-definition-skel
  '(o
    (pel-skels-c++-function-definition))
  "The skeleton of a C function definition block.")

(defvar pel-skels-c++-class-definition-skel
  '(o
    (pel-skels-c++-class-definition))
  "The skeleton of a C class definition block.")


(defvar pel--c++-skels
  '(("File Header"   "file-header"   pel-skels-c++-large-header-skel)
    ("Function"      "function"      pel-skels-c++-function-definition-skel)
    ("Class"         "class"         pel-skels-c++-class-definition-skel)
    ("Define"        "define"        pel-skels-c-pp-define-skel)
    ("Include \"\""  "include-local" pel-skels-c-pp-include-local-skel)
    ("Include <>"    "include-lib"   pel-skels-c-pp-include-global-skel))
  "List of C++ code tempo skeletons.")

(defvar pel--c++-skels-keys '(("file-header"   . "h")
                              ("function"      . "f")
                              ("class"         . "c")
                              ("define"        . "d")
                              ("include-local" . "I")
                              ("include-lib"   . "i"))
  "Key mapping for C++ code skeletons.")

;;-pel-autoload
(defun pel--install-c++-skel (key-map)
  "Create the C skeletons and bind them in the KEY-MAP specified.
This function is meant to be called by the function `pel-init' only."
  (pel-tempo-install-pel-skel
   "c++"
   pel--c++-skels
   key-map
   pel--c++-skels-keys
   "c++"
   'pel-c-skeleton-control))

;;; --------------------------------------------------------------------------
(provide 'pel-skels-cpp)

;;; pel-skels-cpp.el ends here
