;;; pel-skels-c.el --- Tempo skeletons for C.  -*- lexical-binding: t; -*-

;; Created   : Monday, August 24 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-03-17 15:11:16 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2021, 2023, 2025  Pierre Rouleau
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
;; This defines tempo skeletons for C code and header files.  The format of
;; the inserted text is controlled by the user options inside the
;; `pel-c-code-style' customization group.
;;
;; It provides the following skeletons:
;;

;; 1- File module header block: for .c and .h files.
;;
;;   - variable:    `pel-skels-c-large-header-skel'
;;   - function:    `pel-skels-c-file-header-block'
;;   - user options:
;;     - For all C files:
;;       - `pel-c-skel-module-header-block-style'
;;       - `pel-c-skel-insert-file-timestamp'
;;       - `pel-c-skel-with-license'
;;     - For C code files only:
;;       - `pel-c-skel-cfile-section-titles'
;;     - For header files only:
;;       - `pel-c-skel-hfile-section-titles'
;;       - `pel-c-skel-use-include-guards'
;;
;;
;;   For header files, the skeleton can insert a include guard that uses a C
;;   pre-processor symbol made out of the file base name and a automatically
;;   generated UUID.  This provides a super safe include guard code that
;;   eliminates the possibility of symbol clash in C pre-processor include
;;   guards while creating portable C code.  The inclusion of this safe
;;   include guard code is controlled by the variable
;;   `pel-c-skel-use-include-guards', so users that prefer or need to use
;;   the less portable ``#pragma once`` can do that.
;;
;;   The skeleton can create header blocks with several code sections, with or
;;   without line separators.
;;
;;   The `pel-skels-c-file-header-block' function creates the file header
;;   block prompting for the purpose of the file and inserting it if
;;   entered.  It also inserts the date the file is created, the author
;;   information and can insert an automatically updated time stamp and a open
;;   source license text.

;; 2- C function definition header block
;;
;;   - variable:    `pel-skels-c-function-definition-skel'
;;   - function:    `pel-skels-c-function-definition'
;;   - user options:
;;       - `pel-c-skel-function-define-style'
;;       - `pel-c-skel-insert-function-sections'
;;       - `pel-c-skel-function-section-titles'
;;       - `pel-c-skel-function-name-on-first-column'
;;
;;   The function generation skeleton create a C function definition block
;;   preceded with an optional comment block and separator line.  The style is
;;   selected by `pel-c-skel-function-define-style' user option: that can be a
;;   user specified skeleton or one of the supported styles:
;;   - just code, no comment block
;;   - basic-style: a small documentation block before the function
;;     definition.
;;   - man-style: a bigger block with Man style sections describing aspects of
;;     the function.  If `pel-c-skel-insert-function-sections' is t, there's
;;     also a DESCRIPTION section followed by the sections
;;     identified by the `pel-c-skel-function-section-titles' user option.

;; 3- Skeleton and functions for C pre-processor statements:
;;    - #define     : variable: `pel-skels-c-pp-define-skel'
;;                  : function: `pel-skels-c-pp-define'
;;
;;    - #include "" : variable: `pel-skels-c-pp-include-local-skel'
;;                  : function: `pel-skels-c-pp-include-local'
;;
;;    - #include <> : variable: `pel-skels-c-pp-include-global-skel'
;;                  : function: `pel-skels-c-pp-include-global'

;; Limitations:
;;
;; The tempo skeleton templates are currently minimal and do not yet support
;; doxygen formatting and embedded lint specialized comments like I have done
;; in other editing systems in the past.  I'd like to support the excellent
;; Gimpel PC-Lint Plus but also other lint and C tooling systems and provide
;; the ability to select which one to use and provide many capabilities to
;; help build robust C and self-documented code.  I would also like to provide
;; the ability to insert other type of information to increase the flexibility
;; of this skeleton, but I'll do this later when I spend more time coding in C
;; or if I get requests for that.
;;

;; The function ('-') and variable ('>') hierarchy in this file is the
;; following:
;;
;; - `pel--install-c-skel'
;;   > `pel--c-skels'
;;     > `pel-skels-c-large-header-skel'
;;       - `pel-skels-c-file-header-block'
;;     > `pel-skels-c-function-definition-skel'
;;       - `pel-skels-c-function-definition'
;;         - `pel-skels-c-function-def-man'
;;         - `pel-skels-c-function-def-basic'
;;           - `pel-skels-c-function-def'
;;             - `pel-valid-c-function-name'
;;             - `pel-skel-c-separator-line'
;;   > `pel--c-skels-keys'
;;
;;
;; The function `pel--install-c-skel' is only called by `pel-init' when
;; it loads pel_keys.el.


;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)       ; use: pel-current-buffer-filename
(require 'pel--options)    ; use: pel-c-skel-...
(require 'pel--macros)     ; use: pel-append-to
(require 'pel-prompt)      ; use: pel-prompt-purpose-for
(require 'pel-list)        ; use: pel-list-split
(require 'pel-skels)       ; use: pel-skel-create-comment
;;                         ;      pel-skel-author-comment
;;                         ;      pel-skel-time-stamp
;;                         ;      pel-skel-call
(require 'pel-tempo)       ; use: pel-tempo-install-pel-skel
(require 'pel-text-insert) ; use: pel-separator-line
(require 'pel-uuid)        ; use: pel-c-include-guard
(eval-when-compile (require 'subr-x)) ; use: string-trim
;;; --------------------------------------------------------------------------
;;; Code:
;;

;; ---------------------------------------------------------------------------
;; Generic code for both C and C++

(defun pel-skel-section-titles (section-titles cb cc ce line-separator-fct)
  "Return a skeleton list of SECTION-TITLES.
The CB, CC and CE are the comments beginning, continuation and end strings.
The LINE-SEPARATOR-FCT is a function that takes no argument and return a
separator line string.
SECTION-TITLES may be nil.  In that case the function returns nil."
  (when section-titles
    (let ((sk (list 'l)))
      (dolist (mtitle section-titles)
        (pel-append-to
         sk
         (cond
          ;; titles that start with a . are inserted as string (skipping the
          ;; first character)is after a separator line
          ((string= (substring mtitle 0 1) ".")
           (list
            (funcall line-separator-fct)
            (substring mtitle 1)))
          ;; titles that start with a "," are inserted as string (skipping
          ;; first character) without a separator line.
          ((string= (substring mtitle 0 1) ",")
           (list
            (substring mtitle 1)))
          ;; insert normal title section
          (t (list
              (funcall line-separator-fct)
              cb (pel-skel-space-for cb) mtitle 'n
              cc (pel-skel-space-for cc) (make-string (length mtitle) ?-) 'n
              ce 'n (pel-when-text-in ce 'n)
              'p 'n 'n)))))
      sk)))

(defun pel-skel-cc-file-header-block (cc-module-header-block-style
                                      cc-header-module-block/custom
                                      cc-skel-hfile-section-titles
                                      cc-skel-cfile-section-titles
                                      cc-skel-use-include-guards
                                      cc-header-module-block-fct
                                      cc-separator-line-fct
                                      cc-header-extensions)
  "Return a tempo list for a C/C++ file header block.
The format of the file header block is adjusted for the supported file types:
the C/C++ code file and the C/C++ header file.

The behaviour of this function is determined by its arguments that are meant
to be the customization user-option variables of C or C++:

- CC-MODULE-HEADER-BLOCK-STYLE :  pel-c++-skel-module-header-block-style
                               or pel-c-skel-module-header-block-style

- CC-HEADER-MODULE-BLOCK/CUSTOM:  pel-skels-c++-header-module-block/custom
                               or pel-skels-c-header-module-block/custom

- CC-SKEL-HFILE-SECTION-TITLES :  pel-c++-skel-hppfile-section-titles
                               or pel-c-skel-hfile-section-titles

- CC-SKEL-CFILE-SECTION-TITLES :  pel-c++-skel-cppfile-section-titles
                               or pel-c++-skel-cfile-section-titles

- CC-SKEL-USE-INCLUDE-GUARDS   :  pel-c++-skel-use-include-guards
                               or pel-c-skel-use-include-guards

- CC-HEADER-MODULE-BLOCK-FCT   :  pel-skels-c++-header-module-block
  (3 argument)                  or pel-skels-c-header-module-block

- CC-SEPARATOR-LINE-FCT        :  pel-skel-c++-separator-line
  (no argument)                 or pel-skel-c-separator-line

- CC-HEADER-EXTENSIONS         : a list of extension strings."
  (let* ((fname        (pel-current-buffer-filename :sans-directory))
         (f-extension  (file-name-extension fname))
         (is-a-header  (car (member f-extension cc-header-extensions)))
         (cmt-style    (pel-skel-comments-strings))
         (cb           (nth 0 cmt-style))
         (cc           (nth 1 cmt-style))
         (ce           (nth 2 cmt-style))
         (section-titles (if is-a-header
                             cc-skel-hfile-section-titles
                           cc-skel-cfile-section-titles))
         (titles-a-and-b (pel-list-split "." section-titles))
         (section-titles-before-incguard (car titles-a-and-b))
         (section-titles-after-incguard (cadr titles-a-and-b)))

    ;; if there's no split, all sections should be after the include guard.
    (unless section-titles-after-incguard
      (pel-swap section-titles-before-incguard section-titles-after-incguard))
    (goto-char (point-min)) ; TODO: del this but modify skels to force entry at top.
    (list
     'l
     ;; 1- Insert the top level comment block for the top of the file.
     ;; Select the style from `cc-module-header-block-style'
     ;; That block does not end with a separator line.
     (if cc-module-header-block-style
         (pel-skel-call 'cc-module-header-block-style
                        cc-header-module-block/custom
                        fname is-a-header
                        cmt-style)
       (funcall cc-header-module-block-fct
                fname
                (or is-a-header nil)    ; pass extension string if header
                cmt-style))
     ;; 2- Add the remainder for either a header file or code file.
     (if is-a-header
         ;; A: for a header file
         (list
          'l
          ;; Insert section titles that must be inserted before the include guard.
          (when (or (and (not section-titles-before-incguard)
                         cc-skel-use-include-guards)
                    (and (not section-titles-before-incguard)
                         (not section-titles-after-incguard)
                         (not cc-skel-use-include-guards)))
            (funcall cc-separator-line-fct))
          (pel-skel-section-titles section-titles-before-incguard
                                   cb cc ce
                                   cc-separator-line-fct)
          (when (and section-titles-before-incguard
                     cc-skel-use-include-guards)
            (funcall cc-separator-line-fct))
          ;; Insert the selected include guard and the remaining code.
          (list
           'l
           ;; Insert the include guard - of type specified by `cc-skel-use-include-guards'
           (cond
            ;; pragma-once
            ((eq cc-skel-use-include-guards 'pragma-once)
             (list
              'l
              "#pragma once\n"
              'p 'n))
            ;; a #ifdef #define include guard - just the beginning
            ((memq cc-skel-use-include-guards '(t with-uuid))
             (pel-c-include-guard (eq cc-skel-use-include-guards
                                      'with-uuid))))

           ;; Insert the remaining section list if any
           (when (and (not section-titles-after-incguard)
                      (memq cc-skel-use-include-guards '(t with-uuid)))
             (funcall cc-separator-line-fct))

           (pel-skel-section-titles section-titles-after-incguard
                                    cb cc ce
                                    cc-separator-line-fct)
           ;; drop tempo mark on area where will be written if there
           ;; were no sections titles after the include guard.
           (unless section-titles-after-incguard
             (list 'l
                   'n 'p 'n 'n))
           (funcall cc-separator-line-fct)
           ;; Terminate the #ifdef #define include guard if that is requested.
           (when (memq cc-skel-use-include-guards '(t with-uuid))
             "#endif\n")))
       ;; B: for a code file
       (list
        'l
        (pel-skel-section-titles section-titles
                                 cb cc ce
                                 cc-separator-line-fct)
        (funcall cc-separator-line-fct))))))

;; ---------------------------------------------------------------------------
;; Utility functions

(defun pel-skel-c-separator-line ()
  "Return a section separator line for C if required.
If prohibited (by customization) returns nil.
Otherwise return a string that ends with a newline."
  (when pel-c-skel-use-separators
    (concat (pel-separator-line) "\n")))

;; ---------------------------------------------------------------------------
;; File/Module header block

(defun pel-skels-c-header-module-block (fname is-a-header cmt-style)
  "Return a tempo list for the comment block inserted at the top of the C file.
The arguments are:
- FNAME := string.  the name of the current file without path.
- IS-A-HEADER := boolean.  non-nil if the file is a C header file, nil
  otherwise.
- CMT-STYLE := a list of 3 strings: (cb cc ce)
            - cb : comment begin string
            - cc : comment continuation string
            - ce : comment end string."
  (let* ((purpose  (pel-prompt-purpose-for "File" 'p))
         (cb       (nth 0 cmt-style))
         (cc       (nth 1 cmt-style))
         (ce       (nth 2 cmt-style)))
    (list
     'l
     cb " C " (if is-a-header "HEADER" "MODULE") ": "  fname 'n
     cc 'n
     cc " Purpose   : " purpose 'n
     (pel-skel-created-comment cc)
     (pel-skel-author-comment  cc)
     (pel-skel-time-stamp pel-c-skel-insert-file-timestamp cc)
     (pel-skel-copyright-comment pel-c-skel-with-license cc)
     ce (pel-when-text-in ce 'n))))

(defun pel-skels-c-file-header-block ()
  "Return a tempo list for a C file header block.
The format of the file header block is adjusted for the supported file types:
the C code file and the C header file.
The file header portion is controlled by the style selected by the
variable `pel-c-skel-module-header-block-style'."
  (pel-skel-cc-file-header-block pel-c-skel-module-header-block-style
                                 'pel-skels-c-header-module-block/custom
                                 pel-c-skel-hfile-section-titles
                                 pel-c-skel-cfile-section-titles
                                 pel-c-skel-use-include-guards
                                 (function pel-skels-c-header-module-block)
                                 (function pel-skel-c-separator-line)
                                 '("h" "i")))

;; ---------------------------------------------------------------------------
;; C function definitions

(defun pel-valid-c-function-name (text)
  "Return TEXT if it is a valid C function name, nil otherwise.
Replace dash characters with underscores, to simplify typing function
names using underscores."
  (let ((text (replace-regexp-in-string "-" "_" (string-trim text))))
    (when (string-match "\\`[a-zA-Z_][a-zA-Z0-9_]*\\'" text)
      text)))

(defun pel-skels-c-function-def (&optional name)
  "Insert just the function definition code.
The function NAME can be passed via arguments,
prompt user otherwise.
When NAME is specified the optional separator line is *not* inserted:
it's assumed that another function has already done it."
  (let* ((fct-name   (or name (pel-prompt-function
                               (function pel-valid-c-function-name))))
         (sk         (list 'l (unless name (pel-skel-c-separator-line)))))
    (if pel-c-skel-function-name-on-first-column
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

(defun pel-skels-c-function-def-basic (&optional name purpose)
  "Insert a basic function code template with simple comment block.
The function NAME and PURPOSE can be passed via arguments,
prompt user otherwise."
  (let* ((fct-name   (or name (pel-prompt-function
                               (function pel-valid-c-function-name))))
         (purpose    (or purpose (pel-prompt-purpose-for "Function" 'p)))
         (cmt-style  (pel-skel-comments-strings))
         (cb         (nth 0 cmt-style))
         (ce         (nth 2 cmt-style)))
    (list
     'l
     (pel-skel-c-separator-line)
     cb " " fct-name "() -- " purpose 'n
     ce 'n (pel-when-text-in ce 'n)
     (pel-skels-c-function-def fct-name))))

(defun pel-skels-c-function-def-man ()
  "Insert a MAN-style C function definition command block.
This begins with an optional separator line, the name of the
function spread and underlined with its purpose on the same line.
This style is selected when the user option variable
`pel-c-skel-function-define-style' is set to the value man-style.
The comment style is controlled by the CC mode variable
`c-block-comment-flag'."
  (let* ((fct-name        (pel-prompt-function
                           (function pel-valid-c-function-name)))
         (purpose         (pel-prompt-purpose-for "Function" 'p))
         (cmt-style       (pel-skel-comments-strings))
         (cb              (nth 0 cmt-style))
         (cc              (nth 1 cmt-style))
         (ce              (nth 2 cmt-style))
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
         (pel-append-to sk (list ce 'n (pel-when-text-in ce 'n)))))
     (pel-skels-c-function-def fct-name))))

(defun pel-skels-c-function-definition ()
  "Insert a tempo skeleton for the insertion of a C function definition.
Insert the skeleton selected by the user option variable
`pel-c-skel-function-define-style'."
  (cond ((not pel-c-skel-function-define-style)
         (pel-skels-c-function-def))
        ((eq pel-c-skel-function-define-style 'basic-style)
         (pel-skels-c-function-def-basic))
        ((eq pel-c-skel-function-define-style 'man-style)
         (pel-skels-c-function-def-man))
        (t (pel-skel-call 'pel-c-skel-function-define-style
                          'pel-skels-c-function-def/custom))))

;; ---------------------------------------------------------------------------
(defun pel-skels-c-pp-define ()
  "Return a tempo list for a C #define statement.
If point is after text, go to new line.
Leave point after statement."
  (list
   'l
   '& "#define " 'p ))

(defun pel-skels-c-pp-include-local ()
  "Return a tempo list for a C #include \"\" statement.
If point is after text, go to new line.
Leave point when file name goes, then after statement."
  (list
   'l
   '& "#include \"" 'p ".h\"" 'p))

(defun pel-skels-c-pp-include-global ()
  "Return a tempo list for a C #include <> statement.
If point is after text, go to new line.
Leave point when file name goes, then after statement."
  (list
   'l
   '& "#include <" 'p ".h>" 'p))

;; ---------------------------------------------------------------------------
;; Install C Code skeletons

(defvar pel-skels-c-large-header-skel
  '(o
    (pel-skels-c-file-header-block))
  "The skeleton of a C file header block.")

(defvar pel-skels-c-function-definition-skel
  '(o
    (pel-skels-c-function-definition))
  "The skeleton of a C function definition block.")

(defvar pel-skels-c-pp-define-skel
  '((pel-skels-c-pp-define))
  "The skeleton of a C #define statement.")

(defvar pel-skels-c-pp-include-local-skel
  '(o
    (pel-skels-c-pp-include-local))
  "The skeleton of a #include \"\" statement.")

(defvar pel-skels-c-pp-include-global-skel
  '(o
    (pel-skels-c-pp-include-global))
  "The skeleton of a #include <> statement.")

(defvar pel--c-skels
  '(("File Header"   "file-header"   pel-skels-c-large-header-skel)
    ("Function"      "function"      pel-skels-c-function-definition-skel)
    ("Define"        "define"        pel-skels-c-pp-define-skel)
    ("Include \"\""  "include-local" pel-skels-c-pp-include-local-skel)
    ("Include <>"    "include-lib"   pel-skels-c-pp-include-global-skel))


  "List of C code tempo skeletons.")

(defvar pel--c-skels-keys '(("file-header"   . "h")
                            ("function"      . "f")
                            ("define"        . "d")
                            ("include-local" . "I")
                            ("include-lib"   . "i"))
  "Key mapping for C skeletons.")

;;-pel-autoload
(defun pel--install-c-skel (key-map)
  "Create the C skeletons and bind them in the KEY-MAP specified.
This function is meant to be called by the function `pel-init' only."
  (pel-tempo-install-pel-skel
   "c"
   pel--c-skels
   key-map
   pel--c-skels-keys
   "c"
   'pel-c-skeleton-control))

;;; --------------------------------------------------------------------------
(provide 'pel-skels-c)

;;; pel-skels-c.el ends here
