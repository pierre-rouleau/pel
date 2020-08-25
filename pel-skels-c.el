;;; pel-skels-c.el --- Tempo skeletons for C.  -*- lexical-binding: t; -*-

;; Created   : Monday, August 24 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2020-08-25 11:27:38, updated by Pierre Rouleau>

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
(require 'pel-prompt)      ; use: pel-prompt-purpose-for
(require 'pel-skels)       ; use: pel-skel-create-comment
;;                         ;      pel-skel-author-comment
;;                         ;      pel-skel-time-stamp
(require 'pel-tempo)       ; use: pel-tempo-install-pel-skel
(require 'pel-text-insert) ; use: pel-separator-line
(require 'pel-uuid)        ; use: pel-c-include-guard
;;; ----------------------------------------------------------------------------
;;; Code:
;;

(defun pel-skel-c-separator-line ()
  "Return a section separator line for C if required.
If prohibited (by customization) returns nil.
Otherwise return a string that ends with a newline."
  (when pel-elisp-skel-use-separators
    (concat (pel-separator-line) "\n")))


(defun pel-skels-c-file-header-block ()
  "Return a tempo list for a C file header block.
The format of the file header block is adjusted for the supported file types:
the C code file and the C header file."
  (let* ((purpose      (pel-prompt-purpose-for "File" 'p))
         (fname        (pel-current-buffer-filename :sans-directory))
         (fn-extension (file-name-extension fname))
         (is-a-header  (string= fn-extension "h")))
    (list
     'l
     "/* C " (if is-a-header "HEADER" "MODULE") ": "  fname 'n
     "**\n"
     "** Purpose   : " purpose 'n
     (pel-skel-created-comment "**")
     (pel-skel-author-comment  "**")
     (pel-skel-time-stamp pel-c-skel-insert-file-timestamp "**")
     (when pel-c-skel-with-license
       (list 'l
             "**\n"
             (pel-license-text "**")
             "**\n"))
     "*/\n"
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
       (list
        'l
        (when t ; pel-c-skel-insert-sections
          (list
           'l
           "/* Module Description\n"
           "** ------------------\n"
           "**\n"
           "** " 'p 'n
           "**\n"
           "*/\n\n"
           (pel-skel-c-separator-line)
           "/* Header Inclusion\n"
           "** ----------------\n"
           "*/\n\n"
           'p 'n 'n
           (pel-skel-c-separator-line)
           "/* Local Types\n"
           "** -----------\n"
           "*/\n\n"
           'p 'n 'n
           (pel-skel-c-separator-line)
           "/* Local Variables\n"
           "** ---------------\n"
           "*/\n\n"
           'p 'n 'n
           (pel-skel-c-separator-line)
           "/* Code\n"
           "** ----\n"
           "*/\n\n"
           ))
        (list
         'l
         'p 'n 'n
         (pel-skel-c-separator-line)))))))

;; -----------------------------------------------------------------------------
;; Install Emacs Lisp skeletons

(defvar pel-skels-c-large-header-skel
  '(o
    (pel-skels-c-file-header-block))
  "The skeleton of an Emacs Lisp file header block.
Please see the function `tempo-define-template'.")

(defvar pel--c-skels
  '(("File Header" "file-header" pel-skels-c-large-header-skel))
  "List of Emacs Lisp tempo skeletons.")

(defvar pel--c-skels-keys '(("file-header" . "C-h"))
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
