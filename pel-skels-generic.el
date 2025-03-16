;;; pel-skels-generic.el --- Generic code tempo skeletons.  -*- lexical-binding: t; -*-

;; Created   : Sunday, August 30 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-03-16 11:33:33 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2021, 2025  Pierre Rouleau
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
;; This file defines tempo skeletons that can be used for all files.  They are
;; not specialized for a specific programming or markup language.  They only
;; adjust the strings used for comments.

;; The only command is `pel--install-generic-skel' which dynamically creates
;; skeletons commands with keyboard mappings for a specific keyboard map
;; identified by the argument.  The current skeleton definition creates a
;; single skeleton-driven command:
;;
;;  - `pel-skels-generic-file-header-block', mapped to the `h' key, which
;;     writes a file header.
;;
;; The function hierarchy is:
;;
;; - `pel--install-generic-skel'
;;
;; # File Header block:
;;   - `pel-skels-generic-file-header-block'
;;     - `pel-skels-generic-header-module-block'
;;       - `pel-skels-generic-first-line'
;;         - `pel--file-isa-sourced-script'
;;     - `pel--file-isa-sourced-script'
;;
;; Tempo Skeleton List Syntax
;; --------------------------
;;
;; See the `tempo-define-template' docstring for information about
;; the syntax of the tempo skeleton lists.  These lists contain symbols
;; and strings that describe the text to insert and how to insert it.  Inside
;; this file the functions create these tempo skeleton lists and are based on
;; code creating lists based on list that may also contain functions that
;; return symbols and strings.

;;; ----------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)       ; use: pel-current-buffer-filename
(require 'pel--options)    ; use: pel-generic-skel-module-header-block-style
(require 'pel--macros)     ; use: pel-append-to
(require 'pel-prompt)      ; use: pel-prompt-purpose-for
(require 'pel-skels)       ; use: pel-skel-create-comment
;;                         ;      pel-skel-author-comment
;;                         ;      pel-skel-time-stamp
;;                         ;      pel-skel-call
(require 'pel-tempo)       ; use: pel-tempo-install-pel-skel
(require 'pel-text-insert) ; use: pel-separator-line
(eval-when-compile
  (require 'subr-x))       ; use: string-trim-right

;;; ----------------------------------------------------------------------------
;;; Code:
;;

;; --
;; Utility functions

(defun pel-skels-generic-separator-line ()
  "Return a section separator line for generic template is required.
If prohibited (by customization) returns nil.
Otherwise return a string that ends with a newline."
  (when pel-generic-skel-use-separators
    (concat (pel-separator-line) "\n")))

;; -----------------------------------------------------------------------------
;; File/Module header block

(defun pel--file-isa-sourced-script (fname)
  "Return non-nil if the FNAME is the name of a sourced script, nil otherwise.
The identification is done by a match with the user-option variable
`pel-shell-sourced-script-file-name-prefix'"
  (let ((file-extension (file-name-extension fname)))
    (when (and (eq major-mode 'sh-mode)
               pel-shell-sourced-script-file-name-prefix
               (or  (null file-extension)
                    (not (member file-extension pel-shell-script-extensions))))
      (string-match pel-shell-sourced-script-file-name-prefix fname))))

(defun pel-skels-generic-first-line (fname has-shebang)
  "Return a string for the first 1 or 2 lines.

- FNAME := string. the name of the file without path.
- HAS-SHEBANG := t if the buffer has a shebang line, nil otherwise.

It first checks if the buffer is for a source script or a regular, executable
script.

- For sourced script: it returns the first line that holds the name of the
  file.
- For shell script: it returns 1 or 2 lines:
  - If a shebang line is already present, the function only returns the second
    line showing the file type and file name.
  - If the shebang line is not present, the function returns a string with 2
    lines:
    - the shebang line,
    - the line showing the file type and file name."
  (if (pel--file-isa-sourced-script fname)
      ;; For a sourced file
      (concat (format pel-shell-sourced-script-first-line fname) "\n")
    ;; For a script file
    (format "%s%s FILE: %s\n"
            (if (eq major-mode 'sh-mode)
                (if has-shebang
                    "\n# "
                  (format "%s\n# "
                          pel-shell-script-shebang-line))
              " ")
            (upcase (car (split-string (symbol-name major-mode) "-")))
            fname)))

(defun pel-skels-generic-header-module-block (fname cmt-style has-shebang)
  "Return a tempo list for the header/module comment block.
The arguments are:
- FNAME := string.  the name of the current file without path.
- CMT-STYLE := a list of 3 strings: (cb cc ce)
            - cb : comment begin string, or a list of 4 strings
            - cc : comment continuation string
            - ce : comment end string.
- HAS-SHEBANG := t if the buffer has a shebang line, nil otherwise."
  (let* ((purpose  (pel-prompt-purpose-for "File" 'p))
         (cb       (nth 0 cmt-style))
         (has4     (listp cb))
         (cb       (string-trim-right (if has4 (nth 2 cb) cb)))
         (cc       (string-trim-right (if has4 cb (nth 1 cmt-style))))
         (ce       (nth 2 cmt-style)))
    (list
     'l
     (unless has-shebang cb)
     (pel-skels-generic-first-line fname has-shebang)
     cc 'n
     cc " Purpose   : " purpose 'n
     (pel-skel-created-comment cc nil :no-new-line)
     (pel-skel-author-comment  cc nil :no-new-line)
     (pel-skel-time-stamp pel-generic-skel-insert-file-timestamp cc)
     (pel-skel-copyright-comment pel-generic-skel-with-license cc)
     ce (pel-when-text-in ce 'n)
     (pel-separator-line) 'n)))

(defun pel-skels-generic-file-header-block ()
  "Return a tempo list for a generic file header block.
The format of the file header block is adjusted for the comment
style of the current file.
The file header portion is controlled by the style selected by the
`pel-generic-skel-module-header-block-style' user-option."
  ;; First move point to the top of the buffer and check if there is already
  ;; a shebang line.  If there is one, move point to the end of the line,
  ;; insert a new line and prevent inserting a new shebang line.
  (goto-char (point-min))
  (let* ((fname        (pel-current-buffer-filename :sans-directory))
         (cmt-style    (pel-skel-comments-strings))
         (has-shebang  (pel-has-shebang-line))
         (cb       (nth 0 cmt-style))
         (has4     (listp cb))
         (cb       (string-trim-right (if has4 (nth 2 cb) cb)))
         (cc       (string-trim-right (if has4 cb (nth 1 cmt-style))))
         (ce       (nth 2 cmt-style)))

    (when has-shebang
      (move-end-of-line 1))
    (list
     'l
     ;; insert the top level comment block for the top of the file
     ;; Select the style from `pel-generic-skel-module-header-block-style'
     (if pel-generic-skel-module-header-block-style
         (pel-skel-call 'pel-generic-skel-module-header-block-style
                        'pel-skels-generic-header-module-block/custom
                        fname
                        cmt-style)
       (pel-skels-generic-header-module-block fname cmt-style has-shebang))
     ;; then add the remainder for either a header file or code file
     (let ((sk (list 'l)))
       (if pel-generic-skel-module-section-titles
           (dolist (mtitle pel-generic-skel-module-section-titles)
             (pel-append-to sk
                            (list
                             cb " " mtitle 'n
                             cc " " (make-string (length mtitle) ?-) 'n
                             cc 'n
                             cc " " 'p 'n (pel-when-text-in ce (format "%s\n" ce))
                             'n 'n
                             (pel-skels-generic-separator-line))))
         (pel-append-to sk (list
                            'n
                            'p 'n 'n)))
       sk))))

;; -----------------------------------------------------------------------------
;; Install Generic skeletons
;;

(defvar pel-skels-generic-large-header-skel
  '(
    (pel-skels-generic-file-header-block))
  "The skeleton of a generic file header block.")

(defvar pel-skels-generic-function-definition-skel
  '(o
    (pel-skels-generic-function-definition))
  "The skeleton of a generic function definition block.")

(defvar pel--generic-skels
  '(("File Header" "file-header" pel-skels-generic-large-header-skel))
  "List of generic file tempo skeletons.")

(defvar pel--generic-skels-keys '(("file-header" . "h"))
  "Key mapping for generic file skeletons.")

;;-pel-autoload
(defun pel--install-generic-skel (key-map)
  "Create the generic skeletons and bind them in the KEY-MAP specified.
This function is meant to be called by the function `pel-init' only."
  (pel-tempo-install-pel-skel
   "generic"
   pel--generic-skels
   key-map
   pel--generic-skels-keys
   "generic"))

;;; ----------------------------------------------------------------------------
(provide 'pel-skels-generic)

;;; pel-skels-generic.el ends here
