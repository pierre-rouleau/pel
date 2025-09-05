;;; pel-dtreplace.el --- Directory tree find/replace.  -*- lexical-binding: t; -*-

;; Created   : Thursday, September  4 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-09-05 16:36:37 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2025  Pierre Rouleau
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
;; This file provides the `pel-dirtree-find-replace' command which is used to
;; replace text in several files under a specified directory tree.  The code
;; is completely implemented in Emacs Lisp; it does not depend on any shell
;; utility.
;;
;; The `pel-dirtree-replace' customization group holds a set of user-options
;; that identify whether the operation is verbose, printing the information as
;; messages, and whether the modified files are backed-up.
;;
;; The call tree of the functions defined here is:
;;
;; * `pel-dirtree-find-replace'
;;   - `pel--dt-prompt'
;;   - `pel-find-replace'
;;   - `pel--dt'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)          ; use `pel-toggle-and-show-user-option'
(require 'pel--options)       ; use `pel-pkg-for-search'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defgroup pel-dirtree-replace nil
  "Customization of the pel-dirtree-replace commands."
  :group 'pel-pkg-for-search)

(defcustom pel-dirtree-replace-files-is-verbose t
  "Print list of files modified by `pel-dirtree-find-replace'."
  :group 'pel-dirtree-replace
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-dirtree-replace-file-newtext-is-fixedcase t
  "Whether the new text replacement string is fixed case or not.

If it is non-nil, `pel-dirtree-replace-file'  not alter the case of
the replacement text.  Otherwise, maybe capitalize the whole text, or
maybe just word initials, based on the replaced text.  If the replaced
text has only capital letters and has at least one multiletter word,
convert NEWTEXT to all caps.  Otherwise if all words are capitalized
in the replaced text, capitalize each word in NEWTEXT.  Note that
what exactly is a word is determined by the syntax tables in effect
in the current buffer, and the variable `case-symbols-as-words’."
  :group 'pel-dirtree-replace
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-dirtree-replace-file-newtext-is-literal t
  "Whether the new text replacement string is literal or an Emacs regexp.

If optional it is non-nil, NEWTEXT is inserted literally.
Otherwise treat ‘\\’ as special:
  ‘\\&’ in NEWTEXT means substitute original matched text.
  ‘\\N’ means substitute what matched the Nth ‘\\(...\\)’.
       If Nth parens didn’t match, substitute nothing.
  ‘\\\\’ means insert one ‘\\’.
  ‘\\?’ is treated literally
       (for compatibility with ‘query-replace-regexp’).
  Any other character following ‘\\’ signals an error.
Case conversion does not apply to these substitutions."
  :group 'pel-dirtree-replace
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-dirtree-replace-file-backup-suffix ".original"
  "Whether the `pel-dirtree-find-replace' creates a backup file.
If nil, it does not backup files.
If a string, it creates a backup file and the name of the backup is the file
name with the string is appended."
  :group 'pel-dirtree-replace
  :type '(choice
          (const :tag "Do not create backup files" nil)
          (string :tag "Create backup files with following suffix")))

(defcustom pel-dirtree-replace-file-forbidden-dir-re '("/\\.")
  "List of directory names that hold files that must not be modified."
  :group 'pel-dirtree-replace
  :type '(repeat
          (string :tag "Base name regexp of directory to ignore")))

(defvar pel-dirtree-replaced-files nil
  "List of files replaced by last `pel-dirtree-find-replace' command.")

(defvar pel-dirtree-rootdir nil
  "Root directory used in last `pel-dirtree-find-replace' command.")

(defun pel-find-replace (fname text-re new-text)
  "Replace TEXT with NEW-TEXT inside FNAME file.
When `pel-dirtree-replace-files-is-verbose' is non-nil, it
prints a message showing how many instances were replaced."
  (let ((mods 0)
        (backup-fname nil))
    (with-temp-buffer
      (insert-file-contents fname)
      (goto-char (point-min))
      (while (re-search-forward text-re nil :noerror)
        (replace-match new-text
                       pel-dirtree-replace-file-newtext-is-fixedcase
                       pel-dirtree-replace-file-newtext-is-literal)
        (setq mods (1+ mods)))
      (unless (eq mods 0)
        (when pel-dirtree-replace-file-backup-suffix
          (setq backup-fname (format "%s%s" fname
                                     pel-dirtree-replace-file-backup-suffix))
          (copy-file fname backup-fname :ok-if-exists))
        (write-region (point-min) (point-max) fname)
        (push fname pel-dirtree-replaced-files)
        (when pel-dirtree-replace-files-is-verbose
          (message "Modified %s: %d instance of: %s -> %s"
                   fname mods text-re new-text))))))


(defun pel--allow-descent-in (dirname)
  "Return t if recursive descent is allowed in DIRNAME, nil otherwise."
  (let ((forbidden-name-regexp-list pel-dirtree-replace-file-forbidden-dir-re)
        (forbidden-dirname-re nil)
        (is-forbidden nil))
    (while forbidden-name-regexp-list
      (setq forbidden-dirname-re (pop forbidden-name-regexp-list))
      (when (string-match forbidden-dirname-re dirname)
        (setq is-forbidden t)
        (setq forbidden-name-regexp-list nil)))
    (not is-forbidden)))

(defun pel--dt (root-dir fn-re)
  "Return a list of files with names matching FN-RE under ROOT-DIR."
  (directory-files-recursively root-dir
                               fn-re
                               nil
                               (function pel--allow-descent-in)))

(defun pel--dt-prompt  (prompt scope)
  "Print PROMPT formatted, read minibuffer with SCOPE history."
  (substring-no-properties
   (read-from-minibuffer (format "%s: " prompt)
                         nil nil nil
                         (intern (format "pel-dt-fr-%s" scope)))))

;;-pel-autoload
(defun pel-dirtree-find-replace (text-re new-text root-dir fn-re)
  "Replace TEXT-RE with NEW-TEXT in all files named FN-RE under ROOT-DIR.
TEXT-RE is a Emacs regular expression.
FN-RE is also a Emacs regular expression (not a glob!)

The following user-options control various aspects of the operation:
- `pel-dirtree-replace-file-forbidden-dir-re': a list of regexp identifying
  directory base names that are skipped (no recursive descent is done in
  these directories and no file inside them will be modified).
- `pel-dirtree-replace-files-is-verbose': if non-nil (the default) the
  command prints messages listing the names of the modified files and
  prints the count of modified files.
- `pel-dirtree-replace-file-backup-suffix': a string that identifies the
  suffix used when creating backup files: the name of the backup file has
  the original file name with this suffix appended to its name. This can
  also be set to nil, which disables creation of backup files. The default
  is a the \".original\" string.
- `pel-dirtree-replace-file-newtext-is-fixedcase': a boolean that determines
  whether the replacement text is done as a fixed case replacement or
  follows the case folding rules currently used.
- `pel-dirtree-replace-file-newtext-is-literal': a boolean that determines
  whether the replacement text is used literally (when *t*, the default) or
  text is an Emacs regexp, allowing regexp based text replacements.


The function remembers a list of modified files.
Use the command `pel-dt-fr-changed-files-in-dired' to open a Dired
buffer with this list of files."
  (interactive
   (list
    (pel--dt-prompt "Text regexp" 'text-re)
    (pel--dt-prompt (format "%s%sreplacement (%s case)"
                            (if pel-dirtree-replace-files-is-verbose
                                "Verbose "
                              "Silent ")
                            (if pel-dirtree-replace-file-newtext-is-literal
                                ""
                              "regexp ")
                            (if pel-dirtree-replace-file-newtext-is-fixedcase
                                "fixed"
                              "adjusted"))
                    'new-text)
    (read-directory-name "Root directory: " )
    (pel--dt-prompt "File name regexp" 'fn-re)))
  (cond
   ((string= fn-re ".")
    (user-error ". means all files! Use a more restricted regexp!"))
   ((string= fn-re "")
    (user-error "Please specify a non-empty Emacs regexp!"))
   (t
    ;; process each file found open each file
    (setq pel-dirtree-replaced-files nil)
    (mapc (lambda (fname)
            (pel-find-replace fname text-re new-text))
          (pel--dt root-dir fn-re))
    (setq pel-dirtree-rootdir root-dir)
    (when pel-dirtree-replace-files-is-verbose
      (message "Replaced text inside %d files"
               (length pel-dirtree-replaced-files))))))

;;-pel-autoload
(defun pel-dt-fr-set-backup-suffix (new-suffix)
  "Change backup suffix used by `pel-dirtree-find-replace'.
Modify the value of `pel-dirtree-replace-file-backup-suffix' in current session."
  (interactive
   (list
    (pel--dt-prompt (format "Backup suffix [%s]"
                            (or pel-dirtree-replace-file-backup-suffix ""))
                    'new-suffix)))

  (setq pel-dirtree-replace-file-backup-suffix new-suffix)
  (if (string=  new-suffix "")
      (progn
        (setq pel-dirtree-replace-file-backup-suffix nil)
        (message "pel-dirtree-find-replace no longer creates backup files."))
    (message "pel-dirtree-find-replace backup files now use suffix %S" new-suffix)))

;;-pel-autoload
(defun pel-dt-fr-toggle-fixedcase ()
  "Change behaviour of `pel-dirtree-find-replace' string replacement fixedcase.
Toggle `pel-dirtree-replace-file-newtext-is-fixedcase' to change whether
the function performs a fixed case string replacement or adjust capitalization
based on the replaced text."
  (interactive)
  (pel-toggle-and-show-user-option
   'pel-dirtree-replace-file-newtext-is-fixedcase
   :globally
   "t: perform fixed case replacement."
   "nil: perform case adjusted replacement."))

;;-pel-autoload
(defun pel-dt-fr-toggle-literal ()
  "Change behaviour of `pel-dirtree-find-replace' literal string replacement.
Toggle `pel-dirtree-replace-file-newtext-is-literal' to change whether
the function performs a literal string replacement or interpret the new-text
string as an Emacs regexp."
  (interactive)
  (pel-toggle-and-show-user-option
   'pel-dirtree-replace-file-newtext-is-literal
   :globally
   "t: new-text is a literal replacement."
   "nil: new-text is an Emacs regexp."))

;;-pel-autoload
(defun pel-dt-fr-changed-files-in-dired ()
  "Show all files changed by `pel-dirtree-replaced-file' in a dired buffer."
  (interactive)
  (if pel-dirtree-replaced-files
      (let ((fnames pel-dirtree-replaced-files)
            (mod-fnames nil))
        (when pel-dirtree-replace-file-backup-suffix
          (dolist (fn fnames)
            (push (format "%s%s" fn pel-dirtree-replace-file-backup-suffix)
                  mod-fnames))
          (setq fnames (append fnames mod-fnames))
          (setq fnames (sort fnames)))
        (dired (cons (format "%s (modified files)" pel-dirtree-rootdir)
                     fnames)))
    (user-error
     "pel-dirtree-replaced-file has not been used to modify files!")))

;;; --------------------------------------------------------------------------
(provide 'pel-dtreplace)

;;; pel-dtreplace.el ends here
