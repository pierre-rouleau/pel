;;; pel-dtreplace.el --- Directory tree find/replace.  -*- lexical-binding: t; -*-

;; Created   : Thursday, September  4 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-09-04 17:40:33 EDT, updated by Pierre Rouleau>

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
(require 'pel--options)                 ; use pel-pkg-for-search

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

(defcustom pel-dirtree-replace-file-backup-suffix ".original"
  "Whether the `pel-dirtree-find-replace' creates a backup file.
If nil, it does not backup files.
If a string, it creates a backup file and the name of the backup is the file
name with the string is appended."
  :group 'pel-dirtree-replace
  :type '(choice
          (const :tag "Do not create backup files" nil)
          (string :tag "Create backup files with following suffix")))

(defvar pel-dirtree-replaced-files nil
  "List of files replaced by `pel-dirtree-find-replace'")

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
        (replace-match new-text :fixedcase :literal)
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


(defun pel--dt (root-dir fn-re)
  "Return a list of files with names matching FN-RE under ROOT-DIR."
  (directory-files-recursively root-dir
                               fn-re
                               nil
                               (lambda (x) (not (string-match-p "/\\." x)))))

(defun pel--dt-prompt  (prompt scope)
  "Print PROMPT formatted, read minibuffer with SCOPE history."
  (read-from-minibuffer (format "%s: " prompt)
                        nil nil nil
                        (intern (format "pel-dirtree-find-replace-%s" scope))))

;;-pel-autoload
(defun pel-dirtree-find-replace (text-re new-text root-dir fn-re)
  "Replace TEXT-RE  with NEW-TEXT in all files named FN-RE under ROOT-DIR.
TEXT-RE is a Emacs regular expression.
FN-RE is also a Emacs regular expression (not a glob!)
Create backup of each modified file.
List all modified files inside a Dired buffer."
  (interactive
   (list
    (pel--dt-prompt "Text regexp" 'text-re)
    (pel--dt-prompt "Replacement" 'new-text)
    (read-directory-name "Root directory: " )
    (pel--dt-prompt "File name regexp" 'fn-re)))
  ;; process each file found open each file
  (setq pel-dirtree-replaced-files nil)
  (mapc (lambda (fname)
          (pel-find-replace fname text-re new-text))
        (pel--dt root-dir fn-re))
  (message "Replaced: %S" pel-dirtree-replaced-files))

;;; --------------------------------------------------------------------------
(provide 'pel-dtreplace)

;;; pel-dtreplace.el ends here
