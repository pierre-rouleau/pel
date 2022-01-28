;;; pel-cc-find.el --- CC modes find file.  -*- lexical-binding: t; -*-

;; Created   : Monday, November 29 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-01-27 21:52:00, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2022  Pierre Rouleau
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
;;  This provides the `pel-cc-find-activate-finder-method' function that is
;;  meant to be called inside a hook for a major-mode of a programming
;;  language like the CC modes but also other modes where you want to search
;;  for a file name in directories that may be imposed by the project or a
;;  set of tools or both.
;;
;;  Each programming language supported has a user-option with a name that
;;  looks like 'pel-XX-file-finder-method' where 'XX' is the prefix of the
;;  major mode name, like 'c' for `c-mode' and 'c++' for `c++-mode'.
;;
;; The user-option identify various methods to search for the file.  Look at
;; the docstring of `pel-c-file-finder-method' for the description.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)             ; use: `pel-major-mode-symbol-value'
(require 'pel--options)          ; use: `pel-c-file-finder-ini-tool-name'
;;                               ;      `pel-c++-file-finder-ini-tool-name'
(require 'pel-file)              ; use: `pel-filename-at-point-finders'
(require 'pel-ffind)             ; use: `pel-ffind-project-directory'
(require 'pel-ffind-inpath)      ; use: `pel-ffind-inpath-include'
(require 'pel-ini)               ; use: `pel-ini-load'
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defvar pel--c-file-finder-ini-tool-name pel-c-file-finder-ini-tool-name
  "Identifies the name of an extra list of directories to use in C.")

(defvar pel--c++-file-finder-ini-tool-name pel-c++-file-finder-ini-tool-name
  "Identifies the name of an extra list of directories to use in C.")

;;-pel-autoload
(defun pel-cc-set-file-finder-ini-tool-name (&optional tool-name)
  "Select a new value for `pel-CC-file-finder-ini-tool-name'.

CC is 'c' for C files, 'c++' for C++ files."
  (interactive "sTool name: ")
  (cond
   ((eq major-mode 'c-mode)
    (setq pel--c-file-finder-ini-tool-name tool-name)
    (message "\
 pel-open-at-point executed in a C file
 now search the project-path directories and the ones identified by the %s pel.ini key"
             tool-name))
   ((eq major-mode 'c++-mode)
    (setq pel--c++-file-finder-ini-tool-name tool-name)
    (message "\
 pel-open-at-point executed in a C++ file
 now search the project-path directories and the ones identified by the %s pel.ini key"
             tool-name))
   (t (user-error "This is for C and C++ files only!"))))

;; ---------------------------------------------------------------------------
(defun pel-envar-in-string (string)
  "Return names of environment variables extracted from the string.

The variables must be prefixed with a '$' character and must end
with a non alphanumeric or underscore character."

  (let ((varnames nil)
        (idx 0)
        (new-idx nil)
        varname)
    (while (setq new-idx
                 (string-match "\\$\\([[:alnum:]_]*\\)"
                               (substring string idx)))
      (setq varname (match-string 1 (substring string idx)))
      (push varname varnames)
      (setq idx (+ idx new-idx 1 (length varname))))
    (reverse varnames)))

(defun pel-substitute-in-file-name (filename)
  "Substitute environment variables referred to in FILENAME.

Does the same as `substitute-in-file-name' but when an environment variable is
unknown"
  ;; First check and raise a user error if there is any unknown environment
  ;; variable inside the filename string
  (dolist (varname (pel-envar-in-string filename))
    (unless (getenv varname)
      (user-error "In \"%s\", the environment variable %s is unknown!"
                  filename varname)))
  ;; then return the string with  everything substituted.
  (substitute-in-file-name filename))

;; ---------------------------------------------------------------------------

;;-pel-autoload
(defun pel-cc-find-via-pel-ini (filename)
  "Search FILENAME in directories defined by pel.ini [file-finder] section.

Return a list of found file path names."
  (let ((tool-name (pel-major-mode-symbol-value
                    "pel--%s-file-finder-ini-tool-name"))
        (pel-ini-filename (car (pel-generic-find-file "pel.ini")))
        (pel-ini-alist nil)
        (section nil)
        (project-path nil))
    (unless pel-ini-filename
      (user-error "No pel.ini file found in directory tree."))

    (setq pel-ini-alist (pel-ini-load pel-ini-filename))
    (unless pel-ini-alist
      (user-error "File %s is not a valid .INI file"
                  pel-ini-filename))

    (setq section (cdr (assoc "file-finder" pel-ini-alist)))
    (unless section
      (user-error "The INI file %s is missing section [file-finder]"
                  pel-ini-filename))

    (setq project-path (cdr (assoc "project-path" section)))
    (unless project-path
      (user-error "The INI file %s is missing the project-path key"
                  pel-ini-filename))
    ;; Extend path with optional tool-specific directories if one is specified
    ;; and the corresponding key is in the INI file.
    (when tool-name
      (setq project-path
            (delete-dups (append project-path
                                 (cdr (assoc tool-name section))))))
    ;; perform file search inside identified directories
    ;; for each path in the list expand "~" and any environment variable using
    ;; the "$VARNAME" form.
    (delete-dups
     (pel-ffind filename
                (mapcar (function expand-file-name)
                        (condition-case err
                            (mapcar (function pel-substitute-in-file-name)
                                    project-path)
                          (error
                           (user-error
                            "\
Can't find location of %s using include path spec identified in:
 %s::
    %s"
                            filename pel-ini-filename (cadr err)))))))))

;;-pel-autoload
(defun pel-cc-find-activate-finder-method (&optional file-finder-method)
  "Activate the file finder method for buffers of current major-mode."
  (unless file-finder-method
    (setq file-finder-method
          (pel-major-mode-symbol-value "pel-%s-file-finder-method")))
  ;; Set pel-filename-at-point-finders according to what specified in the
  ;; file-finder-method variable for the current major-mode.
  (cond
   ;; --
   ;; Use a generic file tree search
   ((eq file-finder-method 'generic)
    (setq pel-filename-at-point-finders '(pel-generic-find-file)))
   ;; --
   ;; Use directories identified by pel.ini file [file-finder] section
   ((eq file-finder-method 'pel-ini-file)
    (setq pel-filename-at-point-finders (list (function pel-cc-find-via-pel-ini))))
   ;; --
   ;; Search in the directories identified in specified environment variable
   ((stringp file-finder-method)
    (setq pel-filename-at-point-finders
          (list
           (lambda (fn)
             (pel-ffind-inpath-include fn file-finder-method)))))
   ;; --
   ;; Search in explicit list of project and tools directories
   ((and (listp file-finder-method)
         (eq (length file-finder-method) 2))
    (let ((path-list (apply 'append file-finder-method)))
      ;; expand any reference to an environment variable in path-names
      (setq path-list (mapcar (function substitute-in-file-name) path-list))
      (setq pel-filename-at-point-finders
            (list
             (lambda (fn)
               (pel-ffind-inpath fn path-list))))))
   ;; --
   ;; no other method currently supported
   (t (error (format "invalid file-finder-method: %S for %s"
                     file-finder-method
                     major-mode)))))

;;; --------------------------------------------------------------------------
(provide 'pel-cc-find)

;;; pel-cc-find.el ends here
