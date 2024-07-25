;;; pel-cc-find.el --- CC modes find file.  -*- lexical-binding: t; -*-

;; Created   : Monday, November 29 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2024-07-25 12:42:09 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2022, 2024  Pierre Rouleau
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
(require 'pel-ffind)             ; use: `pel-ffind-project-directory',
;;                               ;      `pel-generic-find-file'
(require 'pel-ffind-inpath)      ; use: `pel-ffind-inpath-include'
(require 'pel-ini)               ; use: `pel-ini-load'
(eval-when-compile
  (require 'subr-x))             ; use: `string-join'
;;; --------------------------------------------------------------------------
;;; Code:
;;

;; At the moment PEL supports a header file search mechanism based on the
;; content of the pel.ini file for the following major modes.  Others might be
;; added in the future.
(defconst pel--c-file-finder-supported-modes '(c-mode c++-mode)
  "List of major modes supported by the header file searching mechanism.")

;; We want to support searching in tool-chain specific directories and allow
;; the selection of the tool chain inside Emacs customization but also from
;; the PEL_CC_FIND_TOOLCHAIN shell environment variable, allowing multiple
;; Emacs sessions from different shells using different environment variable
;; values which identify different tool-chain specific directories.
;;
;; - If the environment variable exists its content should be used at startup.
;;   The user can override that by executing the
;;   `pel-cc-set-file-finder-ini-tool-name' command to set and use the
;;   tool-chain name of the current major-mode.
;; - If the environment variable does not exist then user-option specific to
;;   major-mode is used.

(defvar pel--c-file-finder-ini-tool-name (or
                                          (getenv "PEL_CC_FIND_TOOLCHAIN")
                                          pel-c-file-finder-ini-tool-name)
  "Identifies the name of an extra list of directories to use in C.")

(defvar pel--c++-file-finder-ini-tool-name (or
                                            (getenv "PEL_CC_FIND_TOOLCHAIN")
                                            pel-c++-file-finder-ini-tool-name)
  "Identifies the name of an extra list of directories to use in C++.")

;;-pel-autoload
(defun pel-cc-set-file-finder-ini-tool-name (tool-name)
  "Prompt/select a new key name for `pel-CC-file-finder-ini-tool-name'.

This is major-mode specific.  The CC in the variable name is selected by
the current major mode: is \\='c\\=' for C files, \\='c++\\=' for C++ files.

If PEL_CC_FIND_TOOLCHAIN environment variable exists, its value is used
automatically on startup, overriding the value of the user-option."
  (interactive (list (read-string
                      (format "\
Select pel.ini used tool chain name (%s): "
                              (pel-major-mode-symbol-value
                               "pel--%s-file-finder-ini-tool-name")))))
  (if (memq major-mode pel--c-file-finder-supported-modes)
      (progn
        (pel-set-major-mode-symbol "pel--%s-file-finder-ini-tool-name" tool-name)
        (message "\
 pel-open-at-point executed in a %s file now searches
 the project-path directories identified by the 'project-path' key
 *and* the ones identified by the %s key of the pel.ini configuration file.
 This overrides the tool chain name identified by the environment variable
 PEL_CC_FIND_TOOLCHAIN and does not change the value of the user-option."
                 (pel-file-type-for major-mode)
                 (format (pel-string-with-major-mode "%%s-%s-path")
                         tool-name)))
    (user-error "\
This is only supported for the following file types: %s"
                (string-join
                 (mapcar (function pel-file-type-for)
                         pel--c-file-finder-supported-modes)
                 ", "))))

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
      (user-error "No pel.ini file found in directory tree.
 PEL file finder method is set to `pel-ini-file'.
 Did you create a pel.ini file in the directories identified
 by the `pel-project-root-identifiers' user-option?"))

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
    ;; Make sure a path is a list of directories
    (unless (listp project-path)
      (setq project-path (list project-path)))
    ;; Extend path with optional tool-specific directories if one is specified
    ;; and the corresponding key is in the INI file.
    (when tool-name
      (let ((extra-paths (cdr (assoc
                               (format
                                (pel-string-with-major-mode
                                 "%%s-%s-path")
                                (or tool-name
                                    (getenv "PEL_CC_FIND_TOOLCHAIN")))
                               section))))
        (unless (listp extra-paths)
          (setq extra-paths (list extra-paths)))
        (setq project-path
              (delete-dups (append project-path extra-paths)))))
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
File search using method %s as defined in %s user-option,
cannot find location of %s using include path spec identified in:
 %s::
    %s"
                            (pel-major-mode-symbol-value
                             "pel-%s-file-finder-method")
                            (pel-string-with-major-mode
                             "pel-%s-file-finder-method")
                            filename
                            pel-ini-filename
                            (cadr err)))))))))

;;-pel-autoload
(defun pel-cc-find-activate-finder-method (&optional file-finder-method extra-seached-directory-trees)
  "Activate the file finder method for buffers of current major-mode.

Set the search method to FILE-FINDER-METHOD if specified,
otherwise set it to the value held by the user-option that has a
name \\='pel-MODE-file-finder-method\\=' where MODE is replaced by the
major mode name (c, c++, d, etc...).

If EXTRA-SEACHED-DIRECTORY-TREES is non-nil the finder is set to also search
in the list of directory trees identified by the list."
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
                     major-mode))))
  (when extra-seached-directory-trees
    ;; Append a function that searches into all extra directory trees.
    (setq pel-filename-at-point-finders
          (reverse
           (cons (lambda (fn)
                   (pel-generic-find-file fn extra-seached-directory-trees))
                 (reverse pel-filename-at-point-finders))))))

(defun pel--cc-find_info-msg (varname-suffix)
  "Build and return a string describing information for VARNAME-SUFFIX.

The string has 2 lines;
- Line 1: user option name and value
- Line 2: buffer local variable name and value.

The user option has the \"pel-X-Y\" format
where X is the major mode name and Y is the varname-suffix.

The buffer local variable option has the \"pel---X-Y\" format
where X is the major mode name and Y is the varname-suffix."
  (let ((uopt-varname-format      (format "pel-%%s-%s" varname-suffix))
        (bufl-varname-format (format "pel--%%s-%s" varname-suffix)))

    (format "\
- User option:   %-32s : %s
-  buffer local: %-32s : %s"
            (pel-major-mode-symbol-for uopt-varname-format)
            (pel-major-mode-symbol-value-or uopt-varname-format "NOT defined!")
            (pel-major-mode-symbol-for      bufl-varname-format)
            (pel-major-mode-symbol-value-or bufl-varname-format "NOT defined!"))))

;;-pel-autoload
(defun pel-cc-find-show-status (&optional append)
  "Show user-options used to control the file finding of current major mode."
  (interactive "P")

  (let ((user-buffer (current-buffer))
        (user-buffer-major-mode major-mode)
        (pel-insert-symbol-content-context-buffer (current-buffer)))
    (pel-print-in-buffer
     "*pel-cc-ffind-status*"
     "PEL FFIND Status"
     (lambda ()
       "Print user options & buffer local variables of file finding."

       (insert "\nUSER OPTIONS.\n  Global to all major modes:\n")
       (pel-insert-symbol-content-line 'pel-ffind-executable)
       (pel-insert-symbol-content-line 'pel-project-root-identifiers)


       (insert
        (format "\n\n\nUSER OPTIONS.\n  Default values for all %s buffers:\n"
                user-buffer-major-mode))

       (pel-insert-symbol-content-line
        (pel-major-mode-symbol-for "pel-%s-file-finder-method"))
       (pel-insert-symbol-content-line
        (pel-major-mode-symbol-for "pel-%s-file-searched-extra-dir-trees"))
       (pel-insert-symbol-content-line
        (pel-major-mode-symbol-for "pel-%s-file-finder-ini-tool-name"))

       (insert
        (format "\n\n\nACTUAL USED VALUE.
  As described in the docstrings of user options listed above that have
  a name very similar to the variable below (one extra '-' in the varname),
  you can override the default value specified by some user options
  with an environment variable.

  Value used inside all %s and all %s buffers:\n"
                user-buffer user-buffer-major-mode))

       (pel-insert-symbol-content-line
        (pel-major-mode-symbol-for "pel--%s-file-finder-ini-tool-name")))
     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-cc-find)

;;; pel-cc-find.el ends here
