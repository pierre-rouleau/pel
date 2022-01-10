;;; pel-process.el --- PEL process control facilities.  -*- lexical-binding: t; -*-

;; Created   : Friday, August 20 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-01-10 17:11:21, updated by Pierre Rouleau>

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
;; This file provides a set of functions to extend Emacs ability to control
;; the values of environment variables set for an Emacs child process.
;;
;; The user can specify a set of environment variables in the
;; `pel-gui-process-environment' user-option.
;; The function `pel-process-update-environment-from' updates Emacs
;; environment from the values specified by variables that use the same
;; structure; a list of:
;; - environment variable name (a string)
;; - environment variable value (a string)
;; - an action symbol;  one of: use-as-is, append or prepend.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; - `pel-process-update-environment-from'
;;   - `pel-process--envvar-set'
;;   - `pel-process--envvar-append'
;;     - `pel-process--envvar-set'
;;   - `pel-process--envvar-prepend'
;;     - `pel-process--envvar-set'

(defun pel-process--envvar-set (var-name value)
  "Set the environment VAR-NAME to VALUE.

Handle PATH specially: also sets variable `exec-path' and
variable `eshell-path-env' with the specified VALUE.

The VALUE must always be a single string.  For PATH-like environment variables
use the OS specific path separator, which is \":\" on Unix-like OS and \";\"
on Windows."
  (setenv var-name value)
  (when (string= var-name "PATH")
    ;; Update Emacs execution path as well
    (setq exec-path (parse-colon-path value))
    ;; `eshell-path-env' is buffer local: set it globally with setq-default
    (setq-default eshell-path-env value)))

(defun pel-process--envvar-append (var-name value)
  "Append VALUE to VAR-NAME if it exists otherwise just set it.

VAR_NAME is a string; the name of an environment variable that
holds a path-separator separated list of strings, like PATH."
  (let ((new-value (concat (or (getenv var-name) "")
                           path-separator value)))
    (pel-process--envvar-set var-name new-value)))

(defun pel-process--envvar-prepend (var-name value)
  "Prepend VALUE to VAR-NAME if it exists otherwise just set it."
  (let ((new-value (concat value path-separator
                           (or (getenv var-name)) "")))
    (pel-process--envvar-set var-name new-value)))

(defun pel-process-update-environment-from (var-list)
  "Update Emacs environment variables from the VAR-LIST structure.

The VAR-LIST is a list of 3-element lists.  Each list holds:
- environment variable name (a string)
- environment variable value (a string)
- an action symbol;  one of: use-as-is, append or prepend."
  (dolist (var-spec var-list)
    (let ((var-name (nth 0 var-spec))
          (value    (nth 1 var-spec))
          (action   (nth 2 var-spec)))
      (cond
       ((eq action 'use-as-is) (pel-process--envvar-set var-name value))
       ((eq action 'append)    (pel-process--envvar-append var-name value))
       ((eq action 'prepend)   (pel-process--envvar-prepend var-name value))
       (t (display-warning 'pel-process-update-environment-from
                           (format "\
Invalid action in PEL environment control list for variable %s"
                                   var-name)
                           :error))))))

;;; --------------------------------------------------------------------------
(provide 'pel-process)

;;; pel-process.el ends here
