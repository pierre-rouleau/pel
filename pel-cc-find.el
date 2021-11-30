;;; pel-cc-find.el --- CC modes find file.  -*- lexical-binding: t; -*-

;; Created   : Monday, November 29 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-11-29 18:00:08, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021  Pierre Rouleau
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
(require 'pel-file)              ; use: `pel-filename-at-point-finders'
(require 'pel-ffind-inpath)      ; use: `pel-ffind-inpath-include'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-cc-find-activate-finder-method (&optional file-finder-method)
  "Activate the file finder method for buffers of current major-mode."
  (unless file-finder-method
    (setq file-finder-method
         (pel-major-mode-symbol-value "pel-%s-file-finder-method")))
  ;; Set pel-filename-at-point-finders according to what specified in the
  ;; file-finder-method variable for the current major-mode.
  (cond
   ;; Use a generic file tree search
   ((eq file-finder-method 'generic)
    (setq pel-filename-at-point-finders '(pel-generic-find-file)))
   ;; Search in the directories identified in specified environment variable
   ((stringp file-finder-method)
    (setq pel-filename-at-point-finders
          (list
           (lambda (fn)
             (pel-ffind-inpath-include fn file-finder-method)))))
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
   ;; no other method currently supported
   (t (error (format "invalid file-finder-method: %S for %s"
                     file-finder-method
                     major-mode)))))

;;; --------------------------------------------------------------------------
(provide 'pel-cc-find)

;;; pel-cc-find.el ends here
