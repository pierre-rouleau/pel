;;; pel-filedir.el --- File Dirpath Management.  -*- lexical-binding: t; -*-

;; Created   : Thursday, February 25 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-02-25 18:15:52, updated by Pierre Rouleau>

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
;; A collection of functions that manipulate file system file names and
;; directories.
;;
;;
;; The calling hierarchy of the provided functions follow:
;;
;; - `pel-file-in-dir'
;; - `pel-file-in-dir-upwards'
;;    - `pel-dir-is-root'
;;    - `pel-parent-directory'
;;
;;
;; See Emacs conventions:
;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Names.html
;;
;; - Directory:
;;   - directory file name: does not end with a slash. eg.: "~/dev/elisp/pel"
;;   - directory name: ends with a slash.              eg.: "~/dev/elisp/pel/"
;;
;; Argument name conventions:
;;
;; - filename  : Name of a file, no directory path, no slash.
;; - filepath  : Absolute path of a file.  Has a dirpath, a slash, a filename.
;; - dirpath   : Directory relative or absolute path.
;;               Optionally ends with slash.
;;               May use special symbols like "~", "." and ".."
;; - dirname   : Emacs conventional directory name that ends wit ha slash.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)    ; use pel-system-is-windows-p

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-file-in-dir (filename dirpath)
  "Return t if FILENAME is present in directory DIRPATH, nil otherwise.
DIRPATH can be a dirpath name (ending with separator)
or a dirpath file name (not ending with separator)."
  (file-exists-p (expand-file-name filename dirpath)))

(defun pel-dir-is-root (dirpath)
  "Return t is DIRPATH is the root of the file-system."
  (if pel-system-is-windows-p
      (not (null (string-match "\\([a-zA-Z]:\\)?[/\\]\\'"  dirpath)))
    (string= dirpath "/")))

(defun pel-parent-directory (dirpath)
  "Return the absolute path of DIRPATH's parent directory.

Return a directory name, which by Emacs convention, always ends
with a slash.  Return nil if DIRPATH is already the root."
  (unless (pel-dir-is-root dirpath)
    (file-name-directory
     (directory-file-name
      (file-name-as-directory
       (file-truename dirpath))))))

(defun pel-file-in-dir-upwards (filename dirpath &optional root)
  "Return path of FILENAME found in directory DIRPATH or its parents.

Return nil if FILENAME is not present in any of the directories.
By default search in current directory and all its parents up to
the file system's root unless another ROOT is specified.

If ROOT is specified it must be a parent of the directory identified by
dirpath, otherwise the function generates an error.

If FILE is not found in DIRPATH, the parent of DIRPATH is searched."
  (let* ((dirpath  (file-truename dirpath))
         (root     (and root (file-name-as-directory (file-truename root))))
         (filepath (expand-file-name filename dirpath))
         file-found)
    (while (and (not (setq file-found (file-exists-p filepath)))
                (not (pel-dir-is-root dirpath))
                (not (and root (string= dirpath root))))
      (setq dirpath (pel-parent-directory dirpath))
      (setq filepath (expand-file-name filename dirpath)))
    (when file-found
      filepath)))

;;; --------------------------------------------------------------------------
(provide 'pel-filedir)

;;; pel-filedir.el ends here
