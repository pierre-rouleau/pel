;;; pel-filedir.el --- File Dirpath Management.  -*- lexical-binding: t; -*-

;; Created   : Thursday, February 25 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-07-02 10:24:17, updated by Pierre Rouleau>

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
;; - `pel-duplicate-dir'
;; - `pel-subdir-count'
;;   - `pel--dirspec-for-dir-p'
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


;; --

(defun pel-duplicate-dir (source destination &optional with-symlinks)
  "Copy all files of SOURCE directory into DESTINATION directory.
If WITH-SYMLINKS is non-nil create symlinks in DESTINATION to the files
in SOURCE."
  (let (source-fn destination-fn)
    (dolist (file-name (directory-files source))
      (setq source-fn (expand-file-name file-name source))
      (setq destination-fn (expand-file-name file-name destination))
      (if with-symlinks
          (make-symbolic-link source-fn destination-fn)
        (copy-file source-fn destination-fn)))))

;; --

(defun pel--dirspec-for-dir-p (dirspec)
  "Return dirname when DIRSPEC is for a Elpa package directory, nil otherwise.
DIRSPEC is the data structure returned by `directory-files-and-attributes'.
Exclude the directory entries that start with a period."
  (when (and (cadr dirspec)                          ; is a directory
             (not (eq (string-to-char (car dirspec)) ; but does not start with period
                      ?.)))
    (car dirspec)))

(defun pel-subdir-count (dir-path-name)
  "Return number of sub-directories of DIR-PATH-NAME directory."
  (length (mapcar #'car (seq-filter
                         (function pel--dirspec-for-dir-p)
                         (directory-files-and-attributes dir-path-name)))))

;;; --------------------------------------------------------------------------
(provide 'pel-filedir)

;;; pel-filedir.el ends here
