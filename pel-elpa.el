;;; pel-elpa.el --- Elpa pakage management Utilities.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, June 30 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-07-02 10:25:26, updated by Pierre Rouleau>

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
;;  A set of utilities to deal with the ~/.emacs./elpa directory.
;;  These are part of an experiment to see if it is possible to speed-up Emacs
;;  start-up time by modifying the content of the elpa directory and using
;;  symlinks.

;; - `pel-elpa-remove-pure-subdirs'
;; - `pel-elpa-create-copiess'
;;   - `pel-elpa-one-level-packages'
;;     - `pel-elpa-package-directories'
;;       - `pel-elpa-package-dirspec-p'
;; - `pel-el-files-in'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel-filedir)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-el-files-in (dir-path)
  "Return the Emacs Lisp file names inside DIR-PATH."
  (seq-filter (lambda (fn)
                (string= (file-name-extension fn) "el"))

              (directory-files dir-path)))

(defun pel-elpa-package-dirspec-p (dirspec)
  "Return dirname when DIRSPEC is for a Elpa package directory, nil otherwise."
  (when (and (cadr dirspec)             ; is a directory ...
                                        ; ... that has a name that does not start with a period
             (not (eq (string-to-char (car dirspec))
                      ?.)))
    (car dirspec)))

(defun pel-elpa-package-directories (elpa-dirpath)
  "Return a list of package directories inside the ELPA-DIRPATH directory."
  (mapcar #'car
          (seq-filter (function pel-elpa-package-dirspec-p)
                      (directory-files-and-attributes elpa-dirpath))))

(defun pel-elpa-one-level-packages (elpa-dirpath)
  "Return a list of directories in ELPA-DIRPATH that have no sub-directories."
  (let ((elpa-dirnames (pel-elpa-package-directories elpa-dirpath)))
    (seq-filter
     (lambda (dn)
       (when (eq (pel-subdir-count (format "%s/%s" elpa-dirpath dn))
                 0)
         dn))
     elpa-dirnames)))

(defun pel-elpa-create-copies (elpa-dir-path
                               dest-dir-path
                               &optional with-symlinks)
  "Copy all .el and .elc Elpa package files into a single directory.

The single directory is DEST-DIR-PATH.  If WITH-SYMLINKS is
non-nil, don't copy, create symlinks in DIR-PATH-NAME instead.
The Elpa directory where packages are taken from is
ELPA-DIR-PATH.
The function search the packages present in the
ELPA-DIR-PATH directory.  It only considers Elpa packages
that have no sub-directories and therefore have all their files
inside one directory.

When WITH-SYMLINK is non-nil, return a list of (file-path-1 . file-path-2)
cons cells that identify the duplicated file names, or nil if there are none."
  (let ((duplicates nil)
        (elpa-pure-dirnames (pel-elpa-one-level-packages elpa-dir-path))
        source-dir-path-name
        source-fn
        destination-fn)
    (dolist (dirname elpa-pure-dirnames)
      (setq source-dir-path-name (expand-file-name
                                   dirname
                                   (file-truename elpa-dir-path)))
      (dolist (file-name (directory-files source-dir-path-name))
        (when (member (file-name-extension file-name) '("el" "elc"))
          (setq source-fn (expand-file-name file-name source-dir-path-name))
          (setq destination-fn (expand-file-name file-name dest-dir-path))
          (if (file-exists-p destination-fn)
              ;; note: the destination will hold the name of the other
              ;; file when with-symlinks is non-nil
              (push (cons source-fn (file-truename destination-fn))
                    duplicates)
            (if with-symlinks
                (make-symbolic-link source-fn destination-fn)
              (copy-file source-fn destination-fn))))))
    (when with-symlinks
      duplicates)))

(defun pel-elpa-remove-pure-subdirs (elpa-dir-path)
  "Remove all sub-directories of ELPA-DIR-PATH that only hold files."
  (let ((elpa-pure-dir-names (pel-elpa-one-level-packages elpa-dir-path)))
    (dolist (dn elpa-pure-dir-names)
      (delete-directory (expand-file-name dn elpa-dir-path) :recurse))))

;;; --------------------------------------------------------------------------
(provide 'pel-elpa)

;;; pel-elpa.el ends here
