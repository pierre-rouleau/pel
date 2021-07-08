;;; pel-elpa.el --- Elpa pakage management Utilities.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, June 30 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-07-08 14:34:44, updated by Pierre Rouleau>

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
;; - `pel-elpa-create-copies'
;;   - `pel-elpa-one-level-packages'
;;     - `pel-elpa-package-directories'
;;       - `pel-elpa-package-dirspec-p'
;; - `pel-el-files-in'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel-filedir)
(require 'package)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-el-files-in (dir-path)
  "Return the Emacs Lisp file names inside DIR-PATH."
  (seq-filter (lambda (fn)
                (string= (file-name-extension fn) "el"))

              (directory-files dir-path)))

;; ---------------------------------------------------------------------------
;; Update package-alist with package specs of one-level packages
;; -------------------------------------------------------------
;;
;; The one-level packages are all Emacs packages that store all their files
;; inside a single directory.
;; For example, the directory "~/.emacs.d/elpa/a-20201203.1927" holds all its
;; files inside the single directory it has no sub-directories.  That's what I
;; call a "one-level package".
;;
;; To speed up Emacs startup, we can copy all files of one-level packages
;; inside a single directory.  I name this directory: "~/.emacs.d/elpa-copy".
;;
;; Then I create the "~/.emacs.d/elpa-reduced" directory which is the original
;; elpa directory with all one-level package directories removed.
;;
;; Emacs package loading mechanism depends on logic in the package.el
;; file. That uses the `package-alist' variable which holds a associative list
;; between known package names and their package specification.  The package
;; specification is a `cl-defstruct' defined structure of package-desc *type*.
;; That structure has a `dir' slot identifying the directory where the package
;; is defined and a `reqs' slot identifying the other packages for that one.
;;
;; By removing the one-level packages from the elpa directory we prevent Emacs
;; from populating the `package-alist' with the data from those one-level
;; packages since they were removed from elpa and their Emacs Lisp files were
;; placed all inside a single directory (the "~/.emacs.d/elpa-copy"
;; directory).
;;
;; We therefore need a mechanism to complete the content of the
;; `package-alist' variable with the specification of the one-level packages
;; but with the `dir' slot updated to point to "~/.emacs.d/elpa-copy".
;;
;; This is what `pel-elpa-prepend-with-one-level-packages' does.

;; TODO: I need to complete the experimentation and finalize this code.
;; Ideally all work is done when "*un-packaging elpa* in order to minimize
;; processing when Emacs starts.  I have not reached that point yet.

;; Function call hierarchy:

;; - `pel-elpa-prepend-with-one-level-packages'
;;   - `pel-elpa-one-level-package-alist'
;;     - `pel-elpa-load-pkg-descriptor'
;;     - `pel-elpa-one-level-package-files'
;;       - `pel-elpa-pkg-filename'
;;       - `pel-elpa-one-level-packages'
;;         - `pel-elpa-package-directories'
;;           - `pel-elpa-package-dirspec-p'

;; Credits: Thanks to "phils" for mentioning the `package-load-descriptor'
;;  function from package.el as a reply to my question here:
;;  https://emacs.stackexchange.com/questions/66616/how-to-create-a-structure-instance-out-of-a-pkg-el-file/66620
;; It saved me some time in searching for it inside package.el.

(defun pel-elpa-package-dirspec-p (dirspec)
  "Return dirname when DIRSPEC is for a Elpa package directory, nil otherwise."
  (when (and (cadr dirspec)             ; is a directory ...
                                        ; ... that has a name that does not
                                        ; start with a period
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

(defun pel-elpa-pkg-filename (dir &optional with-path)
  "Return the name of the -pkg.el file present in DIR, nil if none found.
The returned file name has no path, unless WITH-PATH is non-nil."
  (let ((fname
         (car (seq-filter (lambda (fn)
                            (and (pel-string-ends-with-p fn "-pkg.el")
                                 (not (pel-string-starts-with-p fn "#"))
                                 (not (pel-string-starts-with-p fn "."))))
                          (directory-files dir)))))
    (if with-path
        (when fname
          (expand-file-name fname dir))
      fname)))

(defun pel-elpa-one-level-package-files (elpa-dirpath)
  "Return all file path-names of one level package -pkg files in ELPA-DIRPATH."
  (mapcar (lambda (dn)
            (pel-elpa-pkg-filename (expand-file-name dn elpa-dirpath)
                                   :with-path))
          (pel-elpa-one-level-packages "~/.emacs.d/elpa")))

(defun pel-elpa-load-pkg-descriptor (pkg-file &optional pkg-dir
                                              result-alist-symbol)
  "Load and return package descriptor from file PKG-FILE.
If a directory is identified in PKG-DIR set the dir slot of the returned
package descriptor to that directory"
  ;; prevent package-process-define-package from updating package-alist
  (let (package-alist)
    (with-temp-buffer
      (insert-file-contents pkg-file)
      (goto-char (point-min))
      (let ((pkg-desc (or (package-process-define-package
                           (read (current-buffer)))
                          (error "Can't find define-package in %s" pkg-file))))
        (when pkg-dir
          (setf (package-desc-dir pkg-desc) pkg-dir))
        (when result-alist-symbol
          (set result-alist-symbol package-alist))
        pkg-desc))))

(defun pel-elpa-one-level-package-alist (elpa-dirpath dest-dir)
  "Return alist of all one-level packages in ELPA-DIRPATH held in DEST_DIR.
The ELPA-DIRPATH is the standard Elpa directory that holds standard Elpa
packages stored in sub-directories.
Create and return a association list that associate the package name
to a `package-desc' structure with a modified `dir' slot pointing to the
directory specified by DEST-DIR."
  (let (alist desc)
    (dolist (pfn (pel-elpa-one-level-package-files elpa-dirpath))
      (setq desc (pel-elpa-load-pkg-descriptor pfn dest-dir))
      (push (list (package-desc-name desc) desc) alist))
    alist))

(defun pel-elpa-prepend-with-one-level-packages (alist-symbol
                                                 elpa-dirpath
                                                 dest-dir)
  "Update alist in ALIST-SYMBOL with package specs of one-level packages.
Extract the package specs from the standard Elpa directory identified by
the ELPA-DIRPATH argument.
Update the dir slot of each package spec data structure to hold the directory
path identified by the DEST-DIR argument.
Return the new value of the ALIST-SYMBOL."
  (set alist-symbol (append (pel-elpa-one-level-package-alist
                             elpa-dirpath
                             dest-dir)
                            (symbol-value alist-symbol)))
  (symbol-value alist-symbol))

;; ---------------------------------------------------------------------------


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
