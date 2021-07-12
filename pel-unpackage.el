;;; pel-unpackage.el --- Un-package elpa to speed up Emacs startup.  -*- lexical-binding: t; -*-

;; Created   : Thursday, July  8 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-07-11 22:21:59, updated by Pierre Rouleau>

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
;; The code in this file attempts to speed Emacs startup.
;;
;; The strategy is to drastically reduce the length of Emacs `load-path'.
;;
;; The length of `load-path' increase each time a new Elpa-compliant package
;; is installed by the package.el Emacs package manager.  These packages
;; consists of Emacs Lisp files stored inside a sub-directory of the Emacs
;; "elpa" directory, which is often located at "~/.emacs.d/elpa".  Each
;; package is inside a directory with a name that identifies the package and
;; its version.  Something like "ace-link-20210121.923" for the version
;; 20210121.923 of the ace-link package.
;;
;; The package directories hold the package Emacs Lisp (.el) files, as well as
;; the package specification structure stored inside a Emacs lisp file named
;; after the package with the suffix "-pkg".  For example the package
;; specification file for the ace-link package described above would be the
;; following file: "~/.emacs.d/elpa/ace-link-20210121.923/ace-link-pkg.el".
;;
;; That file contains the data package-spec `cl-defstruct'-defined data
;; structure.  Something like this:
;;
;;    ;;; -*- no-byte-compile: t -*-
;;    (define-package
;;      "ace-link"
;;      "20210121.923"
;;      "Quickly follow links"
;;      '((avy "0.4.0"))
;;      :commit "e1b1c91b280d85fce2194fea861a9ae29e8b03dd"
;;      :authors '(("Oleh Krehel" . "ohwoeowho@gmail.com"))
;;      :maintainer '("Oleh Krehel" . "ohwoeowho@gmail.com")
;;      :keywords '("convenience" "links" "avy")
;;      :url "https://github.com/abo-abo/ace-link")
;;
;; A large number of Emacs packages, like ace-link, store all their files
;; inside on directory, and have no sub-directories.   I call those: the
;; "one-level package" in opposition of the other Emacs Lisp packages that use
;; sub-directories to store other files.
;;
;; Emacs Lisp, a Lisp-2, has only one namespace for variables and one
;; namespace for functions.  Code in *all* packages, whether they're built-in
;; Emacs or external must all share these 2 namespaces.  Most package file
;; names, if not all, reflect their package name and differ from each
;; other. The functions and variable names in them must all be unique
;; otherwise they risk clashing with each other.
;;
;; Because the file names of all package have a unique name it becomes
;; possible to place them all inside the same directory and place that unique
;; directory inside Emacs `load-path', therefore eliminating relatively slow
;; Emacs startup processing that iterates through each directory in its
;; `load-path'.
;;
;; This can be done for the "one-level packages" but not the others, as their
;; code often relies on the relative position of their sub-directories.
;; Fortunately, as said previously, a large majority of Emacs external
;; packages are "one-level packages"; they use one or several Emacs Lisp files
;; all stored inside one directory.
;;
;; At startup Emacs package.el logic prepares Emacs files loading and checks
;; for the presence of the dependencies of packages. The package.el logic
;; populates the `package-alist' variable with package symbol name and its
;; corresponding package spec.  This information is later used to determine
;; the presence of a package, its dependencies, and the directory where its
;; Emacs Lisp source code and byte-compiled files are located.
;;
;; Normally "one-level packages", like all other packages have an entry inside
;; the `package-alist' and their package spec identifies their corresponding
;; directory.
;;
;; The code here re-organizes the location of the external packages, storing
;; the code of "one-level packages" inside one directory (the "elpa-copy"
;; directory) and leaving the other packages inside their original locations.
;; The code also creates code to define a new value for `package-alist' where
;; the specs for all one-level packages identify the "elpa-copy" directory.
;; This way Emacs package.el logic can still detect the presence of these
;; packages and still identify the package dependencies.
;;
;; The package.el Package Manager does not expect this file re-organization;
;; it expects the files the way they normally are organized. Therefore the
;; code here provides a mechanism to change the directory layout back and
;; forth.  Copies of directories are made.  That consumes more space in the
;; computer file-system but that's the cost of speeding up Emacs startup.
;;
;; Assuming the Elpa is stored inside "~/.emacs.d/elpa" directory,
;; pel-unpackage reorganizes it like this:
;;
;; - "~/.emacs.d/elpa-copy"    : Created by pel-unpackage.
;;                               Holds all Emacs Lisp files of all one-level
;;                               packages.
;;
;; - "~/.emacs.d/elpa-reduced" : Created by pel-unpackage.
;;                               Holds all multi-directory packages; the ones
;;                               that could not be moved out to elpa-copy.
;
;; - "~/.emacs.d/elpa-complete" : The original "~/.emacs.d/elpa" directory
;;                                has been renamed by pel-unpackage.
;;
;; - "~/.emacs.d/elpa"          : symlink pointing to elpa-complete when
;;                                package.el must be used, and pointing to
;;                                elpa-reduced to speed Emacs startup.
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

(require 'pel-package)                  ; use: pel-elpa-dirpath
(require 'pel-elpa)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defconst pel-source-dirpath (file-name-directory (locate-library "pel"))
  "Directory where PEL source files are stored.")

(defun pel-unpackage-build-setup-code (elpa-dirpath dest-dir)
  "Write and byte compile code that extends package-alist with elpa-copy code.
The ELPA-DIRPATH is the location of the Elpa directory containing all
packages installed by the package.el Package Manager.
Store the code inside the DEST-DIR directory in pel-unpackage-init.el"
  (let ((one-level-package-alist (pel-elpa-one-level-package-alist
                                  elpa-dirpath dest-dir))
        (out-file-name (expand-file-name "pel-unpackage-init.el"
                                         pel-source-dirpath)))
    (with-temp-buffer
      (insert (format "\
;;; -*- lexical-binding: t; -*-
;; DO NOT MODIFY - This file is automatically generated by pel-unpackage.el
;;                 It is part of a mechanism to speed up Emacs startup.
;;                 See pel-unpackage.el for details.

(require 'package) ; defines package-alist defvar and package-desc structure.
(defvar pel-unpackage-elpa-copy-package-alist
'%s)

\(defun pel-unpackage-init ()
  \"Complete package-alist with the elpa-copy packages.\"
  (setq package-alist (append pel-unpackage-elpa-copy-package-alist
                              package-alist)))

\(provide 'pel-unpackage-init)

;;; pel-unpackage-init.el ends here

" (pp one-level-package-alist)))
      (write-region (point-min) (point-max) out-file-name)
      (byte-compile-file out-file-name))))

(defun pel--sibling-dir (parent dir)
  "Return path of sibling directory DIR or PARENT directory."
  (file-name-as-directory
   (expand-file-name dir (file-name-directory (directory-file-name parent)))))

;; TODO: complete the following:
;;       - build the automatic loading from the auto load cookies
;;         and add the loading calls to `pel-unpackage-init'
;;       Also add command to restore the package environment.

(defun pel-unpackage ()
  "Prepare the elpa directories and code to speedup Emacs startup."
  (let* ((elpa-dirpath pel-elpa-dirpath)
         (elpa-copy-dirpath     (pel--sibling-dir elpa-dirpath
                                                  "elpa-copy"))
         (elpa-reduced-dirpath  (pel--sibling-dir elpa-dirpath
                                                  "elpa-reduced"))
         (elpa-complete-dirpath (pel--sibling-dir elpa-dirpath
                                                  "elpa-complete"))
         (elpa-is-link    (file-symlink-p (directory-file-name
                                           elpa-dirpath))))
    ;; Ensure that elpa is a directory or a symlink to elpa-complete
    ;; otherwise abort.
    (unless (or (pel-same-fname-p elpa-is-link elpa-complete-dirpath)
                (and (not elpa-is-link)
                     (file-directory-p pel-elpa-dirpath)))
      (error (format "The Elpa directory (%s) differs from normal settings%s"
                     pel-elpa-dirpath
                     (if elpa-is-link
                         (format ": it points to %s" elpa-is-link)
                       ""))))
    ;; Create the pel-unpackage-init.el source code and byte-compile it.
    (pel-unpackage-build-setup-code pel-elpa-dirpath elpa-copy-dirpath)
    ;; Delete old elpa-copy and elpa-reduced if they exist, to start clean
    (dolist (dp (list elpa-copy-dirpath elpa-reduced-dirpath))
      (when (file-exists-p dp)
        (delete-directory dp :recursive)))
    ;; Create elpa-copy directory to hold all one-level package .el files
    (make-directory elpa-copy-dirpath)
    (pel-elpa-create-copies pel-elpa-dirpath elpa-copy-dirpath)
    ;; Byte compile all the elpa-copy .el files
    (byte-recompile-directory elpa-copy-dirpath 0 :force)

    ;; Duplicate elpa inside elpa-reduced then remove the one-level packages
    ;; from it.
    (copy-directory (directory-file-name pel-elpa-dirpath)
                    (directory-file-name elpa-reduced-dirpath))
    (pel-elpa-remove-pure-subdirs elpa-reduced-dirpath)
    ;; Re-organize the elpa directory:
    ;; If elpa is a directory and elpa-complete does not exist: then
    ;; rename elpa to elpa-complete.
    (when (and (file-directory-p elpa-dirpath)
               (not (file-symlink-p (directory-file-name
                                     elpa-dirpath))))
      (when (file-exists-p elpa-complete-dirpath)
        (delete-directory elpa-complete-dirpath :recursive))
      (rename-file (directory-file-name elpa-dirpath)
                   (directory-file-name elpa-complete-dirpath)))
    ;; If there is a elpa symlink remove it and create a new one that points
    ;; to elpa-reduced
    (when (file-exists-p (directory-file-name elpa-dirpath))
      (delete-file (directory-file-name elpa-dirpath)))
    (make-symbolic-link (directory-file-name elpa-reduced-dirpath)
                        (directory-file-name elpa-dirpath))

    ;; Return the directory paths
    (list elpa-dirpath
          elpa-copy-dirpath
          elpa-reduced-dirpath
          elpa-complete-dirpath)))


;;; --------------------------------------------------------------------------
(provide 'pel-unpackage)

;;; pel-unpackage.el ends here
