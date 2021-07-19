;;; pel-setup.el --- Control PEL Emacs setup from normal to fast startup.  -*- lexical-binding: t; -*-

;; Created   : Thursday, July  8 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-07-19 08:28:24, updated by Pierre Rouleau>

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
;; `pel-setup-fast' reorganizes it like this:
;;
;; - "~/.emacs.d/elpa-complete" : The original "~/.emacs.d/elpa" directory
;;                                has been renamed (or duplicated) by
;;                                `pel-setup-fast'.
;;
;; - "~/.emacs.d/elpa-reduced" : Created by `pel-setup-fast'.  Holds all
;;                               multi-directory packages; the ones that are
;;                               not "one-level packages".  It also contains
;;                               the "pel-bundle-yyyymmdd-001" directory, an
;;                               artificial package created by `pel-setup-fast'
;;                               which holds the code of all one-level
;;                               packages removed from the original elpa
;;                               directory.  The tail end of the directory
;;                               name is the date of the creation of the
;;                               directory.
;;
;; - "~/.emacs.d/elpa-reduced/pel-bundle-yyymmdd-001": Holds the content of
;;                               all one-level packages that it replaces. It
;;                               is created by `pel-setup-fast' with the name
;;                               tail set to its creation date.  This
;;                               simulates a fictitious package, pel-bundle,
;;                               and aside of all files from the original
;;                               one-level it holds the 2 important package
;;                               required files that are also created by the
;;                               function `pel-setup-fast':
;;                               - pel-bundle-autoloads.el
;;                               - pel-bundle-pkg.el
;;
;; - "~/.emacs.d/elpa"          : symlink pointing to elpa-complete when
;;                                package.el must be used, and pointing to
;;                                elpa-reduced to speed Emacs startup.
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;

(require 'pel--base)                    ; use: pel-unix-socket-p
(require 'pel-package)                  ; use: pel-elpa-dirpath
(require 'pel-elpa)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defconst pel-source-dirpath (file-name-directory (locate-library "pel"))
  "Directory where PEL source files are stored.")

(defconst pel-fast-startup-setup-fname (expand-file-name
                                        "pel-setup-package-builtin-versions.el"
                                        user-emacs-directory)
  "Name of code file that must be executed by fast-startup init/ early-init.
When fast startup is not activated, this file must be deleted.")

(defun pel--sibling-dir (parent dir)
  "Return path of sibling directory DIR of PARENT directory."
  (file-name-as-directory
   (expand-file-name dir (file-name-directory (directory-file-name parent)))))


(defun pel--switch-elpa-to (elpa-dirname destination-dirname)
  "Switch elpa symlink to the elpa-complete sub-directory."
  (when (file-exists-p elpa-dirname)
    (delete-file elpa-dirname))
  (make-symbolic-link destination-dirname elpa-dirname))

(defun pel-switch-to-elpa-complete ()
  "Switch elpa symlink to the elpa-complete sub-directory."
  (interactive)
  (let* ((elpa-dirname           (directory-file-name pel-elpa-dirpath))
         (elpa-complete-dirname  (directory-file-name
                                  (pel--sibling-dir elpa-dirname
                                                    "elpa-complete"))))
    (pel--switch-elpa-to elpa-dirname elpa-complete-dirname)))

(defun pel-switch-to-elpa-reduced ()
  "Switch elpa symlink to the elpa-reduced sub-directory."
  (let* ((elpa-dirname           (directory-file-name pel-elpa-dirpath))
         (elpa-reduced-dirname  (directory-file-name
                                  (pel--sibling-dir elpa-dirname
                                                    "elpa-reduced"))))
    (pel--switch-elpa-to elpa-dirname elpa-reduced-dirname)))


(defun pel-generate-autoload-file-for (dir)
  "Prepare (compile + autoload) all files in directory DIR.
Return the complete name of the generated autoload file."
  (require 'autoload)
  (if (boundp 'generated-autoload-file)
      (let ((original-generated-autoload-file  generated-autoload-file))
        (setq generated-autoload-file (expand-file-name
                                       "pel-bundle-autoloads.el"
                                       dir))
        (condition-case-unless-debug err
            (progn
              (update-directory-autoloads dir)
              (kill-buffer "pel-bundle-autoloads.el"))
          (error
           (display-warning
            'pel-generate-autoload-file-for
            (format "Failed generating the %s/pel-bundle-autoloads.el: %s"
                    dir err))))
        (setq generated-autoload-file original-generated-autoload-file))
    (error "The autoload.el variable generated-autoload-file isn't bounded!")))


(defun pel-create-bundle-pkg-file (dirname &optional time-stamp)
  "Create the pel-bundle-pkg.el file inside DIRNAME directory.
Use the specified TIME-STAMP string as the version otherwise the
following format-time-string format string is used:
\"%Y%m%d.%H%M\".
Return the complete file path name of the file written."
  (let ((file-path-name (expand-file-name "pel-bundle-pkg.el" dirname)))
    (with-temp-file file-path-name
      (goto-char (point-min))
      (insert (format "\
;;; -*- no-byte-compile: t -*-
\(define-package
  \"pel-bundle\"
  \"%s\"
  \"Bundle one-level packages inside a single directory\"
  nil
  :authors '((\"Pierre Rouleau\" . \"prouleau001@gmail.com\"))
  :maintainer '(\"Pierre Rouleau\" . \"prouleau001@gmail.com\")
  :keywords '(\"convenience\")
  :url \"https://github.com/pierre-rouleau/pel\")
" (or time-stamp (format-time-string "%Y%m%d.%H%M")))))
    file-path-name))


(defvar pel-running-with-bundled-packages) ; prevent byte-compiler warnings
(defun pel-bundled-mode (activate)
  "When ACTIVATE is non-nil activate PEL bundled mode, de-activate it otherwise.
Return a (activate . byte-compile result) cons cell."
  (setq pel-running-with-bundled-packages activate)
  (cons  pel-running-with-bundled-packages
         (byte-compile-file (concat (file-name-sans-extension
                                     (locate-library "pel_keys"))
                                    ".el"))))

(defun pel-setup-add-to-builtin-packages (pkg-versions fname)
  "Write code in FNAME that adds the PKG-VERSIONS to the Emacs builtins.
The code adds each entry to the `package--builtin-versions'."
  (with-temp-file fname
    (erase-buffer)
    (goto-char (point-min))
    (insert (format "\
;;; Built automatically by PEL for fast Emacs startup.  -*- lexical-binding: t; -*-
\(require 'package)

(defvar pel-fast-startup-builtin-packages
  '%S
  \"List of packages dependencies to add to package--builtin-versions.\")

\(defun pel-fast-startup-set-builtins ()
  \"Prevent package from downloading a set of package dependencies.\"
  (dolist (dep-ver pel-fast-startup-builtin-packages)
    (add-to-list 'package--builtin-versions dep-ver))
  pel-fast-startup-builtin-packages)

(defun pel--pct (_packages)
  \"Filter packages to prevent downloads.\"
  nil)

(advice-add  'package-compute-transaction  :filter-return (function pel--pct))

" pkg-versions))))


;; --

(defun pel-copy-only-file (original-copy-file file &rest args)
  "Copy files but not Unix sockets."
  (unless (pel-unix-socket-p file)
    (apply original-copy-file
           file
           args)))

;;-pel-autoload
(defun pel-setup-fast ()
  "Prepare the elpa directories and code to speedup Emacs startup."
  (interactive)
  (let ((cd-original (cd ".")))
    (condition-case-unless-debug err
        (let* ((new-pel-bundle-dirpath nil)
               (elpa-dirpath pel-elpa-dirpath)
               (pel-bundle-dirpath (pel--sibling-dir elpa-dirpath
                                                     "pel-bundle"))
               (elpa-reduced-dirpath  (pel--sibling-dir elpa-dirpath
                                                        "elpa-reduced"))
               (elpa-complete-dirpath (pel--sibling-dir elpa-dirpath
                                                        "elpa-complete"))
               (elpa-is-link    (file-symlink-p (directory-file-name
                                                 elpa-dirpath)))
               (time-stamp (format-time-string "%Y%m%d.%H%M")))
          ;; Ensure that elpa is a directory or a symlink to elpa-complete
          ;; otherwise abort.
          (when (and elpa-is-link
                     (not (pel-symlink-points-to-p (directory-file-name
                                                    elpa-dirpath)
                                                   elpa-complete-dirpath)))
            (error "The elpa symlink (%s) should point to elpa-complete.\
 It point to %s instead! Aborting, fix the directory setting!"
                   pel-elpa-dirpath
                   elpa-is-link))

          ;; Ensure that pel-bundle directory does not exists.  That's a temporary
          ;; directory where all one-level package files are stored and then used to
          ;; create the -autoload.el and the -pkg.el file before it is moved into
          ;; the elpa-reduced directory and renamed with a time stamp.  Issue a
          ;; message when the directory exists and delete it.
          (when (file-exists-p pel-bundle-dirpath)
            (delete-directory pel-bundle-dirpath :recursive)
            (message (format "The %s directory already exists! It was deleted!"
                             pel-bundle-dirpath)))
          ;;
          ;; Delete old elpa-reduced if it exists: it contains the old pel-bundle
          ;; and the multi-level packages that could not be bundled in the previous
          ;; execution of `pel-setup-fast'.
          (when (file-exists-p elpa-reduced-dirpath)
            (delete-directory elpa-reduced-dirpath :recursive))
          ;;
          ;; Create pel-bundle temporary directory to hold all one-level package .el
          ;; files.  At first create it the directory as a sibling of the elpa
          ;; directory because elpa-reduced is not created yet.
          (make-directory pel-bundle-dirpath)
          (pel-elpa-create-copies pel-elpa-dirpath pel-bundle-dirpath)
          ;; Create the pel-bundle-pkg.el file inside it.
          (pel-create-bundle-pkg-file pel-bundle-dirpath time-stamp)
          ;;
          ;; Create the pel-bundle-autoloads.el file inside it.
          (cd pel-bundle-dirpath)
          ;; Build a pel-bundle-autoloads.el inside the pel-bundle directory.
          (pel-generate-autoload-file-for pel-bundle-dirpath)
          (cd elpa-dirpath)
          ;;
          ;; Duplicate elpa inside elpa-reduced then remove the one-level packages
          ;; from it: they have been placed inside the pel-bundle directory before.
          ;; - Normally, copy-directory would fail when attempting to copy a
          ;;   Unix socket file, like those in elpa/gnupg.  To prevent the
          ;;   error, replace copy-file by pel-copy-only-file which does not attempt
          ;;   to copy the Unix socket files.
          (advice-add 'copy-file :around #'pel-copy-only-file)
          (copy-directory (directory-file-name pel-elpa-dirpath)
                          (directory-file-name elpa-reduced-dirpath))
          (advice-remove 'copy-file #'pel-copy-only-file)
          ;; - Remove the one-level package sub-directories from elpa-reduced,
          ;;   only leaving the multi-directory packages in elpa-reduced.
          (pel-elpa-remove-pure-subdirs elpa-reduced-dirpath)
          ;;
          ;; Disable the dependencies of all (multi-directory) packages left in the
          ;; elpa-reduced directory.  This returns an alist of (package version)
          ;; that should be added to the variable `package--builtin-versions' during
          ;; init.el before the call to `package-activate-all' or
          ;; `package-initialize'.  In Emacs ≥ 27 it must be set in early-init.el.
          (pel-setup-add-to-builtin-packages
           (pel-elpa-disable-pkg-deps-in elpa-reduced-dirpath)
           pel-fast-startup-setup-fname)
          ;;
          ;; Move the pel-bundle directory inside the elpa-reduced directory:
          ;; effectively creating a pel-bundle package "pel-bundle" that contains
          ;; all the files of the one-level packages that were extracted from the
          ;; original elpa directory (the elpa-complete directory).
          ;; Give the pel-bundle directory a version number corresponding to today's
          ;; date.
          (setq new-pel-bundle-dirpath (expand-file-name
                                        (format "pel-bundle-%s" time-stamp)
                                        elpa-reduced-dirpath))
          (rename-file (directory-file-name pel-bundle-dirpath)
                       new-pel-bundle-dirpath)
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
          (pel-switch-to-elpa-reduced)
          ;; Re-compile pel_keys.el with `pel-running-with-bundled-packages'
          ;; bound to t to prevent PEL from downloading and installing
          ;; external packages while PEL runs in PEL bundled mode.
          (pel-bundled-mode t))
      (error
       (display-warning 'pel-setup-fast
                        (format "Failed fast startup setup: %s" err))))
    (cd cd-original))
  (message "Restart Emacs to complete the fast startup PEL/Emacs operation mode!"))

;; --
(defun pel-setup-normal ()
  "Restore normal PEL/Emacs operation mode."
  (interactive)
  (let* ((elpa-dirpath pel-elpa-dirpath)
         (elpa-reduced-dirpath  (pel--sibling-dir elpa-dirpath
                                                  "elpa-reduced")))
    ;; Restore PEL's ability to download and install external packages
    (pel-bundled-mode nil)
    ;;  Restore the normal, complete Elpa directory.
    (pel-switch-to-elpa-complete)
    ;; Remove files used in fast-start setup
    (when (file-exists-p elpa-reduced-dirpath)
      (delete-directory elpa-reduced-dirpath :recursive))
    (when (file-exists-p pel-fast-startup-setup-fname)
      (delete-file pel-fast-startup-setup-fname))
    ;; inform user.
    (message "Restart Emacs to complete the normal PEL/Emacs operation mode!")))

;;; --------------------------------------------------------------------------
(provide 'pel-setup)

;;; pel-setup.el ends here
