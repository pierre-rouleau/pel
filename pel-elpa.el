;;; pel-elpa.el --- Elpa pakage management Utilities.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, June 30 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-07-21 16:38:00, updated by Pierre Rouleau>

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
;;  The functions here are used by code that manage Elpa-compliant packages as
;;  well as re-organize the Emacs directory to speed-up Emacs start-up time.
;;
;;
;; *- `pel-el-files-in'

;; *- `pel-elpa-one-level-package-alist'
;; *  - `pel-elpa-load-pkg-descriptor'
;; *  - `pel-elpa-one-level-package-files'
;; *    - `pel-elpa-pkg-filename'
;; &    - `pel-elpa-one-level-packages'
;; &      - `pel-elpa-package-directories'
;; &        - `pel-elpa-package-dirspec-p'

;; *- `pel-elpa-remove-pure-subdirs'
;; *- `pel-elpa-create-copies'
;; *  - `pel-elpa-one-level-packages'
;; *    - `pel-elpa-package-directories'
;; *      - `pel-elpa-package-dirspec-p'


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
;; For example, the package made out of https://github.com/plexus/a.el is a
;; directory like "~/.emacs.d/elpa/a-20201203.1927" which holds all its
;; files inside the single directory; it has no sub-directories.
;;
;; To speed up Emacs startup, we can copy all files of one-level packages
;; inside a single directory.  And that directory is set-up in the same ways
;; as if it was a normal package: the directory is a sub-directory of the elpa
;; directory used in "PEL unpackage mode" and is called the "pel-bundle".
;;
;; That "pel-bundle" directory is organized in such a way as creating a
;; pel-bundle package which contains all source files of the original
;; one-level packages it replaces.  Once fully installed by the function
;; `pel-setup-bundled-operation-mode', it contains a file "pel-bundle-autoloads.el"
;; which holds the auto-loading code of all the original one-level packages it
;; replaces and a "pel-bundle-pkg.el" file that contains the specification of
;; this fake package.
;;
;; The code creates the "~/.emacs.d/elpa-reduced" directory which is the original
;; elpa directory with all one-level package directories removed.  The
;; "pel-bundle" package is placed inside that directory.
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
;;
;; The code in `pel-setup-bundled-operation-mode'  uses the logic here to
;; prepare the directories and code that is then used by PEL startup to take
;; advantage of storing most packages inside the same directory.
;;


;; Function call hierarchy:
;;
;; - `pel-elpa-one-level-package-alist'
;;   - `pel-elpa-load-pkg-descriptor'
;;   - `pel-elpa-one-level-package-files'
;;     - `pel-elpa-pkg-filename'
;;     - `pel-elpa-one-level-packages'
;;       - `pel-elpa-package-directories'
;;         - `pel-elpa-package-dirspec-p'

;; Credits: Thanks to "phils" for mentioning the `package-load-descriptor'
;;  function from package.el as a reply to my question here:
;;  https://emacs.stackexchange.com/questions/66616/how-to-create-a-structure-instance-out-of-a-pkg-el-file/66620
;; It saved me some time in searching for it inside package.el.

(defun pel-elpa-package-dirspec-p (dirspec)
  "Return dirname when DIRSPEC is for a Elpa package directory, nil
otherwise.
The DIRSPEC is the data structure returned by the function
`directory-files-and-attributes'."
  (let (dirname)
    (when (and (cadr dirspec)           ; is a directory ...
                                        ; ... that has a name that does not
                                        ; start with a period
               (not (eq (string-to-char (setq dirname (car dirspec)))
                        ?.))            ; and is not the archives directory
               (not (member dirname '("archives" "gnupg"))))
      dirname)))

(defun pel-elpa-package-directories (elpa-dirpath)
  "Return a list of package directories inside the ELPA-DIRPATH directory."
  (mapcar #'car
          (seq-filter (function pel-elpa-package-dirspec-p)
                      (directory-files-and-attributes elpa-dirpath))))

;; --

(defun pel-elpa-one-level-packages (elpa-dirpath)
  "Return a list of directories in ELPA-DIRPATH that have no sub-directories."
  (let ((elpa-dirnames (pel-elpa-package-directories elpa-dirpath)))
    (seq-filter
     (lambda (dn)
       (when (eq (pel-subdir-count (expand-file-name dn elpa-dirpath)) 0)
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
package descriptor to that directory.
If the RESULT-ALIST-SYMBOL is specified set its value with the result alist."
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

(defun pel-elpa-pkg-files-in (dirpath)
  "Return the -pkg.el filenames of all Elpa packages in DIRPATH."
  (mapcar (lambda (dn)
            (pel-elpa-pkg-filename (expand-file-name dn dirpath) :with-path))
          (pel-elpa-package-directories dirpath)))

(defun pel-elpa-disable-pkg-deps-in (dirpath)
  "Disable the dependencies of all -pkg.el files of packages in DIRPATH.
Return an alist of (package versions) that should be added to the
variable `package--builtin-versions'."
  ;; The package descriptor data structure is a cl-defstruct of type
  ;; `package-desc' defined in the package.el library file.
  ;; The slot `reqs' is what interests us here.
  (let (pkg-spec pkg-deps-versions dep vers)
    (dolist (pkg-fn (pel-elpa-pkg-files-in dirpath))
      (when pkg-fn
        ;; Read the package descriptor structure into the pkg-spec variable.
        (setq pkg-spec (pel-elpa-load-pkg-descriptor pkg-fn))
        ;; Add the dependencies not already in `package--builtin-versions' to
        ;; the pkg-deps-versions
        (dolist (dep-ver (package-desc-reqs pkg-spec))
          (setq dep (car dep-ver))
          (unless (assoc dep package--builtin-versions)
            (setq vers (cadr dep-ver))
            (push dep vers)
            (unless (assoc dep pkg-deps-versions)
              (push vers pkg-deps-versions))))
        ;; Erase the `reqs' slot value from it, effectively erasing the
        ;; package dependants.
        (setf (package-desc-reqs pkg-spec) nil)
        ;; Write the modified structure back into its file.
        (package-generate-description-file pkg-spec pkg-fn)))
    pkg-deps-versions))

;; ---------------------------------------------------------------------------


(defun pel-elpa-create-copies (elpa-dir-path
                               dest-dir-path
                               &optional with-symlinks)
  "Copy all .el files of one-level Elpa packages into a single directory.

The single directory is DEST-DIR-PATH.  This directory must
already exist.  If WITH-SYMLINKS is non-nil, ELPA-DIR-PATH files
are not copied to DEST-DIR-PATH: instead the function creates
symlinks in DEST-DIR-PATH to files inside ELPA-DIR-PATH.

The function only copies .el and .elc files from the Elpa packages that
have no sub-directories (called one-level packages) and therefore
have all their files inside one directory.  It does *not* copy
the files *-autoloads.el? and *-pkg.el? nor any file that have a name that
starts with a period of a # sign.

When WITH-SYMLINK is non-nil, the function returns a list of
(file-path-1 . file-path-2) cons cells that identify the duplicated file
names, or nil if there are no duplicate. When WITH-SYMLINKS is nil the
function does not attempt to detect duplicate and returns nil."
  (let ((duplicates nil)
        (elpa-pure-dirnames (pel-elpa-one-level-packages elpa-dir-path))
        source-dir-path-name
        source-fn
        destination-fn
        fnse)
    (dolist (dirname elpa-pure-dirnames)
      (setq source-dir-path-name (expand-file-name
                                  dirname
                                  (file-truename elpa-dir-path)))
      (dolist (file-name (directory-files source-dir-path-name))
        (when (and (not (member (substring file-name 0 1) '("." "#")))
                   (member (file-name-extension file-name) '("el" "elc"))
                   (not (pel-string-ends-with-p (setq fnse
                                                      (file-name-sans-extension
                                                       file-name))
                                                "-pkg"))
                   (not (pel-string-ends-with-p fnse "-autoloads")))
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



;; -----

(defun pel-elpa-package-name-for (dirname)
  "Return the package name for the specific DIRNAME, return nil on error.
The DIRNAME is expected to be a package-name.-version string.
This only perform a string manipulation to extract the package name."
  (save-match-data
    (when (string-match "\\([a-zA-Z0-9-]+\\)-[0-9.]+" dirname))
    (match-string 1 dirname)))

(defun pel-elpa-package-alist-of-dir (dirpath)
  "Return a package-alist format alist of packages inside DIRPATH."
  (let (pkg-alist
        pkg-name
        pkg-fname
        pkg-pathname
        pkg-desc)
    (dolist (pkg-dirname (pel-elpa-package-directories dirpath) pkg-alist)
      (setq pkg-name (pel-elpa-package-name-for pkg-dirname))
      (setq pkg-pathname (expand-file-name pkg-dirname dirpath))
      (setq pkg-fname (expand-file-name (format "%s-pkg.el" pkg-name)
                                        pkg-pathname))
      (setq pkg-desc (pel-elpa-load-pkg-descriptor pkg-fname))
      ;; the loaded value does not set the directory; set it here
      (setf (package-desc-dir pkg-desc) pkg-pathname)
      (push (list (intern pkg-name) pkg-desc) pkg-alist))))

;;; --------------------------------------------------------------------------
(provide 'pel-elpa)

;;; pel-elpa.el ends here
