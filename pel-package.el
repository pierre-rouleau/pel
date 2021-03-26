;;; pel-package.el --- PEL package management.  -*- lexical-binding: t; -*-

;; Created   : Monday, March 22 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-03-26 16:30:31, updated by Pierre Rouleau>

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
;;  This file holds the logic PEL uses to control the management of Emacs Lisp
;;  packages, mainly the automatic *removal* of packages when a PEL user
;;  option is changed from active to inactive.
;;
;;  The code uses information stored into properties of `pel-use-' defcustom
;;  user-option variables.
;;
;;  The strategy for module cleanup is the following: when the `pel-cleanup'
;;  command is executed, it extracts the list of all packages that should be
;;  located in the Emacs elpa directory and in the PEL utils directory.  It
;;  gets this information by processing the properties of each `pel-use-'
;;  user-option variables that are non-nil. For each of them it calls the
;;  function `pel-packages-for'.  The function returns nil if nothing is
;;  expected to be present, otherwise it returns a list of (type . package)
;;  cons cells where type is either 'elpa or 'utils and package is a symbol
;;  that holds the name of the elpa package or the utils .el file name.  By
;;  doing this for all `pel-use-' user option we accumulate the list of
;;  expected packages.  Then  by looking into the directories we can remove or
;;  disable the exceeding package (by moving the package into an *attic*
;;  directory). For elpa package the package name is removed-files from the
;;  `package-selected-packages' variable and the active customization file is
;;  updated.
;;
;;  Removing packages that are not used improves Emacs speed: it reduces the
;;  load path: unfortunately the package management creates one directory per
;;  package and place this directory in the load path.  This is not so much an
;;  issue with the files stored in the utils directory as only one directory
;;  is inside the load path.
;;
;;  TODO: complete the file by adding the package removal logic. 🚧
;;


;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: pel-as-string, pel-as-symbol
(require 'pel--options)                 ; use: pel-elpa-package-to-keep

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-user-option-p (symbol)
  "Return t when SYMBOL is a valid PEL User-option, nil otherwise."
  (and (custom-variable-p symbol)
       (eq t (compare-strings "pel-use-" nil nil
                              (symbol-name symbol) 0 8))))

(defun pel-user-options ()
  "Return a list of all pel-use- user-option symbols."
  (let ((symbols '()))
    (mapatoms
     (lambda (symbol)
       (when (pel-user-option-p symbol)
         (push symbol symbols))))
    symbols))

(defun pel--assert-valid-user-option  (symbol)
  "Assert that the SYMBOL argument is a valid PEL user-option symbol.
Return t if it is, issue an error otherwise."
  (if (pel-user-option-p symbol)
      t
    (error "Invalid pel-package-for argument: %S.\
  It is not a valid PEL user-option!" symbol)))

;; TODO complete pel-install-from-elpa-attic
;;-pel-autoload
(defun pel-install-from-elpa-attic (pkg)
  "Install package PKG from the local copy stored in the elpa-attic directory.

Return t on success, nil otherwise.
The elpa-attic directory is the ~/.emacs.d/pel-elpa-attic directory."
  (display-warning 'pel-install-from-elpa-attic
                   (format "(pel-install-from-elpa-attic %s) not yet implemented!"
                           pkg)
                   :error)
  nil)

(defun pel-package-for (symbol)
  "Return package info for specified PEL-USER-OPTION.
PEL-USER-OPTION must be a `pel-use-' user-option symbol.
Returns a list of (type . package) cons cells for the external
package(s) that are installed when this user-option is turned on, if any.
The type is either 'elpa or 'utils.
Returns nil when:
- the user-option is not requesting anything to be installed
  - when the user-option is off
  - when the user option request to use a built-in package."
  (pel--assert-valid-user-option symbol)
  (when (symbol-value symbol)
    (let ((attribute-value (get symbol :package-is)))
      (cond
       ;; no attribute - use symbol suffix.  Standard Elpa.
       ((null attribute-value)
        (list (cons 'elpa
                    (intern (substring (symbol-name symbol) 8)))))
       ;; built-in attribute: return nil: nothing to manage
       ((eq attribute-value :builtin-emacs)
        nil)
       ;; gate attribute : user-option acts as a gate.
       ((eq attribute-value :a-gate)
        nil)
       ;; in-utils attribute: return the name from symbol but from utils
       ((eq attribute-value :in-utils)
        (list (cons 'utils
                    (intern (substring (symbol-name symbol) 8)))))
       ;; a cons form is used - evaluate it to extract its content
       ;; the evaluation may perform some checks and should have no side-effect.
       ;; The form should evaluate to a list of (type . package-name) cons cell(s).
       ((consp attribute-value)
        (eval attribute-value))
       ;; a symbol indicates an Elpa-based package
       ((symbolp attribute-value)
        (list (cons 'elpa attribute-value)))
       ;; Everything else is invalid
       (t
        (error "Invalid PEL package spec for %s: %S" symbol
               attribute-value))))))

(defun pel-packages-for (symbol)
  "Return availability specs for specified PEL user-option SYMBOL.

SYMBOL must be a PEL `pel-use-' user-option symbol.

Returns a (potentially nil) list of (type . package) cons cells
where `type' represents where the package should be located and
`package' is the package name.  The possible values for `type'
are:
  - elpa : stating that `package' should be available in the local
    elpa directory.
  - utils: stating that `package' should be available in the local
    PEL utils directory.
The returned information identifies what *should* be the state of the package
installation.  It is mainly used to accumulate lists of the packages that must
remain in the local elpa and PEL utils when a PEL cleanup is requested after
some PEL user-options have been turned off."
  (pel--assert-valid-user-option symbol)
  (let ((parent-user-options (get symbol :requires))
        (pkg-spec-list '())
        (a-parent-is-disabled nil)
        (a-parent-is-enabled nil)
        (requires-all-parents nil))
    ;; The parent-user-options may be a single symbol or a list of symbols.
    ;; If it's a list of symbols, its first element may be the `:all' symbol
    ;; to indicate that all parents must be active for the SYMBOL to be
    ;; installed.  Remember the condition and remove it from the list.
    ;; If its only a symbol, transform it into a list of 1 symbol.
    (if (listp parent-user-options)
        (when (eq (car parent-user-options) :all)
          (setq requires-all-parents t)
          (setq parent-user-options (cdr parent-user-options)))
      (setq parent-user-options (list parent-user-options)))
    ;; The package spec for SYMBOL is installed along its parent's
    ;; when the symbol value of each of the parent symbols is non-nil.
    ;; If any of them is nil, then the package for SYMBOL will not be
    ;; installed and only the active parent(s) is/are installed.
    (dolist (parent-user-option parent-user-options)
      (if (symbol-value parent-user-option)
          (progn
            (setq a-parent-is-enabled t)
            (dolist (spec (pel-package-for parent-user-option))
              (when spec
                (push spec pkg-spec-list))))
        (setq a-parent-is-disabled t)))
    ;; If there is no required parents, or
    ;; all parents are require and are all enabled, or
    ;; one of several parent is required and one is enabled
    ;; then the package corresponding to the SYMBOL is installed.
    ;; TODO: does not handle more complex situations like: (A and B) are both
    ;; needed or (C and D) or E.
    (when (or (null parent-user-options)
              (and requires-all-parents
                   (not a-parent-is-disabled))
              (and (not requires-all-parents)
                   a-parent-is-enabled))
      (dolist (spec (pel-package-for symbol))
        (when spec
          (push spec pkg-spec-list))))
    ;; return the list of packages that should be present.
    pkg-spec-list))

(defun pel-symbol-name-< (s1 s2)
  "Compare name strings of S1 and S2 symbols."
  (string< (symbol-name s1) (symbol-name s2)))

(defun pel-elpa-pkg-dependencies (pkg)
  "Return a list of package symbols that are elpa dependencies of package PKG.

PKG may be a symbol or a string."
  ;; Make sure that pkg is present, if it is not then its dependants
  ;; are not installed via that package.
  (when (locate-library (pel-as-string pkg))
    (if (and (require 'package nil :no-error)
             (fboundp 'package--get-deps))
        (package--get-deps (pel-as-symbol pkg))
      (error "Failed loading package"))))

(defun pel-activated-packages (&optional without-dependants)
  "Return a list of packages activated by PEL user-options.

The lists include all dependant packages unless WITHOUT-DEPENDANTS is
specified and non-nil.

Return a list of 2 lists:
- first list is a list of elpa package symbols,
- second list is a list of utils file name symbols.

The elements of each list are sorted by alphabetical order of
their names."
  (let ((elpa-list '())
        (utils-list '()))
    (dolist (user-option (pel-user-options))
      (dolist (spec (pel-packages-for user-option))
        (cond
         ;; elpa package
         ((eq 'elpa (car spec))
          (let  ((elpa-pkg (cdr spec)))
            (unless (memq elpa-pkg elpa-list)
              (push elpa-pkg elpa-list)
              (unless without-dependants
                (dolist (dep-pkg (pel-elpa-pkg-dependencies elpa-pkg))
                  (unless (memq dep-pkg elpa-list)
                    (push dep-pkg elpa-list)))))))
         ;; utils package
         ((eq 'utils (car spec))
          (let ((utils-pkg (cdr spec)))
            (unless (memq utils-pkg utils-list)
              (push utils-pkg utils-list))))
         (t (error "Invalid spec for %s: %S" user-option spec)))
        ))
    (list
     (sort elpa-list (function pel-symbol-name-<))
     (sort utils-list (function pel-symbol-name-<)))))

;; pel-autoload
(defun pel-package-stats ()
  "Display number of packages required by PEL"
  (interactive)
  (let* ((all-activated (pel-activated-packages))
         (activated     (pel-activated-packages :without-deps))
         (elpa-all      (length (car all-activated)))
         (elpa-base     (length (car activated)))
         (elpa-deps     (- elpa-all elpa-base))
         (utils-all     (length (cadr all-activated)))
         (utils-base    (length (cadr activated)))
         (utils-deps    (- utils-all utils-base))
         (user-options  (pel-user-options)))
    (message "\
Number of PEL user-options  : %d (%d are active)
PEL activated elpa  packages: %d%s
PEL Activated utils files   : %d%s"
             (length user-options)
             (length (seq-filter (lambda (x) (symbol-value x)) user-options))
             elpa-base
             (if (> elpa-deps 0)
                 (format " (with %d extra dependency packages)" elpa-deps)
               "")
             utils-base
             (if (> utils-deps 0)
                 (format " (with %d extra dependency files)" utils-deps)
               ""))))


(defun pel-el-file-for (filename)
  "Return the .el filename of an .elc FILENAME."
  (format "%s.el" (file-name-sans-extension filename)))

(defun pel-remove-invalid-elc (directory)
  "Remove the old and the orphaned elc files in DIRECTORY.

DIRECTORY must be a directory path string.
Returns the list of removed file names."
  (let ((removed-files '())
        (elc-files (directory-files directory :full-path "\\.elc\\'"))
        (el-file nil))
    (dolist (elc-file elc-files)
      (setq el-file (pel-el-file-for elc-file))
      (unless (or (file-exists-p el-file)
                  (file-newer-than-file-p el-file elc-file))
        (push elc-file removed-files)
        (delete-file elc-file)))
    removed-files))


(defconst pel-utils-dirpath (file-name-as-directory
                             (expand-file-name "utils" user-emacs-directory))
  "Absolute path of the PEL utils directory.")

(defconst pel-utils-attic-dirpath (file-name-as-directory
                                   (expand-file-name "utils-attic"
                                                     user-emacs-directory))
  "Absolute path of the PEL utils-attic directory.")

(defun pel-active-and-excess-utils ()
  "Return a list of 2 lists of utils Emacs Lisp files: active and not active.

Each returned list contains directory relative .el file names, in sorted name
order.
The first list identifies the files that are currently active, requested by
PEL user-options.
The second list identifies the files that are currently not used by the PEL
user options."
  (let ((utils-el-files (directory-files pel-utils-dirpath nil "\\.el\\'"))
        (active-utils-files '())
        (excess-utils-files '()))
    (dolist (utils-symbol (cadr (pel-activated-packages)))
      (push (format "%s.el" utils-symbol) active-utils-files))
    (dolist (util-el-file utils-el-files)
      (unless (member util-el-file active-utils-files)
        (push util-el-file excess-utils-files)))
    (list (reverse active-utils-files)
          (reverse excess-utils-files))))

(defun pel-utils-unrequired ()
  "Return the list of utils files not currently required."
  (cadr (pel-active-and-excess-utils)))

(defun pel-clean-utils (&optional verbose)
  "Move all unrequired Emacs Lisp files from utils to utils-attic directory."
  (let ((unrequired-files (pel-utils-unrequired)))
    (when unrequired-files
      (unless (file-exists-p pel-utils-attic-dirpath)
        (make-directory pel-utils-attic-dirpath))
      (dolist (file unrequired-files)
        (rename-file (expand-file-name file pel-utils-dirpath)
                     pel-utils-attic-dirpath))
      (when verbose
        (message "Moved %d files from %s to %s\nThe files are: %s"
                 (length unrequired-files)
                 pel-utils-dirpath
                 pel-utils-attic-dirpath
                 unrequired-files)))))


(defconst pel-elpa-dirpath  (file-name-as-directory
                             (expand-file-name "elpa"
                                               user-emacs-directory))
  "Absolute path of the user elpa directory.")

(defconst pel-elpa-attic-dirpath  (file-name-as-directory
                                   (expand-file-name "elpa-attic"
                                                     user-emacs-directory))
  "Absolute path of the user elpa-attic directory.")

(defun pel-move-to-dir (file dir)
  "Move FILE to directory DIR.
FILE may represent a file or a directory.
DIR must represent a directory.
Trailing slash is not required for DIR but allowed."
  ;; make sure DIR does not end with slash otherwise rename-file will
  ;; act as if we wanted to move FILE into DIR as opposed to rename it.
  (rename-file (directory-file-name file)
               (directory-file-name dir)))

(defun pel-elpa-dirs-for (pkg)
  "Return a list of all directories for specified package PKG.

PKG may be a symbol or a string.
Each directory is specified with full path: a directory inside the elpa
directory."
  (directory-files pel-elpa-dirpath
                   :full-path
                   (format "\\`%s-[0-9-.]+\\'"
                           (regexp-quote (pel-as-string pkg)))))




(defun pel-move-elpa-to-elpa-attic (pkg)
  "Move all versions of PKG package from elpa to the elpa-attic directory."
  (display-warning 'pel-move-elpa-to-elpa-attic
                   (format "TODO: complete pel-move-elpa-to-elpa-attic: %s"
                           pkg)
                   :error))




;; TODO: complete the cleanup of elpa
(defun pel-clean (&optional verbose)
  "Move all unrequired packages to their attic directory."
  (pel-clean-utils verbose))

;;; --------------------------------------------------------------------------
(provide 'pel-package)

;;; pel-package.el ends here
