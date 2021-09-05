;;; pel-package.el --- PEL package management.  -*- lexical-binding: t; -*-

;; Created   : Monday, March 22 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-09-05 17:24:23, updated by Pierre Rouleau>

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
;;  user-option variables that are non-nil.  For each of them it calls the
;;  function `pel-packages-for'.  That function returns nil if nothing is
;;  expected to be available, otherwise it returns a list of (type . package)
;;  cons cells where type is either 'elpa or 'utils and package is a symbol
;;  that holds the name of the elpa package or the utils .el file name.  By
;;  doing this for all `pel-use-' user option we accumulate the list of
;;  packages that should be available.  Then by looking into the directories
;;  we can remove or disable the exceeding package (by moving the package into
;;  an *attic* directory). For elpa package the package name is removed-files
;;  from the `package-selected-packages' variable and the active customization
;;  file is updated.
;;
;;  Removing packages that are not used improves Emacs speed: it reduces the
;;  length of the load path that tends to grow rapidly with new packages
;;  installed because, unfortunately, the package management creates one
;;  directory per package and place this directory in the load path.  This is
;;  not so much an issue with the files stored in the utils directory as only
;;  one directory is inside the load path.
;;
;;  The file provides two commands:
;;
;;  - Use `pel-package-info' to get a quick overview of the packages requested
;;    by PEL user-options, their dependencies and the packages that must
;;    remain because they are use by Emacs running in another mode
;;    (graphic/TTY).  Produce a more detailed report in a *pel-user-options*
;;    buffer by passing an argument to the command.
;; - Use the command `pel-cleanup' to deactivate all packages in excess:
;;   packages that are not in the list of:
;;
;;   - packages that PEL requires via its `pel-use-' user-options,
;;   - their dependencies,
;;   - packages that are *locked*, installed by Emacs running in another mode
;;     of operation (graphics vs TTY),
;;   - packages installed manually that you have identified in the following
;;     PEL user-options:
;;                       - `pel-elpa-packages-to-keep'
;;                       - `pel-utils-packages-to-keep'
;;
;;     The `pel-cleanup' command does not delete files and directories.
;;     Instead it moves them into *attic* directories where they can be
;;     retrieved later.  This way if you stop using a specific Elpa package
;;     and then it disappears from MELPA (because the author stops maintaining
;;     it - that happens) you will still have access to it. If however you
;;     disable a package that is already in the *attic* directory, then it
;;     will be deleted but placed in your computer trash can where you can
;;     extract it if you want.
;;
;; The file also provides the `pel-install-from-elpa-attic' function, used to
;; install files from the elpa attic, allowing quick restoration of disabled
;; elpa package without having to access the Internet.
;;
;;  The code identifies you local Elpa and utils directories and their attic
;;  counterparts, normally stored inside the ~/.emacs.d directory or the
;;  equivalent.  The location of those directories is stored inside the
;;  following defconst variables:
;;
;;                       - `pel-elpa-dirpath'
;;                       - `pel-elpa-attic-dirpath'
;;                       - `pel-utils-dirpath'
;;                       - `pel-utils-attic-dirpath'

;; The function call trees are shown here:
;;
;; * `pel-package-info'
;;   - `pel-activated-packages'
;;   - `pel-user-options'
;;     - `pel-user-option-p'
;;   - `pel--show-pkgs-for'
;;   - `pel-elpa-unrequired'            (see its call tree below)
;;   - `pel-utils-unrequired'           (see its call tree below)
;;   - `pel--show-pkgs-in-excess-for'
;;
;;
;; * `pel-cleanup'
;;   - `pel-clean-utils'
;;     - `pel-utils-unrequired'
;;       - `pel-active-and-excess-utils'
;;         - `pel-activated-packages'
;;           - `pel-user-options'
;;             - `pel-user-option-p'
;;           - `pel-packages-for'
;;             - `pel-package-for'
;;               - `pel--assert-valid-user-option'
;;                 - `pel-user-option-p'
;;               - `pel-package-also-required-p'
;;               - `pel-restricted-active-user-option-p'
;;                 - `pel--assert-valid-user-option'
;;                   - `pel-user-option-p'
;;               - `pel-spec-for-symbol-attribute'
;;           - `pel-elpa-pkg-dependencies'
;;           - `pel-symbol-name-<'
;;     - `pel-remove-invalid-elc'
;;       - `pel-el-file-for'
;;   - `pel-clean-elpa'
;;     - `pel-elpa-unrequired'
;;       - `pel-activated-packages'     (see its call tree above)
;;       - `pel-elpa-packages-in-dir'
;;       - `pel-symbol-name-<'
;;     - `pel-move-elpa-pkg-to-elpa-attic'
;;       - `pel-elpa-dirs-for'
;;       - `pel-move-to-dir'
;;     - `pel-clean-package-selected-packages'
;;     - `pel-clean-package-selected-packages-in-file'
;;
;;  - `pel-install-from-elpa-attic'
;;    - `pel-elpa-dirs-for'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: `pel-as-string', `pel-as-symbol'
;;                                      ;      `pel-print-in-buffer'
;;                                      ;      `pel-in-fast-startup-p'
(require 'pel--options)                 ; use: `pel-elpa-packages-to-keep'
;;                                      ;      `pel-utils-packages-to-keep'
;;                                      ;      `pel-elpa-obsolete-packages'
(require 'pel-navigate)                 ; use: `pel-backward-token-start'
(require 'pel-elpa)                     ; use: `pel-elpa-package-directories'
;;                                      ;      `pel-el-files-in'
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-locate-elpa ()
  "Return the absolute path of the local Elpa directory.

PEL init.el file changes the value of `package-user-dir' to
ensure that the value used by package.el is a true directory
name, not a symlink.  That is done to ensure that the entries
inside the `load-path' remain valid even if an external Emacs/PEL
process switches the startup mode and changes the target of the
elpa symlink.

Before this is done, the code stores the original value of
`package-user-dir', which corresponds to the user-option value
stored in the custom file, into `pel-package-user-dir-original'.

PEL will be able to use this value to transform the original elpa
directory into a symlink that points to a directory named
'elpa-complete'.

It's possible that PEL is used with a init.el file that has not
yet been populated with the proper code. In that case the
`pel-package-user-dir-original' variable may not exist, so

In the remote possibility that `package-user-dir' is not bound
then the \"elpa\" sub-directory of the directory identified by
the variable `user-emacs-directory' is used."
  (file-name-as-directory (if (bound-and-true-p pel-package-user-dir-original)
                              (expand-file-name pel-package-user-dir-original)
                            (if (and (require 'package nil :no-error)
                                     (boundp 'package-user-dir))
                                (expand-file-name package-user-dir)
                              (expand-file-name "elpa" user-emacs-directory)))))


(defconst pel-elpa-dirpath  (pel-locate-elpa)
  "Absolute path of the user elpa directory or symlink.
This may differ from the value of `package-user-dir' when a symlink
is used as PEL init files ensure that `package-user-dir' is set to the
target of the elpa symlink while `pel-elpa-dirpath' is always set to the
path of the elpa directory or symlink if it exists.
Note that you can have several elpa directories if you set `package-user-dir'
inside your init.el file.")

(defconst pel-elpa-attic-dirpath
  (file-name-as-directory
   (pel-elpa-name (expand-file-name "elpa-attic" user-emacs-directory)
                  (and (bound-and-true-p pel-init-support-dual-environment-p)
                       pel-emacs-is-graphic-p)))
  "Absolute path of the user elpa-attic directory.
PEL supports a pel-attic directory for dual independent customization when
it is requested as specified by the presence of `pel-init-support-dual-environment-p'
symbol set to t.")

(defconst pel-utils-dirpath
  (file-name-as-directory
   (pel-elpa-name (expand-file-name pel-utils-dirname user-emacs-directory)
                  (and (bound-and-true-p pel-init-support-dual-environment-p)
                       pel-emacs-is-graphic-p)))
  "Absolute path of the PEL utils directory.
PEL supports a utils directory for dual independent customization when
it is requested as specified by the presence of `pel-init-support-dual-environment-p'
symbol set to t.")

(defconst pel-utils-attic-dirpath
  (file-name-as-directory
   (pel-elpa-name (expand-file-name (concat pel-utils-dirname "-attic")
                                    user-emacs-directory)
                  pel-emacs-is-graphic-p))
  "Absolute path of the PEL utils-attic directory.")

(defconst pel-required-packages '(popup)
  "List of package names that PEL always uses.")

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
    (error "Invalid argument: %S.\
  It is not a valid PEL user-option!" symbol)))

(defun pel--assert-valid-expression (expr &optional context)
  "Assert that EXPR is really a Lisp expression or symbol, not a value.
Return t if EXPR is an expression, issue an error otherwise.
If CONTEXT is specified, it is printed in the error message when
EXPR is not an expression."
  (if (pel-expression-p expr)
      t
    (error "Invalid expression: %S%s" expr
           (if context
               (format " : %s" context)
             ""))))
;; --

(defun pel-restricted-active-user-option-p (symbol)
  "Return t when user-option SYMBOL has an active restriction, nil otherwise.

An active restriction is specified by the `:restricted-to'
attribute which identifies a condition.  When this condition is
nil the related package cannot be removed: the restriction is
active.

PEL currently supports restrictions related to the mode Emacs
runs in: either graphics or TTY.  For example, a package that runs
in graphics mode can only be removed in graphics mode.  So when running in
terminal mode, such a package is identified as active to prevent the
`pel-cleanup' function to remove it when the user-option is not active."
  (pel--assert-valid-user-option symbol)
  (let ((restricted-prop (get symbol :restricted-to)))
    (and restricted-prop
         (not (eval restricted-prop)))))


(defun pel-spec-for-symbol-attribute (symbol property
                                             &optional no-property-is-elpa)
  "Extract the spec list for the PROPERTY of specified `pel-use-' SYMBOL.
Returns a list of (`type' . `package-name') cons cells.
Where:
- `type'         := 'elpa or 'utils
- `package-name' := a Emacs package name."
  (let ((attribute-value (get symbol property)))
    (cond
     ;; no attribute - use symbol suffix.  Standard Elpa.
     ((null attribute-value)
      (when no-property-is-elpa
        (list (cons 'elpa
                    (intern (substring (symbol-name symbol) 8))))))
     ;; built-in attribute: return nil: nothing to manage
     ((eq attribute-value :builtin-emacs)
      nil)
     ;; gate attribute : user-option acts as a gate.
     ((eq attribute-value :a-gate)
      nil)
     ;; in-utils attribute: return the name from symbol but from
     ;; utils
     ((eq attribute-value :in-utils)
      (list (cons 'utils
                  (intern (substring (symbol-name symbol) 8)))))
     ;; a cons form is used - evaluate it to extract its content the
     ;; evaluation may perform some checks and should have no
     ;; side-effect.  The form should evaluate to a list of (type
     ;; . package-name) cons cell(s).
     ((consp attribute-value)
      (eval attribute-value))
     ;; a symbol indicates an Elpa-based package
     ((symbolp attribute-value)
      (list (cons 'elpa attribute-value)))
     ;; Everything else is invalid
     (t
      (error "Invalid PEL package spec for %s: %S" symbol
             attribute-value)))))

(defun pel-package-also-required-p (symbol)
  "Return evaluated form associated with :also-required-when property of pel-use- SYMBOL.
Return nil otherwise.
Raise issue if SYMBOL is not a pel-use- symbol or when the value of the
:also-required-when property is not a form to evaluate."
  (pel--assert-valid-user-option symbol)
  (let ((boolean-form (get symbol :also-required-when)))
    (when (and boolean-form
               (pel--assert-valid-expression boolean-form "for :also-required-when"))
      (eval boolean-form))))

(defun pel-package-for (symbol &optional ignore-restriction)
  "Return package info for specified PEL user-option SYMBOL.
SYMBOL must be a `pel-use-' user-option symbol.
Returns a list of (type . package) cons cells for the external
package(s) that are installed when this user-option is turned on, if any.
The type is either 'elpa or 'utils.

The lists include all external packages that cannot be removed
because of imposed restriction unless IGNORE-RESTRICTION is
non-nil.

Returns nil when:
- the user-option is not requesting anything to be installed:
  - when the user-option is off and nothing else force its installation
  - when the user option request to use a built-in package."
  (pel--assert-valid-user-option symbol)
  ;; Package for the symbol is active when the pel-use- user-option is non-nil
  ;; or when the symbol is for a restricted package and the restriction
  ;; applies and is not ignored.
  (when (or (symbol-value symbol)
            (pel-package-also-required-p symbol)
            (and (not ignore-restriction)
                 (pel-restricted-active-user-option-p symbol)))
    (let ((specs (pel-spec-for-symbol-attribute
                  symbol :package-is :no-property-is-elpa)))
      ;; Some `pel-use-' user-options are for packages that do not completely
      ;; identify their dependencies in their pkg-X.el file.  We complete it
      ;; with the :requires-package property.  Extract these specs and append
      ;; them to the specs.
      ;;
      ;; TODO: currently packages inserted like these and not explicitly
      ;; requested by a `pel-use-' user-option are not identified as
      ;; dependencies in the report.  There should be some indication about it
      ;; being requested via a dependency identified by PEL.
      (dolist (xtra-spec (pel-spec-for-symbol-attribute
                          symbol :requires-package))
        (push xtra-spec specs))
      ;; return the complete list of specs
      specs)))

;; --

(defun pel-packages-for (symbol &optional ignore-restriction)
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

The lists include all packages that cannot be removed because of imposed
restriction unless IGNORE-RESTRICTION is non-nil.

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
    ;; The package spec for SYMBOL is active along its parent's
    ;; when the symbol value of each of the parent symbols is non-nil.
    ;; If any of them is nil, then the package for SYMBOL will not be
    ;; installed and only the active parent(s) is/are installed.
    (dolist (parent-user-option parent-user-options)
      (if (symbol-value parent-user-option)
          (progn
            (setq a-parent-is-enabled t)
            (dolist (spec (pel-package-for parent-user-option
                                           ignore-restriction))
              (when spec
                (push spec pkg-spec-list))))
        (setq a-parent-is-disabled t)))
    ;; If - there is no required parents, or
    ;;    - all parents are required and are all enabled, or
    ;;    - one of several parent is required and one is enabled, or
    ;;    - the package is also required by another package that is enabled,
    ;; then the package corresponding to the SYMBOL is installed.
    ;; TODO: does not handle more complex situations like: (A and B) are both
    ;; needed or (C and D) or E.
    (when (or (null parent-user-options)
              (and requires-all-parents
                   (not a-parent-is-disabled))
              (and (not requires-all-parents)
                   a-parent-is-enabled)
              (pel-package-also-required-p symbol))
      (dolist (spec (pel-package-for symbol ignore-restriction))
        (when spec
          (push spec pkg-spec-list))))
    ;; return the list of packages that should be present.
    pkg-spec-list))

;; ----

(defun pel-symbol-name-< (s1 s2)
  "Compare name strings of S1 and S2 symbols."
  (string< (symbol-name s1) (symbol-name s2)))

(defun pel-elpa-pkg-dependencies (pkg)
  "Return a list of package symbols that are elpa dependencies of package PKG.

PKG may be a symbol or a string."
  ;; Make sure that pkg is present, if it is not then its dependants
  ;; are not installed via that package.
  (when (locate-library (pel-as-string pkg))
    (setq pkg (pel-as-symbol pkg))
    (if (and (require 'package nil :no-error)
             (fboundp 'package--get-deps))
        (condition-case err
            (let ((pkg-arg      pkg)
                  (dependencies '()))
              ;; package--get-deps was modified on the October 6th 2019.
              ;; The argument for the new version is a list.
              (when pel-emacs-27-or-later-p
                (setq pkg-arg (list pkg-arg)))
              (setq dependencies (package--get-deps pkg-arg))
              ;; package--get-deps of October 6th 2019 leaves the searched
              ;; pkg in the list of dependencies it returns.  The old code did
              ;; not do that. IMHO its a bug, violating the principle of least
              ;; surprise, but it looks like the only place where it is
              ;; invoked requires it to be included.
              ;; Since I don't want the pkg to be listed in its dependencies,
              ;; I remove it if it is there.
              (when (memq pkg dependencies)
                (delete pkg dependencies))
              dependencies)
          (wrong-type-argument
           (unless (memq pkg pel-elpa-obsolete-packages)
             (display-warning
              'pel-elpa-pkg-dependencies
              (format "Error extracting dependencies for %s : %s
Is it obsolete? If so it should be added to pel-elpa-obsolete-packages."
                      pkg err)
              :error))
           nil))
      (error "Failed loading package"))))

(defun pel-activated-packages (&optional without-dependants ignore-restriction)
  "Return a list of packages activated by PEL user-options.

The lists include all dependant packages unless WITHOUT-DEPENDANTS is
specified and non-nil.
The list includes all packages that cannot be removed because of imposed
restriction unless IGNORE-RESTRICTION is non-nil.

Return a list of 2 lists:
- first list is a list of elpa package symbols,
- second list is a list of utils file name symbols.

The elements of each list are sorted by alphabetical order of
their names."
  (let ((elpa-list '())
        (utils-list '()))
    (dolist (user-option (pel-user-options))
      (dolist (spec (pel-packages-for user-option ignore-restriction))
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

;; --

(defun pel--show-pkgs-for (group-name all-pkgs pkgs+lock pkgs+deps to-keep)
  "Utility: insert description of used packages.
- GROUP-NAME: String: either \"Elpa\" or \"Utils\".
- ALL-PKGS:   List of all packages for this group.
- PKGS+LOCK:  List of packages requested by PEL user-options and the ones
              that are included because of restriction locks.
- PKGS+DEPS:  List of packages requested by PEL user-options and their
              dependencies.
TO-KEEP:      List of package symbols or file name strings that are installed
              independently from PEL and must therefore not be removed by the
              execution of the `pel-cleanup' function."
  (insert (format "\n%s activated packages:\n" group-name))
  (let ((n 0))
    (dolist (pkg all-pkgs)
      (let ((isa-dep (and (not (memq pkg pkgs+lock))
                          (memq pkg pkgs+deps)))
            (isa-lck (and (not (memq pkg pkgs+deps))
                          (memq pkg pkgs+lock))))
        (setq n (1+ n))
        (insert (format "- %3d: %-40s%s%s\n"
                        n
                        pkg
                        (if isa-dep "  (dependency) "
                          "               ")
                        (if isa-lck "  (requested by restriction)"
                          "")))))
    (when to-keep
      (setq n 0)
      (insert (format "%s manually installed:\n" group-name))
      (dolist (pkg to-keep)
        (setq n (1+ n))
        (insert (format "- %3d: %-40s\n" n pkg))))))

(defun pel--show-pkgs-in-excess-for (group pkgs)
  "Utility: list GROUP package PKGS in excess.
Return the number of packages in excess."
  (let ((n 0))
    (when pkgs
      (insert (format "\n%s packages in excess:\n" group))
      (dolist (pkg pkgs)
        (setq n (1+ n))
        (insert (format "- %3d: %s\n" n pkg))))
    n))

;; declare package.el variables  to prevent byte-compiler warnings
(defvar package-selected-packages)
(defvar package-alist)
(defvar  package-activated-list)

(defun pel--elpa-stats (base deps locked)
  "Return formatted string for package BASE, DEPS and LOCKED counts.
When running in fast startup setup, the counts are meaningless, so return
a string that says so."
  (if (pel-in-fast-startup-p)
      "Not available in PEL fast startup setup."
    (format "%3d (%3d dependants, %d imposed by restrictions)"
            base deps locked)))

;; pel-autoload
(defun pel-package-info (&optional full-report)
  "Display information about packages required by PEL.
Prints the information on the echo area unless a FULL-REPORT
argument is specified.  In that case, prints a complete report inside a special
*pel-user-options* buffer, listing all packages, indicating whether the
package is in elpa or utils and whether it is a dependency or included because
of a restriction lock."
  (interactive "P")
  (let* ((all-activated   (unless (pel-in-fast-startup-p)
                            (pel-activated-packages))) ; all (with dependencies & locks)
         (activated+lock  (unless (pel-in-fast-startup-p)
                            (pel-activated-packages :without-deps)))
         (activated-bdeps (unless (pel-in-fast-startup-p)
                            (pel-activated-packages nil :without-locks)))
         (elpa-all          (car all-activated))
         (n-elpa-all        (length elpa-all))
         (elpa+lock         (car activated+lock))
         (n-elpa-base       (length elpa+lock))
         (elpa-bdeps        (car activated-bdeps))
         (n-elpa-bdeps      (length elpa-bdeps))
         (n-elpa-deps       (- n-elpa-all n-elpa-base))
         (n-elpa-locked     (- n-elpa-all n-elpa-bdeps))
         (utils-all         (cadr all-activated))
         (n-utils-all       (length utils-all))
         (utils+lock        (cadr activated+lock))
         (n-utils-base      (length utils+lock))
         (utils-bdeps       (cadr activated-bdeps))
         (n-utils-bdeps     (length utils-bdeps))
         (n-utils-deps      (- n-utils-all n-utils-base))
         (n-utils-locked    (- n-utils-all n-utils-bdeps))
         (user-options      (pel-user-options))
         (overview  (format "\
- custom-file                 : %s
- package-user-dir            : %s
- %3d Elpa packages stored in : %s
- %3d Utils files   stored in : %s
- size of load-path           : %d directories
- Number of PEL user-options  : %3d (%d are active)
- PEL activated elpa  packages: %s
- PEL Activated utils files   : %s
- # loaded files              : %d
- # features                  : %d
- # package-alist             : %d
- # packages activated        : %d
- # packages selected         : %d
- Emacs init-time             : %s"
                            custom-file
                            package-user-dir
                            (length
                             (pel-elpa-package-directories package-user-dir))
                            package-user-dir
                            (length (pel-el-files-in pel-utils-dirpath))
                            pel-utils-dirpath
                            (length load-path)
                            (length user-options)
                            (length (seq-filter
                                     (lambda (x) (symbol-value x))
                                     user-options))
                            (pel--elpa-stats n-elpa-base n-elpa-deps
                                             n-elpa-locked)
                            (pel--elpa-stats n-utils-base n-utils-deps
                                             n-utils-locked)
                            (length load-history)
                            (length features)
                            (length package-alist)
                            (length package-activated-list)
                            (length package-selected-packages)
                            (if (and (require 'time nil :no-error)
                                     (fboundp 'emacs-init-time))
                                (emacs-init-time)
                              "?"))))
    (if full-report
        (if (pel-in-fast-startup-p)
            (user-error "PEL is running in fast-startup.  This is only available in normal mode!")
          (pel-print-in-buffer
           "*pel-user-options*"
           "PEL User Option activated packages"
           (lambda ()
             "Print full report."
             (insert (format "\n%s\n
Elpa packages and Utils files are shown below.  The dependencies
and lock restrictions are identified.  Note that a package
required by PEL may also be a dependency of another package; the
ones identified as dependencies may also be requested by PEL
user-options.\n"
                             overview))
             (pel--show-pkgs-for "Elpa" elpa-all elpa+lock elpa-bdeps
                                 pel-elpa-packages-to-keep)
             (pel--show-pkgs-for "Utils" utils-all utils+lock utils-bdeps
                                 pel-utils-packages-to-keep)
             (let ((elpa-in-excess (pel-elpa-unrequired))
                   (utils-in-excess (pel-utils-unrequired)))
               (if (or elpa-in-excess
                       utils-in-excess)
                   (progn
                     (insert "
\npel-cleanup would remove the following packages:\n")
                     (pel--show-pkgs-in-excess-for "Elpa" elpa-in-excess)
                     (pel--show-pkgs-in-excess-for "Utils" utils-in-excess))
                 (insert "\n\nNo package is in excess."))))))
      (message overview))))


(defun pel-inactive-user-options ()
  "Return a list of inactive PEL user-options symbols."
  (seq-filter (lambda (usr-opt)
                (not (symbol-value usr-opt)))
              (pel-user-options)))

;; ----

(defun pel-el-file-for (filepath)
  "Return the .el filepath of an .elc FILEPATH."
  (format "%s.el" (file-name-sans-extension filepath)))

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

;; --

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
    ;; Some packages are identified by the user as used, even though PEL may
    ;; not requests it via user-options; make sure to not identify these
    ;; packages as utils packages in excess.
    (dolist (util-el-file utils-el-files)
      (unless (member util-el-file active-utils-files)
        (unless (member util-el-file pel-utils-packages-to-keep)
          (push util-el-file excess-utils-files))))
    (list (reverse active-utils-files)
          (reverse excess-utils-files))))

(defun pel-utils-unrequired ()
  "Return the list of utils files not currently required."
  (cadr (pel-active-and-excess-utils)))

(defun pel-clean-utils (&optional dry-run)
  "Move all unrequired Emacs Lisp files from utils to utils-attic directory.
Byte-compile all Emacs Lisp files in the utils directory.
Remove orphaned elc files from the utils directory.
Don't execute when DRY-RUN is non-nil.
Return the a list of 2 lists:
- a list of .el files that have been or would have been removed,
- a list of the .elc orphaned files that were also removed."
  (let ((unrequired-files (pel-utils-unrequired))
        (removed-elc-files '()))
    (unless dry-run
      (when unrequired-files
        (unless (file-exists-p pel-utils-attic-dirpath)
          (make-directory pel-utils-attic-dirpath))
        (dolist (file unrequired-files)
          (let ((utils-filename (expand-file-name file pel-utils-dirpath))
                (attic-filename (expand-file-name file
                                                  pel-utils-attic-dirpath)))
            (if (file-exists-p attic-filename)
                (delete-file utils-filename)
              (rename-file utils-filename pel-utils-attic-dirpath))))
        ;; byte recompile all .el files newer than .elc or when the .elc is
        ;; missing.
        (byte-recompile-directory pel-utils-dirpath 0)
        ;; remove any orphaned .elc (a .elc without a .el file)
        (setq removed-elc-files (pel-remove-invalid-elc pel-utils-dirpath))))
    (list unrequired-files removed-elc-files)))

;; --

(defun pel-elpa-dirs-for (pkg &optional in-attic)
  "Return a list of all directories for specified package PKG.

PKG may be a symbol or a string.

By default, return package directory names available in the elpa
directory, but if the IN-ATTIC argument is non-nil, return
packages in the elpa-attic directory. Each directory is specified
with full path.

The returned list of directory paths is sorted in alphabetical
order.  For several versions of a given package the most recent
is placed last."
  (directory-files (if in-attic
                       pel-elpa-attic-dirpath
                     package-user-dir)
                   :full-path
                   (format "\\`%s-[0-9-.]+\\'"
                           (regexp-quote (pel-as-string pkg)))))

(defun pel-move-to-dir (file dir)
  "Move FILE to directory DIR.
- FILE may represent a file or a directory.  When it is a directory, the entire
  directory tree is moved.
- DIR must represent a directory.
  Trailing slash is however not required for DIR but allowed."
  ;; make sure DIR ends with slash otherwise rename-file will act as if we
  ;; wanted to rename FILE into DIR as opposed to move FILE into DIR.
  (rename-file (directory-file-name file)
               (file-name-as-directory dir)))

(defun pel-move-elpa-pkg-to-elpa-attic (pkg &optional dry-run)
  "Move all versions of PKG package from elpa to the elpa-attic directory.

If any elpa package directory already exists in the elpa-attic directory, then
the elpa package directory is only deleted.

If DRY-RUN is non-nil don't move, don't delete.
In all cases, return a list of directories that have or would have been
removed."
  (let ((removed-dirpaths '()))
    (dolist (dirpath (pel-elpa-dirs-for pkg))
      (push dirpath removed-dirpaths)
      (unless dry-run
        (let ((new-location (expand-file-name
                             (file-name-nondirectory dirpath)
                             pel-elpa-attic-dirpath)))
          (if (file-exists-p new-location)
              (delete-directory dirpath :recursively)
            (pel-move-to-dir dirpath pel-elpa-attic-dirpath)))))
    removed-dirpaths))

;; --

(defun pel-clean-package-selected-packages (pkgs)
  "Remove package symbols in PKGS from the `package-selected-package' form."
  (if (and (require 'package nil :noerror)
           (boundp 'package-selected-packages))
      (dolist (pkg pkgs)
        (setq package-selected-packages (delete pkg package-selected-packages)))
    (error "Can't modify package-selected-package!")))

(defun pel-clean-package-selected-packages-in-file (pkgs &optional filepath)
  "Remove packages PKGS from the filed package-selected-package form.

Remove each package identified in the PKGS list from the
`package-selected-package' form in the customization file.
PKGS may be a symbol or a list of symbols.
If FILEPATH is specified, modify the content of that file, otherwise
modify the file specified by the variable `custom-file'.
Save the modified file.
Return the number of symbols that were removed from the
`package-selected-package' form."
  (let ((edited-filepath (or filepath custom-file))
        (pkgs (if (listp pkgs) pkgs (list pkgs)))
        ;; (buffer-save-without-query t)
        (remove-count 0))
    (with-temp-file edited-filepath
      (insert-file-contents edited-filepath)
      ;; use superword-mode to ensure that movement commands jump over
      ;; punctuation symbols sometimes used in symbol names.
      (superword-mode 1)
      (when (search-forward "'(package-selected-packages")
        ;; narrow the region to the package-selected-package form
        (pel-backward-token-start)
        (left-char)
        (set-mark-command nil)
        (forward-sexp)
        (narrow-to-region (region-beginning) (region-end))
        ;; With the region narrowed, remove the specified package symbols
        ;; for the list
        (dolist (pkg pkgs)
          (let ((pkg-string (pel-as-string pkg)))
            (goto-char (point-min))
            ;; search for the package symbol. It may fail.
            (when (re-search-forward
                   (format "[ (]%s[ )]"
                           (regexp-quote pkg-string))
                   nil :noerror)
              (backward-word)
              (delete-char (length pkg-string))
              ;; if symbol was not the last in the list, delete the space
              ;; separator if there was one. 32 := SPACE character.
              (if (eq 32 (char-after))
                  (delete-char 1)
                ;; if it was at the end of the list delete the space separator
                ;; that was before the symbol.
                (left-char)
                (when (eq 32 (char-after))
                  (delete-char 1)))
              (setq remove-count (1+ remove-count)))))
        (widen)
        remove-count))))

(defun pel-elpa-packages-in-dir ()
  "Return a list of symbol for all packages present in local Elpa directory.

The function search the directory identified by the variable
`pel-elpa-dirpath'.

The directory holds sub-directories, one per package/version.
The directory may hold several versions of a specific Elpa package.
The returned list contains only one symbol identifying the package for each
version of that package.
The list of package symbols is sorted by symbol names."
  (let ((elpa-pkg-dir-names  (directory-files
                              pel-elpa-dirpath nil ".+[0-9-.]+\\'"))
        (elpa-pkg-names '()))
    (dolist (dir-name elpa-pkg-dir-names)
      (when (eq 0 (string-match "\\`\\([^ ]+\\)-[0-9-.]+\\'" dir-name))
        (let ((pkg-name  (intern (match-string 1 dir-name))))
          (unless (memq pkg-name elpa-pkg-names)
            (push pkg-name elpa-pkg-names)))))
    (reverse elpa-pkg-names)))

(defun pel-elpa-unrequired ()
  "Return a list of the elpa packages that are not required by PEL.
Packages not required are packages not requested by any PEL user-option or any
of their dependencies.
The returned list contains symbols, each symbol is the name (without any
version numbering) of the elpa package.  The list is sorted."
  (let ((activated-elpa (car (pel-activated-packages)))
        (available-elpa (pel-elpa-packages-in-dir))
        (excess-elpa    '()))
    ;; Some packages are identified by the user as used, even though PEL may
    ;; not requests it via user-options; make sure to not identify these
    ;; packages as elpa packages in excess.
    (dolist (pkg pel-elpa-packages-to-keep)
      (unless (memq pkg activated-elpa)
        (push pkg activated-elpa)))
    ;; Some other packages are always used by PEL.  These should not be
    ;; removed.
    (dolist (pkg pel-required-packages)
      (unless (memq pkg activated-elpa)
        (push pkg activated-elpa)))
    ;; Now identify the packages present inside the elpa directory that are
    ;; not required.
    (dolist (elpa-pkg available-elpa)
      (unless (memq elpa-pkg activated-elpa)
        (push elpa-pkg excess-elpa)))
    (sort excess-elpa (function pel-symbol-name-<))))

;; --

(defun pel-clean-elpa (&optional dry-run)
  "Remove Elpa packages not requested by PEL user-options.
Perform the following:
- Move all unrequired Emacs Lisp packages from elpa to elpa-attic directory.
- Remove their symbol from the `package-selected-packages' variable.
- Remove their symbol from the customization `package-selected-packages' list
  located inside the currently used customization file (identified by the
  content of the variable `custom-file'.
Don't execute when DRY-RUN is non-nil.
Return a list of elpa directories moved or deleted."
  (let ((unrequired-elpa (pel-elpa-unrequired))
        (moved-elpa-dirs '()))
    (when unrequired-elpa
      ;; If any Elpa package is in excess, move it in the elpa-attic
      ;; unless this is a dry-run (in which case just accumulate the directory
      ;; names in the moved-elpa-dirs list).  If the directory is already
      ;; inside the elpa-attic then delete it.
      (unless (file-exists-p pel-elpa-attic-dirpath)
        (make-directory pel-elpa-attic-dirpath))
      (dolist (pkg unrequired-elpa)
        (setq moved-elpa-dirs
              (append moved-elpa-dirs
                      (pel-move-elpa-pkg-to-elpa-attic pkg dry-run))))
      ;; Also remove the packages from the package-selected-package form
      ;; in memory and the one stored in the currently active customization
      ;; file.
      (unless dry-run
        (pel-clean-package-selected-packages unrequired-elpa)
        (pel-clean-package-selected-packages-in-file unrequired-elpa)))
    ;; We could also update load-path and remove the paths related to the
    ;; package directories removed, but let's play safe and wait for Emacs
    ;; to restart: the load-path will be updated then.
    moved-elpa-dirs))

(defun pel-cleanup (&optional dry-run)
  "Move all unrequired packages to their attic directory.

With optional argument DRY-RUN, do nothing just report what would
be done.  Print a description of the operation in the
*pel-cleanup* buffer.
This command is *not* available when PEL operates in fast startup."
  (interactive "P")
  (if (pel-in-fast-startup-p)
      (user-error "pel-cleanup is not available in fast startup operation!
Use pel-setup-normal to return to normal operation.")
    (when (or dry-run
              (y-or-n-p "Proceed with removal of non-required packages? "))
      (let* ((utils-results     (pel-clean-utils dry-run))
             (removed-el-files  (car utils-results))
             (removed-elc-files (cadr utils-results))
             (moved-elpa-dirs   (pel-clean-elpa dry-run)))
        (pel-print-in-buffer
         "*pel-cleanup*"
         (if dry-run "Dry-run of PEL Cleanup"
           "PEL Cleanup")
         (lambda ()
           (let ((n 0)
                 verb-moved
                 verb-Moved
                 verb-Removed)
             (if dry-run
                 (setq verb-moved "that would have been moved"
                       verb-Moved "Would move"
                       verb-Removed "Would remove")
               (setq verb-moved "moved"
                     verb-Moved "Moved"
                     verb-Removed "Removed"))
             (insert (format "
The PEL cleanup removes packages that are not needed, based on
the value of the `pel-use-' customization user-options.

PEL does not remove packages that are dependencies of packages
that are activated by the user-options or packages manually
installed that have been identified in the following user-options:

- `pel-elpa-packages-to-keep',
- `pel-utils-packages-to-keep'.

******************
**IMPORTANT NOTE**
******************

- If you want to install packages that are not managed by PEL,
  please add their names to the lists mentioned above, otherwise
  a `pel-cleanup' will remove them.
- Also note that if a package is already present inside the attic
 directory the file in the utils or elpa directory is removed.



PEL CLEANUP %s:
**********************

" (if dry-run "DRY - RUN"
    "EXECUTION")))
             (when dry-run
               (insert "This is a dry-run ONLY.  NOTHING was done!

The remainder of the message shows what would have been done if
you elected to perform a real cleanup by issuing the
`pel-cleanup' command without the key prefix and confirming your
intention by typing 'y' to its prompt.

"))
             (when (or removed-el-files removed-elc-files)
               (insert (format "%s %d files,
from: %s
to  : %s
%sThe files %s to utils-attic are:\n\n"
                               verb-Moved
                               (length removed-el-files)
                               pel-utils-dirpath
                               pel-utils-attic-dirpath
                               (if removed-elc-files
                                   (format "%s %d orphaned .elc files.\n"
                                           verb-Removed
                                           (length removed-elc-files))
                                 "")
                               verb-moved))
               (dolist (fn removed-el-files)
                 (setq n (1+ n ))
                 (insert (format "- %3d: %s\n" n fn))))
             (when moved-elpa-dirs
               (insert (format "\n\nElpa packages %s,
from: %s
to  : %s :\n\n"
                               verb-moved
                               pel-elpa-dirpath
                               pel-elpa-attic-dirpath))
               (setq n 0)
               (dolist (pkgdir moved-elpa-dirs)
                 (setq n (1+ n))
                 (insert (format "- %3d: %s\n" n pkgdir))))
             (unless (or removed-el-files
                         removed-elc-files
                         moved-elpa-dirs)
               (insert "Nothing to cleanup!!")))))))))

;; --

;;-pel-autoload
(defun pel-install-from-elpa-attic (pkg)
  "Install package PKG from the local copy stored in the elpa-attic directory.

Return t on success, nil otherwise.
The elpa-attic directory is the ~/.emacs.d/pel-elpa-attic directory."
  ;; Get the name of the most recent package pkg stored in the elpa-attic if
  ;; any.  The most recent has a directory name that sorts last as we use
  ;; MELPA packages which uses ISO-8601 format.
  (let ((elpa-attic-pkg-dirpath (car-safe
                                 (last (pel-elpa-dirs-for pkg :in-attic))))
        (installation-succeeded nil))
    (when elpa-attic-pkg-dirpath
      (let ((dest-dirpath
             (expand-file-name (file-name-nondirectory
                                elpa-attic-pkg-dirpath)
                               pel-elpa-dirpath)))
        (unless (file-exists-p dest-dirpath)
          (copy-directory elpa-attic-pkg-dirpath
                          (file-name-as-directory pel-elpa-dirpath)
                          :keep-time))
        (push dest-dirpath load-path)
        (load-library (pel-as-string pkg))
        (setq installation-succeeded t)))
    installation-succeeded))

;;; --------------------------------------------------------------------------
(provide 'pel-package)

;;; pel-package.el ends here
