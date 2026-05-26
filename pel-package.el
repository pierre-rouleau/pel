;;; pel-package.el --- PEL package management.  -*- lexical-binding: t; -*-

;; Created   : Monday, March 22 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-05-25 23:17:21 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026  Pierre Rouleau
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
;;  an *attic* directory).  For elpa package the package name is removed from
;;  the `package-selected-packages' variable and the active customization file
;;  is updated.
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
;;    (graphic/TTY).  Produce a more detailed report in a *pel-package-info*
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
;;
;;   - `pel-elpa-dirpath'
;;     - `pel-locate-elpa'
;;
;;   - `pel-elpa-attic-dirpath'
;;   - `pel-utils-dirpath'
;;   - `pel-utils-attic-dirpath'

;; The function call trees are shown here:
;;
;; * `pel-package-info-all'
;;    - `pel-load-all'
;;    * `pel-package-info'
;;      - `pel-activated-packages'
;;      - `pel-user-options'
;;        - `pel-user-option-p'
;;      - `pel--show-pkgs-for'
;;      - `pel-elpa-unrequired'            (see its call tree below)
;;      - `pel-utils-unrequired'           (see its call tree below)
;;      - `pel--show-pkgs-in-excess-for'
;;      - `pel-package-upgradable'
;;
;;
;; * `pel-cleanup'
;;   - `pel-clean-renamed-packages'
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
(require 'pel--base)            ; use: `pel-as-string', `pel-as-symbol'
;;                              ;      `pel-print-in-buffer'
;;                              ;      `pel-in-fast-startup-p'
;;                              ;      `pel-emacs-config-features-string'
;;                              ;      `pel-hardware-model-string'
;;                              ;      `pel-insert-list-content'
(require 'pel--options)         ; use: `pel-elpa-packages-to-keep'
;;                              ;      `pel-utils-packages-to-keep'
;;                              ;      `pel-elpa-obsolete-packages'
(require 'pel-navigate)         ; use: `pel-backward-token-start'
(require 'pel-elpa)             ; use: `pel-elpa-package-directories'
;;                              ;      `pel-el-files-in'
;;                              ;      `pel-elpa-pkg-version-regexp'
;;                              ;      `pel-elpa-pkg-dirname-regexp'
;;                              ;      `pel-elpa-package-name-for'
(require 'cl-lib)               ; use: `cl-remove-if'
(require 'seq)                  ; use: `seq-filter'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-locate-elpa ()
  "Return the absolute path of the local Elpa directory (or symlink).

PEL early-init.el and init.el file update the dynamic value of the Emacs
`package-user-dir' user-option to ensure that the value used by
package.el:

- corresponds to what is required when PEL operates in dual tty/graphics
  setup environment for Emacs running in terminal or graphics mode,
- is a true directory name, not the symlink that may point to it.

This is done to ensure that the entries inside the `load-path' remain
valid even if an external Emacs/PEL process switches the startup mode
and changes the target of the elpa symlink.

PEL init.el code stores the original value of `package-user-dir', which
corresponds to the user-option value stored in the custom file, into
`pel-package-user-dir-original'.

PEL will be able to use this value to transform the original elpa
directory into a symlink that points to a directory named
\"elpa-complete\" or \"elpa-complete-graphics\" when dual tty/graphics
mode is used and Emacs is a GUI process.

It's possible that PEL is used with a init.el file that has not yet been
populated with the proper code and `pel-package-user-dir-original'
variable does not exist. In that case the function report a warning and
tries to locate the elpa directory the standard way using the standard
`package-user-dir' or by building its path via the
`user-emacs-directory' value.

If you get the warning you should update your init.el file (and possibly
the early-init.el) by using what PEL provides in the example/init
directory."
  (file-name-as-directory
   (if (bound-and-true-p pel-package-user-dir-original)
       (expand-file-name pel-package-user-dir-original)
     ;;
     ;; `pel-package-user-dir-original' does not exist!
     ;; You are not using a PEL compliant init.el file!
     ;; But warn only at load or run time, not at compile time
     ;; and not when running ERT based tests.
     (unless (or (bound-and-true-p byte-compile-current-file)
                 (bound-and-true-p comp-native-compiling)
                 (bound-and-true-p ert--test-execution-info)
                 (getenv "EMACS_TEST_VERBOSE"))
       (let ((errmsg
              (format "⚠️  Emacs init.el does not comply with PEL requirements!
    Please update your init.el%s! Use PEL example/init files as templates."
                      (if pel-emacs-27-or-later-p
                          " and your early-init.el"
                        ""))))
         (if noninteractive
             ;; If issued from a command line Emacs invocation stop with an
             ;; error
             (error errmsg)
           ;; otherwise, during run time, just display warning error
           (display-warning 'pel-locate-elpa errmsg :error))))
     ;; Return something that is still valid for Emacs despite the problem.
     (if (and (require 'package nil :noerror)
              (boundp 'package-user-dir))
         (expand-file-name package-user-dir)
       (expand-file-name "elpa" user-emacs-directory)))))


(defconst pel--elpa-dirpath-original  (pel-locate-elpa)
  "Absolute path of the user elpa directory or symlink used when Emacs starts.

This may differ from the value of `package-user-dir' when a symlink is
used as PEL init files ensure that `package-user-dir' is set to the
target of the elpa symlink while `pel--elpa-dirpath-original' is always
set to the path of the elpa directory or symlink if it exists.  Note
that you can have several elpa directories if you set `package-user-dir'
inside your init.el file.

Evaluate (`pel-elpa-dirpath' \\='switch-dir-at-startup) to access this value.")

(defun pel-elpa-dirpath (type)
  "Return absolute path of user elpa directory or symlink as by TYPE.

The returned string always ends with a / indicating a directory and
depends on the TYPE argument, which can be one of the following symbols:

- \\='switch-dir             The elpa (or elpa-graphics) directory which may
                          also be a symlink (and normally is once PEL has
                          setup the ability to activate the fast startup
                          mode).

- \\='switch-dir-at-startup  The elpa (or elpa-graphics) directory which may
                          also be a symlink (and normally is once PEL has
                          setup the ability to activate the fast startup
                          mode) seen when Emacs started.

- \\='final-dir-at-startup   The final directory elpa-complete, elpa-reduced,
                          elpa-complete-graphics or elpa-reduced-graphics
                          directory used by this Emacs session when it
                          started.

- \\='final-dir-now          The final directory elpa-complete, elpa-reduced,
                          elpa-complete-graphics or elpa-reduced-graphics
                          directory as currently identified in the file
                          system by the elpa or elpa-graphics symlink.
                          When a symlink is used, the returned value is
                          fully resolved and identifies the final directory
                          target.

To understand the purpose of this function you need to understand the
way PEL organizes the `user-emacs-directory' (normally ~/.emacs.d) with
respect to the way it stores the Elpa-compliant packages in order to
support two different features:

- Feature 1: PEL fast startup mode
- Feature 2: PEL dual-mode: dual tty/graphics customization mode

The Elpa compliant packages are normally stored inside a directory named
\"elpa\".

When PEL fast startup mode is activated, PEL renames the \"elpa\"
directory to \"elpa-complete\" and creates a symbolic link named
\"elpa\" that points to \"elpa-complete\".  After, PEL creates a
\"elpa-reduced\" directory to store all multi-directory packages used in
\"elpa-complete\" and then creates a \"pel-bundle\" package that holds
the Emacs Lisp files of all single-directory packages in the current
content of the \"elpa-complete\" directory.  This reduces dramatically
the number of package directories located inside the \"elpa-reduced\"
compared to the number stored in \"elpa-complete\" directory and
significantly speeds up Emacs startup time.

When PEL dual-mode is activated, PEL supports two different Emacs
customization files and two sets of Elpa directories: one set used by
Emacs running in terminal mode and another set for Emacs running in
graphics mode. This way packages used for Emacs in graphics mode are not
stored in the Elpa directories used by Emacs running in terminal mode
and vice versa.  This further improves Emacs startup speed.

PEL early-init.el and init.el provide logic to setup important variables
in PEL and Emacs package management: `package-user-dir' and
`pel-package-user-dir-original'.

It's also important to understand that PEL design allows the use of
multiple independent Emacs processes.  One Emacs process might be
started while PEL is setup for the normal operation mode.  Later another
Emacs session may activate the PEL fast-startup mode.  When doing that
the PEL setup code changes the target of the \"~/.emacs.d/elpa symlink\"
to point to \"~/.emacs.d/elpa-reduced\" directory after updating the
content of the \"elpa-reduced\" directory.  This will affect the
behaviour of all Emacs processes started from that moment but does *not*
affect the Emacs sessions already running because these Emacs sessions
use the \"~/.emacs.d/elpa-complete\" directory that was not modified.

It becomes important for the PEL logic to be able to identify the
real (final) directory in some situations, and identify the location of
the symlink (switch) in other situations.  And this must be easy to
identify the one that the current Emacs session started with and the
final directory currently identified by the symlink that can differ from
the one the current Emacs session is using because the PEL logic changed
the target of the symlink to change the mode from normal to fast or vice
versa.

Here's a representation of the symlink and directories:

  switch-dir          final-dir
   |                      |
   |                      |
   v                      v
                                            \\
                 +-- elpa-complete          |
                 |                          |
                 |                          |
  elpa --------->+                          |  For Emacs:
                 |                          |  - in terminal mode
                 |                          |  - in graphics mode when
                 +-- elpa-reduced           |    when dual-mode is not
                                            /    used.

                                            \\
                 +-- elpa-complete-graphics |
                 |                          |
                 |                          |
  elpa-graphics->+                          |  For Emacs in graphics
                 |                          |  mode when dual-mode is
                 |                          |  used.
                 +-- elpa-reduced-graphics  |
                                            /
"
  (file-name-as-directory
   (cond
    ;;
    ((eq type 'final-dir-at-startup)
     ;; although PEL init ensure package-user-dir ends with a "/"
     ;; don't take any chances: ensure it does.
     package-user-dir)
    ;;
    ((memq type '(final-dir-now switch-dir))
     (let ((switch-dir (pel-locate-elpa)))
       (if (eq type 'switch-dir)
           switch-dir
         ;; final-dir-now requested
         (if (file-symlink-p (directory-file-name switch-dir))
             (file-truename switch-dir)
           switch-dir))))
    ;;
    ((eq type 'switch-dir-at-startup) pel--elpa-dirpath-original)
    (t (error "Invalid pel-elpa-dirpath argument: %S" type)))))

(defun pel-elpa-attic-dirpath ()
  "Return the absolute path of the user elpa-attic directory.
PEL supports a pel-attic directory for dual independent
customization when it is requested as specified by the presence
of `pel-init-support-dual-environment-p' symbol set to t."
  (file-name-as-directory
   (pel-elpa-name
    (expand-file-name "elpa-attic" user-emacs-directory)
    (and (bound-and-true-p pel-init-support-dual-environment-p)
         (display-graphic-p)))))

(defun pel-utils-dirpath ()
  "Absolute path of the PEL utils directory.
PEL supports a utils directory for dual independent customization
when it is requested as specified by the presence of
`pel-init-support-dual-environment-p' symbol set to t."
  (file-name-as-directory
   (pel-elpa-name
    (expand-file-name pel-utils-dirname user-emacs-directory)
    (and (bound-and-true-p pel-init-support-dual-environment-p)
         (display-graphic-p)))))

(defun pel-utils-attic-dirpath ()
  "Absolute path of the PEL utils-attic directory."
  (file-name-as-directory
   (pel-elpa-name
    (expand-file-name (concat pel-utils-dirname "-attic")
                      user-emacs-directory)
    (and (bound-and-true-p pel-init-support-dual-environment-p)
         (display-graphic-p)))))

(defconst pel-required-packages '(popup)
  "List of package names that PEL always uses.")

(defun pel-user-options ()
  "Return a list of all pel-use- user-option symbols."
  (let ((symbols ()))
    (mapatoms
     (lambda (symbol)
       (when (pel-user-option-p symbol)
         (push symbol symbols))))
    (nreverse symbols)))

(defun pel-commands ()
  "Return a list of PEL command symbols."
  (let ((symbols ())
        (cmd-name nil))
    (mapatoms
     (lambda (symbol)
       (when (and (commandp symbol)
                  (progn
                    (setq cmd-name (symbol-name symbol))
                    (and (string-prefix-p "pel-" cmd-name)
                         (not (string-prefix-p "pel-∑" cmd-name))
                         (not (string-prefix-p "pel-⅀" cmd-name)))))
         (push symbol symbols))))
    (nreverse symbols)))

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

(defconst pel--regxp-pel-ensure
  "^ *(pel-ensure-package-elpa +\\(\\(\\s_\\|\\sw\\)+\\)"
  "Regexp to find & extract package name installed by `pel-ensure-package-elpa'.")

(defconst pel--regxp-from-github
  "^ *(pel-install-github-files* +\\(?:\\\"\\([-[:alnum:]\\./]+\\)\\\"\\)"
  "Regexp to find & extra dir names of github extracted packages.
Group 1 is the path that holds the package name.")

(defconst pel--regxp-from-gitlab
  "^ *(pel-install-gitlab-files* +\\(?:\\\"\\([-[:alnum:]\\./]+\\)\\\"\\) +\\(?:\\\"\\([-[:alnum:]\\./]+\\)\\\"\\)"
  "Regexp to find & extra dir names of gitlab extracted packages.
Group 2 is package name.")

(defconst pel--regexp-pel-install-file
  "(pel-install-file[\n \\t]+\\\"[-[:alnum:]~:/._]+\\\"[\n \\t]+\\\"\\(.+\\)\\(?:\\.el\\)?\\\""
  "Regexp to extract package name from a `pel-install-file' form.
Name of package is in group 1.")

(defconst pel--regexp-quelpa-pkg-name
  "(pel-quelpa-install\\s-+(\\([-[:alnum:]]+\\)"
  "Regexp to extract package name from a `pel-quelpa-install' form.
Matches forms like:
  (pel-quelpa-install (pkgname :fetcher ...))
or multi-line forms where `pkgname' appears on the next line after indentation.
The `\\s-+' matches any whitespace, including newlines and leading spaces.
Group 1 captures the package symbol name.")

(defun pel--pkg-installed-by-pel (regexp &optional group extracter)
  "Return list of package names PEL can install with specified mechanism.
The installation mechanism is specified by the REGEXP expression
and the optional GROUP number and EXTRACTER function."
  (or group (setq group 1))
  (let ((pkg-names nil)
        found
        found-text)
    (with-temp-buffer
      (insert-file-contents (locate-library "pel_keys.el"))
      (goto-char (point-min))
      (while
          (progn
            (setq found
                  (re-search-forward regexp nil :noerror))
            (when found
              (setq found-text (match-string group))
              (when extracter
                (setq found-text (λc extracter found-text)))
              (if found-text
                  (unless (member found-text pkg-names)
                    (push found-text pkg-names))
                (message "FAILED extraction of: %s" (match-string group) ))
              found))))
    (nreverse pkg-names)))

(defconst pel--regexp-github-pkg-path
  "[-[:alnum:]\\.]+/\\([-[:alnum:]\\.]+\\)/\\(?:master\\|upstream\\)"
  "Regexp to extract package name from a Github master path.")

(defun pel--extract-github-pkg-name (github-pkg-path)
  "Extract package name from a Github package path used in pel_keys."
  (with-temp-buffer
    (insert github-pkg-path)
    (goto-char (point-min))
    (when (re-search-forward pel--regexp-github-pkg-path nil :noerror)
      (match-string 1))))

(defun pel-installable-packages ()
  "Return a list of 3 lists of packages PEL can install.
The returned list has the form (ELPA QUELPA OTHERS), where:
- ELPA   : packages installable from Elpa-compliant repositories,
           identified by `pel-ensure-package-elpa' forms.
- QUELPA : packages installable via quelpa from VCS sources,
           identified by `pel-quelpa-install' forms.
- OTHERS : the quelpa tool itself and packages installed from
           GitHub, GitLab, or local files.
Note: a package appearing under both ELPA and QUELPA means PEL
uses a conditional branch to choose between sources (e.g. lispy)."
  (let ((pkgs-from-elpa
         (sort (pel--pkg-installed-by-pel pel--regxp-pel-ensure 1)
               (function string<)))
        (pkgs-from-quelpa
         (sort (pel--pkg-installed-by-pel pel--regexp-quelpa-pkg-name 1)
               (function string<)))
        (pkgs-others
         (sort
          (append
           '("quelpa")                  ; the quelpa tool itself
           (pel--pkg-installed-by-pel pel--regxp-from-github 1
                                      (function pel--extract-github-pkg-name))
           (pel--pkg-installed-by-pel pel--regxp-from-gitlab 2)
           (pel--pkg-installed-by-pel pel--regexp-pel-install-file 1))
          (function string<))))
    (list pkgs-from-elpa pkgs-from-quelpa pkgs-others)))        ; 3 lists

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
- `type'         := \\='elpa or \\='utils
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
  "Return eval'ed form associated with :also-required-when property of SYMBOL.

The SYMBOL is expected to be a pel-use SYMBOL.
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
The type is either \\='elpa or \\='utils.

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
                  symbol :package-is :no-property-is-elpa))
          (xtra-specs nil))
      ;; Some `pel-use-' user-options are for packages that do not completely
      ;; identify their dependencies in their pkg-X.el file.  We complete it
      ;; with the :requires-package property.  Extract these specs and append
      ;; them to the specs, but don't allow multiple instance.
      ;;
      ;; TODO: currently packages inserted like these and not explicitly
      ;; requested by a `pel-use-' user-option are not identified as
      ;; dependencies in the report.  There should be some indication about it
      ;; being requested via a dependency identified by PEL.
      (dolist (xtra-spec (pel-spec-for-symbol-attribute
                          symbol :requires-package))
        (unless (member xtra-spec specs)
          (push xtra-spec xtra-specs)))
      (setq specs (nconc specs (nreverse xtra-specs)))
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
        (pkg-spec-list ())
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
    (nreverse pkg-spec-list)))

;; ----

(defun pel-symbol-name-< (s1 s2)
  "Compare name strings of S1 and S2 symbols."
  (string< (symbol-name s1) (symbol-name s2)))

(defun pel--pkg-deps-via-package-alist (pkg)
  "Return a flat list of transitive dependency symbols for PKG.
Walks `package-alist' using the stable public `package-desc-reqs' API.
This is a version-independent fallback used when neither
`package--get-deps' nor `package--dependencies' is available.
PKG is a symbol; the returned list does NOT include PKG itself."
  ;; Breadth-first traversal of package-alist.
  (let ((seen '())
        (queue (list pkg)))
    (while queue
      (let ((current (pop queue)))
        (unless (memq current seen)
          (let ((pkg-desc (cadr (assq current package-alist))))
            (when pkg-desc
              (push current seen)
              (dolist (req (package-desc-reqs pkg-desc))
                (unless (memq (car req) seen)
                  (push (car req) queue))))))))
    ;; Exclude PKG itself: we report only its dependencies.
    (delq pkg seen)))

(defun pel-elpa-pkg-dependencies (pkg)
  "Return a list of package symbols that are elpa dependencies of package PKG.

PKG may be a symbol or a string.

Implementation strategy by Emacs version:
- Emacs ≤ 26 : `package--get-deps' accepts a single symbol argument.
- Emacs 27-29: `package--get-deps' accepts a list argument (signature
  changed in the October 2019 Emacs commit).  The post-2019 version also
  includes PKG itself in its result; it is stripped here.
- Emacs 30+  : `package--get-deps' was removed.  PEL uses
  `pel--pkg-deps-via-package-alist' which performs the identical BFS
  traversal with only the stable public APIs `package-alist' and
  `package-desc-reqs', avoiding reliance on any changed internal
  package.el function."
  ;; Make sure that pkg is present, if it is not, its dependants
  ;; are not installed via that package.
  (when (locate-library (pel-as-string pkg))
    (setq pkg (pel-as-symbol pkg))
    (if (require 'package nil :noerror)
        (cond
         ;; ------------------------------------------------------------------
         ;; Emacs ≤ 29: `package--get-deps' is available.
         ;; The argument changed from a symbol to a list in October 2019
         ;; (Emacs 27 era).  The post-2019 form also returns PKG itself;
         ;; strip it for a consistent return contract.
         ;;
         ((fboundp 'package--get-deps)
          (condition-case err
              (let* ((pkg-arg      (if pel-emacs-27-or-later-p
                                       (list pkg)
                                     pkg))
                     (dependencies (condition-case inner-err
                                       (package--get-deps pkg-arg)
                                     (error
                                      (display-warning
                                       'emacs-pkg-dependencies
                                       (format "\
Warning: %s dependencies are not identified properly: %s
Please report the issue to the package developer
if this is a recent version of the package."
                                               pkg-arg inner-err))
                                      nil))))
                ;; package--get-deps (post Oct 2019) includes PKG itself;
                ;; use delq (eq-based) since pkg is a symbol.
                (delq pkg dependencies))
            (wrong-type-argument
             (unless (memq pkg pel-elpa-obsolete-packages)
               (display-warning
                'pel-elpa-pkg-dependencies
                (format "\
Error extracting dependencies for %s: %s
Is it obsolete? If so it should be added to `pel-elpa-obsolete-packages'."
                        pkg err)
                :error))
             nil)))
         ;; ------------------------------------------------------------------
         ;; Emacs 30+: package--get-deps was removed.
         ;; Use the stable-public-API BFS walker.
         ;;
         (t
          (pel--pkg-deps-via-package-alist pkg)))
      (error "Failed loading package"))))

(defun pel-activated-packages (&optional without-dependants ignore-restriction)
  "Return a list of packages activated by PEL user-options.

The lists include all dependant packages unless WITHOUT-DEPENDANTS is
specified and non-nil.
The list includes all packages that cannot be removed because of imposed
restriction unless IGNORE-RESTRICTION is non-nil.

Return a list of 3 lists:
- 1: list is a list of elpa package symbols,
- 2: list is a list of utils file name symbols.
- 3: list of error messages, if any

The elements of each list are sorted by alphabetical order of
their names."
  (let ((elpa-list ())
        (utils-list ())
        (error-list nil))
    (condition-case err
        (dolist (user-option (pel-user-options))
          ;; (message ".. Testing: %s" (symbol-name user-option))
          (condition-case err
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
                 (t (push (format "Invalid spec for %s: %S" user-option spec)
                          error-list))))
            (error
             (push (format "*** PEL Specification Error: %S for %s: %S *****"
                           err
                           (symbol-name user-option)
                           user-option)
                   error-list))))
      (error
       (push (format "*** PEL Specification Error: %S" err)
             error-list)))
    (list
     (sort elpa-list (function pel-symbol-name-<))
     (sort utils-list (function pel-symbol-name-<))
     error-list)))

;; --

(defun pel--insert-pkgs-list (title pkgs)
  "Insert a numbered list of packages in current buffer.

PKGS is the list of packages to print.
TITLE is the text that must be printed before the list.
Return the length of PKGS."
  (let ((n 0))
    (insert title)
    (dolist (pkg pkgs)
      (pel+= n 1)
      (insert (format "- %3d: %s\n" n pkg)))
    n))

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
        (pel+= n 1)
        (insert (format "- %3d: %-40s%s%s\n"
                        n
                        pkg
                        (if isa-dep "  (dependency) "
                          "               ")
                        (if isa-lck "  (requested by restriction)"
                          "")))))
    (when to-keep
      (pel--insert-pkgs-list
       (format "%s manually installed:\n" group-name)
       to-keep))))

(defun pel--show-pkgs-in-excess-for (group pkgs)
  "Utility: list GROUP package PKGS in excess.
Return the number of packages in excess."
  (if pkgs
      (pel--insert-pkgs-list (format "\n%s packages in excess:\n" group) pkgs)
    0))

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


(defvar pel-home-dirpath-name)   ; prevent compiler warning: defined in init.el

(defun pel-doc-pdf-file-count ()
  "Return the number of PEL documentation PDF files."
  (length (directory-files-recursively pel-home-dirpath-name "\\.pdf$")))

;; Identify packages that can be upgraded
;; --------------------------------------

(defun pel-package-upgradable ()
  "Return a list of upgradable elpa-compliant packages."
  (require 'package)
  ;; create a temporary special buffer to use package-menu-mode
  (let ((upgradables nil)
        (buf  (get-buffer-create "*pel-package*")))
    (with-current-buffer buf
      ;; Since some packages have their descriptions include non-ASCII
      ;; characters...
      (setq buffer-file-coding-system 'utf-8)
      (package-menu-mode)
      (when (fboundp 'package--ensure-package-menu-mode)
        (package--ensure-package-menu-mode))
      ;; Fetch the remote list of packages.
      ;; (package-menu--refresh-contents)
      (package-refresh-contents)
      (package-menu--generate nil t)
      (setq upgradables (package-menu--find-upgrades)))
    ;; return a list of all keys; the names of packages that could be upgraded.
    (mapcar 'car upgradables)))

;;-pel-autoload
(defun pel-package-info (&optional full-report on-stdout)
  "Display information about packages required by PEL.

Print the information in *pel-package-info* buffer, unless
ON-STDOUT is non-nil, in which case it prints it in the echo
area.  By default prints a short report with the main
information, but if FULL-REPORT is non-nil (interactively with
any prefix argument), then it prints a longer report listing all
packages, indicating whether the package is in elpa or utils and
whether it is a dependency or included because of a restriction
lock.
The function does not support printing a full report on stdout."
  (interactive "P")
  (message "pel-package-info: Gathering information...")
  (let* ((all-activated   (unless (pel-in-fast-startup-p)
                            (pel-activated-packages))) ; all (with dependencies & locks)
         (activated+lock  (unless (pel-in-fast-startup-p)
                            (pel-activated-packages :without-deps)))
         (activated-bdeps (unless (pel-in-fast-startup-p)
                            (pel-activated-packages nil :without-locks)))
         (elpa-all          (car all-activated))
         (errors            (nth 2 all-activated))
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
         (installable-pkgs  (pel-installable-packages))
         (n-installable-elpa   (length (nth 0 installable-pkgs)))
         (n-installable-quelpa (length (nth 1 installable-pkgs)))
         (n-installable-others (length (nth 2 installable-pkgs)))
         (upgradable-pkgs   (pel-package-upgradable))
         (overview
          (format "\
- custom-file                : %s
- package-user-dir           : %s
- %3d Elpa packages stored in: %s
- %3d Utils files   stored in: %s
- size of load-path          : %d directories
- # pel-use-... user-options : %3d (%d are active)
- # packages PEL can install : %d Elpa-compliant, %d quelpa,  %d others
- PEL activated elpa packages: %s
- PEL Activated utils files  : %s
- # loaded files             : %d
- # features                 : %d
- # package-alist            : %d
- # packages activated       : %d%s
- # packages selected        : %d (explicitly selected; no deps, no built-ins)
- # PEL loaded commands      : %d
- # upgradable elpa packages : %d
- # PEL PDF files            : %d
- Emacs init-time            : %s
- Emacs version              : %s
- Emacs config features      : %s
- Hardware model, OS info    : %s"
                  custom-file           ; customization  file
                  package-user-dir      ; package directory
                  (length               ; # elpa packages
                   (pel-elpa-package-directories
                    package-user-dir))
                  package-user-dir      ; package directory
                  (length               ; # packages in PEL utils
                   (pel-el-files-in (pel-utils-dirpath)))
                  (pel-utils-dirpath)   ; PEL utils directory}
                  (length load-path)    ; load-path size
                  (length user-options) ; total # user-options
                  (length (seq-filter   ; # of non-nil user-options
                           (lambda (x) (symbol-value x))
                           user-options))
                  ;; # package PEL can install
                  n-installable-elpa
                  n-installable-quelpa
                  n-installable-others
                  (pel--elpa-stats n-elpa-base ; PEL activated elpa packages
                                   n-elpa-deps
                                   n-elpa-locked)
                  (pel--elpa-stats n-utils-base ; PEL activated utils files
                                   n-utils-deps
                                   n-utils-locked)
                  (length load-history)              ; # loaded files
                  (length features)                  ; # features
                  (length package-alist)             ; # package-alist
                  (length package-activated-list)    ; # packages activated
                  (if (pel-in-fast-startup-p) ""
                    " <<= # of packages used by Emacs")
                  (length package-selected-packages) ; # packages selected
                  (length (pel-commands))            ; # PEL commands
                  (length upgradable-pkgs) ; # upgradable PEL packages
                  (pel-doc-pdf-file-count) ; # PEL PDF files
                  (if (and (require 'time nil :noerror)
                           (fboundp 'emacs-init-time))
                      (emacs-init-time)
                    "?")
                  (emacs-version)
                  (pel-emacs-config-features-string)
                  (pel-hardware-model-string))))
    (when errors
      (setq overview
            (concat
             "****** ERRORS:\n"
             (mapconcat #'identity errors "\n")
             "\n***************\n")))
    (if on-stdout
        (message overview)
      (pel-print-in-buffer
       "*pel-package-info*"
       "PEL package & user-options activated packages"
       (lambda ()
         "Print report."
         (insert overview)
         (when (pel-in-fast-startup-p)
           (insert "
In PEL fast startup mode, the Elisp files of single directory packages
are stored inside the pel-bundle package.  This reduces the number of
packages seen by Emacs and speeds up Emacs startup.

Since several packages have been merged into the pel-bundle package,
it becomes difficult to know the exact number of Emacs packages used
in this system.  If you want to see the exact number of Emacs Lisp
packages used in this system, execute this command when PEL is operating
in normal mode either with the `make stats` command or inside Emacs.\n\n"))
         (unless full-report
           (insert "
More information about Elpa packages and Utils files are printed in the
full report. Request it by invoking the command with a prefix argument."))
         (when full-report
           (insert (format "\n
Elpa packages and Utils files are shown below.
The dependencies and lock restrictions are identified.
Note that a package required by PEL may also be a dependency
of another package; the ones identified as dependencies may
also be requested by PEL user-options.\n")
                   )

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
               (insert "\n\nNo package is in excess.")))
           (pel--insert-pkgs-list
            "\n\nList of Elpa-compliant packages PEL can install:\n"
            (car installable-pkgs))
           (pel--insert-pkgs-list
            "\n\nList of other packages PEL can install:\n"
            (cadr installable-pkgs))
           (pel--insert-pkgs-list
            "\nList of elpa-compliant packages that could be updated:\n"
            upgradable-pkgs)))))
    (when errors
      (error "PEL Specification errors: %d" (length errors)))))

(defconst loaded-file-name (or load-file-name buffer-file-name)
  "Path of this file, set at load time.")

(defconst pel-pkg-stat-excluded-files
  '(".dir-locals.el"        ; does not define PEL code
    "install-pel.el"        ; does not define PEL code
    "pel-autoloads.el"      ; does not define PEL code
    "pel-package.el"        ; this file
    "pel-pkg.el"            ; does not define PEL code
    "pel_keys.el"           ; loaded by `pel-package-info-all'
    "pel__hydra.el"         ; special PEL code to exclude.
    )
  "List of names of PEL Emacs Lisp files not used to gather statistics.")

(defun pel--elisp-files ()
  "Return a list of the PEL Emacs Lisp files that must be taken into account.

Requires git to be available in PATH and the PEL source to be inside a git
repository.

Exclude files identified in `pel-pkg-stat-excluded-files' to gather the
statistics."
  (condition-case err
      (let* ((default-directory (file-name-directory loaded-file-name))
             (current-dir-files (process-lines "git" "ls-files" "--" ":(glob)*.el"))
             (filtered-files (cl-remove-if
                              (lambda (file)
                                (member file pel-pkg-stat-excluded-files))
                              current-dir-files)))
        filtered-files)
    (error
     (user-error "pel--elisp-files: git call failed: %s"
                 (error-message-string err)))))


(defun pel-load-all ()
  "Load *all* PEL files.

Use this to compute statistics.
Requires git to be available in PATH and the PEL files to be
inside a git repository (uses `pel--elisp-files')."
  (interactive)
  (let ((current-directory (file-name-directory loaded-file-name)))
    (dolist (file (pel--elisp-files))
      (load-file (expand-file-name file current-directory)))))

(defun pel-package-info-message ()
  "Print PEL package information on stdout."
  (pel-package-info nil t))

(defun pel-package-info-all ()
  "Generate statistics with all PEL files loaded.

CAUTION: Use only for computing statistics as it loads all the packages
and all PEL files!!"
  (interactive)
  (load-library "pel_keys")
  (pel-load-all)
  ;; In Emacs 27+, use `package-activate-all' to activate packages
  ;; found in the subdirectories under `package-user-dir' (set to
  ;; elpa-reduced by the --eval block in the Makefile stats target).
  ;; `package-activate-all' is a lighter operation than `package-initialize'
  ;; (no rescan), and is the correct API for Emacs 27+.
  ;; Without this, `package-activated-list' would be incomplete.
  (require 'package)
  (if (fboundp 'package-activate-all)
      (package-activate-all)            ; Emacs 27+
    (package-initialize))               ; Emacs 26 fallback
  ;; Now print the information.
  (pel-package-info-message))

;; ---------------------------------------------------------------------------

(defun pel-inactive-user-options ()
  "Return a list of inactive PEL user-options symbols."
  (seq-filter (lambda (usr-opt)
                (not (symbol-value usr-opt)))
              (pel-user-options)))

;; ----

(defun pel-el-file-for (filepath)
  "Return the .el filepath of an .elc FILEPATH."
  (format "%s.el" (file-name-sans-extension filepath)))

(defun pel-remove-invalid-elc (directory &optional dry-run)
  "Identify the old and the orphaned elc files in DIRECTORY.
Delete them unless DRY-RUN is non-nil.
DIRECTORY must be a directory path string.
Returns the list of removed file names."
  (let ((removed-files ())
        (elc-files (directory-files directory :full-path "\\.elc\\'"))
        (el-file nil))
    (dolist (elc-file elc-files)
      (setq el-file (pel-el-file-for elc-file))
      (unless (or (file-exists-p el-file)
                  (file-newer-than-file-p el-file elc-file))
        (push elc-file removed-files)
        (unless dry-run
          (delete-file elc-file))))
    (nreverse removed-files)))

;; --

(defun pel-active-and-excess-utils ()
  "Return a list of 2 lists of utils Emacs Lisp files: active and not active.

Each returned list contains directory relative .el file names, in sorted name
order.
- The first list identifies the files that are currently active, requested by
  PEL user-options.
- The second list identifies the files that are currently not used by the PEL
  user options."
  (let ((utils-el-files (directory-files (pel-utils-dirpath) nil "\\.el\\'"))
        (active-utils-files ())
        (excess-utils-files ()))
    (dolist (utils-symbol (cadr (pel-activated-packages)))
      (push (format "%s.el" utils-symbol) active-utils-files))
    ;; Some packages are identified by the user as used, even though PEL may
    ;; not requests it via user-options; make sure to not identify these
    ;; packages as utils packages in excess.
    (dolist (util-el-file utils-el-files)
      (unless (member util-el-file active-utils-files)
        (unless (member util-el-file pel-utils-packages-to-keep)
          (push util-el-file excess-utils-files))))
    ;; return both lists
    (list (nreverse active-utils-files)
          (nreverse excess-utils-files))))

(defun pel-utils-unrequired ()
  "Return the list of utils files not currently required."
  (cadr (pel-active-and-excess-utils)))

(defun pel-clean-utils (&optional dry-run)
  "Move all unrequired Emacs Lisp files from utils to utils-attic directory.
Byte-compile all Emacs Lisp files in the utils directory.
Remove orphaned elc files from the utils directory.
Don't execute when DRY-RUN is non-nil.
Return the a cons of 2 lists:
- a list of .el files that have been or would have been removed,
- a list of the .elc orphaned files that were also removed."
  (let ((unrequired-files (pel-utils-unrequired))
        ;; get list of orphaned .elc (a .elc without a .el file). Delete them
        ;; when no dry-run.
        (removed-elc-files (pel-remove-invalid-elc (pel-utils-dirpath) dry-run)))
    (unless dry-run
      (when unrequired-files
        (unless (file-exists-p (pel-utils-attic-dirpath))
          (make-directory (pel-utils-attic-dirpath)))
        (dolist (file unrequired-files)
          (let ((utils-filename (expand-file-name file (pel-utils-dirpath)))
                (attic-filename (expand-file-name file
                                                  (pel-utils-attic-dirpath))))
            (if (file-exists-p attic-filename)
                (delete-file utils-filename)
              (rename-file utils-filename (pel-utils-attic-dirpath)))))
        ;; byte recompile all .el files newer than .elc or when the .elc is
        ;; missing.
        (byte-recompile-directory (pel-utils-dirpath) 0)))
    (cons unrequired-files removed-elc-files)))

;; --

(defun pel-pkgs-sorted-by-version (pkg-dirs)
  "Return a list of ELPA PKG-DIRS sorted by their version.
The function assumes that:
- the last hyphen in the name separates the package name from the package
  version number,
- every package has the same name,
- version strings may be numeric (e.g. YYYYMMDD.HHMMSS, 1.2.3) or may
  include alphanumeric pre-release suffixes (e.g. 0.9.1pre, 1.0alpha)."
  (sort (copy-sequence pkg-dirs)
        (lambda (a b)
          (let ((fn-a (file-name-nondirectory a))
                (fn-b (file-name-nondirectory b)))
            (let* ((ver-re (concat "-\\(" pel-elpa-pkg-version-regexp "\\)$"))
                   (v-a (and (string-match ver-re fn-a) (match-string 1 fn-a)))
                   (v-b (and (string-match ver-re fn-b) (match-string 1 fn-b))))
              (cond
               ;; Compare version numbers if possible otherwise compare as strings.
               ((and v-a v-b)
                (condition-case nil
                    (version< v-a v-b)
                  (error (string< v-a v-b))))
               ;; a has no version, sort before b
               (v-b t)
               ;; b has no version or neither, keep order
               (t nil)))))))

(defun pel-elpa-dirs-for (pkg &optional in-attic)
  "Return a list of directory names for specified package PKG.

PKG may be a symbol or a string identifying a package name.
Something like \\='seq, \"seq\", \\='lispy or \"lispy\".

Each returned string is the name of a directory with full path.
Each string does not end with a slash character.

The returned list of directory paths is sorted in alphabetical
order.  For several versions of a given package the most recent
is placed last: they are sorted by version numbers.

By default, return package directory names available in the elpa
directory, but if the IN-ATTIC argument is non-nil, return
packages present in the elpa-attic directory."
  (pel-pkgs-sorted-by-version
   (directory-files (if in-attic
                        (pel-elpa-attic-dirpath)
                      package-user-dir)
                    :full-path
                    (format "\\`%s-%s\\'"
                            (regexp-quote (pel-as-string pkg))
                            pel-elpa-pkg-version-regexp))))

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
  (let ((removed-dirpaths ()))
    (dolist (dirpath (pel-elpa-dirs-for pkg))
      (push dirpath removed-dirpaths)
      (unless dry-run
        (let ((new-location (expand-file-name
                             (file-name-nondirectory dirpath)
                             (pel-elpa-attic-dirpath))))
          (if (file-exists-p new-location)
              (delete-directory dirpath :recursively)
            (pel-move-to-dir dirpath (pel-elpa-attic-dirpath))))))
    (nreverse removed-dirpaths)))

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
              (pel+= remove-count 1))))
        (widen)
        remove-count))))

(defun pel-elpa-packages-in-dir (type)
  "Return a list of symbol for all packages present in local Elpa directory.

The TYPE argument is the same as the one for `pel-elpa-dirpath'.

The function search the directory identified by `pel-elpa-dirpath'.

The directory holds sub-directories, one per package/version.
The directory may hold several versions of a specific Elpa package.
The returned list contains only one symbol identifying the package for each
version of that package.
The list of package symbols is sorted by symbol names."
  (let ((elpa-pkg-dir-names  (directory-files
                              (pel-elpa-dirpath type)
                              nil
                              pel-elpa-pkg-dirname-regexp))
        (elpa-pkg-names ()))
    (dolist (dir-name elpa-pkg-dir-names)
      (let ((pkg-name-str (pel-elpa-package-name-for dir-name)))
        (when pkg-name-str
          (let ((pkg-name (intern pkg-name-str)))
            (unless (memq pkg-name elpa-pkg-names)
              (push pkg-name elpa-pkg-names))))))
    (nreverse elpa-pkg-names)))

(defun pel-elpa-unrequired ()
  "Return a list of the elpa packages that are not required by PEL.
Packages not required are packages not requested by any PEL user-option or any
of their dependencies.
The returned list contains symbols, each symbol is the name (without any
version numbering) of the elpa package.  The list is sorted."
  (let ((activated-elpa (car (pel-activated-packages)))
        (available-elpa (pel-elpa-packages-in-dir 'final-dir-at-startup))
        (excess-elpa    ()))
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
        (moved-elpa-dirs ()))
    (when unrequired-elpa
      ;; If any Elpa package is in excess, move it in the elpa-attic
      ;; unless this is a dry-run (in which case just accumulate the directory
      ;; names in the moved-elpa-dirs list).  If the directory is already
      ;; inside the elpa-attic then delete it.
      (unless (file-exists-p (pel-elpa-attic-dirpath))
        (make-directory (pel-elpa-attic-dirpath)))
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

;; --

(defconst pel--elpa-package-renames
  '((go-translate . gt))
  "Alist of known ELPA package renames: (OLD-PACKAGE . NEW-PACKAGE).
Each entry is a cons cell where the car is the symbol for the old/superseded
package name and the cdr is the symbol for its replacement.

When `pel-clean-renamed-packages' detects that both the old and new package
directories exist in the active elpa directory, it moves the old one to the
elpa-attic directory to prevent filename collisions in the fast-startup bundle.

Known renames:
- go-translate was renamed to gt (https://github.com/lorniu/gt.el).")

(defun pel-clean-renamed-packages (&optional dry-run)
  "Move superseded (renamed) ELPA packages from elpa to elpa-attic.

Checks `pel--elpa-package-renames' for known package renames.  For each
entry, when both the old (superseded) and new (replacement) package
directories are present in the active elpa directory, the old package is
moved to the elpa-attic directory.  The new package is never touched.

This prevents filename collisions in the fast-startup bundle created by
`pel-elpa-create-copies', which requires unique filenames across all packages.

If the destination directory in elpa-attic already exists the old package
directory is deleted instead of moved.

If DRY-RUN is non-nil, no files are moved or deleted; the function only
returns the list of old package directories that would have been moved.

Return a list of old package directory paths that were moved (or would have
been moved in a dry run)."
  (let ((moved-dirs ()))
    (dolist (rename pel--elpa-package-renames)
      (let* ((old-pkg  (car rename))
             (new-pkg  (cdr rename))
             (old-dirs (pel-elpa-dirs-for old-pkg))
             (new-dirs (pel-elpa-dirs-for new-pkg)))
        ;; Only retire the old package when its replacement is already present,
        ;; to avoid accidentally removing the sole copy of the files.
        (when (and old-dirs new-dirs)
          (unless (or dry-run (file-exists-p (pel-elpa-attic-dirpath)))
            (make-directory (pel-elpa-attic-dirpath)))
          (setq moved-dirs
                (append moved-dirs
                        (pel-move-elpa-pkg-to-elpa-attic old-pkg dry-run)))
          ;; Also scrub the old package symbol from package-selected-packages
          ;; in memory and in the customization file so Emacs no longer tries
          ;; to activate it on the next start.
          (unless dry-run
            (pel-clean-package-selected-packages (list old-pkg))
            (pel-clean-package-selected-packages-in-file (list old-pkg))))))
    moved-dirs))

(defun pel-cleanup (&optional dry-run)
  "Move all unrequired packages to their attic directory.

With optional argument DRY-RUN, do nothing just report what would be
done.  Print a description of the operation in the *pel-cleanup* buffer.

This command is *not* available when PEL operates in fast startup or
when running inside an Emacs server client (emacsclient).  It must be
run from a standalone Emacs process."
  (interactive "P")
  (when (pel-in-fast-startup-p)
    (user-error "pel-cleanup is not available in fast startup operation!
Use pel-setup-normal to return to normal operation."))
  (when (frame-parameter nil 'client) ; in any server client (terminal or GUI)?
    (user-error "pel-cleanup is not available in a server client!
Use a normal Emacs process."))
  ;;
  ;; When state permits it, proceed.
  (when (or dry-run
            (y-or-n-p "Proceed with removal of non-required packages? "))
    (message "%s" (propertize "Checking PEL user-options and packages..." 'face 'bold))
    (let* ((utils-results       (pel-clean-utils dry-run))
           (removed-el-files    (car utils-results))
           (removed-elc-files   (cdr utils-results))
           ;; Retire superseded (renamed) packages BEFORE the general cleanup
           ;; so that pel-clean-elpa does not also try to process them.
           (renamed-pkg-dirs    (pel-clean-renamed-packages dry-run))
           (moved-elpa-dirs     (pel-clean-elpa dry-run)))
      (message "")
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
               (setq verb-moved   "that would have been moved"
                     verb-Moved   "Would move"
                     verb-Removed "Would remove")
             (setq verb-moved   "moved"
                   verb-Moved   "Moved"
                   verb-Removed "Removed"))
           (insert "
The PEL cleanup removes packages that are not needed, based on
the value of the `pel-use-' customization user-options.

PEL does not remove packages that are dependencies of packages
that are activated by the user-options or packages manually
installed that have been identified in the following user-options:
")
           (pel-insert-list-content 'pel-elpa-packages-to-keep nil nil nil 'on-same-line)
           (pel-insert-list-content 'pel-utils-packages-to-keep nil nil nil 'on-same-line)
           (insert (format "

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
           (when removed-elc-files
             (insert (format "%s %d orphaned .elc files.\n"
                             verb-Removed (length removed-elc-files))))
           (when removed-el-files
             (insert (format "%s %d files,\nfrom: %s\nto  : %s\n"
                             verb-Moved
                             (length removed-el-files)
                             (pel-utils-dirpath)
                             (pel-utils-attic-dirpath)))
             (insert (format "The files %s to utils-attic are:\n\n"
                             verb-moved))
             (dolist (fn removed-el-files)
               (pel+= n 1)
               (insert (format "- %3d: %s\n" n fn))))
           ;; ---- Check superseded/renamed packages ----
           (when renamed-pkg-dirs
             (when (or removed-el-files removed-elc-files)
               (insert "\n\n"))
             (insert (format "Superseded (renamed) Elpa packages %s to elpa-attic,
from: %s
to  : %s

See `pel--elpa-package-renames' for the list of known renames.\n\n"
                             verb-moved
                             (pel-elpa-dirpath 'final-dir-at-startup)
                             (pel-elpa-attic-dirpath)))
             (setq n 0)
             (dolist (pkgdir renamed-pkg-dirs)
               (pel+= n 1)
               (insert (format "- %3d: %s\n" n pkgdir)))
             (pel-insert-list-content 'pel--elpa-package-renames nil nil nil 'on-same-line))
           ;; ---- report general unrequired packages ----
           (when moved-elpa-dirs
             (when (or removed-el-files removed-elc-files renamed-pkg-dirs)
               (insert "\n\n"))
             (insert (format "Elpa packages %s,
from: %s
to  : %s :\n\n"
                             verb-moved
                             (pel-elpa-dirpath 'final-dir-at-startup)
                             (pel-elpa-attic-dirpath)))
             (setq n 0)
             (dolist (pkgdir moved-elpa-dirs)
               (pel+= n 1)
               (insert (format "- %3d: %s\n" n pkgdir))))
           (unless (or removed-el-files
                       removed-elc-files
                       renamed-pkg-dirs
                       moved-elpa-dirs)
             (insert "Nothing to cleanup!!"))))))))

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
        (installation-succeeded nil)
        (used-elpa-dirpath (pel-elpa-dirpath 'final-dir-at-startup)))
    (when elpa-attic-pkg-dirpath
      (let ((dest-dirpath
             (expand-file-name (file-name-nondirectory
                                elpa-attic-pkg-dirpath)
                               used-elpa-dirpath)))
        (unless (file-exists-p dest-dirpath)
          (copy-directory elpa-attic-pkg-dirpath
                          (file-name-as-directory used-elpa-dirpath)
                          :keep-time))
        ;; Add the directory to load-path if it does not already exists.
        ;; Entries in load-path do not have the trailing "/"; therefore
        ;; remove it from the one we enter.
        (add-to-list 'load-path (directory-file-name dest-dirpath))
        (load-library (pel-as-string pkg))
        (setq installation-succeeded t)))
    installation-succeeded))

;;; --------------------------------------------------------------------------
(provide 'pel-package)

;;; pel-package.el ends here
