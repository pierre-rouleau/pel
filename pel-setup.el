;;; pel-setup.el --- Control PEL Emacs switch from normal to fast-startup mode and back.  -*- lexical-binding: t; -*-

;; Created   : Thursday, July  8 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-08-30 12:14:59, updated by Pierre Rouleau>

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
;; This file provides the following PEL-specific facilities:
;;
;; Setup for Dual Customization Environment : terminal(TTY) / graphics
;; -------------------------------------------------------------------
;;
;; - The ability to setup Emacs to support two independents environments: one
;;   for Emacs running in terminal (TTY) mode and another for Emacs running in
;;   graphics mode. Each of them have their own customization file, their
;;   own set of Elpa directories to store the external Elpa-compliant
;;   packages.  This  allows taking advantage of the strengths of the
;;   terminal-based and graphics-based Emacs by customizing each environment
;;   with the packages that are used in each.
;;
;;   - The command `pel-setup-dual-environment' sets up the required files
;;     inside the `user-emacs-directory' if they are not already present.
;;     This must be done once.
;;
;; This is quite useful on OS, like macOS, where the graphics Emacs is
;; noticeably slower than its terminal counterpart.
;;
;;
;; Dynamic control to switch to a fast-startup operation mode
;; ----------------------------------------------------------
;;
;; This provides the ability to reduce the Emacs initialization startup time.
;; Depending on what is used the speedup can be quite noticeable.
;;
;; For example, I have achieved a emacs-startup-time of about 0.15 seconds on
;; Emacs 26.3 running in terminal mode on macOS 2014 4 GHz Intel Core i7
;; computer with 240 installed external packages!
;;
;; This dynamic control of fast-startup also supports the dual
;; terminal-mode/graphics-mode environments.  When these are used, both
;; environments are switched together regardless of whether the switch was
;; requested by the command running inside Emacs in terminal or graphics mode.
;;
;; When running multiple Emacs processes on a computer, switching to
;; fast-startup or back to normal-startup mode from one Emacs instance does
;; not affect the other instances because files are not deleted.
;;
;; PEL uses the built-in Emacs package management provided by the package.el
;; builtin library.  The fast-startup operation mode improves startup speed
;; and does not prevent using package.el features to explicitly install
;; packages in the current environment.  However, these packages will not be
;; known to PEL.  In fast-startup mode PEL disables its internal automatic
;; package management facilities and does not download, install or remove
;; packages based on modification of the customization.
;;
;; The strategy is to reduce the length of Emacs `load-path' to a minimum.
;; This can therefore be used in conjunction of the Emacs 27+
;; `package-quickstart' feature to reduce Emacs startup time further.
;;
;; The length of `load-path' increases each time a new Elpa-compliant package
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
;; inside one directory; they have no sub-directories.  I call those "one-level
;; packages" in opposition of the other Emacs Lisp packages that use
;; sub-directories to store other files.  From what I have seen so far most
;; Emacs external packages use only one directory but not all.
;;
;; Emacs Lisp, a Lisp-2, has only one namespace for variables and one
;; namespace for functions.  Code in *all* packages, whether they're built-in
;; Emacs or external must all share these namespaces (and some other).  Most
;; package file names, if not all, reflect their package name and differ from
;; each other. The functions and variable names in them must all be unique
;; otherwise they risk clashing with each other.
;;
;; Because the file names of all package have a unique name it becomes
;; possible to place them all inside the same directory (a bundle) and place
;; that unique directory inside Emacs `load-path', therefore eliminating
;; relatively slow Emacs startup processing that iterates through each
;; directory in its `load-path'.
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
;; the code of "one-level packages" inside one directory (the "pel-bundle"
;; package directory) and leaving the other packages inside their original
;; locations.  The code also creates a pel-bundle-package.el and a
;; pel-bundle-autoloads.el file stored inside the pel-bundle directory
;; creating a Elpa-compliant pel-bundle package that includes all files of
;; one-level packages.
;;
;; Special code must be run by init.el or early-init.el to prevent the
;; package.el code from attempting to download the packages again.  The
;; required code is described inside the following files:
;;
;; - example/init/init-5.el and
;; - example/init/early-init.el
;;
;; The PEL installation instructions require you to install init.el and
;; early-init.el files that contain the code of these files.
;;
;; To provide ability quickly switch from a normal setup to a fast-startup
;; setup and back, PEL uses a symlink to point to one of two Elpa directories
;; inside the `user-emacs-directory'. Assuming that `user-emacs-directory' is
;; "~/.emacs.d", these directories are:
;;
;; - "~/.emacs.d/elpa-complete" : The original "~/.emacs.d/elpa" directory
;;                                renamed (or duplicated) by `pel-setup-fast'.
;;
;; - "~/.emacs.d/elpa-reduced" : Created by `pel-setup-fast'.  Holds all
;;                               multi-directory packages; the ones that are
;;                               not "one-level packages".  It also contains
;;                               the "pel-bundle-yyyymmdd-hhmm" directory; the
;;                               PEL bundle.
;;
;; The PEL bundle is created each time the command `pel-setup-fast' is
;; executed to prepare Emacs for a fast startup.  Again, assuming that
;; `user-emacs-directory' is "~/.emacs.d" the PEL bundle directory is:
;;
;; - "~/.emacs.d/elpa-reduced/pel-bundle-yyymmdd-hhmm": Simulates a fictitious
;;                               Elpa-compliant pel-bundle package.  The
;;                               directory holds symlinks to the .el and .elc
;;                               files of all one-level packages that it
;;                               replaces. It is created by `pel-setup-fast'
;;                               with the name tail set to its creation date.
;;                               It also holds the 2 important package
;;                               required files that are also created by the
;;                               function `pel-setup-fast':
;;                               - pel-bundle-autoloads.el
;;                               - pel-bundle-pkg.el
;;
;; PEL also converts the original elpa directory into a symbolic link that
;; points to either the following directories:
;;
;; - elpa-complete (in normal startup mode),
;; - elpa-reduced  (in fast-startup mode).
;;
;; When the dual terminal(TTY)/graphics customization support is used, PEL
;; uses one set of symlink and Elpa directories per environment: it
;; creates the following extra symlinks and directories for Emacs independent
;; graphics mode:
;;
;; - elpa-graphics symlink that points to one of the following directories:
;; - elpa-complete-graphics (used in normal mode for the independent graphics
;;   mode),
;; - elpa-reduced-graphics (used in fast startup mode for the independent
;;   graphics mode).
;;
;; *************
;; **IMPORTANT**
;; *************
;;
;; Again, for all of this to work properly you must instrument your init.el
;; and, if you use it, your early-init.el.  See the code sample examples
;; inside the following files:
;;
;; - example/init/init-5.el and
;; - example/init/early-init.el
;;
;; The code in pel-setup, specifically the function
;; `pel--update-emacs-user-file' and its caller edit the init.el and
;; early-init.el: they set the value of defcustom forms which control the
;; Emacs package.el behaviour at startup.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;

(require 'pel--base)                  ; use: `pel-unix-socket-p'
;;                                    ;      `pel-string-when'
;;                                    ;      `pel-sibling-dirpath'
;;                                    ;      `pel-point-symlink-to'
;;                                    ;      `pel-emacs-is-graphic-p'
(require 'pel--options)               ; use: `pel-compile-pel-bundle-autoload'
(require 'pel-ccp)                    ; use: `pel-delete-whole-line'
(require 'pel-custom)                 ; use: `pel-customize-save
(require 'pel-package)                ; use: `pel-elpa-dirpath'
(require 'pel-elpa)                   ; use: `pel-elpa-create-copies'
;;                                    ;      `pel-elpa-disable-pkg-deps-in'
;;                                    ;      `pel-elpa-package-alist-of-dir'
;;                                    ;      `pel--adjust-path-for-graphics'
;;                                    ;      `pel--adjusted-fname'
(eval-when-compile (require 'subr-x)) ; use: `string-join'
(require 'cus-edit)                   ; use: `custom-file'`

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defconst pel-init-detected-dual-environment-p
  (and (boundp 'pel-init-support-dual-environment-p)
       pel-init-support-dual-environment-p)
  "Identifies whether PEL uses dual environment -- as detected by init.el.

When using the dual environment, PEL has 2 independent sets of
customization files and package directories: one when Emacs runs
in terminal/TTY mode and another when Emacs runs in graphic mode.

Code must NOT modify this value as it represents the state seen
by init.el when Emacs started!")

;; ---------------------------------------------------------------------------
;; Local utilities
;; ---------------

(defun pel--compile-file-if (el-fname byte-compile-it)
  "Byte compile file EL-FNAME if BYTE-COMPILE-IT is set.
Otherwise delete the .elc file if it exists."
  (if byte-compile-it
      (byte-compile-file el-fname)
    ;; no compilation needed; remove any left-over .elc file
    (let ((elc-fname (concat el-fname "c")))
      (when (file-exists-p elc-fname)
        (delete-file elc-fname)))))

(defun pel--other-custom-file ()
  "Return the name of the customization file used by the other mode."
  (pel--adjusted-fname custom-file :force (not pel-emacs-is-graphic-p)))

;; ---------------------------------------------------------------------------
;; pel-copy-directory : copies a directory, skipping Unix socket files
;; -------------------------------------------------------------------

(defun pel-copy-only-file (original-copy-file file &rest args)
  "Copy files but not Unix sockets."
  (unless (pel-unix-socket-p file)
    (apply original-copy-file
           file
           args)))

(defun pel-copy-directory (source dest)
  "Copy SOURCE directory into DEST directory. Skip socket files.

Both SOURCE and DIRECTORY may be in the directory name (with a terminating
slash) or in file name (without the terminating slash) format."
  ;; - Normally, copy-directory would fail when attempting to copy
  ;;   a Unix socket file, like those in elpa/gnupg.  To prevent
  ;;   the error, replace copy-file by pel-copy-only-file which
  ;;   does not attempt to copy the Unix socket files.
  (advice-add 'copy-file :around #'pel-copy-only-file)
  (unwind-protect
      (copy-directory (directory-file-name source)
                      (directory-file-name dest))
    (advice-remove 'copy-file #'pel-copy-only-file)))

;; ---------------------------------------------------------------------------
;; Utilities to edit Emacs init.el and early-init.el files
;; -------------------------------------------------------
;;
;; This code can update Emacs init.el file that has the content specified
;; by the example/init/init-5.el file.  You can write more logic inside your
;; own init.el file of course, but to use PEL setup features, you *MUST* have
;; the code present inside example/init/init-5.el.
;;
;; On Emacs 27 and later the same is true for the early-init.el file: you must
;; use a file that contains what is inside the file example/init/early-init.el
;;
;; The following functions are used to edit it.

;; - `pel--set-dual-environment-in-emacs-init'
;;   - `pel--update-emacs-user-file'
;;
;; - `pel--set-dual-environment-in-emacs-early-init'
;;   - `pel--update-emacs-user-file'
;;
;; Package quickstart management, available for supporting Emacs 27 and later
;; also use the low level function to setup the early-init file in a code
;; section below.
;;
;; - `pel--setup-early-init'
;;   - `pel--update-emacs-user-file'


(defun pel--update-emacs-user-file (fname symbol-values
                                          &optional byte-compile-it)
  "Update FNAME file: set symbol to value from the SYMBOL-VALUES alist.

The FNAME is assumed to be located inside the user Emacs directory.
FNAME can be init.el or early-init.el and must define the symbols
inside defconst forms.

Raise an error if the function does not find the `defconst' form
defining a specified symbol."
  (let (varname
        new-value
        re-pattern)
    (with-temp-file fname
      (insert-file-contents fname)
      (dolist (symbol.value symbol-values)
        (setq varname (symbol-name (nth 0 symbol.value)))
        (setq new-value (format "%S" (nth 1 symbol.value)))
        (goto-char (point-min))
        (setq re-pattern (format "^(defconst +%s +\\(.+\\) *$" varname))
        (if (re-search-forward re-pattern nil :noerror)
            (replace-match new-value :fixedcase :literal nil 1)
          (user-error "Can't find regexp %s in '%s'" re-pattern fname))))
    (pel--compile-file-if fname byte-compile-it)))

(defun pel--set-dual-environment-in-emacs-init (use)
  "Update Emacs user init.el for dual environment when USE is non-nil.

Set `pel-init-support-dual-environment-p' to t when use is
non-nil, to nil otherwise.  Byte compile the result file if the
`pel-compile-emacs-init' user-option is turned on."
  (let ((init-fname (locate-user-emacs-file "init.el")))
    ;; Make sure an init.el file is present.
    (unless (file-exists-p init-fname)
      (user-error "File %s is not found!" init-fname))
    (pel--update-emacs-user-file
     init-fname
     (list
      (list 'pel-init-support-dual-environment-p (not (null use))))
     pel-compile-emacs-init)))

(when pel-emacs-27-or-later-p
  (defun pel--set-dual-environment-in-emacs-early-init (use)
    "Update Emacs user early-init.el for dual environment when USE is non-nil."
    (let ((early-init-fname (locate-user-emacs-file "early-init.el")))
      ;; Make sure an early-init.el file is present.
      (unless (file-exists-p early-init-fname)
        (user-error "File %s is not found!" early-init-fname))
      (pel--update-emacs-user-file
       early-init-fname
       (list
        (list 'pel-early-init-support-dual-environment-p (not (null use))))
       pel-compile-emacs-early-init)))
  (declare-function pel--set-dual-environment-in-emacs-early-init "pel-setup"))

;; ---------------------------------------------------------------------------
;; Support for dual environments
;; -----------------------------
;;
;; PEL supports a dual environment where the terminal/TTY mode uses one
;; customization file and a set of package directories and the graphics mode
;; use another, independent customization file and package directories.
;;
;; By default this mode is not activated.  The user can activate it by
;; executing the `pel-setup-dual-environment' command.  That function creates
;; all the necessary files and directories.
;; - It also updates the init.el and the early-init.el (in Emacs 27 and later)
;;    by setting the `pel-init-support-dual-environment-p' constant to t,
;;    allowing logic in those initialization files to force Emacs to use the
;;    graphic specific environment when Emacs is running in graphics mode.
;; - It also updates the `pel-support-dual-environment' user-option in both
;;   customization files.
;;
;; Later when Emacs starts, the code inside pel_keys.el executed by `pel-init'
;; schedule the execution of `pel-setup-check-dual-environment' to check if
;; everything is consistent, tries to fix the issues and report remaining
;; issues to the user in case of inconsistencies.  This operation is delayed a
;; little to ensure it does not slow the startup.
;;
;; * `pel-setup-check-dual-environment'
;;
;; * `pel-setup-dual-environment'
;;   - `pel--create-dir'
;;     - `pel--dir-exists-p'
;;   - `pel-dual-environment-problems'
;;   - `pel--other-custom-file'
;;
;; * `pel-setup-info-dual-environment'
;;   - `pel-dual-environment-problems'
;;

;; If you need independent customization for Emacs running in terminal (TTY)
;; mode and in graphics mode, then do the following:
;;
;; - Execute `pel-setup-dual-environment' to create all necessary files and
;;   directories. It duplicates your single custom file and your single elpa
;;   and utils package directories.  It also updates the init.el and
;;   early-init.el (on Emacs 27 and later) optionally byte compiling them if
;;   requested by the current user options.
;; - Restart Emacs.

(defun pel-dual-environment-problems ()
  "Return list of string describing problems found in dual custom environment.
Return nil if no problems were found and all is OK, ready to use Emacs in
independent environments for terminal and graphics mode."
  (let* ((custom-fname (pel--adjusted-fname custom-file :force nil))
         (g-custom-fname (pel--adjusted-fname custom-file
                                              :force :for-graphics))
         (elpa-dpath (pel--adjusted-fname pel-elpa-dirpath :force nil))
         (elpa-dname (directory-file-name elpa-dpath))
         (g-elpa-dname (pel--adjusted-fname elpa-dname
                                            :force :for-graphics))
         (elpa-complete-dname (pel-sibling-dirname pel-elpa-dirpath
                                                   "elpa-complete"))
         (g-elpa-complete-dname (pel--adjusted-fname elpa-complete-dname
                                                     :force :for-graphics))
         (utils-dpath (pel--adjusted-fname pel-utils-dirpath
                                           :force nil))
         (g-utils-dpath (pel--adjusted-fname pel-utils-dirpath
                                             :force :for-graphics))
         (issues nil))
    (unless (file-exists-p custom-fname)
      (pel-push-fmt issues "File      missing : %s" custom-fname))
    (unless (file-exists-p g-custom-fname)
      (pel-push-fmt issues "File      missing : %s" g-custom-fname))
    (if (file-exists-p elpa-dname)
        (unless (file-symlink-p elpa-dname)
          (pel-push-fmt issues "Is not a symlink  : %s" elpa-dname))
      (pel-push-fmt issues "Directory missing : %s" elpa-dname))
    (if (file-exists-p g-elpa-dname)
        (unless (file-symlink-p g-elpa-dname)
          (pel-push-fmt issues "Is not a symlink  : %s" g-elpa-dname))
      (pel-push-fmt issues "Directory missing : %s" g-elpa-dname))
    (if (file-exists-p elpa-complete-dname)
        (unless (file-directory-p elpa-complete-dname)
          (pel-push-fmt issues "Is not a directory: %s" elpa-complete-dname))
      (pel-push-fmt issues "Directory missing : %s" elpa-complete-dname))
    (if (file-exists-p g-elpa-complete-dname)
        (unless (file-directory-p g-elpa-complete-dname)
          (pel-push-fmt issues "Is not a directory: %s" g-elpa-complete-dname))
      (pel-push-fmt issues "Directory missing : %s" g-elpa-complete-dname))
    (if (file-exists-p utils-dpath)
        (unless (file-directory-p utils-dpath)
          (pel-push-fmt issues "Is not a directory: %s" utils-dpath))
      (pel-push-fmt issues "Directory missing : %s" utils-dpath))
    (if (file-exists-p g-utils-dpath)
        (unless (file-directory-p g-utils-dpath)
          (pel-push-fmt issues "Is not a directory: %s" g-utils-dpath))
      (pel-push-fmt issues "Directory missing : %s" g-utils-dpath))
    (unless pel-support-dual-environment
      (pel-push-fmt issues
          "pel-support-dual-environment user-options is not set"))
    (reverse issues)))

;;-pel-autoload
(defun pel-setup-info-dual-environment ()
  "Display current PEL customization setup.
Check two independent customization files for terminal/tty and graphics mode
are requested and if so check if they are setup properly.
Report an error and list problems if there are any, otherwise display the
current setup."
  (interactive)
  (if (and  (boundp 'pel-init-support-dual-environment-p)
            pel-init-support-dual-environment-p)
      (let ((problems (pel-dual-environment-problems)))
        (if problems
            (let ((problem-count (length problems)))
              (user-error "\
The file %s is requesting the use of dual tty/graphics customization.
 However the following %s %s:\n - %s"
                          (locate-user-emacs-file "init.el")
                          (pel-count-string problem-count "problem"
                                            nil :no-count-for-1)
                          (pel-pluralize problem-count "remains" "remain")
                          (string-join problems "\n - ")))
          (message  "PEL is ready to use 2 independent customization files:
 One for terminal/TTY: %s
 One for graphics    : %s"
                    (pel--adjusted-fname custom-file :force nil)
                    (pel--adjusted-fname custom-file :force :for-graphics))))
    (message "PEL is currently using a single customization file: %s"
             custom-file)))

;; --

(defun pel--dir-exists-p (dname)
  "Return t if DNAME exists and is a directory, nil if it does not exists.
Raise a user-error if DNAME exists but is not a directory."
  (when (file-exists-p dname)
    (unless (file-directory-p dname)
      (user-error "%s is not a directory!" dname))
    t))

(defun pel--create-dir (gdname dname name )
  "Copy GDNAME to DNAME which abbreviates to NAME unless it exists.
Return a list of performed actions (in reverse order of execution)."
  (let ((actions nil)
        (dpath (file-name-as-directory dname)))
    (unless (pel--dir-exists-p gdname)
      (if (pel--dir-exists-p dname)
          (progn
            (pel-copy-directory dname gdname)
            (pel-push-fmt actions "Copied %s to %s" dname gdname))
        (if (pel--dir-exists-p dpath)
            (progn
              (pel-copy-directory dpath gdname)
              (pel-push-fmt actions "Copied %s to %s" dpath gdname))
          (user-error "Can't find %s directory.  Looked for:\n- %s\n- %s"
                      name dname dpath))))
    actions))

(defun pel--setup-dual-environment (&optional reason-msg)
  "Setup Emacs environment to support 2 independent customization.

Utility function. If REASON-MSG is specified include that message on error."
  (let* ((custom-fname (pel--adjusted-fname custom-file :force nil))
         (g-custom-fname (pel--adjusted-fname custom-file
                                              :force :for-graphics))
         (elpa-dpath (pel--adjusted-fname pel-elpa-dirpath :force nil))
         (elpa-dname (directory-file-name elpa-dpath))
         (g-elpa-dname (pel--adjusted-fname elpa-dname
                                            :force :for-graphics))
         (elpa-complete-dname (pel-sibling-dirname pel-elpa-dirpath
                                                   "elpa-complete"))
         (g-elpa-complete-dname (pel--adjusted-fname elpa-complete-dname
                                                     :force :for-graphics))
         (utils-dname (pel--adjusted-fname (directory-file-name pel-utils-dirpath)
                                           :force nil))
         (g-utils-dname (pel--adjusted-fname (directory-file-name pel-utils-dirpath)
                                             :force :for-graphics))
         (actions nil))
    ;; 1:
    ;; Create a custom-file for graphics mode unless it already exists.
    (unless (file-exists-p g-custom-fname)
      (if (file-exists-p custom-fname)
          (progn
            (copy-file custom-fname g-custom-fname)
            (pel-push-fmt actions "Copied %s to %s "
              custom-fname g-custom-fname))
        (user-error "Expected customization file %s does not exists!"
                    custom-fname)))
    ;; 2:
    ;; Create a elpa directory for graphics mode unless it already exists.
    (pel-prepend-to
     (pel--create-dir g-elpa-complete-dname elpa-complete-dname "elpa")
     actions)
    ;; 3:
    ;; Create the utils for graphics mode unless it already exists.
    (pel-prepend-to
     (pel--create-dir g-utils-dname utils-dname "utils")
     actions)
    ;; 4:
    ;; Rename the elpa directory to elpa-complete unless it's already done
    (when (and (file-exists-p elpa-dname)
               (not (file-symlink-p elpa-dname)))
      (when (file-exists-p elpa-complete-dname)
        (user-error "Both %s and %s exist! Manual cleanup required!"
                    elpa-dpath elpa-complete-dname))
      (rename-file (directory-file-name elpa-dpath) elpa-complete-dname)
      (pel-push-fmt actions "Renamed %s to %s"
        elpa-dpath elpa-complete-dname))
    ;; 5:
    ;; Create the main elpa directory into a symlink to the elpa-complete
    ;; directory.
    (unless (file-symlink-p elpa-dname)
      (pel-point-symlink-to elpa-dname elpa-complete-dname)
      (pel-push-fmt actions "Created symlink %s pointing to %s"
        elpa-dname elpa-complete-dname))
    ;; 6:
    ;; Create the elpa-graphics symlink to the elpa-complete-graphics
    (unless (file-symlink-p g-elpa-dname)
      (pel-point-symlink-to g-elpa-dname g-elpa-complete-dname)
      (pel-push-fmt actions "Created symlink %s pointing to %s"
        g-elpa-dname g-elpa-complete-dname))
    ;; 7:
    ;; Update init.el: set pel-init-support-dual-environment-p
    ;; to the value of pel-init-detected-dual-environment-p
    (condition-case err
        (progn
          (pel--set-dual-environment-in-emacs-init t)
          (when pel-emacs-27-or-later-p
            (pel--set-dual-environment-in-emacs-early-init t))
          (pel-push-fmt actions "Updated init.el%s. Please restart Emacs!"
            (pel-string-when pel-emacs-27-or-later-p
                             " and early-init.el")))
      (error
       (progn
         (display-warning
          'pel-setup-dual-environment
          (format "Problem updating Emacs initialization file: \n %s" err)
          :error))))
    ;; 8:
    ;; Remember this setting inside customization files
    (pel-customize-save 'pel-support-dual-environment t)
    (pel-customize-save 'pel-support-dual-envionment t
                        (pel--other-custom-file))
    (setq pel-support-dual-environment t)
    ;; Display performed actions.
    (let ((remaining-problems (pel-dual-environment-problems))
          (done-text (when actions
                       (format "Completed the following:\n- %s"
                               (string-join
                                (reverse actions) "\n- ")))))
      (if remaining-problems
          (let ((remaining-actions (format "some problems remain.\
 Please fix them manually:\n- %s" (string-join remaining-problems "\n- "))))
            (if actions
                (user-error "%s%s\n Unfortunately %s"
                            (pel-string-when reason-msg)
                            done-text
                            remaining-actions)
              (user-error "%sNothing can be done since %s"
                          (pel-string-when reason-msg)
                          remaining-actions)))
        (if actions
            (message "%s\n All is now OK!" done-text)
          (message "Nothing to do, it's already setup."))))))

;;-pel-autoload
(defun pel-setup-dual-environment ()
  "Setup Emacs environment to support 2 independent customization.

Provide support for a customization and the Elpa directories
required for the following two modes Emacs operation:
- terminal/TTY
- graphics

After trying to set everything for the use of dual environment it
displays a message describing the state.  It lists the actions
performed and any remaining problems which you will have to fix
manually.  If all is now OK it will say so, or if all was already
ok, it will also say so.

Normally Emacs makes no distinction between those and uses the
exact same set of customization files and Elpa packages for Emacs
operating in those two different modes.  If you want to manage
the customization and packages used when Emacs operates in
terminal/TTY mode one way and when Emacs operates in graphics
mode another way, with PEL, then use that command.

See the comments at in the commentary section of pel-setup.el for
more information."
  (interactive)
  (when (y-or-n-p "Activate independent customization & packages for terminal\
 & graphic mode? ")
    (pel--setup-dual-environment)))

;;-pel-autoload
(defun pel-setup-check-dual-environment ()
  "Check dual environment status, activate it if needed, warn about issues.

This function is meant to be called by `pel-init' to check if the user-option
correspond to what is identified inside the init.el and early-init.el files.

Returns t if all is ok, nil otherwise."
  (let ((symbols '(pel-init-support-dual-environment-p))
        (is-ok t))
    (when pel-emacs-27-or-later-p
      (push 'pel-early-init-support-dual-environment-p symbols))
    (dolist (symbol symbols)
      (unless (eq pel-support-dual-environment (symbol-value symbol))
        (setq is-ok nil)))
    (unless is-ok
      (if pel-support-dual-environment
          (pel--setup-dual-environment
           "Dual environment is requested by user-option, but not active!\n")
        (display-warning
         'pel-setup-check-dual-environment
         (format "\
There are inconsistencies in the PEL dual environment setup.
 It is not requested by the `pel-setup-check-dual-environment' user-option
 BUT the initialization %s %S identify a different request.
 Please fix that.
 Refer to PEL user Manual installation notes and read information about
 'Support for Independent Customization of Graphics and Terminal based\
 Emacs'."
                 (pel-pluralize (length symbols) "symbol")
                 symbols)
         :error)))
    is-ok))

;; ---------------------------------------------------------------------------
;; Fast-Startup Support
;; --------------------

(defconst pel-fast-startup-init-fname (expand-file-name
                                       "pel-fast-startup-init.el"
                                       user-emacs-directory)
  "Name of code file that must be executed by fast-startup in init/early-init.
When fast startup is not activated, this file must be deleted.")

(defvar pel--setup-changed nil
  "Identifies that PEL setup has changed.
Only set by `pel-setup-fast' or `pel-setup-normal'. Never cleared.")

;; Emacs >= 27:" package quickstart with fast startup & dual customization
;; ----------------------------------------------------------------------
;;
;; For Emacs 27 and after, PEL must use the early-init.el file and must
;; identify the state of the following features:
;;
;;   - Package quickstart
;;   - Dual environment
;;   - GUI launched Emacs
;;
;; The function `pel--setup-early-init' updates the content of the
;; early-init.el file.  It is used by the commands `pel-setup-with-quickstart'
;; and `pel-setup-no-quickstart'.
;;
;; The function `pel--setup-early-init' is also called by the commands that
;; affects:
;;
;;   - use of dual environment: the `pel-setup-dual-environment' command,
;;   - fast and normal startup (on Emacs >= 27, to ensure proper operation
;;     there), by the commands `pel-setup-fast' and `pel-setup-normal'.




;; * `pel-setup-with-quickstart'
;;   - `pel--setup-early-init'
;;     - `pel--update-emacs-user-file'
;;   - `pel--set-package-quickstart'
;;     - `pel--build-package-quickstart'
;;       - `pel--package-qs'
;;    - `pel--store-with-package-quickstart'
;;
;; * `pel-setup-no-quickstart'
;;   - `pel--remove-package-quickstart-files'
;;   - `pel--store-with-package-quickstart'
;;   - `pel--setup-early-init'
;;     - `pel--update-emacs-user-file'


;; - Execute the `pel-setup-with-quickstart' command to set everything up: it
;;   sets the user-option in the custom file(s) and edit the init.el and
;;   early-init.el files.
;; - Restart Emacs.


;; ---------
(when pel-emacs-27-or-later-p

  (defvar pel--quickstart-forced-fname nil
    "Unless nil, forced name of package quickstart file.")

  (defun pel--package-qs (original-package-quickstart-refresh)
    "(Re)Generate the package quickstart file currently active."
    (if pel--quickstart-forced-fname
        ;; Force a different name for package-quickstart
        (if (boundp 'package-quickstart-file)
            (let ((original-fname package-quickstart-file))
              (setq package-quickstart-file pel--quickstart-forced-fname)
              (unwind-protect
                  (funcall original-package-quickstart-refresh)
                (setq package-quickstart-file original-fname)))
          (message
           "WARNING: The package-quickstart-file is unbound, preventing PEL\
 from controlling which file package-quickstart-refresh (re)generates!")
          (funcall original-package-quickstart-refresh))
      ;;
      ;; Use the normal package-quickstart.el file name.
      (funcall original-package-quickstart-refresh)))
  (declare-function pel--package-qs "pel-setup")

  ;; --

  (defun pel--build-package-quickstart (dirpath)
    "Utility: build package-quickstart.el for specific PEL mode and DIRPATH.

DIRPATH is the path of a ELpa-compliant directory used.
Normally that's the elpa directory inside the user-emacs-directory but
that can be the elpa-reduced directory for fast startup or then ones
for graphics mode when the dual mode is used.

When dual tty/graphics mode is supported, this function controls
the name of the package-quickstart.el using the function
`pel--adjusted-fname'. The caller must ensure the proper value for
`pel--adjust-path-for-graphics' is set."
    (if (and (require 'package nil :no-error)
             (fboundp 'package-quickstart-refresh)
             (boundp 'package-quickstart-file))
        (progn
          (advice-add 'package-quickstart-refresh :around #'pel--package-qs)
          (unwind-protect
              (let ((package-user-dir dirpath) ; force new package-user-dir
                    (package-alist (pel-elpa-package-alist-of-dir dirpath)))
                (setq pel--quickstart-forced-fname
                      (pel--adjusted-fname package-quickstart-file))
                (package-quickstart-refresh)
                ;; Byte-compile it if requested, otherwise remove its .elc
                (pel--compile-file-if pel--quickstart-forced-fname
                                      (and pel-compile-package-quickstart
                                           (pel-remove-no-byte-compile-in
                                            pel--quickstart-forced-fname))))
            (progn
              (advice-remove 'package-quickstart-refresh #'pel--package-qs)
              (setq pel--quickstart-forced-fname nil))))
      ;; report any error
      (error "Failed accessing package-quickstart")))
  (declare-function pel--build-package-quickstart "pel-setup")

  (defun pel--set-package-quickstart (dirpath for-graphics)
    "Utility: activate package quickstart for DIRPATH.

DIRPATH is the path of a Elpa-compliant directory used.
Normally that's the elpa directory inside the user-emacs-directory but
that can be the elpa-reduced directory for fast startup or then ones
for graphics mode when the dual mode is used.

FOR-GRAPHICS is:
- t when setting package quickstart for Emacs running in graphics mode and
  dual environment where graphic mode has a specific custom file,
- nil when Emacs runs in terminal/TTY mode or in graphics mode without dual
  environment set."
    (pel--build-package-quickstart (pel--adjusted-fname dirpath
                                                        :force for-graphics)))
  (declare-function pel--set-package-quickstart "pel-setup")

  (defun pel--setup-early-init (pkg-quickstart)
    "Create valid PEL early-init.el file and set its behaviour.

The behaviour is controlled by:
- PKG-QUICKSTART: t to activate package quickstart, nil to disable it.
- The value of `pel-init-detected-dual-environment-p' determines
  whether PEL is used with a dual environment for terminal/TTY and graphics
  mode or just a single one as usual for Emacs.
- The value of `pel-shell-detection-envvar' user-option determines what
  environment variable absence is used to detect GUI launched Emacs."
    (let ((early-init-fname (locate-user-emacs-file "early-init.el")))
      ;; Make sure an early-init file is present.
      (unless (file-exists-p early-init-fname)
        (copy-file pel-early-init-template
                   (locate-user-emacs-file "early-init.el")))
      (pel--update-emacs-user-file
       early-init-fname
       (list
        (cons 'pel-early-init-support-package-quickstart-p pkg-quickstart)
        (cons 'pel-early-init-support-dual-environment-p
              pel-init-detected-dual-environment-p)
        (cons 'pel-early-init-shell-detection-envvar
              pel-shell-detection-envvar))
       pel-compile-emacs-early-init)))
  (declare-function pel--setup-early-init "pel-setup")

  (defun pel--store-with-package-quickstart (value)
    "Set `pel-support-package-quickstart' to new VALUE globally and persistently.

Store value in all relevant custom file(s)."
    (setq pel-support-package-quickstart value)
    ;; store in default custom-file
    (pel-customize-save 'pel-support-package-quickstart value)
    (when pel-init-detected-dual-environment-p
      ;; store in the custom file of the other mode
      (pel-customize-save 'pel-support-package-quickstart value
                          (pel--other-custom-file))))
  (declare-function pel--store-with-package-quickstart "pel-setup")

  ;;-pel-autoload
  (defun pel-setup-with-quickstart ()
    "Activate package quickstart for current context.

The context includes the PEL startup mode and PEL's ability
to deal with independent customization for terminal and graphics mode.

This function:
- ensures that the early-init.el file identifies PEL activation
  of package quickstart by editing the file.
- creates or refreshes the package-quickstart.el file(s)."
    (interactive)
    (let ((startup-mode (pel--startup-mode)))
      (when (eq startup-mode 'inconsistent)
        (user-error "PEL startup mode is inconsistent.
  Please check and fix before activating the package quickstart!"))
      ;; All is fine: proceed.
      (message "Activating package quickstart...")
      (pel--setup-early-init t)
      (let ((elpa-dpath (pel-sibling-dirpath
                         pel-elpa-dirpath
                         (if (eq startup-mode 'fast)
                             "elpa-reduced"
                           "elpa-complete"))))
        (pel--set-package-quickstart elpa-dpath nil)
        (when pel-init-detected-dual-environment-p
          (pel--set-package-quickstart elpa-dpath t)))
      ;; Remember user setting: in pel-support-package-quickstart user-option
      (pel--store-with-package-quickstart t))
    ;; display state
    (pel-setup-info :now))

  (defun pel--remove-package-quickstart-files (for-graphics)
    "Remove package quickstart file identified by the FOR-GRAPHICS argument."
    (if (and (require 'package nil :no-error)
             (boundp 'package-quickstart-file))
        (let ((fname (pel--adjusted-fname package-quickstart-file
                                          :force for-graphics)))
          ;; remove the package-quickstart.el file
          (when (file-exists-p fname)
            (delete-file fname))
          ;; remove the corresponding .elc file if it exists.
          (setq fname (concat fname "c"))
          (when (file-exists-p fname)
            (delete-file fname)))
      (user-error "Cannot access package-quickstart-file variable!")))
  (declare-function pel--remove-package-quickstart-files "pel-setup")

  ;;-pel-autoload
  (defun pel-setup-no-quickstart ()
    "Disable package quickstart.
Support PEL startup modes and PEL dual independent customization files."
    (interactive)
    (message "Disabling package quickstart...")
    (pel--remove-package-quickstart-files nil)
    ;; when dual independent customization mode is used delete the
    ;; graphics specific files.
    (pel--remove-package-quickstart-files t)
    ;; Remember user setting:
    ;; - in pel-support-package-quickstart user-option
    (pel--store-with-package-quickstart nil)
    ;; - and inside the early-init.el
    (pel--setup-early-init nil)

    ;; display state
    (pel-setup-info :now)))

;; ---------------------------------------------------------------------------
;; elpa symlink control
;;  - - - - - - - - - -

(defun pel-switch-to-elpa-complete ()
  "Switch elpa symlink to the elpa-complete sub-directory."
  (interactive)
  (let* ((elpa-dirname
          (pel--adjusted-fname (directory-file-name pel-elpa-dirpath)))
         (elpa-complete-dirname
          (pel--adjusted-fname (directory-file-name
                                (pel-sibling-dirpath elpa-dirname
                                                     "elpa-complete")))))
    (pel-point-symlink-to elpa-dirname elpa-complete-dirname)))

(defun pel-switch-to-elpa-reduced ()
  "Switch elpa symlink to the elpa-reduced sub-directory."
  (let* ((elpa-dirname
          (pel--adjusted-fname (directory-file-name pel-elpa-dirpath)))
         (elpa-reduced-dirname
          (pel--adjusted-fname (directory-file-name
                               (pel-sibling-dirpath elpa-dirname
                                                 "elpa-reduced")))))
    (pel-point-symlink-to elpa-dirname elpa-reduced-dirname)))


(defun pel-remove-no-byte-compile-in (filename)
  "Remove the no-byte-compile file variable from FILENAME.
Return t when done, nil otherwise."
  (with-temp-file filename
    (auto-fill-mode -1)
    (when (file-exists-p filename)
      (insert-file-contents filename))
    (goto-char (point-min))
    (when (re-search-forward "^;+ +no-byte-compile: +t *$" nil :noerror)
      (pel-delete-whole-line)
      t)))

(defun pel-generate-autoload-file-for (dir)
  "Prepare (compile + autoload) all files in directory DIR.
Return the complete name of the generated autoload file."
  (require 'autoload)
  (let ((generated-fname nil))
    (if (boundp 'generated-autoload-file)
        (let ((original-generated-autoload-file  generated-autoload-file))
          (setq generated-autoload-file (expand-file-name
                                         "pel-bundle-autoloads.el"
                                         dir))
          (condition-case-unless-debug err
              (progn
                (update-directory-autoloads dir)
                (setq generated-fname generated-autoload-file)
                (kill-buffer "pel-bundle-autoloads.el"))
            (error
             (display-warning
              'pel-generate-autoload-file-for
              (format "Failed generating the %s/pel-bundle-autoloads.el: %s"
                      dir err)
              :error)))
          (setq generated-autoload-file original-generated-autoload-file))
      (error "The autoload.el variable generated-autoload-file isn't bounded!"))
    generated-fname))


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


(defvar pel-running-in-fast-startup-p) ; prevent byte-compiler warnings
(defun pel-bundled-mode (activate)
  "When ACTIVATE is non-nil activate PEL bundled mode, de-activate it otherwise.
Return a (activate . byte-compile result) cons cell."
  (setq pel-running-in-fast-startup-p activate)
  (cons  pel-running-in-fast-startup-p
         (byte-compile-file (concat (file-name-sans-extension
                                     (locate-library "pel_keys"))
                                    ".el"))))

(defun pel-setup-fast-startup-init (fname deps-pkg-versions-alist extra-code)
  "Write code in FNAME that adds the DEPS-PKG-VERSIONS-ALIST to Emacs.

The function writes the function pel-fast-startup-init.
That function code adds each entry of DEPS-PKG-VERSIONS-ALIST to Emacs
`package--builtin-versions'.  The DEPS-PKG-VERSIONS-ALIST list corresponds to
the package dependencies gathered from the X-pkg.el for each package X whose
code was placed in the elpa-reduced/pel-bundle *pseudo-package* that was not
already part of the `package--builtin-versions' list.  By adding those
package/version inside the `package--builtin-versions' list we ensure that
Emacs package.el logic will not attempt to download these packages.  We don't
need Emacs to download them because they have already been downloaded when
Emacs was in normal startup mode.  The DEPS-PKG-VERSIONS-ALIST list was
originally returned by `pel-elpa-disable-pkg-deps-in'."
  (with-temp-file fname
    (erase-buffer)
    (goto-char (point-min))
    (insert (format "\
;;; Setup Emacs for PEL fast startup.  -*- lexical-binding: t; -*-
;;; DO NOT EDIT! It will be overwritten next time pel-setup-fast is executed!

\(require 'package)

\(defvar pel-running-in-fast-startup-p nil)

\(defvar pel-fast-startup-builtin-packages
  '%S
  \"List of bundled package/versions to add to package--builtin-versions.\")

\(defvar pel-force-graphic-specific-files nil
  \"Force graphics specific files when non-nil. Set by `pel-fast-startup-init'.\")

;; pel-fast-startup-init must be called either inside early-init.el
;; (for Emacs >= 27) or inside init.el for older versions of Emacs.
;;
\(defun pel-fast-startup-init (&optional force-graphics from-early-init)
  \"Setup data to support the fast-startup mode.

- #1: Add pkg/version of the dependencies of packages whose code has been been
      bundled into elpa-reduced/pel-bundle *pseudo-package* to
      `package--builtin-versions' to prevent their downloads.
- #2: For Emacs >= 27, an extra step is required: add elpa-reduced/pel-bundle
      package to `load-path' when the function is executed during early-init
      because the `package-refresh' does not generate code to add the
      pel-bundle to the `load-path'.  This code is not required when the
      function is called from init or when Emacs is earlier than version 27.

Return the pkg/version alist.\"
  (setq pel-force-graphic-specific-files force-graphics)
  ;; step 1:
  (dolist (dep-ver pel-fast-startup-builtin-packages)
    (add-to-list 'package--builtin-versions dep-ver))
  %s
  ;; Return the list of package/versions corresponding to the bundled packages
  pel-fast-startup-builtin-packages)


;; ----
;; Prevent Emacs package.el logic from adding more package to download
;; by ensuring that `package-compute-transaction' returns an empty list.
;;
;; This is needed because in PEL fast-startup mode all external
;; packages have already been downloaded but they are not visible to
;; package.el logic at this point because the packages have been bundled
;; together inside the elpa-reduce/pel-bundle *pseudo-package*.

\(defun pel--pct (_packages)
  \"Filter packages to prevent downloads.\"
  nil)

\(advice-add  'package-compute-transaction  :filter-return (function pel--pct))

;; ----
;; Ensure that `package-load-all-descriptors' uses the graphics-specific
;; directory when forcing use of graphics specific files.  This is the case
;; when Emacs runs in graphics mode and PEL dual independent customization
;; feature is enabled.

;; The pel--graphic-file-name translates a file name to the graphics
;; specific name
\(defun pel--graphic-file-name (fname)
  \"Appends '-graphics' to the end of a .el, .elc or extension less FNAME.\"
  ;; use only functions implemented in C
  (let ((ext (substring fname -3)))
    (cond
     ((string-match \"-graphics\" fname) fname)
     ((string-equal ext \".el\") (concat (substring fname 0 -3) \"-graphics.el\"))
     ((string-equal ext \"elc\") (concat (substring fname 0 -4) \"-graphics.elc\"))
     (t                        (concat fname \"-graphics\")))))

\(defun pel--pkg-load-all-descriptors (original-fct)
  \"Execute ORIGINAL-FCT with a controlled value of `package-user-dir'.\"
  (let ((package-user-dir (if pel-force-graphic-specific-files
                              (pel--graphic-file-name package-user-dir)
                            package-user-dir)))
    (funcall original-fct)))

\(advice-add
 'package-load-all-descriptors :around (function pel--pkg-load-all-descriptors))

;; ---------------------------------------------------------------------------

" deps-pkg-versions-alist extra-code))))


;; --

(defun pel--fast-setup-met-criteria ()
  "Return a cons of 2 lists of strings: met-criteria and problems.
If the first list is nil then PEL/Emacs operates in normal mode.
If the first list has 3 members, then PEL/Emacs operates in fast startup mode."
  (let* ((pel--adjust-path-for-graphics pel-emacs-is-graphic-p)
         (met-criteria nil)
         (issues nil)
         (elpa-dirpath (pel--adjusted-fname pel-elpa-dirpath))
         (elpa-reduced-dirpath  (pel--adjusted-fname
                                 (pel-sibling-dirpath elpa-dirpath
                                                      "elpa-reduced"))))
    (if (pel-in-fast-startup-p)
        (push "Identified as fast startup by function `pel-in-fast-startup'"
              met-criteria)
      (pel-push-fmt issues "The `pel-in-fast-startup' is not set."))

    (if (file-exists-p pel-fast-startup-init-fname)
        (push (format "Fast startup setup file is present: %s"
                      pel-fast-startup-init-fname)
              met-criteria)
      (pel-push-fmt issues "The file %s indicating fast startup not found."
        pel-fast-startup-init-fname))
    (if (pel-symlink-points-to-p (directory-file-name elpa-dirpath)
                                 elpa-reduced-dirpath)
        (push "elpa symlink points to elpa-reduced" met-criteria)
      (pel-push-fmt issues "elpa symlink (%s) does not point to elpa-reduced (%s)"
        elpa-dirpath elpa-reduced-dirpath))
    (cons met-criteria issues)))

(defun pel--startup-mode ()
  "Return whether PEL/Emacs operates in fast startup mode.
Returns: 'normal, 'fast or 'inconsistent."
  (let ((count (length (car (pel--fast-setup-met-criteria)))))
    (cond ((eq count 0) 'normal)
          ((eq count 3) 'fast)
          (t 'inconsistent))))

(defun pel--setup-mode-description (for-graphics)
  "Describe the mode context of a setup.
The FOR-GRAPHICS argument identifies the setup forced for independent graphics."
  (if for-graphics
      "independent graphics mode"
    (if pel-init-detected-dual-environment-p
        "independent terminal/tty mode"
      "all modes")))

(defun pel--setup-fast (for-graphics)
  "Prepare the elpa directories and code to speedup Emacs startup.

The FOR-GRAPHICS argument is t when changing the environment for the
Emacs running in graphics mode and has a custom file that is independent from
the file used by Emacs running in terminal (TTY) mode.  It is nil when there
is only one or when its for the terminal (TTY) mode."
  (let ((step-count 0)
        (pel--adjust-path-for-graphics for-graphics)
        (cd-original (cd ".")))
    (condition-case-unless-debug err
        (let* ((pel-elpa-dirpath-adj (pel--adjusted-fname pel-elpa-dirpath))
               (pel-bundle-dirpath (pel-sibling-dirpath pel-elpa-dirpath
                                                        "pel-bundle"))
               (elpa-reduced-dirpath (pel--adjusted-fname
                                      (pel-sibling-dirpath pel-elpa-dirpath
                                                           "elpa-reduced")))
               (elpa-complete-dirpath (pel--adjusted-fname
                                       (pel-sibling-dirpath pel-elpa-dirpath
                                                            "elpa-complete")))
               (elpa-is-link (file-symlink-p (pel--adjusted-fname
                                              (directory-file-name
                                               pel-elpa-dirpath))))
               (time-stamp (format-time-string "%Y%m%d.%H%M"))
               (new-pel-bundle-dirpath (expand-file-name
                                        (format "pel-bundle-%s" time-stamp)
                                        elpa-reduced-dirpath)))
          ;; Ensure that elpa is a directory or a symlink to the required
          ;; elpa-complete. Otherwise abort.
          (when (and elpa-is-link
                     (not (pel-symlink-points-to-p (directory-file-name
                                                    (pel--adjusted-fname
                                                     pel-elpa-dirpath))
                                                   elpa-complete-dirpath)))
            (error "The %s symlink (%s) should point to the %s directory.
It points to %s instead! Aborting, fix the directory setting!
Compared: symlink %s to target %s.
- for-graphics          = %s
- pel-elpa-dirpath      = %s
- pel-elpa-dirpath-adj  = %s
- pel-bundle-dirpath    = %s
- elpa-reduced-dirpath  = %s
- elpa-complete-dirpath = %s
- elpa-is-link          = %s"
                   (pel--adjusted-fname "elpa")
                   pel-elpa-dirpath-adj
                   (pel--adjusted-fname "elpa-complete")
                   elpa-is-link
                   (directory-file-name
                    (pel--adjusted-fname
                     pel-elpa-dirpath))
                   elpa-complete-dirpath
                   for-graphics
                   pel-elpa-dirpath
                   pel-elpa-dirpath-adj
                   pel-bundle-dirpath
                   elpa-reduced-dirpath
                   elpa-complete-dirpath
                   elpa-is-link))
          (setq step-count (1+ step-count)) ; STEP 1

          ;; Ensure that pel-bundle directory does not exists.  That's a
          ;; temporary directory where all one-level package files are
          ;; stored and then used to create the -autoload.el and the
          ;; -pkg.el file before it is moved into the elpa-reduced
          ;; directory and renamed with a time stamp.  Issue a message
          ;; when the directory exists and delete it.
          (when (file-exists-p pel-bundle-dirpath)
            (delete-directory pel-bundle-dirpath :recursive)
            (message (format "Directory %s existed already; deleted it."
                             pel-bundle-dirpath)))
          (setq step-count (1+ step-count)) ; STEP 2
          ;;
          ;; Delete old elpa-reduced if it exists: it contains the old
          ;; pel-bundle and the multi-level packages that could not be
          ;; bundled in the previous execution of `pel-setup-fast'.
          (when (file-exists-p elpa-reduced-dirpath)
            (delete-directory elpa-reduced-dirpath :recursive))
          (setq step-count (1+ step-count)) ; STEP 3
          ;;
          ;; Create pel-bundle temporary directory to hold all one-level
          ;; package .el files.  At first create the directory as a sibling of
          ;; the elpa directory because elpa-reduced is not created yet.
          (make-directory pel-bundle-dirpath)
          (setq step-count (1+ step-count)) ; STEP 4
          (pel-elpa-create-copies pel-elpa-dirpath-adj pel-bundle-dirpath
                                  :with-symlinks)
          (setq step-count (1+ step-count)) ; STEP 5
          ;; Create the pel-bundle-pkg.el file inside it.
          (pel-create-bundle-pkg-file pel-bundle-dirpath time-stamp)
          (setq step-count (1+ step-count)) ; STEP 6
          ;;
          ;; Create the pel-bundle-autoloads.el file inside it.
          (cd pel-bundle-dirpath)
          (setq step-count (1+ step-count)) ; STEP 7
          ;; Build a pel-bundle-autoloads.el inside the pel-bundle
          ;; directory.
          (let ((autoload-fname (pel-generate-autoload-file-for
                                 pel-bundle-dirpath)))
            (setq step-count (1+ step-count)) ; STEP 8
            ;; Make the file byte-compilable (by removing restriction)
            (when (and pel-compile-pel-bundle-autoload
                       (pel-remove-no-byte-compile-in autoload-fname))
              ;; Then byte-compile it
              (byte-compile-file autoload-fname)))
          (setq step-count (1+ step-count)) ; STEP 9
          ;;
          (cd pel-elpa-dirpath-adj)
          (setq step-count (1+ step-count)) ; STEP 10
          ;;
          ;; Duplicate elpa inside elpa-reduced then remove the one-level
          ;; packages from it: they have been placed inside the pel-bundle
          ;; directory before.
          (pel-copy-directory pel-elpa-dirpath-adj elpa-reduced-dirpath)
          (setq step-count (1+ step-count)) ; STEP 11
          ;;
          ;; - Remove the one-level package sub-directories from
          ;;   elpa-reduced, only leaving the multi-directory packages in
          ;;   elpa-reduced.
          (pel-elpa-remove-pure-subdirs elpa-reduced-dirpath)
          (setq step-count (1+ step-count)) ; STEP 12
          ;;
          ;; Disable the dependencies of all (multi-directory) packages
          ;; left in the elpa-reduced directory, by calling the function
          ;; `pel-elpa-disable-pkg-deps-in' which returns an alist of (package
          ;; version) values identifying the dependencies of the packages left
          ;; in the elpa-reduced. We don't want Emacs to attempt the download
          ;; of these package since they have already been downloaded and
          ;; placed into the pel-bundle *pseudo package*.  So create code that
          ;; will run at init (or early-init) time in a Emacs lisp file that
          ;; will be detected and loaded by both early-init.el and init.el.
          ;; Call `pel-setup-fast-startup-init' to create the
          ;; pel-fast-startup-init.el file that will be used as an indication
          ;; to init and early-init that PEL fast-startup is requested.  These
          ;; files will call its function: `pel-fast-startup-init' and will
          ;; pass whether its invoked for graphics mode and whether it's
          ;; called from early-init.
          ;;
          ;; It's called by early-init for Emacs ≥ 27, and called by init for
          ;; earlier versions of Emacs, in both cases to add the (package
          ;; version) dependencies to simulate builtins by adding them to the
          ;; variable `package--builtin-versions' during init.el before the
          ;; call to `package-activate-all' or `package-initialize'.
          ;;
          ;; Only one instance of that pel-fast-startup-init.el file needs to
          ;; be created, even when dual independent customization is used.
          ;; So do it when for-graphics is nil.
          (unless for-graphics
            (pel-setup-fast-startup-init
             pel-fast-startup-init-fname
             (pel-elpa-disable-pkg-deps-in elpa-reduced-dirpath)
             (pel-string-when pel-emacs-27-or-later-p
                              (format ";; step 2: (only for Emacs >= 27)
  (when from-early-init
      (add-to-list 'load-path
                   (format \"%s\"
                           (if force-graphics \"-graphics\" \"\"))))"
                                      (replace-regexp-in-string
                                       "elpa-reduced"
                                       "elpa-reduced%s"
                                       new-pel-bundle-dirpath)))))
          (setq step-count (1+ step-count)) ; STEP 13
          ;;
          ;; Move the pel-bundle directory inside the elpa-reduced
          ;; directory: effectively creating a pel-bundle package
          ;; "pel-bundle" that contains all the files of the one-level
          ;; packages that were extracted from the original elpa directory
          ;; (the elpa-complete directory).  Give the pel-bundle directory
          ;; a version number corresponding to today's date.
          (rename-file (directory-file-name pel-bundle-dirpath)
                       new-pel-bundle-dirpath)
          (setq step-count (1+ step-count)) ; STEP 14
          ;; Re-organize the elpa directory:
          ;; If elpa is a directory and elpa-complete does not exist: then
          ;; rename elpa to elpa-complete.
          (when (and (file-directory-p pel-elpa-dirpath-adj)
                     (not (file-symlink-p (directory-file-name
                                           pel-elpa-dirpath-adj))))
            (when (file-exists-p elpa-complete-dirpath)
              (delete-directory elpa-complete-dirpath :recursive))
            (rename-file (directory-file-name pel-elpa-dirpath-adj)
                         (directory-file-name elpa-complete-dirpath)))
          (setq step-count (1+ step-count)) ; STEP 15
          ;; If there is a elpa symlink remove it and create a new one
          ;; that points to elpa-reduced
          (pel-switch-to-elpa-reduced)
          (setq step-count (1+ step-count)) ; STEP 16
          ;; Re-compile pel_keys.el with
          ;; `pel-running-in-fast-startup-p' bound to t to prevent PEL
          ;; from downloading and installing external packages while PEL
          ;; runs in PEL bundled mode.
          (pel-bundled-mode t)
          (setq step-count (1+ step-count)) ; STEP 17
          ;; handle package quickstart
          (when pel-emacs-27-or-later-p
            (pel--setup-early-init pel-support-package-quickstart)
            (setq step-count (1+ step-count)) ; STEP 18 (Emacs >= 27)
            (if pel-support-package-quickstart
                (pel--set-package-quickstart elpa-reduced-dirpath for-graphics)
              (pel--remove-package-quickstart-files for-graphics))
            (setq step-count (1+ step-count)))) ; STEP 19 (Emacs >= 27)
      (error
       (display-warning 'pel-setup-fast
                        (format "\
Failed fast startup setup for %s after %d of %d steps: %s
 Please inspect the %s directory to restore a valid setup.
 See pel-setup.el commentary for further information.
 Please also report the problem as a bug in the PEL Github project."
                                (pel--setup-mode-description for-graphics)
                                step-count
                                (if pel-emacs-27-or-later-p 19 17)
                                err
                                user-emacs-directory))))
    (cd cd-original)))

(defun pel--with-package-quickstart-p ()
  "Return t when package quickstart is currently used, nil otherwise."
  (and (require 'package nil :no-error)
       (boundp 'package-quickstart)
       package-quickstart
       (file-exists-p (locate-user-emacs-file "early-init.el"))
       (file-exists-p (locate-user-emacs-file
                       (pel--adjusted-fname "package-quickstart.el"
                                            :force pel-emacs-is-graphic-p)))))

(defun pel--with-quickstart-state-msg (prompt &optional show-requested-status)
  "Augment PROMPT to ask about package-quickstart if necessary.

If SHOW-REQUESTED-STATUS is specified the message should also display
the new requested status instead of the current state."
  (let ((current-quickstart-status (if show-requested-status
                                       pel-support-package-quickstart
                                     (pel--with-package-quickstart-p))))
    (if pel-emacs-27-or-later-p
        (format "%s %s package quickstart support"
                prompt
                (if current-quickstart-status
                    "with"
                  "without"))
      prompt)))

;;-pel-autoload
(defun pel-setup-fast ()
  "Prepare the elpa directories and code to speedup Emacs startup."
  (interactive)
  (if (eq (pel--startup-mode) 'fast)
      (error "PEL/Emacs is already setup for fast startup!")
    (when (y-or-n-p (pel--with-quickstart-state-msg
                     "Change to fast startup mode"
                     :show-requested-quickstart))
      ;; First setup the environment used by terminal (TTY) and graphics mode
      ;; when they both use the same
      (pel--setup-fast nil)
      ;; When graphics mode Emacs has its own customization, then also setup
      ;; the second environment; the one specific to Emacs running in graphics
      ;; mode.
      (when pel-init-detected-dual-environment-p
        (pel--setup-fast t))
      ;; inform user
      (message "Restart Emacs to complete switching to fast startup mode!%s"
               (pel-string-when
                pel-init-detected-dual-environment-p
                "\n Affects Emacs running in terminal and graphics mode!"))
      (setq pel--setup-changed t))))

;; --
(defun pel--setup-normal (for-graphics)
  "Restore normal PEL/Emacs operation mode.

The FOR-GRAPHICS argument is t when changing the environment for the
Emacs running in graphics mode and has a custom file that is independent from
the file used by Emacs running in terminal (TTY) mode.  It is nil when there
is only one or when its for the terminal (TTY) mode."
  (let ((pel--adjust-path-for-graphics for-graphics))
    ;; PEL is currently running in fast-startup mode. Switch back to normal.
    ;; Restore PEL's ability to download and install external packages
    (pel-bundled-mode nil)
    ;;  Restore the normal, complete Elpa directory.
    (pel-switch-to-elpa-complete)
    ;; Remove the file that is used to identify using fast-startup
    ;; but leave the elpa-reduced directory around in case some other
    ;; Emacs process is currently running in fast-start operation mode.
    (when (file-exists-p pel-fast-startup-init-fname)
      (delete-file pel-fast-startup-init-fname))
    (when pel-emacs-27-or-later-p
      (pel--setup-early-init pel-support-package-quickstart)
      (if pel-support-package-quickstart
          (pel--set-package-quickstart pel-elpa-dirpath for-graphics)
        (pel--remove-package-quickstart-files for-graphics)))))

;;-pel-autoload
(defun pel-setup-normal ()
  "Restore normal PEL/Emacs operation mode."
  (interactive)
  (if (eq (pel--startup-mode) 'normal)
      (error "PEL/Emacs is already using the normal setup!")
    (when (y-or-n-p (pel--with-quickstart-state-msg
                     "Restore normal startup mode"
                     :show-requested-quickstart))
      ;; First setup the environment used by terminal (TTY) and graphics mode
      ;; when they both use the same
      (pel--setup-normal nil)
      ;; When graphics mode Emacs has its own customization, then also setup
      ;; the second environment; the one specific to Emacs running in graphics
      ;; mode.
      (when pel-init-detected-dual-environment-p
        (pel--setup-normal t))
      ;; inform user
      (message "Restart Emacs to complete switching back to normal startup mode!%s"
               (pel-string-when
                pel-init-detected-dual-environment-p
                "\n Affects Emacs running in terminal and graphics mode!"))
      (setq pel--setup-changed t))))

;; --
;;-pel-autoload
(defun pel-setup-info (&optional with-now)
  "Display current state of PEL setup: whether normal or in fast startup."
  (interactive)
  (let ((mode (pel--startup-mode))
        (now-msg (if with-now "now " "")))
    (if pel--setup-changed
        (message "You already changed the setup to %s but did not stop Emacs!
 You may experience inconsistent Emacs behaviour.  It's best to restart Emacs!"
                 mode)
      (cond ((eq mode 'fast)
             (message "PEL/Emacs %soperates in fast startup mode%s.
 Emacs starts faster because single directory packages are bundled inside
 a single directory: %selpa-reduced/pel-bundle-YYYYMMDD.hhmm.
 However, in this setup mode, PEL is not able to install any external package.
 To install new package return to normal mode by executing pel-setup-normal."
                      now-msg
                      (pel--with-quickstart-state-msg "")
                      user-emacs-directory))
            ((eq mode 'normal)
             (message "PEL/Emacs %soperates in normal mode%s.
 In this mode Emacs packages are setup the way Emacs normally organizes them.
 You can install new packages and you can use PEL customization to add and
 install new package, or use pel-cleanup to remove the unused ones.
 With a large number of external packages Emacs normally starts slower
 because of the larger number of directories inside %selpa
 If you want to speed up Emacs startup execute pel-setup-fast."
                      now-msg
                      (pel--with-quickstart-state-msg "")
                      user-emacs-directory))
            (t
             (let* ((met-criteria.problems (pel--fast-setup-met-criteria))
                    (met-criteria (car met-criteria.problems))
                    (problems (cdr met-criteria.problems)))
               (user-error "PEL/Emacs mode is inconsistent!
 Check the elpa directory inside %s and restore normal setup.
 Only some conditions of a fast startup setup are met:
 - %s
 The following problem remain:
 - %s"
                           user-emacs-directory
                           (string-join met-criteria "\n - ")
                           (string-join problems "\n - "))))))))

;;; --------------------------------------------------------------------------
(provide 'pel-setup)

;;; pel-setup.el ends here

; LocalWords:  quickstart
