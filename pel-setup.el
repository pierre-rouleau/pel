;;; pel-setup.el --- Control PEL Emacs switch from normal to fast-startup mode and back.  -*- lexical-binding: t; -*-

;; Created   : Thursday, July  8 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-09-04 19:37:26, updated by Pierre Rouleau>

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
;; `pel--update-emacs-user-file' (in pel-setup-base.el) and its callers edit
;; the init.el and early-init.el: they set the value of defcustom forms which
;; control the Emacs package.el behaviour at startup.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;

(require 'pel--base)           ; use: `pel-unix-socket-p'
;;                             ;      `pel-string-when'
;;                             ;      `pel-sibling-dirpath'
;;                             ;      `pel-point-symlink-to'
;;                             ;      `pel-emacs-is-graphic-p'
(require 'pel--options)        ; use: `pel-compile-pel-bundle-autoload'
(require 'pel-custom)          ; use: `pel-customize-save'
(require 'pel-package)         ; use: `pel-elpa-dirpath'
(require 'pel-elpa)            ; use: `pel-elpa-create-copies'
;;                             ;      `pel-elpa-disable-pkg-deps-in'
;;                             ;      `pel-elpa-name'
(require 'pel-setup-base)      ; use: `pel--detected-dual-environment-in-init-p'
;;                             ;      `pel--other-custom-file'
;;                             ;      `pel--set-user-option'
;;                             ;      `pel-remove-no-byte-compile-in'
;;                             ;      `pel--update-emacs-user-file'
;;                             ;      `pel--create-early-init-if-missing'
;;                             ;      `pel-fast-startup-init-fname'
;;                             ;      `pel--fast-startup-setup-changed'
;;                             ;      `pel--setup-mode-description'
;;                             ;      `pel--fast-setup-met-criteria'
;;                             ;      `pel--startup-mode'
;;                             ;      `pel--with-quickstart-state-msg'

;; Then following functions are defined in pel-setup-27, which is
;; only used in Emacs >= 27.
;; The pel-emacs-27 file is only loaded in Emacs >= 27.
;; - `pel--set-dual-environment-in-emacs-early-init',
;; - `pel--setup-early-init',
;; - `pel--create-package-quickstart',
;; - `pel--remove-package-quickstart-file'.

(when pel-emacs-27-or-later-p
  (require 'pel-setup-27))

(eval-when-compile (require 'subr-x)) ; use: `string-join'
(require 'cus-edit)                   ; use: `custom-file'`

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defvar pel--detected-dual-environment-in-init-changed-p nil
  "Set when pel--detected-dual-environment-in-init-p is modified by code.")

;; ---------------------------------------------------------------------------
;; Local utilities
;; ---------------


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
;;   - `pel--update-emacs-user-file'                   (in pel-setup-base)
;;
;; - `pel--set-dual-environment-in-emacs-early-init'   (in pel-setup-27)
;;   - `pel--update-emacs-user-file'                   (in pel-setup-base)
;;
;; Package quickstart management, available for supporting Emacs 27 and later
;; also use the low level function to setup the early-init file in a code
;; section below.
;;
;; - `pel--setup-early-init'                           (in pel-setup-27)
;;   - `pel--update-emacs-user-file'                   (in pel-setup-base)


(defun pel--set-dual-environment-in-emacs-init (use)
  "Update Emacs user init.el for dual environment when USE is non-nil.

Set `pel-init-support-dual-environment-p' to t when use is
non-nil, to nil otherwise.  Byte compile the result file if the
`pel-compile-emacs-init' user-option is turned on."
  (pel--update-emacs-user-file
   "init.el"
   (list
    (list 'pel-init-support-dual-environment-p (not (null use))))
   pel-compile-emacs-init))

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
  (let* ((custom-fname    (pel-elpa-name custom-file nil))
         (custom-fname-g  (pel-elpa-name custom-file :for-graphics))
         (elpa-dp         (pel-elpa-name pel-elpa-dirpath nil))
         (elpa-dn         (directory-file-name elpa-dp))
         (elpa-dn-g       (pel-elpa-name elpa-dn :for-graphics))
         (elpa-cmplt-dn   (pel-sibling-dirname pel-elpa-dirpath "elpa-complete"))
         (elpa-cmplt-dn-g (pel-elpa-name elpa-cmplt-dn :for-graphics))
         (utils-dp        (pel-elpa-name pel-utils-dirpath nil))
         (utils-dp-g      (pel-elpa-name pel-utils-dirpath :for-graphics))
         (issues nil)
         ;; vertical align names by imposing a padding to all messages
         (pel-problems-text-length -35))
    ;;
    ;; For each: check if there are problems with files, directories and
    ;; symlinks. Push the list of problem descriptions in the issues list.
    ;;
    (pel-prepend-to issues (pel-file-problems custom-fname))
    (pel-prepend-to issues (pel-file-problems custom-fname-g))
    ;;
    (pel-prepend-to issues (pel-symlink-problems elpa-dn   "Directory"))
    (pel-prepend-to issues (pel-symlink-problems elpa-dn-g "Directory"))
    ;;
    (pel-prepend-to issues (pel-dir-problems elpa-cmplt-dn))
    (pel-prepend-to issues (pel-dir-problems elpa-cmplt-dn-g))
    ;;
    (pel-prepend-to issues (pel-dir-problems utils-dp))
    (pel-prepend-to issues (pel-dir-problems utils-dp-g))
    ;;
    (unless pel-support-dual-environment
      (pel-push-fmt issues "pel-support-dual-environment user-option not set"))
    (reverse issues)))

;;-pel-autoload
(defun pel-setup-info-dual-environment ()
  "Display current PEL customization setup.
Check two independent customization files for terminal/tty and graphics mode
are requested and if so check if they are setup properly.
Report an error and list problems if there are any, otherwise display the
current setup."
  (interactive)
  (pel-setup-validate-init-files)
  (if pel--detected-dual-environment-in-init-p
      (let ((problems (pel-dual-environment-problems)))
        (when pel--detected-dual-environment-in-init-changed-p
          (pel-push-fmt problems
              "Dual environment state was modified. Please restart Emacs."))
        (if problems
            (user-error
             (pel-format-problem-messages
              problems "However"
              "The file %s requests using dual tty/graphics customization."
              (locate-user-emacs-file "init.el")))
          (message  "PEL is ready to use 2 independent customization files:
 One for terminal/TTY: %s
 One for graphics    : %s"
                    (pel-elpa-name custom-file nil)
                    (pel-elpa-name custom-file :for-graphics))))
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

(defun pel--create-dir (gdname dname name)
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
  (let* ((custom-fn       (pel-elpa-name custom-file nil))
         (custom-fn-g     (pel-elpa-name custom-file :for-graphics))
         (elpa-dp         (pel-elpa-name pel-elpa-dirpath nil))
         (elpa-dn         (directory-file-name elpa-dp))
         (elpa-dn-g       (pel-elpa-name elpa-dn :for-graphics))
         (elpa-cmplt-dn   (pel-sibling-dirname pel-elpa-dirpath "elpa-complete"))
         (elpa-cmplt-dn-g (pel-elpa-name elpa-cmplt-dn :for-graphics))
         (utils-dn        (pel-elpa-name (directory-file-name pel-utils-dirpath)
                                          nil))
         (utils-dn-g      (pel-elpa-name (directory-file-name pel-utils-dirpath)
                                          :for-graphics))
         (actions nil))
    ;; 1:
    ;; Create a custom-file for graphics mode unless it already exists.
    (unless (file-exists-p custom-fn-g)
      (if (file-exists-p custom-fn)
          (progn
            (copy-file custom-fn custom-fn-g)
            (pel-push-fmt actions "Copied %s to %s " custom-fn custom-fn-g))
        (user-error "Expected customization file %s does not exists!"
                    custom-fn)))
    ;; 2:
    ;; Create a elpa directory for graphics mode unless it already exists.
    (pel-prepend-to actions
                    (pel--create-dir elpa-cmplt-dn-g elpa-cmplt-dn "elpa"))
    ;; 3:
    ;; Create the utils for graphics mode unless it already exists.
    (pel-prepend-to actions (pel--create-dir utils-dn-g utils-dn "utils"))
    ;; 4:
    ;; Rename the elpa directory to elpa-complete unless it's already done
    (when (and (file-exists-p elpa-dn)
               (not (file-symlink-p elpa-dn)))
      (when (file-exists-p elpa-cmplt-dn)
        (user-error "Both %s and %s exist! Manual cleanup required!"
                    elpa-dp elpa-cmplt-dn))
      (rename-file (directory-file-name elpa-dp) elpa-cmplt-dn)
      (pel-push-fmt actions "Renamed %s to %s" elpa-dp elpa-cmplt-dn))
    ;; 5:
    ;; Create the main elpa directory into a symlink to the elpa-complete
    ;; directory.
    (unless (file-symlink-p elpa-dn)
      (pel-point-symlink-to elpa-dn elpa-cmplt-dn)
      (pel-push-fmt actions "Created symlink %s pointing to %s"
        elpa-dn elpa-cmplt-dn))
    ;; 6:
    ;; Create the elpa-graphics symlink to the elpa-complete-graphics
    (unless (file-symlink-p elpa-dn-g)
      (pel-point-symlink-to elpa-dn-g elpa-cmplt-dn-g)
      (pel-push-fmt actions "Created symlink %s pointing to %s"
        elpa-dn-g elpa-cmplt-dn-g))
    ;; 7:
    ;; Update init.el: turn pel-init-support-dual-environment-p
    ;; inside init.el and early-init.el.
    ;; Set pel--detected-dual-environment-in-init-p to remember it locally.
    (condition-case err
        (progn
          (pel--set-dual-environment-in-emacs-init t)
          (when pel-emacs-27-or-later-p
            (declare-function pel--set-dual-environment-in-emacs-early-init
                              "pel-setup-27")
            (pel--set-dual-environment-in-emacs-early-init t))
          (setq pel--detected-dual-environment-in-init-p t)
          (setq pel--detected-dual-environment-in-init-changed-p t)
          (pel-push-fmt actions "Updated init.el%s. Please restart Emacs!"
            (pel-string-when pel-emacs-27-or-later-p " and early-init.el")))
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
  (pel-setup-validate-init-files)
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
;; elpa symlink control
;;  - - - - - - - - - -

(defun pel--switch-to-elpa (name for-graphics)
    "Switch elpa symlink to the elpa-NAME sub-directory.
The FOR_GRAPHICS is non-nil when dual environment is set and Emacs runs in
graphic mode."
    (let ((adj (lambda (fn) (pel-elpa-name fn for-graphics))))
      (pel-point-symlink-to (λc adj pel-elpa-dirpath)
       (λc adj (pel-sibling-dirpath pel-elpa-dirpath name)))))

(defun pel-switch-to-elpa-complete (for-graphics)
  "Switch elpa symlink to the elpa-complete sub-directory.
The FOR_GRAPHICS is non-nil when dual environment is set and Emacs runs in
graphic mode."
  (pel--switch-to-elpa "elpa-complete" for-graphics))

(defun pel-switch-to-elpa-reduced (for-graphics)
  "Switch elpa symlink to the elpa-reduced sub-directory."
  (pel--switch-to-elpa "elpa-reduced" for-graphics))

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


(defvar pel-running-in-fast-startup-p) ; Really defined in init.el.
;;                                     ; here: prevent byte-compiler warnings.
(defun pel-bundled-mode (activate)
  "Activate PEL bundled mode if ACTIVATE is non-nil, de-activate it otherwise.

This byte-compiles the pel_keys.el file with a new behaviour for the following
macros that control PEL's management of external package installation and
loading:
- `pel-install-github-files'
- `pel-install-github-file'
- `pel-ensure-pkg'
- `pel-ensure-package'

When ACTIVATE is non-nil, then the function `pel-in-fast-startup-p' returns t
which prevents these macros to emit code that check for presence of external
package and to load them.

Return a (activate . byte-compile result) cons cell."
  (setq pel-running-in-fast-startup-p activate)
  (cons pel-running-in-fast-startup-p
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
\(defun pel-fast-startup-init (&optional force-graphics using-package-quickstart)
  \"Setup data to support the fast-startup mode.

- #1: Add pkg/version of the dependencies of packages whose code has been been
      bundled into elpa-reduced/pel-bundle *pseudo-package* to
      `package--builtin-versions' to prevent their downloads.
- #2: For Emacs >= 27, when package-quickstart is used,  an extra step is
      required: add the elpa-reduced/pel-bundle-YYYMMMDD-hhmm package
      to `load-path' when the function is executed during early-init.
      This is needed because the package quickstart function `package-refresh'
      does not generate code to add the pel-bundle to the `load-path'.
      There's no need to add it when the function is called from init.el
      or from early-init.el when package quickstart is not used.

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
  \"Appends \\\"-graphics\\\" to the end of a .el, .elc or extension less FNAME.
  Also expands to the file true name, replacing symlinks by what they point to.\"
  ;; use only functions implemented in C or elisp available early.
  (let ((ext (substring fname -3)))
    (file-truename
     (cond
      ((string-match \"-graphics\" fname) fname)
      ((string-equal ext \".el\") (concat (substring fname 0 -3) \"-graphics.el\"))
      ((string-equal ext \"elc\") (concat (substring fname 0 -4) \"-graphics.elc\"))
      (t                        (concat fname \"-graphics\"))))))

\(defun pel--pkg-load-all-descriptors (original-fct)
  \"Execute ORIGINAL-FCT with a controlled value of `package-user-dir'.\"
  (let ((package-user-dir (if pel-force-graphic-specific-files
                              (pel--graphic-file-name package-user-dir)
                            (file-truename package-user-dir))))
    (funcall original-fct)))

\(advice-add
 'package-load-all-descriptors :around (function pel--pkg-load-all-descriptors))

;; ---------------------------------------------------------------------------

" deps-pkg-versions-alist extra-code))))


;; --

(defun pel--prepare-main-elpa-dir (for-graphics)
  "Transform elpa directory into a symlink to elpa-complete FOR-GRAPHICS.

The elpa directory is identified by the `pel-package-user-dir-original'
variable set by init.el before renaming the `package-user-dir' to control the
content of the `load-path'.
- If this identifies a directory:
  - If the name of that directory is already an elpa-complete directory,
    issue an error that explains the problem.
  - Otherwise, rename the directory to elpa-complete (or
    elpa-complete-graphics if FOR-GRAPHIC is non-nil) and then create a
    symlink that has the same name as the original directory and which points
    to the elpa-complete (or elpa-complete-graphics) directory.
- If this already is a symlink, then do nothing.

Return a list of performed action descriptions in reverse order."
  (if (boundp 'pel-package-user-dir-original)
      (let* ((elpa-dp       (pel-elpa-name pel-package-user-dir-original
                                            for-graphics))
             (elpa-dp-cmplt (pel-elpa-name (pel-sibling-dirpath
                                             elpa-dp "elpa-complete")
                                            for-graphics))
             (actions nil))
        (unless (or (file-symlink-p elpa-dp)
                    (bound-and-true-p package-quickstart))
          ;; the main elpa is a directory, not a symbolic link
          ;; make sure it does not already use the elpa-complete name;
          ;; Complain if it does.  However only perform this check when
          ;; not using package quickstart because in package quickstart I have
          ;; not found a way to identify the original package-user-dir which
          ;; identifies the symlink when fast start is used and then elpa-dp
          ;; ends up having the value of the symlink target.  TODO.
          (when (pel-same-fname-p elpa-dp elpa-dp-cmplt)
            (user-error "Invalid elpa directory in %s:
 The elpa directory name (%s) clash with PEL startup management strategy.
 PEL uses a symlink to points to either %s or %s.
 Please rename your elpa directory & update the package-user-dir user-option."
                        user-emacs-directory
                        elpa-dp
                        (pel-elpa-name "elpa-complete" for-graphics)
                        (pel-elpa-name "elpa-reduced"  for-graphics)))
          ;; all ok, rename and/or create symlink
          (when (file-exists-p elpa-dp)
            (rename-file (directory-file-name elpa-dp)
                         (directory-file-name elpa-dp-cmplt))
            (pel-push-fmt actions "Renamed %s to %s"
              elpa-dp elpa-dp-cmplt))
          (make-symbolic-link elpa-dp-cmplt elpa-dp)
          (pel-push-fmt actions
              "Created %s symlink that points to %s"
            elpa-dp elpa-dp-cmplt))
        actions)
    (user-error "Invalid init.el file detected:
  The `pel-package-user-dir-original' symbol is unknown.
  PEL cannot safely manage Emacs startup mode.
  Please update your init.el file; use pel/example/init/init-5.el template.")))

(defun pel--elpa-symlink-problems (elpa-dirpath for-graphics)
  "Check validity of ELPA_DIRPATH used FOR-GRAPHICS.

- The ELPA-DIRPATH should be the same as what is returned by the
  function `pel-locate-elpa' for standard Emacs operation (when FOR-GRAPHICS
  is nil).
- They should both identify a directory, not a file.
- The ELPA-DIRPATH and its dirname equivalent should exist.
- The ELPA-DIRPATH dirname equivalent should be a symlink
  pointing to a elpa-complete directory that exists.

The function verifies all that and return a list of problems detected if any
problem were detected. It returns nil if all is OK."
  (let* ((problems nil)
         (original-elpa-dirpath (pel-locate-elpa))
         (elpa-dirname  (pel-elpa-name (directory-file-name elpa-dirpath)
                                       for-graphics))
         (elpa-symlink  (file-symlink-p elpa-dirname))
         (elpa-complete-dirpath (pel-elpa-name
                                 (pel-sibling-dirpath elpa-dirpath
                                                      "elpa-complete")
                                 for-graphics)))
    ;;
    (unless for-graphics
      (unless (string= elpa-dirpath original-elpa-dirpath)
        (pel-push-fmt problems "Internal inconsistency between:
    - elpa-dirpath      : %s
    - its original value: %s"
          elpa-dirpath
          original-elpa-dirpath)))
    ;;
    (unless (directory-name-p elpa-dirpath)
      (pel-push-fmt problems "Internal inconsistency:
    - elpa-dirpath does not identify a directory but a file: %s"
        elpa-dirpath))
    ;;
    (if (and (not (file-exists-p elpa-dirpath))
             (not (file-symlink-p elpa-dirname)))
        (pel-push-fmt problems "\
The directory (or symlink to the directory) that should hold
   the external Elpa packages does not exist: %s
 - PEL infers it from the `package-user-dir' value stored in the
   customization file (%s).
   - That value is: %s"
          elpa-dirpath
          (pel-elpa-name custom-file for-graphics)
          original-elpa-dirpath)
      ;;
      ;; elpa exists, check its validity, but only do that
      ;;  when package quickstart is disabled as PEL is not yet capable of
      ;;  getting to the original package-user-dir when package quickstart is
      ;;  used. TODO.
      (unless (bound-and-true-p package-quickstart)
        (if elpa-symlink
            (progn
              (unless (pel-symlink-points-to-p elpa-dirname
                                               elpa-complete-dirpath)
                (pel-push-fmt problems "The elpa symlink target is invalid.
   - Current symlink target : %s
   - Expected symlink target: %s"
                  elpa-symlink
                  elpa-complete-dirpath))
              ;;
              (unless (file-name-absolute-p elpa-symlink)
                (pel-push-fmt problems
                    "The %s symlink target is not an absolute path:
   - Current symlink target : %s
   - Expected symlink target: %s
 Attempting a repair."
                  elpa-dirpath
                  elpa-symlink
                  elpa-complete-dirpath)
                ;; try to repair it
                (pel-point-symlink-to elpa-dirname elpa-complete-dirpath)
                (setq elpa-symlink (file-symlink-p elpa-dirname)))
              ;;
              (unless (directory-name-p elpa-symlink)
                (pel-push-fmt problems
                    "\
The elpa symlink target format does not use a directory name format:
   - Current symlink target : %s
   - Expected symlink target: %s
 Attempting a repair."
                  elpa-symlink
                  elpa-complete-dirpath)
                ;; try to repair it.
                (pel-point-symlink-to elpa-dirname elpa-complete-dirpath)))
          (pel-push-fmt problems "The elpa is not a symlink  : %s"
            elpa-dirname))))
    (reverse problems)))

(defun pel--validate-elpa-symlink (elpa-dirpath for-graphics )
  "Check validity of ELPA_DIRPATH used FOR-GRAPHICS.

The function raises en error describing detected errors if any.
If all is OK, it just returns nil."
  (let ((problems (pel--elpa-symlink-problems elpa-dirpath for-graphics)))
    (when problems
      (user-error
       (pel-format-problem-messages
        problems nil
        "\
The Emacs directory (%s) is not compatible with PEL startup management."
        user-emacs-directory)))))

(defun pel--setup-fast (for-graphics)
  "Prepare the elpa directories and code to speedup Emacs startup.

The FOR-GRAPHICS argument is t when changing the environment for the
Emacs running in graphics mode and has a custom file that is independent from
the file used by Emacs running in terminal (TTY) mode.  It is nil when there
is only one or when its for the terminal (TTY) mode."
  (let (;; define closures used to reduce visual clutter
        (adj (lambda (fn) (pel-elpa-name fn for-graphics)))
        (elpa-sibling (lambda (dp) (pel-sibling-dirpath pel-elpa-dirpath dp)))
        (step-count 0)
        (cd-original (cd ".")))
    (condition-case-unless-debug err
        (let* ((elpa-dp-adj      (λc adj pel-elpa-dirpath))
               (elpa-reduced-dp  (λc adj (λc elpa-sibling "elpa-reduced")))
               (elpa-complete-dp (λc adj (λc elpa-sibling "elpa-complete")))
               (elpa-complete-dp-adj (λc adj elpa-complete-dp))
               (bundle-dp        (λc elpa-sibling "pel-bundle"))
               (time-stamp       (format-time-string "%Y%m%d.%H%M"))
               (new-bundle-dp    (expand-file-name
                                  (format "pel-bundle-%s" time-stamp)
                                  elpa-reduced-dp))
               (actions nil))
          ;; Ensure that elpa is a symlink to an elpa-complete directory.
          ;; Transform the directory into a symlink to elpa-complete if
          ;; necessary. Remember what was done in the actions.
          ;; This will likely need to be done the very first time PEL fast
          ;; startup mode is activated.
          (pel-prepend-to actions (pel--prepare-main-elpa-dir for-graphics))
          (setq step-count (1+ step-count)) ; STEP 1
          ;;
          (pel--validate-elpa-symlink pel-elpa-dirpath for-graphics)
          (setq step-count (1+ step-count)) ; STEP 2
          ;;
          ;; The pel-bundle directory should not exists.  That's a
          ;; temporary directory where all one-level package files are
          ;; stored and then used to create the -autoload.el and the
          ;; -pkg.el file before it is moved into the elpa-reduced
          ;; directory and renamed with a time stamp.  Issue a message
          ;; when the directory exists and delete it.
          (when (file-exists-p bundle-dp)
            (delete-directory bundle-dp :recursive)
            (pel-push-fmt actions "Directory %s existed already; deleted it."
              bundle-dp))
          (setq step-count (1+ step-count)) ; STEP 3
          ;;
          ;; Delete old elpa-reduced if it exists: it contains the old
          ;; pel-bundle and the multi-level packages that could not be
          ;; bundled in the previous execution of `pel-setup-fast'.
          (when (file-exists-p elpa-reduced-dp)
            (delete-directory elpa-reduced-dp :recursive)
            (pel-push-fmt actions "Deleted old directory %s" elpa-reduced-dp))
          (setq step-count (1+ step-count)) ; STEP 4
          ;;
          ;; Create pel-bundle temporary directory to hold symlinks to all
          ;; one-level package .el[c] files located in the elpa complete
          ;; directory.  At first create the directory as a sibling of
          ;; the elpa directory because elpa-reduced is not created yet.
          (make-directory bundle-dp)
          (setq step-count (1+ step-count)) ; STEP 5
          (pel-elpa-create-copies elpa-complete-dp-adj bundle-dp :with-symlinks)
          (setq step-count (1+ step-count)) ; STEP 6
          ;; Create the pel-bundle-pkg.el file inside it.
          (pel-create-bundle-pkg-file bundle-dp time-stamp)
          (setq step-count (1+ step-count)) ; STEP 7
          ;; Create the pel-bundle-autoloads.el file inside it.
          (cd bundle-dp)
          (setq step-count (1+ step-count)) ; STEP 8
          ;; Make the autoloads file byte-compilable (by removing restriction)
          ;; and then byte-compile it.
          (let ((autoload-fname (pel-generate-autoload-file-for bundle-dp)))
            (setq step-count (1+ step-count)) ; STEP 9
            (when (and pel-compile-pel-bundle-autoload
                       (pel-remove-no-byte-compile-in autoload-fname))
              (byte-compile-file autoload-fname)))
          (setq step-count (1+ step-count)) ; STEP 10
          ;;
          (cd elpa-dp-adj)
          (setq step-count (1+ step-count)) ; STEP 11
          ;;
          ;; Duplicate elpa-complete inside elpa-reduced then remove the
          ;; one-level packages from elpa-reduced: they have been placed inside
          ;; the pel-bundle directory before.  Just leave the multi-directory
          ;; package directories inside the elpa-reduced directory.
          (pel-copy-directory elpa-dp-adj elpa-reduced-dp)
          (setq step-count (1+ step-count)) ; STEP 12
          (pel-elpa-remove-pure-subdirs elpa-reduced-dp)
          (setq step-count (1+ step-count)) ; STEP 13
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
             (pel-elpa-disable-pkg-deps-in elpa-reduced-dp)
             (pel-string-when pel-emacs-27-or-later-p
                              (format ";; step 2: (only for Emacs >= 27)
  (when using-package-quickstart
      (add-to-list 'load-path
                   (format \"%s\"
                           (if force-graphics \"-graphics\" \"\"))))"
                                      (replace-regexp-in-string
                                       "elpa-reduced"
                                       "elpa-reduced%s"
                                       new-bundle-dp)))))
          (setq step-count (1+ step-count)) ; STEP 14
          ;;
          ;; Move the pel-bundle directory inside the elpa-reduced
          ;; directory: effectively creating a pel-bundle package
          ;; "pel-bundle" that contains all the files of the one-level
          ;; packages that were extracted from the original elpa directory
          ;; (the elpa-complete directory).  Give the pel-bundle directory
          ;; a version number corresponding to today's date.
          (rename-file (directory-file-name bundle-dp) new-bundle-dp)
          (setq step-count (1+ step-count)) ; STEP 15
          ;;
          ;; If there is a elpa symlink remove it and create a new one
          ;; that points to elpa-reduced
          (pel-switch-to-elpa-reduced for-graphics)
          (setq step-count (1+ step-count)) ; STEP 16
          ;; Re-compile pel_keys.el with
          ;; `pel-running-in-fast-startup-p' bound to t to prevent PEL
          ;; from downloading and installing external packages while PEL
          ;; runs in PEL bundled mode.
          (pel-bundled-mode t)
          (setq step-count (1+ step-count)) ; STEP 17
          ;;
          ;; handle package quickstart when running Emacs ≥ 27
          (when pel-emacs-27-or-later-p
            (declare-function pel--setup-early-init "pel-setup-27")
            (declare-function pel--create-package-quickstart "pel-setup-27")
            (declare-function pel--remove-package-quickstart-files "pel-setup-27")
            (pel--setup-early-init pel-support-package-quickstart)
            (setq step-count (1+ step-count)) ; STEP 18 (Emacs >= 27)
            (if pel-support-package-quickstart
                (pel--create-package-quickstart elpa-reduced-dp for-graphics)
              (pel--remove-package-quickstart-files for-graphics))
            (setq step-count (1+ step-count))) ; STEP 19 (Emacs >= 27)
          (pel-message-for "Completed:" actions))
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



;;-pel-autoload
(defun pel-setup-fast ()
  "Prepare the elpa directories and code to speedup Emacs startup."
  (interactive)
  (pel-setup-validate-init-files)
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
      (when pel--detected-dual-environment-in-init-p
        (pel--setup-fast t))
      ;; inform user
      (message "Restart Emacs to complete switching to fast startup mode!%s"
               (pel-string-when
                pel--detected-dual-environment-in-init-p
                "\n Affects Emacs running in terminal and graphics mode!"))
      (setq pel--fast-startup-setup-changed t))))

;; --
(defun pel--setup-normal (for-graphics)
  "Restore normal PEL/Emacs operation mode.

The FOR-GRAPHICS argument is t when changing the environment for the
Emacs running in graphics mode and has a custom file that is independent from
the file used by Emacs running in terminal (TTY) mode.  It is nil when there
is only one or when its for the terminal (TTY) mode."
  ;; PEL is currently running in fast-startup mode. Switch back to normal.
  ;; Restore PEL's ability to download and install external packages
  (pel-bundled-mode nil)
  ;;  Restore the normal, complete, Elpa directory.
  (pel-switch-to-elpa-complete for-graphics)
  ;; Remove the file that is used to identify using fast-startup
  ;; but leave the elpa-reduced directory around in case some other
  ;; Emacs process is currently running in fast-start operation mode.
  (when (file-exists-p pel-fast-startup-init-fname)
    (delete-file pel-fast-startup-init-fname))
  (when pel-emacs-27-or-later-p
    (require 'pel-setup-27)
    (pel--setup-early-init pel-support-package-quickstart)
    (if pel-support-package-quickstart
        (pel--create-package-quickstart pel-elpa-dirpath for-graphics)
      (pel--remove-package-quickstart-files for-graphics))))

;;-pel-autoload
(defun pel-setup-normal ()
  "Restore normal PEL/Emacs operation mode."
  (interactive)
  (pel-setup-validate-init-files)
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
      (when pel--detected-dual-environment-in-init-p
        (pel--setup-normal t))
      ;; inform user
      (message "Restart Emacs to complete switching back to normal startup mode!%s"
               (pel-string-when
                pel--detected-dual-environment-in-init-p
                "\n Affects Emacs running in terminal and graphics mode!"))
      (setq pel--fast-startup-setup-changed t))))

;;; --------------------------------------------------------------------------
(provide 'pel-setup)

;;; pel-setup.el ends here

; LocalWords:  quickstart
