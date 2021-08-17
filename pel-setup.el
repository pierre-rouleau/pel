;;; pel-setup.el --- Control PEL Emacs switch from normal to fast-startup mode and back.  -*- lexical-binding: t; -*-

;; Created   : Thursday, July  8 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-08-17 15:42:36, updated by Pierre Rouleau>

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
;; not affect the other instances.
;;
;; PEL uses the built-in Emacs package management provided by the package.el
;; builtin library.  The fast-startup operation mode improves startup speed
;; and does not prevent using package.el features to explicitly install
;; packages in the current environment.  However, these packages will not be
;; known to PEL.  In fast-startup mode PEL disables its internal automatic
;; package management facilities and does not download, install nor remove
;; packages based on modification of the customization.
;;
;; The strategy is to reduce the length of Emacs `load-path' to a minimum.
;; This can therefore be used in conjunction of the Emacs 27+
;; `package-quickstart' feature to reduce Emacs startup time further.
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
;; inside on directory; they have no sub-directories.  I call those "one-level
;; packages" in opposition of the other Emacs Lisp packages that use
;; sub-directories to store other files.  From what I have seen so far most
;; Emacs external packages use only one directory.
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
;; Support for dual terminal/graphics mode customization
;; -----------------------------------------------------

;; If you need independent customization for Emacs running in terminal (TTY)
;; mode and in graphics mode, set `pel-use-graphic-specific-custom-file-p'
;; inside your init.el file as described by file example/init-5.el .
(defconst pel-used-with-independent-graphics-customization
  (and (boundp 'pel-use-graphic-specific-custom-file-p)
       pel-use-graphic-specific-custom-file-p)
  "Set to t if PEL is used with terminal and graphics mode customization.
Set to nil if only one customization file is used.
Code must NOT modify this value!")

(defun pel--dir-exists-p (dname)
  "Return t if DNAME exists and is a directory, nil if it does not exists.
Raise a user-error if DNAME exists and is not a directory."
  (when (file-exists-p dname)
    (unless (file-directory-p dname)
      (user-error "%s is not a directory!" dname))
    t))

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
         (problems nil))
    (unless (file-exists-p custom-fname)
      (push (format "File      missing : %s" custom-fname) problems))
    (unless (file-exists-p g-custom-fname)
      (push (format "File      missing : %s" g-custom-fname) problems))
    (if (file-exists-p elpa-dname)
        (unless (file-symlink-p elpa-dname)
          (push (format "Is not a symlink  : %s" elpa-dname) problems))
      (push (format "Directory missing : %s" elpa-dname) problems))
    (if (file-exists-p g-elpa-dname)
        (unless (file-symlink-p g-elpa-dname)
          (push (format "Is not a symlink  : %s" g-elpa-dname) problems))
      (push (format "Directory missing : %s" g-elpa-dname) problems))
    (if (file-exists-p elpa-complete-dname)
        (unless (file-directory-p elpa-complete-dname)
          (push (format "Is not a directory: %s" elpa-complete-dname) problems))
      (push (format "Directory missing : %s" elpa-complete-dname) problems))
    (if (file-exists-p g-elpa-complete-dname)
        (unless (file-directory-p g-elpa-complete-dname)
          (push (format "Is not a directory: %s" g-elpa-complete-dname) problems))
      (push (format "Directory missing : %s" g-elpa-complete-dname) problems))
    (if (file-exists-p utils-dpath)
        (unless (file-directory-p utils-dpath)
          (push (format "Is not a directory: %s" utils-dpath) problems))
      (push (format "Directory missing : %s" utils-dpath) problems))
    (if (file-exists-p g-utils-dpath)
        (unless (file-directory-p g-utils-dpath)
          (push (format "Is not a directory: %s" g-utils-dpath) problems))
      (push (format "Directory missing : %s" g-utils-dpath) problems))
    (unless pel-used-with-independent-graphics-customization
      (push (format "Please set pel-use-graphic-specific-custom-file-p to t \
in OPTION A code inside the file %s" (locate-user-emacs-file "init.el"))
            problems))
    (reverse problems)))

(defun pel--create-dir (gdname dname name )
  "Copy GDNAME to DNAME which abbreviates to NAME unless it exists.
Return a list of performed actions."
  (let ((completed-actions nil)
        (dpath (file-name-as-directory dname)))
    (unless (pel--dir-exists-p gdname)
      (if (pel--dir-exists-p dname)
          (progn
            (pel-copy-directory dname gdname)
            (push (format "Copied %s to %s" dname gdname) completed-actions))
        (if (pel--dir-exists-p dpath)
            (progn
              (pel-copy-directory dpath gdname)
              (push (format "Copied %s to %s" dpath gdname) completed-actions))
          (user-error "Can't find %s directory.  Looked for:\n- %s\n- %s"
                      name dname dpath))))
    completed-actions))


;;-pel-autoload
(defun pel-setup-info-dual-environment ()
  "Display current PEL customization setup.
Check two independent customization files for terminal/tty and graphics mode
are requested and if so check if they are setup properly.
Report an error and list problems if there are any, otherwise display the
current setup."
  (interactive)
  (if (and  (boundp 'pel-use-graphic-specific-custom-file-p)
            pel-use-graphic-specific-custom-file-p)
      (let ((problems (pel-dual-environment-problems)))
        (if problems
            (let ((problem-count (length problems)))
              (user-error "\
The file %s is requesting the use of dual tty/graphics customization.
 However the following %s %s:\n - %s"
                          (locate-user-emacs-file "init.el")
                          (pel-count-string problem-count "problem" nil :no-count-for-1)
                          (pel-pluralize problem-count "remains" "remain")
                          (string-join problems "\n - ")))
          (message  "PEL is ready to use 2 independent customization files:
 One for terminal/TTY: %s
 One for graphics    : %s"
                    (pel--adjusted-fname custom-file :force nil)
                    (pel--adjusted-fname custom-file :force :for-graphics))))
    (message "PEL is currently using a single customization file: %s" custom-file)))

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
  (when (y-or-n-p "Activate independent customization for terminal & graphics\
 mode? ")
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
           (completed-actions nil))
      ;;
      ;; Create a custom-file for graphics mode unless it already exists.
      (unless (file-exists-p g-custom-fname)
        (if (file-exists-p custom-fname)
            (progn
              (copy-file custom-fname g-custom-fname)
              (push (format "Copied %s to %s " custom-fname g-custom-fname)
                    completed-actions))
          (user-error "Expected customization file %s does not exists!"
                      custom-fname)))
      ;;
      ;; Create a elpa directory for graphics mode unless it already exists.
      (setq completed-actions
            (append (pel--create-dir g-elpa-complete-dname elpa-complete-dname "elpa")
                    completed-actions))
      ;;
      ;; Create the utils for graphics mode unless it already exists.
      (setq completed-actions
            (append (pel--create-dir g-utils-dname utils-dname "utils")
                    completed-actions))
      ;;
      ;; Rename the elpa directory to elpa-complete unless it's already done
      (when (and (file-exists-p elpa-dname)
                 (not (file-symlink-p elpa-dname)))
        (when (file-exists-p elpa-complete-dname)
          (user-error "Both %s and %s exist! Manual cleanup required!"
                      elpa-dpath elpa-complete-dname))
        (rename-file (directory-file-name elpa-dpath) elpa-complete-dname)
        (push (format "Renamed %s to %s" elpa-dpath elpa-complete-dname)
              completed-actions))
      ;;
      ;; Create the main elpa directory into a symlink to the elpa-complete
      ;; directory.
      (unless (file-symlink-p elpa-dname)
        (pel-point-symlink-to elpa-dname elpa-complete-dname)
        (push (format "Created symlink %s pointing to %s"
                      elpa-dname elpa-complete-dname)
              completed-actions))
      ;;
      ;; Create the elpa-graphics symlink to the elpa-complete-graphics
      (unless (file-symlink-p g-elpa-dname)
        (pel-point-symlink-to g-elpa-dname g-elpa-complete-dname)
        (push (format "Created symlink %s pointing to %s"
                      g-elpa-dname g-elpa-complete-dname)
              completed-actions))
      ;;
      ;; Display performed actions.
      (let ((remaining-problems (pel-dual-environment-problems))
            (done-text (when completed-actions
                         (format "Completed the following:\n- %s"
                                 (string-join
                                  (reverse completed-actions) "\n- ")))))
        (if remaining-problems
            (let ((remaining-actions (format "some problems remain.\
 Please fix them manually:\n- %s" (string-join remaining-problems "\n- "))))
              (if completed-actions
                  (user-error "%s\n Unfortunately %s"
                              done-text
                              remaining-actions)
                (user-error "Nothing can be done since %s" remaining-actions)))
          (if completed-actions
              (message "%s\n All is now OK!" done-text)
            (message "Nothing to do, it's already setup.")))))))

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

(when (>= emacs-major-version 27)

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
                (when (and pel-compile-package-quickstart
                           (pel-remove-no-byte-compile-in
                            pel--quickstart-forced-fname))
                  ;; Byte-Compile it if requested
                  (byte-compile-file pel--quickstart-forced-fname)))
            (progn
              (advice-remove 'package-quickstart-refresh #'pel--package-qs)
              (setq pel--quickstart-forced-fname nil))))
      ;; report any error
      (error "Failed accessing package-quickstart")))
  (declare-function pel--build-package-quickstart "pel-setup")

  (defun pel--activate-package-quickstart (dirpath for-graphics)
    "Utility: activate package quickstart.

DIRPATH is the path of a Elpa-compliant directory used.
Normally that's the elpa directory inside the user-emacs-directory but
that can be the elpa-reduced directory for fast startup or then ones
for graphics mode when the dual mode is used.

The FOR-GRAPHICS argument is t when changing the environment for the
Emacs running in graphics mode and has a custom file that is independent from
the file used by Emacs running in terminal (TTY) mode.  It is nil when there
is only one or when its for the terminal (TTY) mode."
    (let ((pel--adjust-path-for-graphics for-graphics))
      (pel--build-package-quickstart dirpath)))
  (declare-function pel--activate-package-quickstart "pel-setup")

  (defun pel--setup-early-init (&optional error-if-exists)
    "Ensure that a valid early-init.el file is available."
    (let ((early-init-fname (locate-user-emacs-file "early-init.el")))
      (if (file-exists-p early-init-fname)
          (when error-if-exists
            (user-error "The file %s already exists!
 If you want to keep it please see `pel-early-init-with-package-quickstart'
 and `pel-early-init-without-package-quickstart' for instructions."
                        early-init-fname))
        ;; All is fine: no early-init.el file yet.  Proceed.
        (copy-file pel-early-init-with-package-quickstart
                   (locate-user-emacs-file "early-init.el")))))
  (declare-function pel--setup-early-init "pel-setup")

  ;;-pel-autoload
  (defun pel-setup-package-quickstart ()
    "Activate package quickstart for current context.

The context includes the PEL startup mode and PEL's ability
to deal with independent customization for terminal and graphics mode.

This function copies the file identified by the user-option variable
`pel-early-init-with-package-quickstart' your early-init.el and creates
or refreshes the package-quickstart.el file(s)."
    (interactive)
    (let ((startup-mode (pel--startup-mode)))
      (when (eq startup-mode 'inconsistent)
        (user-error "PEL startup mode is inconsistent.
  Please check and fix before activating the package quickstart!"))
      ;; All is fine: proceed.
      (pel--setup-early-init :error-if-exists)
      (let ((elpa-dpath (pel-sibling-dirpath
                         pel-elpa-dirpath
                         (if (eq startup-mode 'fast)
                             "elpa-reduced"
                           "elpa-complete"))))
        (pel--activate-package-quickstart elpa-dpath nil)
        (when pel-used-with-independent-graphics-customization
          (pel--activate-package-quickstart elpa-dpath t)))
      ;; Remember user setting: in pel-with-package-quickstart user-option
      ;; TODO save in current custom file
      (setq pel-with-package-quickstart t)))

  (defun pel--remove-package-quickstart-files (for-graphics
                                               &optional expect-early-init)
    "Remove package quickstart files identified by the FOR-GRAPHICS argument."
    (if (and (require 'package nil :no-error)
             (boundp 'package-quickstart-file))
        (let ((early-init-fname (locate-user-emacs-file "early-init.el"))
              (fname))
          (unless for-graphics
            (when (and expect-early-init
                       (not (file-exists-p early-init-fname)))
              (user-error "Package quickstart is not active.
 No %s file found to activate it."
                          early-init-fname))
            ;; replace or remove early-init.el as requested by user-option
            (if pel-early-init-without-package-quickstart
                (copy-file pel-early-init-without-package-quickstart
                           early-init-fname)
              (delete-file early-init-fname)))
          ;; remove the package-quickstart.el file and the .elc file
          ;; if it exists.
          (setq fname (pel--adjusted-fname package-quickstart-file
                                           :force for-graphics))
          (when (file-exists-p fname)
            (delete-file fname))
          (setq fname (concat fname "c"))
          (when (file-exists-p fname)
            (delete-file fname)))
      (user-error "Cannot access package-quickstart-file variable!")))
  (declare-function pel--remove-package-quickstart-files "pel-setup")

  ;;-pel-autoload
  (defun pel-setup-without-package-quickstart ()
    "Disable package quickstart.
Support PEL startup modes and PEL dual independent customization files."
    (interactive)
    (pel--remove-package-quickstart-files nil)
    ;; when dual independent customization mode is used delete the
    ;; graphics specific files.
    (pel--remove-package-quickstart-files t)
    ;; Remember user setting: in pel-with-package-quickstart user-option
    ;; TODO save in current custom file
    (setq pel-with-package-quickstart nil)))

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

;; pel-fast-startup-init must be called either inside early-init.el
;; (for Emacs >= 27) or inside init.el for older versions of Emacs.
;;
\(defun pel-fast-startup-init (&optional force-graphics)
  \"Setup data to support the fast-startup mode.

- #1: Add pkg/version of the dependencies of packages whose code has been been
      bundled into elpa-reduced/pel-bundle *pseudo-package* to
      `package--builtin-versions' to prevent their downloads.
- #2: For Emacs >= 27, an extra step is required: add elpa-reduced/pel-bundle
      package to load-path because for those versions of Emacs this must be
      done during early-init (which calls pel-fast-startup-init).
      For Emacs earlier than 27 this last step is not performed and its done
      inside the init.el file.

Return the pkg/version alist.\"
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

;; ---------------------------------------------------------------------------

" deps-pkg-versions-alist extra-code))))


;; --

(defun pel--fast-setup-met-criteria ()
  "Return a cons of 2 lists of strings: met-criteria and problems.
If the first list is nil then PEL/Emacs operates in normal mode.
If the first list has 3 members, then PEL/Emacs operates in fast startup mode."
  (let* ((pel--adjust-path-for-graphics pel-emacs-is-graphic-p)
         (met-criteria nil)
         (problems nil)
         (elpa-dirpath (pel--adjusted-fname pel-elpa-dirpath))
         (elpa-reduced-dirpath  (pel--adjusted-fname (pel-sibling-dirpath elpa-dirpath
                                                                         "elpa-reduced"))))
    (if (pel-in-fast-startup-p)
        (push "Identified as fast startup by function `pel-in-fast-startup'"
              met-criteria)
      (push "The `pel-in-fast-startup' is not set." problems))

    (if (file-exists-p pel-fast-startup-init-fname)
        (push (format "Fast startup setup file is present: %s"
                      pel-fast-startup-init-fname)
              met-criteria)
      (push (format "The file %s indicating fast startup not found."
                    pel-fast-startup-init-fname)
            problems))
    (if (pel-symlink-points-to-p (directory-file-name elpa-dirpath)
                                 elpa-reduced-dirpath)
        (push "elpa symlink points to elpa-reduced" met-criteria)
      (push (format "elpa symlink (%s) does not point to elpa-reduced (%s)"
                    elpa-dirpath
                    elpa-reduced-dirpath)
            problems))
    (cons met-criteria problems)))

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
    (if pel-used-with-independent-graphics-customization
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
        (let* ((pel-elpa-dirpath-adj (pel--adjusted-fname
                                      pel-elpa-dirpath))
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
          ;; elpa-complete ;; otherwise abort.
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
          (setq step-count (1+ step-count))

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
          (setq step-count (1+ step-count))
          ;;
          ;; Delete old elpa-reduced if it exists: it contains the old
          ;; pel-bundle and the multi-level packages that could not be
          ;; bundled in the previous execution of `pel-setup-fast'.
          (when (file-exists-p elpa-reduced-dirpath)
            (delete-directory elpa-reduced-dirpath :recursive))
          (setq step-count (1+ step-count))
          ;;
          ;; Create pel-bundle temporary directory to hold all one-level
          ;; package .el files.  At first create it the directory as a
          ;; sibling of the elpa directory because elpa-reduced is not
          ;; created yet.
          (make-directory pel-bundle-dirpath)
          (setq step-count (1+ step-count))
          (pel-elpa-create-copies pel-elpa-dirpath-adj pel-bundle-dirpath
                                  :with-symlinks)
          (setq step-count (1+ step-count))
          ;; Create the pel-bundle-pkg.el file inside it.
          (pel-create-bundle-pkg-file pel-bundle-dirpath time-stamp)
          (setq step-count (1+ step-count))
          ;;
          ;; Create the pel-bundle-autoloads.el file inside it.
          (cd pel-bundle-dirpath)
          (setq step-count (1+ step-count))
          ;; Build a pel-bundle-autoloads.el inside the pel-bundle
          ;; directory.
          (let ((autoload-fname (pel-generate-autoload-file-for
                                 pel-bundle-dirpath)))
            (setq step-count (1+ step-count))
            ;; Make the file byte-compilable (by removing restriction)
            (when (and pel-compile-pel-bundle-autoload
                       (pel-remove-no-byte-compile-in autoload-fname))
              ;; Then byte-compile it
              (byte-compile-file autoload-fname)))
          (setq step-count (1+ step-count))
          ;;
          (cd pel-elpa-dirpath-adj)
          (setq step-count (1+ step-count))
          ;;
          ;; Duplicate elpa inside elpa-reduced then remove the one-level
          ;; packages from it: they have been placed inside the pel-bundle
          ;; directory before.
          (pel-copy-directory pel-elpa-dirpath-adj elpa-reduced-dirpath)
          (setq step-count (1+ step-count))
          ;;
          ;; - Remove the one-level package sub-directories from
          ;;   elpa-reduced, only leaving the multi-directory packages in
          ;;   elpa-reduced.
          (pel-elpa-remove-pure-subdirs elpa-reduced-dirpath)
          (setq step-count (1+ step-count))
          ;;
          ;; Disable the dependencies of all (multi-directory) packages
          ;; left in the elpa-reduced directory.  This returns an alist of
          ;; (package version) that should be added to the variable
          ;; `package--builtin-versions' during init.el before the call to
          ;; `package-activate-all' or `package-initialize'.  In Emacs ≥
          ;; 27 it must be set in early-init.el.
          (pel-setup-fast-startup-init
           pel-fast-startup-init-fname
           (pel-elpa-disable-pkg-deps-in elpa-reduced-dirpath)
           (pel-string-when (>= emacs-major-version 27)
                            (format ";; step 2: (only for Emacs >= 27)
  (push (format \"%s\"
                (if force-graphics \"-graphics\" \"\"))
        load-path)"
                                    (replace-regexp-in-string
                                     "elpa-reduced"
                                     "elpa-reduced%s"
                                     new-pel-bundle-dirpath) )))
          (setq step-count (1+ step-count))
          ;;
          ;; Move the pel-bundle directory inside the elpa-reduced
          ;; directory: effectively creating a pel-bundle package
          ;; "pel-bundle" that contains all the files of the one-level
          ;; packages that were extracted from the original elpa directory
          ;; (the elpa-complete directory).  Give the pel-bundle directory
          ;; a version number corresponding to today's date.
          (rename-file (directory-file-name pel-bundle-dirpath)
                       new-pel-bundle-dirpath)
          (setq step-count (1+ step-count))
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
          (setq step-count (1+ step-count))
          ;; If there is a elpa symlink remove it and create a new one
          ;; that points to elpa-reduced
          (pel-switch-to-elpa-reduced)
          (setq step-count (1+ step-count))
          ;; Re-compile pel_keys.el with
          ;; `pel-running-in-fast-startup-p' bound to t to prevent PEL
          ;; from downloading and installing external packages while PEL
          ;; runs in PEL bundled mode.
          (pel-bundled-mode t)
          (setq step-count (1+ step-count))
          ;; With Emacs ≥ 27 if package-quickstart is used more work is
          ;; required: package-quickstart-refresh must be done while
          ;; package-alist includes all packages now in elpa (which is
          ;; pel-reduced). Support dual tty/graphics mode by processing
          ;; a file with modified name if required.
          (when (>= emacs-major-version 27)
            (if pel-with-package-quickstart
                ;; package-quickstart is requested: create/update files
                (progn
                  ;; create early-init if it's missing
                  (pel--setup-early-init)
                  ;; create/update the package-quickstart
                  (pel--activate-package-quickstart
                   (pel--adjusted-fname elpa-reduced-dirpath)
                   for-graphics))
              ;; package-quickstart is not requested: delete files not needed
              (pel--remove-package-quickstart-files for-graphics)))
          (setq step-count (1+ step-count)))
      (error
       (display-warning 'pel-setup-fast
                        (format "\
Failed fast startup setup for %s after %d of %d steps: %s
 Please inspect the %s directory to restore a valid setup.
 See pel-setup.el commentary for further information.
 Please also report the problem as a bug in the PEL Github project."
                                (pel--setup-mode-description for-graphics)
                                step-count
                                18
                                err
                                user-emacs-directory))))
    (cd cd-original)))

(defun pel--with-package-quickstart-p ()
  "Return t when package quickstart is currently used, nil otherwise."
  (and (require 'package nil :no-error)
       (boundp 'package-quickstart)
       package-quickstart))

(defun pel--with-quickstart-state-msg (prompt &optional show-requested-status)
  "Augment PROMPT to ask about package-quickstart if necessary.

If SHOW-REQUESTED-STATUS is specified the message should also display
the new requested status instead of the current state."
  (let ((current-quickstart-status (if show-requested-status
                                       pel-with-package-quickstart
                                     (pel--with-package-quickstart-p))))
    (if (>= emacs-major-version 27)
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
      (when pel-used-with-independent-graphics-customization
        (pel--setup-fast t))
      ;; inform user
      (message "Restart Emacs to complete switching to fast startup mode!%s"
               (pel-string-when
                pel-used-with-independent-graphics-customization
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
    ;; With Emacs ≥ 27 if package-quickstart is used more work is required:
    ;; package-quickstart-refresh must be done while package-alist
    ;; includes all packages now in elpa (which is elpa and elpa-complete).
    (when (>= emacs-major-version 27)
      (if pel-with-package-quickstart
          ;; package-quickstart is requested: create/update files
          (progn
            ;; create early-init if it's missing
            (pel--setup-early-init)
            ;; create/update the package-quickstart
            (pel--build-package-quickstart
             (pel--adjusted-fname pel-elpa-dirpath)))
        ;; package-quickstart is not requested: delete files not needed
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
      (when pel-used-with-independent-graphics-customization
        (pel--setup-normal t))
      ;; inform user
      (message "Restart Emacs to complete switching back to normal startup mode!%s"
               (pel-string-when
                pel-used-with-independent-graphics-customization
                "\n Affects Emacs running in terminal and graphics mode!"))
      (setq pel--setup-changed t))))

;; --
;;-pel-autoload
(defun pel-setup-info ()
  "Display current state of PEL setup: whether normal or in fast startup."
  (interactive)
  (let* ((mode (pel--startup-mode)))
    (if pel--setup-changed
        (message "You already changed the setup to %s but did not stop Emacs!
 You may experience inconsistent Emacs behaviour.  It's best to restart Emacs!"
                 mode)
      (cond ((eq mode 'fast)
             (message "PEL/Emacs operates in fast startup mode%s.
 Emacs starts faster because single directory packages are bundled inside
 a single directory: %selpa-reduced/pel-bundle-YYYYMMDD.hhmm.
 However, in this setup mode, PEL is not able to install any external package.
 To install new package return to normal mode by executing pel-setup-normal."
                      (pel--with-quickstart-state-msg "")
                      user-emacs-directory))
            ((eq mode 'normal)
             (message "PEL/Emacs operates in normal mode%s.
 In this mode Emacs packages are setup the way Emacs normally organizes them.
 You can install new packages and you can use PEL customization to add and
 install new package, or use pel-cleanup to remove the unused ones.
 With a large number of external packages Emacs normally starts slower
 because of the larger number of directories inside %selpa
 If you want to speed up Emacs startup execute pel-setup-fast."
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
