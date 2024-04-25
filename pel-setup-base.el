;;; pel-setup-base.el --- PEL setup base logic used by Emacs >= 27 but also in previous version.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, August 31 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2024-04-25 16:04:40 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2024  Pierre Rouleau
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
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)           ; use: `pel-emacs-27-or-later-p'
;;                             ;      `pel-string-when'
(require 'pel--options)        ; use: `pel-early-init-template'
(require 'pel-ccp)             ; use: `pel-delete-whole-line'
(require 'pel-custom)          ; use: `pel-customize-save'
(require 'pel-elpa)            ; use: `pel-elpa-name'
(require 'pel-package)         ; use: `pel-elpa-dirpath'

(eval-when-compile (require 'subr-x)) ; use: `string-join'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defconst pel--detected-dual-environment-in-init-p
  (bound-and-true-p pel-init-support-dual-environment-p)
  "Identifies whether PEL uses dual environment -- as detected by init.el.

When using the dual environment, PEL has 2 independent sets of
customization files and package directories: one when Emacs runs
in terminal/TTY mode and another when Emacs runs in graphic mode.

Code can only modify this value after successfully changing
`pel-init-support-dual-environment-p' inside the init.el file and
early-init.el file (on Emacs >= 27).
In the current code this is only done by `pel--setup-dual-environment'")

;; ---------------------------------------------------------------------------
;; Emacs Initialisation file validation
;; ------------------------------------

(defconst pel--expected-init-file-version "0.2"
  "Must match what is in the example/init/init.el")

(defconst pel--expected-early-init-file-version "0.2"
  "Must match what is in the example/init/early-init.el")

(defun pel--setup-init-file-problems (&optional early-init-must-exist)
  "Return a list of problems in the initialization files if any.

If EARLY-INIT-MUST-EXIST is non-nil the early-init.el file must exist for
Emacs >= 27, otherwise it's not a hard requirement.

Return nil if all is OK."
  (let ((problems nil))
    (if (boundp 'pel-init-file-version)
        (unless (string= (pel-as-string pel-init-file-version)
                         pel--expected-init-file-version)
          (pel-push-fmt problems
              "Invalid pel-init-file-version: %s instead of expected %s
   Please update your init.el file.
   Use the pel/example/init/init.el as template."
            pel-init-file-version pel--expected-init-file-version))
      (pel-push-fmt problems
          "Invalid init.el file (%s): not ready for PEL startup management.
   Use the pel/example/init/init.el as template." user-init-file))


    (when pel-emacs-27-or-later-p
      (if (file-exists-p (locate-user-emacs-file "early-init.el"))
          (if (boundp 'pel-early-init-file-version)
              (unless (string= (pel-as-string pel-early-init-file-version)
                               pel--expected-early-init-file-version)
                (pel-push-fmt problems
                    "Invalid pel-early-init-file-version: %s instead of expected %s
   Please update your early-init.el file.
   Use the pel/example/init/early-init.el as template."
                  pel-early-init-file-version
                  pel--expected-early-init-file-version))
            (pel-push-fmt problems
                "Invalid early-init.el file (%searly-init.el): not ready for PEL startup management.
   Use the pel/example/init/early-init.el as template."
              (file-name-parent-directory user-init-file)))

        (when early-init-must-exist
          (pel-push-fmt problems
              "This feature requires a PEL compatible early-init.el file
   inside %s and that is missing.  Create one, using
   pel/example/init/early-init.el as template."
            (file-name-parent-directory user-init-file)))))
    ;;
    ;; The next check cannot be performed when quickstart is used because in
    ;; that case I cannot find a way to get the original value of
    ;; package-user-dir inside the early-init.el file.  TODO.
    (unless (bound-and-true-p package-quickstart)
      (if (boundp 'pel-package-user-dir-original)
          (when (and (not (file-symlink-p pel-package-user-dir-original))
                     (pel-same-fname-p
                      pel-package-user-dir-original
                      (pel-elpa-name (pel-sibling-dirpath
                                      pel-package-user-dir-original
                                      "elpa-complete")
                                     pel-emacs-is-graphic-p)))
            (pel-push-fmt problems
                "Incompatible elpa directory : %s.
   It clashes with PEL's logic to flip between -complete and -reduced."
              pel-package-user-dir-original))
        (pel-push-fmt problems
            "The init.el file is incompatible with PEL startup management:
 - `pel-package-user-dir-original' is not bound.
   It should be set in init.el to remember `package-user-dir'.")))
    ;;
    (reverse problems)))


(defun pel-setup-validate-init-files (&optional early-init-must-exist)
  "Validate Emacs initialization files.

If EARLY-INIT-MUST-EXIST is non-nil the early-init.el file must exist for
Emacs >= 27, otherwise it's not a hard requirement.
Raise error describing the problem if there is one."
  (let ((problems (pel--setup-init-file-problems early-init-must-exist)))
    (when problems
      (user-error
       (pel-format-problem-messages
        problems nil
        "Emacs user directory (%s) is incompatible with PEL startup management."
        user-emacs-directory)))))

;; ---------------------------------------------------------------------------
;; Package Quickstart Support
;; --------------------------

(defun pel--with-package-quickstart-p ()
  "Return t when package quickstart is currently used, nil otherwise."
  (and (require 'package nil :no-error)
       (boundp 'package-quickstart)
       package-quickstart
       (file-exists-p (locate-user-emacs-file "early-init.el"))
       (file-exists-p
        (locate-user-emacs-file
         (pel-elpa-name
          "package-quickstart.el"
          (and pel--detected-dual-environment-in-init-p
               pel-emacs-is-graphic-p))))))

(defun pel--with-quickstart-state-msg (prompt &optional show-requested-status
                                              just-changed)
  "Augment PROMPT to ask about package-quickstart if necessary.

PROMPT can be an empty string to create a status string instead.
If SHOW-REQUESTED-STATUS is specified the message should also display
the new requested status instead of the current state."
  (let ((current-quickstart-status (if show-requested-status
                                       pel-support-package-quickstart
                                     (pel--with-package-quickstart-p))))
    (if pel-emacs-27-or-later-p
        (format "%s %s package quickstart support%s"
                prompt
                (if current-quickstart-status
                    "with"
                  "without")
                (pel-string-when
                 just-changed
                 ", but that has just been modified so please restart Emacs"))
      prompt)))

;; ---------------------------------------------------------------------------
;; Fast-Startup Support
;; --------------------

(defconst pel-fast-startup-init-fname (expand-file-name
                                       "pel-fast-startup-init.el"
                                       user-emacs-directory)
  "Name of code file that must be executed by fast-startup in init/early-init.
When fast startup is not activated, this file must be deleted.")

(defvar pel--fast-startup-setup-changed nil
  "Identifies that PEL setup has changed.
Only set by `pel-setup-fast' or `pel-setup-normal'. Never cleared.")

;; PEL Fast-Startup status extraction
;; ----------------------------------

(defun pel--setup-mode-description (for-graphics)
  "Describe the mode context of a setup.
The FOR-GRAPHICS argument identifies the setup forced for independent graphics."
  (if for-graphics
      "dual-environment graphics mode"
    (if pel--detected-dual-environment-in-init-p
        "dual-environment terminal/tty mode"
      "single custom-file modes")))

(defun pel--fast-setup-met-criteria ()
  "Check if the setup meets fast startup settings.

Return a list of 3 elements:
- test count, the number of tests, an integer.
- list of met-criteria strings.
- list of problem description strings.

PEL is considered in normal mode if all tests fail: the
met-criteria list is nil and the side of the problem list is
equal to the number of tests.  The setup is considered valid fast
startup if all tests pass."
  (let ((met-criteria nil)
        (issues nil)
        (test-count 0))
    ;;
    (setq test-count (1+ test-count))
    (if (pel-in-fast-startup-p)
        (pel-push-fmt met-criteria
            "Identified as fast startup by function `pel-in-fast-startup'")
      (pel-push-fmt issues "The `pel-in-fast-startup' is not set."))
    ;;
    (setq test-count (1+ test-count))
    (if (file-exists-p pel-fast-startup-init-fname)
        (pel-push-fmt met-criteria "Fast startup setup file is present: %s"
          pel-fast-startup-init-fname)
      (pel-push-fmt issues "The file %s indicating fast startup not found."
        pel-fast-startup-init-fname))
    ;;
    (dolist (for-graphic (if pel--detected-dual-environment-in-init-p
                             '(nil t)
                           '(nil)))
      (let* ((mode-description (pel--setup-mode-description for-graphic))
             (elpa-dirpath (pel-elpa-name pel-elpa-dirpath for-graphic))
             (elpa-reduced-dirpath (pel-elpa-name (pel-sibling-dirpath
                                                         elpa-dirpath "elpa-reduced")
                                                        for-graphic)))
        (setq test-count (1+ test-count))
        (if (pel-same-fname-p (directory-file-name elpa-dirpath)
                              elpa-reduced-dirpath)
            (pel-push-fmt met-criteria "%s elpa is using elpa-reduced"
              mode-description)
          (pel-push-fmt issues "%s elpa (%s) is not equal and does not point\
 to elpa-reduced (%s)"
            mode-description elpa-dirpath elpa-reduced-dirpath))))
    (list test-count (reverse met-criteria) (reverse issues))))

(defun pel--startup-mode ()
  "Return whether PEL/Emacs operates in fast startup mode.
Returns: \\='normal, \\='fast or \\='inconsistent."
  (let* ((tc.met-criteria.issues (pel--fast-setup-met-criteria))
         (test-count   (nth 0 tc.met-criteria.issues))
         (met-criteria (length (nth 1 tc.met-criteria.issues)))
         (issues       (length (nth 2 tc.met-criteria.issues))))
    (cond
     ((and (eq met-criteria 0) (eq issues test-count)) 'normal)
     ((and (eq met-criteria test-count) (eq issues 0)) 'fast)
     (t 'inconsistent))))

;; --
;;-pel-autoload
(defun pel-setup-info (&optional with-now pq-just-modified)
  "Display current state of PEL setup: whether normal or in fast startup.
Optional arguments:
- WITH-NOW: set to t to put the word \\='now\\=' in the text.
- PQ-JUST_MODIFIED: set to t if package quickstart activation has been
                    modified in the Emacs session."
  (interactive)
  (pel-setup-validate-init-files)
  (let ((mode (pel--startup-mode))
        (now-msg (if with-now "now " "")))
    (if pel--fast-startup-setup-changed
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
                      (pel--with-quickstart-state-msg "" nil pq-just-modified)
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
                      (pel--with-quickstart-state-msg "" nil pq-just-modified)
                      user-emacs-directory))
            (t
             (let* ((tc.met-criteria.problems (pel--fast-setup-met-criteria))
                    (met-criteria (nth 1 tc.met-criteria.problems))
                    (problems (nth 2  tc.met-criteria.problems)))
               (user-error "The PEL/Emacs startup mode is inconsistent!
 Only some conditions of a fast startup setup are met:
 - %s
 The following problem remain:
 - %s
 Check the elpa directory inside %s.
 Restore normal setup: try: M-x pel-setup-normal
 In the worst case restore normal setup manually."
                           (string-join met-criteria "\n - ")
                           (string-join problems "\n - ")
                           user-emacs-directory)))))))

;; ---------------------------------------------------------------------------


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

(defun pel--create-early-init-if-missing ()
  "Check if the early-init file is present, if not create one."
  (let ((early-init-fname (locate-user-emacs-file "early-init.el")))
    (unless (file-exists-p early-init-fname)
      (copy-file pel-early-init-template early-init-fname))))

(defun pel--compile-file-if (el-fname byte-compile-it)
  "Byte compile file EL-FNAME if BYTE-COMPILE-IT is set.
Otherwise delete the .elc file if it exists."
  (if byte-compile-it
      (byte-compile-file el-fname)
    ;; no compilation needed; remove any left-over .elc file
    (let ((elc-fname (concat el-fname "c")))
      (when (file-exists-p elc-fname)
        (delete-file elc-fname)))))

(defun pel--update-emacs-user-file (fname symbol-values
                                          &optional byte-compile-it)
  "Update FNAME file: set symbol to value from the SYMBOL-VALUES alist.

The FNAME is assumed to be located inside the user Emacs directory.
FNAME can be init.el or early-init.el and must define the symbols
inside defconst forms.

Raise an error if the function does not find the `defconst' form
defining a specified symbol."
  (let ((fname (locate-user-emacs-file fname))
        varname
        new-value
        re-pattern)
    (unless (file-exists-p fname)
      (user-error "File %s is not found!" fname))
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

(defun pel--other-custom-file ()
  "Return the name of the customization file used by the other mode."
  (pel-elpa-name custom-file (not pel-emacs-is-graphic-p)))

(defun pel--set-user-option (user-option value)
  "Set USER-OPTION symbol to specified VALUE.

Update the global variable and all used custom files."
  (set user-option value)
  (pel-customize-save user-option value)
  (when pel--detected-dual-environment-in-init-p
    ;; store in the custom file of the other mode
    (pel-customize-save user-option value (pel--other-custom-file))))

;;; --------------------------------------------------------------------------
(provide 'pel-setup-base)

;;; pel-setup-base.el ends here
