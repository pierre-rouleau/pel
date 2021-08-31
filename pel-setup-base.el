;;; pel-setup-base.el --- PEL setup base logic used by Emacs >= 27 but also in previous version.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, August 31 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-08-31 18:32:40, updated by Pierre Rouleau>

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
(require 'pel-elpa)            ; use: `pel--adjusted-fname'
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
         (pel--adjusted-fname
          "package-quickstart.el"
          :force (and pel--detected-dual-environment-in-init-p
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
      "independent graphics mode"
    (if pel--detected-dual-environment-in-init-p
        "independent terminal/tty mode"
      "all modes")))

(defun pel--fast-setup-met-criteria ()
  "Return a cons of 2 lists of strings: met-criteria and problems.
If the first list is nil then PEL/Emacs operates in normal mode.
If the first list has 3 members, then PEL/Emacs operates in fast startup mode.
Return a (test-count met-criteria issues) list."
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
             (elpa-dirpath (pel--adjusted-fname pel-elpa-dirpath
                                                :force for-graphic))
             (elpa-reduced-dirpath (pel--adjusted-fname
                                    (pel-sibling-dirpath elpa-dirpath
                                                         "elpa-reduced")
                                    :force for-graphic)))
        (setq test-count (1+ test-count))
        (if (pel-symlink-points-to-p (directory-file-name elpa-dirpath)
                                     elpa-reduced-dirpath)
            (pel-push-fmt met-criteria "%s elpa symlink points to elpa-reduced"
              mode-description)
          (pel-push-fmt issues "%s elpa symlink (%s) does not point\
 to elpa-reduced (%s)"
            mode-description elpa-dirpath elpa-reduced-dirpath))))
    (list test-count met-criteria issues)))

(defun pel--startup-mode ()
  "Return whether PEL/Emacs operates in fast startup mode.
Returns: 'normal, 'fast or 'inconsistent."
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
- WITH-NOW: set to t to put the word 'now' in the text.
- PQ-JUST_MODIFIED: set to t if package quickstart activation has been
                    modified in the Emacs session."
  (interactive)
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
               (user-error "PEL/Emacs mode is inconsistent!
 Check the elpa directory inside %s and restore normal setup.
 Only some conditions of a fast startup setup are met:
 - %s
 The following problem remain:
 - %s"
                           user-emacs-directory
                           (string-join met-criteria "\n - ")
                           (string-join problems "\n - "))))))))

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
  (pel--adjusted-fname custom-file :force (not pel-emacs-is-graphic-p)))

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
