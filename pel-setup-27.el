;;; pel-setup-27.el --- PEL setup control for Emacs >= 27.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, August 31 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-09-03 12:09:45, updated by Pierre Rouleau>

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
;; This file contains the pel-setup logic that is only available for Emacs 27
;; and later.  It is loaded by pel-setup.el when pel-setup detects Emacs 27 or
;; later.



;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--options)               ; use: `pel-compile-package-quickstart'
;;                                    ;      `pel-compile-emacs-early-init'

(require 'pel-elpa)                   ; use: `pel-elpa-package-alist-of-dir'
;;                                    ;      `pel-elpa-name'

(require 'pel-setup-base)             ; use: `pel--set-user-option'
;;                                    ;      `pel--compile-file-if'
;;                                    ;      `pel-remove-no-byte-compile-in'
;;                                    ;      `pel--update-emacs-user-file'
;;; --------------------------------------------------------------------------
;;; Code:
;;
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
;;   - `pel--set-package-quickstart-in-early-init'
;;     - `pel--update-emacs-user-file'
;;   - `pel--create-package-quickstart'
;;   - `pel--store-with-package-quickstart'
;;
;; * `pel-setup-no-quickstart'
;;   - `pel--remove-package-quickstart-files'
;;   - `pel--store-with-package-quickstart'
;;   - `pel--set-package-quickstart-in-early-init'
;;     - `pel--update-emacs-user-file'

;; - Execute the `pel-setup-with-quickstart' command to set everything up: it
;;   sets the user-option in the custom file(s) and edit the init.el and
;;   early-init.el files.
;; - Restart Emacs.

(defun pel--set-dual-environment-in-emacs-early-init (use)
  "Update Emacs user early-init.el for dual environment when USE is non-nil."
  (pel--create-early-init-if-missing)
  (pel--update-emacs-user-file
   "early-init.el"
   (list
    (list 'pel-early-init-support-dual-environment-p (not (null use))))
   pel-compile-emacs-early-init))

;; ---------------------------------------------------------------------------

(defun pel--create-package-quickstart (dirpath for-graphics)
  "Utility: build package-quickstart.el for specific PEL mode and DIRPATH.

DIRPATH is the path of a ELpa-compliant directory used.
Normally that's the elpa directory inside the user-emacs-directory but
that can be the elpa-reduced directory for fast startup or then ones
for graphics mode when the dual mode is used.

FOR-GRAPHICS is:
- t when setting package quickstart for Emacs running in graphics mode and
  dual environment where graphic mode has a specific custom file,
- nil when Emacs runs in terminal/TTY mode or in graphics mode without dual
  environment set."
  (if (and (require 'package nil :no-error)
           (fboundp 'package-quickstart-refresh)
           (boundp 'package-quickstart-file))
      (let* ((package-user-dir        (pel-elpa-name dirpath for-graphics))
             (package-quickstart-file (pel-elpa-name package-quickstart-file for-graphics))
             (package-alist           (pel-elpa-package-alist-of-dir package-user-dir)))
        (package-quickstart-refresh)
        ;; Byte-compile it if requested, otherwise remove its .elc
        (pel--compile-file-if package-quickstart-file
                              (and pel-compile-package-quickstart
                                   (pel-remove-no-byte-compile-in
                                    package-quickstart-file))))
    ;; report any error
    (error "Failed accessing package-quickstart")))

(defun pel--set-package-quickstart-in-early-init (use)
  "Update Emacs early-init.el file package quickstart setting to USE."
  (pel--create-early-init-if-missing)
  (pel--update-emacs-user-file
   "early-init.el"
   (list
    (list 'pel-early-init-support-package-quickstart-p use))
   pel-compile-emacs-early-init))

(defun pel--setup-early-init (pkg-quickstart)
  "Create valid PEL early-init.el file and set its behaviour.

The behaviour is controlled by:
- PKG-QUICKSTART: t to activate package quickstart, nil to disable it.
- The value of `pel--detected-dual-environment-in-init-p' determines
  whether PEL is used with a dual environment for terminal/TTY and graphics
  mode or just a single one as usual for Emacs.
- The value of `pel-shell-detection-envvar' user-option determines what
  environment variable absence is used to detect GUI launched Emacs."
  (pel--create-early-init-if-missing)
  (pel--update-emacs-user-file
   "early-init.el"
   (list
    (list 'pel-early-init-support-package-quickstart-p pkg-quickstart)
    (list 'pel-early-init-support-dual-environment-p
          pel--detected-dual-environment-in-init-p)
    (list 'pel-early-init-shell-detection-envvar
          pel-shell-detection-envvar))
   pel-compile-emacs-early-init))

(defun pel--store-with-package-quickstart (value)
  "Set `pel-support-package-quickstart' to new VALUE globally and persistently.

Also set `package-quickstart' to VALUE.
Store the updated values in all relevant custom file(s)."
  (pel--set-user-option 'pel-support-package-quickstart value)
  (pel--set-user-option 'package-quickstart value))

;;-pel-autoload
(defun pel-setup-with-quickstart ()
  "Activate package quickstart for current context.

The context includes the PEL startup mode and PEL's ability
to deal with independent customization for terminal and graphics mode.

This function:
- ensures that the early-init.el file identifies PEL activation
  of package quickstart by editing the file.
- creates or refreshes the package-quickstart.el file(s).

Available for Emacs 27 and later only."
  (interactive)
  (pel-setup-validate-init-files :early-init-must-exist)
  (when (y-or-n-p "Activate Emacs package quickstart?")
    (let ((startup-mode (pel--startup-mode)))
      (when (eq startup-mode 'inconsistent)
        (user-error "PEL startup mode is inconsistent.
  Please check and fix before activating the package quickstart!"))
      ;; All is fine: proceed.
      (message "Activating package quickstart...")
      (pel--set-package-quickstart-in-early-init t)
      (let ((elpa-dpath (pel-sibling-dirpath
                         pel-elpa-dirpath
                         (if (eq startup-mode 'fast)
                             "elpa-reduced"
                           (if (file-exists-p (locate-user-emacs-file "elpa-complete"))
                               "elpa-complete"
                             "elpa")))))
        (pel--create-package-quickstart elpa-dpath nil)
        (when pel--detected-dual-environment-in-init-p
          (pel--create-package-quickstart elpa-dpath t)))
      ;; Remember user setting: in pel-support-package-quickstart user-option
      (pel--store-with-package-quickstart t))
    ;; display state
    (pel-setup-info :now :pkg-quickstart-just-modified)))

;; --
(defun pel--remove-package-quickstart-files (for-graphics)
  "Remove package quickstart file identified by the FOR-GRAPHICS argument."
  (if (and (require 'package nil :no-error)
           (boundp 'package-quickstart-file))
      (let ((fname (pel-elpa-name package-quickstart-file
                                        for-graphics)))
        ;; remove the package-quickstart.el file
        (when (file-exists-p fname)
          (delete-file fname))
        ;; remove the corresponding .elc file if it exists.
        (setq fname (concat fname "c"))
        (when (file-exists-p fname)
          (delete-file fname)))
    (user-error "Cannot access package-quickstart-file variable!")))

;;-pel-autoload
(defun pel-setup-no-quickstart ()
  "Disable package quickstart.
Support PEL startup modes and PEL dual independent customization files."
  (interactive)
  (pel-setup-validate-init-files :early-init-must-exist)
  (when (y-or-n-p "Disable Emacs package quickstart?")
    (message "Disabling package quickstart...")
    (pel--remove-package-quickstart-files nil)
    ;; When dual independent customization mode is used delete the
    ;; graphics specific files.  If it is not used the files do not exist
    ;; and the function does nothing.
    (pel--remove-package-quickstart-files t)
    ;;
    ;; Remember user setting:
    ;; - in pel-support-package-quickstart user-option
    (pel--store-with-package-quickstart nil)
    ;; - and inside the early-init.el
    (pel--set-package-quickstart-in-early-init nil)

    ;; display state
    (pel-setup-info :now :pkg-quickstart-just-modified)))

;;; --------------------------------------------------------------------------
(provide 'pel-setup-27)

;;; pel-setup-27.el ends here
