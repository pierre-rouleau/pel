;;; pel-vcs.el --- VCS management.  -*- lexical-binding: t; -*-

;; Created   : Thursday, September  9 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-03-21 16:55:43 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2025  Pierre Rouleau
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
(require 'pel--base)                    ; use: `pel-current-buffer-filename'
(require 'pel-filedir)                  ; use: `pel-file-in-dir-upwards'
(require 'pel-prompt)                   ; use: `pel-select-symbol-from'
(require 'vc)                           ; use `vc-switch-backend' / `vc-change-backend'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defconst pel--vcs-repo-fnames (list (cons ".hg"  'Hg)
                                     (cons ".git" 'Git)
                                     (cons ".bzr" 'Bzr)
                                     (cons "_MTN" 'Mtn)
                                     (cons ".svn" 'SVN)
                                     (cons "CVS"  'CVS))
  "VCS type repository files.")

(defun pel-vcs-backends-for (filepath)
  "Return a list of VCS back-ends available for the specified FILEPATH.
Return nil if there are no VCS back-end.
The list identify VCS back-end with the symbols used in `vc-handled-backends'."
  (let ((detected-backends nil)
        (filepath (if (file-directory-p filepath)
                      (file-name-as-directory filepath)
                    filepath)))
    (dolist (fname.backend pel--vcs-repo-fnames (reverse detected-backends))
      (when (pel-file-in-dir-upwards
             (car fname.backend)
             (file-name-directory filepath))
        (push (cdr fname.backend) detected-backends)))))


(defun pel--vc-switch-backend (file backend)
  "Proxy for `vc-switch-backend' or `vc-change-backend'"
  (with-no-warnings
    ;; Prior to Emacs 30, the function was vc-switch-backend.
    ;; Emacs devs wanted to remove it but I convinced them to keep it.
    ;; So at Emacs 30 it was renamed to vc-change-backend.
    (if pel-emacs-30-or-later-p
        (vc-change-backend file backend)
      (vc-switch-backend file backend))))

;;-pel-autoload
(defun pel-vcs-switch-backend ()
  "Switch VCS back-end for the current file.

If no VCS back-end or only one VCS back-end is available for the
file, the command issues an error, otherwise it prompts for the
VCS back-end to use and switch the VCS back-end for this file.
The switch is temporary: it is restricted to the current Emacs
session.

Use this command when the file is managed by more than one VCS
back-end."
  (interactive)
  (let* ((current-filename (pel-current-buffer-filename))
         (vcs-backends (pel-vcs-backends-for current-filename)))
    (if vcs-backends
        (if (= (length vcs-backends) 1)
            (user-error "There is only one VCS back-end for %s: %s"
                        current-filename (car vcs-backends))
          (require 'vc nil :no-error)

          (pel--vc-switch-backend current-filename
                                  (pel-select-symbol-from "VCS" vcs-backends)))
      (user-error "Found no VCS back-end for the file %s" current-filename))))


;; ---------------------------------------------------------------------------
;; Log VC Commands
;; ---------------



(defun pel--vcs-log-vc (command files flags)
  "Log VC command."
  (let ((outbuf (get-buffer-create "*pel-vc-log*")))
    (with-current-buffer outbuf
      (goto-char (point-max))
      (insert (format "%s %s %s\n" command files flags)))))

;;-pel-autoload
(defun pel-vcs-toggle-vc-log ()
  "Start/stop logging VC commands in the *pel-vc-log* buffer.
When starting, the command does not create the buffer.
It is created on the first VC event."
  (interactive)
  (unless (boundp 'vc-post-command-functions)
    (require 'vc-dispatcher nil :no-error))
  (when (boundp 'vc-post-command-functions)
    (if (and vc-post-command-functions
             (member 'pel--vcs-log-vc vc-post-command-functions))
        (progn
          (remove-hook 'vc-post-command-functions 'pel--vcs-log-vc)
          (message "VC Logging stopped."))
      (add-hook 'vc-post-command-functions 'pel--vcs-log-vc)
      (message "Start VC logging in the *pel-vc-log* buffer."))))

;;; --------------------------------------------------------------------------
(provide 'pel-vcs)

;;; pel-vcs.el ends here
