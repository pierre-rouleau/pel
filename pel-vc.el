;;; pel-vc.el --- PEL extensions to the Emacs VC package.  -*- lexical-binding: t; -*-

;; Created   : Saturday, April 23 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-11-12 18:05:27 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2022  Pierre Rouleau
;; Credit: `pel-vc-dir-hide-state' is heavily derived from Magnus Henoch
;;         code taken from https://lists.gnu.org/archive/html/bug-gnu-emacs/2010-05/msg00205.html
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
;; This file provides a set of commands that enhance the vc-dir mode.
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--options)
(require 'vc-dir)

;;; --------------------------------------------------------------------------
;;; Code:

;; Extensions for vc-dir mode
;; --------------------------

(defun pel-vc-ignore-marked ()
  "Ignore all marked file files in the vc-dir buffer."
  (interactive)
  (mapcar 'vc-ignore (vc-dir-marked-files)))

;; Hide files is specific state
;; ----------------------------
;; Credit: Magnus Henoch wrote the original version of this function.

(defun pel-vc-dir-hide-state (states)
  "Hide files whose state is in STATES.

Prompt for the STATE.
Supports tab completion of the available states."
  (interactive
   (list
    (if vc-ewoc
        (mapcar 'intern
                (completing-read-multiple
                 "Hide files that are in state(s): "
                 (let (possible-states)
                   (ewoc-map (lambda (item)
                               (let ((state (vc-dir-fileinfo->state item)))
                                 (when state
                                   (cl-pushnew state possible-states))
                                 nil))
                             vc-ewoc)
                   (mapcar 'symbol-name possible-states))
                 nil t))
      (error "Not in a vc-dir buffer"))))
  (let ((inhibit-read-only t))
    (ewoc-filter vc-ewoc
                 (lambda (file)
                   (not (memq (vc-dir-fileinfo->state file) states))))))

;; ----------------------------
;; Add ability to use the --verbose option on svn log without affecting other
;; Subversion commands.

(defvar vc-svn-global-switches)         ; declaration to prevent warning

(defun pel-vc-svn-print-log-verbose (oldfun &optional working-revision limit)
  "Wrapper for `vc-print-log' that forces --verbose switch for Subversion."
  (if (and pel-vcs-svn-verbose-log
           (string= (vc-backend (pel-current-buffer-filename)) "SVN")
           (boundp 'vc-svn-global-switches))
      (let ((vc-svn-global-switches (append vc-svn-global-switches '("--verbose"))))
        (funcall oldfun working-revision limit))
    ;; normally just execute the function unchanged.
    (funcall oldfun working-revision limit)))

(defun pel-vc-svn-print-root-log-verbose (oldfun &optional limit)
  "Wrapper for `vc-print-log' that forces --verbose switch for Subversion."
  (if (and pel-vcs-svn-verbose-log
           (string= (vc-backend (pel-current-buffer-filename)) "SVN")
           (boundp 'vc-svn-global-switches))
      (let ((vc-svn-global-switches (append vc-svn-global-switches '("--verbose"))))
        (funcall oldfun limit))
    ;; normally just execute the function unchanged.
    (funcall oldfun limit)))


;;-pel-autoload
(defun pel-vc-svn-init ()
  "Initialize extra VC support."
  (advice-add 'vc-print-log      :around #'pel-vc-svn-print-log-verbose)
  (advice-add 'vc-print-root-log :around #'pel-vc-svn-print-root-log-verbose))

;;; --------------------------------------------------------------------------
(provide 'pel-vc)

;;; pel-vc.el ends here
