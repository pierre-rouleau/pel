;;; pel-rpm-spec.el --- RPM Secificiation File Utilities.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, November 20 2024.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2024-11-23 10:26:31 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2024  Pierre Rouleau
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
;; Tools for RPM specification file management

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'compile)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defvar pel-home-dirpath-name)   ; prevent compiler warning: defined in init.el

(defun pel-rpm-spec-lint ()
  "Lint the current RPM spec file, show errors in compilation-mode buffer."
  (interactive "P")
  (compile
   ;; use the shell script that executes rpmlint and transforms its output
   ;; to comply with compile-mode supported error/warning lines.
   (format "%s/bin/rpmlint-4emacs %s"
           pel-home-dirpath-name
           (shell-quote-argument (buffer-file-name)))
   nil))

(defun pel-rpm-build ()
  "Build the RPM corresponding to current RPM spec file.

Prompt for the command line with rpmbuild -ba as default,
supporting history.
Write report in a compile-mode buffer.
"
  (interactive)
  (let ((cmd (read-shell-command
              "Build RPM with: "
              "rpmbuild -ba "
              'pel-rpm-build-hist)))
    (compile
     (format "%s %s"
             cmd
             (shell-quote-argument (buffer-file-name)))
     nil)))

;;; --------------------------------------------------------------------------
(provide 'pel-rpm-spec)

;;; pel-rpm-spec.el ends here
