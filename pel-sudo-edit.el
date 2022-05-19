;;; pel-sudo-edit.el --- Sudo edit support.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, April 20 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-05-19 11:12:15 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2022  Pierre Rouleau
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
;; This file provides the `pel-edit-as-root' function that opens a file as root,
;; enabling the editing of files such as Linux /etc/fstab.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel--sudo-filename (filename)
  "Return the FILENAME prefixed with sudo decoration for tramp."
  (concat "/sudo:root@localhost:" filename))


(defun pel-edit-as-root (&optional arg)
  "Open a file as root with sudo.

If already visiting a file and a prefix ARG is specified
then edit currently visited file as root."
  (interactive "P")
  ;; Tramp gets used to access file via sudo.  And in some cases, if the
  ;; current working directory is under VCS control, the Emacs VC support
  ;; seems to interfere with Tramp.  To prevent this, change the directory to
  ;; user's home for the duration of the command since that directory is
  ;; unlikely to be under control of some VCS.
  (let ((original-cwd default-directory))
    (unwind-protect
     (progn
       (cd "~")
       (if (and buffer-file-name
                arg)
           (find-alternate-file (pel--sudo-filename buffer-file-name))
         (find-file (pel--sudo-filename
                     (ido-read-file-name "Find file(as root): ")))))
     (cd original-cwd))))

;;; --------------------------------------------------------------------------
(provide 'pel-edit-as-root)

;;; pel-sudo-edit.el ends here
