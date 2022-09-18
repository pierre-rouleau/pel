;;; pel-sudo-edit.el --- Sudo edit support.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, April 20 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-09-18 14:24:38 EDT, updated by Pierre Rouleau>

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
;;
;; The current implementation provide a work-around to problem affecting tramp
;; causing it to hang when P4 VC backend is enabled.

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
  ;; Tramp is used to access file via sudo.
  ;;
  ;; Work-round to tramp hanging issue(s):
  ;; - I noticed that when the Perforce VC backend is active the operation
  ;;   fails, hanging with ''Checking ‘vc-registered’ for ...''
  ;; - I also noticed that if I remove P4 from the `vc-handled-backends' list
  ;;   the problem disappears.
  ;; So for now: temporary disable P4 backend during the
  ;; execution of the command.
  ;; - I have also seen reports that it's best to change current directory
  ;;   when executing otherwise interaction with the VC backend might also hit.
  ;;   I have not recently seen that problem but for the moment I continue to
  ;;   change current directory when executing the command just in case.
  (let ((original-vc-handled-backends vc-handled-backends)
	(original-cwd default-directory))
    (unwind-protect
	(progn
	  (cd "~")
	  (when (memq 'P4 vc-handled-backends)
	    (setq vc-handled-backends (remove 'P4 vc-handled-backends)))
	  (if (and buffer-file-name
		   arg)
	      (find-alternate-file (pel--sudo-filename buffer-file-name))
	    (find-file (pel--sudo-filename
			(ido-read-file-name "Find file(as root): ")))))
      (cd original-cwd)
      (setq vc-handled-backends original-vc-handled-backends))))

;;; --------------------------------------------------------------------------
(provide 'pel-edit-as-root)

;;; pel-sudo-edit.el ends here
