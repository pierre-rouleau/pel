;;; pel-timestamp.el --- File time-stamps management.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, June 10 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-06-10 15:05:58 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2025  Pierre Rouleau
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

(require 'pel--base)       ; use: `pel-toggle-and-show-user-option'
(require 'pel--options)    ; use: `pel-update-time-stamp',
;;                         ;      `pel-update-copyright'
(require 'time-stamp)      ; use: `time-stamp'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;* Update Time Stamp
;;  =================

(defun pel-toggle-update-time-stamp-on-save (&optional globally)
  "Toggle time-stamp update on file save and display current state.
By default change behaviour for local buffer only.
When GLOBALLY argument is non-nil, change it for all buffers for the current
Emacs editing session (the change does not persist across Emacs sessions).
To modify the global state permanently modify the customized value of the
`pel-update-time-stamp' user option via the `pel-pkg-for-filemng'
group customize buffer."
  (interactive "P")
  (pel-toggle-and-show-user-option 'pel-update-time-stamp globally))


(defun pel--update-time-stamp ()
  "Update time stamp if currently active."
  (when pel-update-time-stamp
    (time-stamp)))

;;* Update Copyright Notice
;;  =======================

(defun pel-toggle-update-copyright-on-save (&optional globally)
  "Toggle copyright update on file save and display current state.
By default change behaviour for local buffer only.
When GLOBALLY argument is non-nil, change it for all buffers for the current
Emacs editing session (the change does not persist across Emacs sessions).
To modify the global state permanently modify the customized value of the
`pel-update-copyright' user option via the `pel-pkg-for-filemng'
group customize buffer."
  (interactive "P")
  (pel-toggle-and-show-user-option 'pel-update-copyright globally))

(defun pel--update-copyright (&optional arg interactivep)
  "Update time stamp if currently active.
With prefix ARG, replace the years in the notice rather than adding
the current year after them.  If necessary, and
‘copyright-current-gpl-version’ is set, any copying permissions
following the copyright are updated as well.
If non-nil, INTERACTIVEP tells the function to behave as when it’s called
interactively."
  (when pel-update-copyright
    (copyright-update arg interactivep)))

;;; --------------------------------------------------------------------------
(provide 'pel-timestamp)

;;; pel-timestamp.el ends here
