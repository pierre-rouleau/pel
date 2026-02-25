;;; pel-timestamp.el --- File time-stamps management.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, June 10 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-02-25 14:12:04 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2025, 2026  Pierre Rouleau
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
(require 'time-stamp)      ; use: `time-stamp', `time-stamp-active'

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
group customize buffer.
This also update the value of `time-stamp-active'."
  (interactive "P")
  (pel-toggle-and-show-user-option 'pel-update-time-stamp globally)
  (if globally
      (setq time-stamp-active pel-update-time-stamp)
    (setq-local time-stamp-active pel-update-time-stamp))
  (when pel-update-time-stamp
    (unless (memq 'pel--update-time-stamp before-save-hook)
      (add-hook 'before-save-hook  'pel--update-time-stamp))))


(defun pel-update-time-stamp-patterns ()
  "Update file time stamp matching `pel-update-time-stamp-pattern-regexps'."
  (save-excursion
    (dolist (regexp pel-update-time-stamp-pattern-regexps)
      (goto-char (point-min))
      (while (re-search-forward regexp nil :noerror)
        (replace-match
         (format-time-string pel-date-wkd-time-iso-format nil t)
         :fixedcase
         :literal
         nil
         1)))))

(defun pel--update-time-stamp ()
  "Update time stamp in file if currently active."
  (when (and buffer-file-name
             pel-update-time-stamp)
    (time-stamp)
    (pel-update-time-stamp-patterns)))

;;-pel-autoload
(defun pel-time-stamp-control-show-info (&optional append)
  "Display buffer current time stamp control variables and their state.
The information is shown inside a *pel-time-stamp-info* help buffer."
  (interactive "P")
  (let ((pel-insert-symbol-content-context-buffer (current-buffer)))
    (pel-print-in-buffer
     "*pel-time-stamp-info*"
     "Automatic File Time Stamp Control"
     (lambda ()
       ;; (insert (propertize  "* Control Time Stamp Update on File Save:"
       ;;                      'face
       ;;                      'bold))
       (insert (substitute-command-keys "
PEL provides control of the hook logic required to automate the update
of time stamp when a file is saved; it controls it with the value of the
`pel-update-time-stamp' option.  On startup, PEL sets up the hook for a
function that updates time stamp when it is non-nil.

- You can change this dynamically with `pel-toggle-update-time-stamp-on-save'
  command, bound to \\[pel-toggle-update-time-stamp-on-save].
- It also updates `time-stamp-active' for consistency.
- When Emacs starts, PEL set `time-stamp-active' to the value of
  `pel-update-time-stamp' to ensure consistency.
- Note that `time-stamp-toggle-active' (bound to \\[time-stamp-toggle-active])
  only toggle `time-stamp-active' which affects whether time stamp is updated
  by the `time-stamp' command (bound to \\[time-stamp]).
- For a time stamp to be updated on file save, both variables must be non-nil.
"))
       (pel-insert-symbol-content-line
        'pel-update-time-stamp nil
        (lambda (v)
          (if v
              "Time stamp updated on file save in this session."
            "Time stamps are not updated in this session.")))
       (pel-insert-symbol-content-line
        'time-stamp-active nil
        "Only controls whether time-stamp command updates the time stamp.")

       (insert "\n\n")
       (insert (propertize  "*Time Stamp Location and Format Control:" 'face
                            'bold))
       (insert "

The location and format of the time stamp is either controlled by the single
`time-stamp-pattern' or all of the other 4 user-options, all 4 of them
otherwise you risk using a mix of what you want and what was already active.
")
       (pel-insert-symbol-content-line 'time-stamp-pattern)
       (pel-insert-symbol-content-line 'time-stamp-line-limit)
       (pel-insert-symbol-content-line 'time-stamp-start)
       (pel-insert-symbol-content-line 'time-stamp-end)
       (pel-insert-symbol-content-line 'time-stamp-format))
     (unless append :clear-buffer)
     :use-help-mode)))

;; ---------------------------------------------------------------------------
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


(defun pel--skip-copyright-for (fname)
  "Return non-nil if instructed not to update copyright for FNAME.
FNAME is a file name with absolute path.  Return non-nil if this file
name is identified in the `pel-skip-copyright-in' user-option."
  (let* ((fpath (expand-file-name fname)))
    (catch 'match
      (dolist (to-skip pel-skip-copyright-in)
        (let ((to-skip-abs (expand-file-name to-skip)))
             (if (file-directory-p to-skip-abs)
                 (when (string-match-p
                        (format "^%s" (regexp-quote to-skip-abs))
                        fpath)
                   (throw 'match to-skip))
               (when (string= to-skip-abs fpath)
                 (throw 'match to-skip))))))))


(defun pel--update-copyright (&optional arg interactivep)
  "Update time stamp if currently active.
With prefix ARG, replace the years in the notice rather than adding
the current year after them.  If necessary, and
‘copyright-current-gpl-version’ is set, any copying permissions
following the copyright are updated as well.
If non-nil, INTERACTIVEP tells the function to behave as when it’s called
interactively."
  (when pel-update-copyright
    (unless (pel--skip-copyright-for (pel-current-buffer-filename))
      (copyright-update arg interactivep))))

;;; --------------------------------------------------------------------------
(provide 'pel-timestamp)

;;; pel-timestamp.el ends here
