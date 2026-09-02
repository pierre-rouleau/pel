;;; pel-org.el --- PEL Org mode utilities  -*- lexical-binding: t; -*-

;; Created   : Saturday, August 29 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-09-02 16:23:43 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2026  Pierre Rouleau
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
;; This file defines utility functions for Org Mode support.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use `pel-running-under-ssh-p'
(require 'pel--keys-macros)
(require 'cus-edit)                     ; use: `customize-option'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defvar pel-has-detected-org-file) ; prevent warning; defined in pel_keys.el
                                   ; and set by a hook function when an org
                                   ; file is opened.

(defun pel-org-open-pdf (&optional open-github-page-p)
  "Open Ɱ Org-Mode PEL PDF.

By default the function opens the local PDF file unless the
OPEN-GITHUB-PAGE-P is specified, in which case it opens the GitHub
hosted raw PDF file.  However, if the user-option variable
`pel-flip-help-pdf-arg' is set, it's the other way around: the
GitHub remote file is opened by default."
  (interactive "P")
  (pel-help-open-pdf "mode-org-mode" open-github-page-p))


(defun pel-org-set-refile-targets (&optional other-window)
  "Customize `org-refile-targets'."
  (interactive "p")
  ;; For some reason I don't yet understand, customizing
  ;; `org-refile-targets' before an Org file is opened shows
  ;; an error in the value and does not allow modifying the user-option, even
  ;; if the org.el is loaded.  So prevent execution until an org file is opened.
  (unless (bound-and-true-p pel-has-detected-org-file)
    (user-error "Open an org-mode file first."))
  (if other-window
      (customize-option-other-window 'org-refile-targets))
  (customize-option 'org-refile-targets))


;; Inject our custom function into Org-mode's global notification variable

(defun pel-org-notify (msg)
  "Notifier - display MSG on echo area and in OS-specific notification.
Inside a SSH session, just display the message in the echo area."
  (unless (pel-running-under-ssh-p)
    (let ((title "Org Mode"))
      (cond
       ;; 1. macOS (Plays the native 'Glass' alert sound)
       ((eq system-type 'darwin)
        (let* ((script
                (format "display notification \"%s\" with title \"%s\" sound name \"Glass\""
                        msg title)))
          (call-process "osascript" nil 0 nil "-e" script)))

       ;; 2. Linux (Uses notify-send and plays a system sound via canberra-gtk-play)
       ((eq system-type 'gnu/linux)
        (progn
          (call-process "notify-send" nil 0 nil title msg)
          (if (executable-find "canberra-gtk-play")
              (call-process "canberra-gtk-play" nil 0 nil "--id" "complete"))))

       ;; 3. Windows (Triggers standard system notification sound natively)
       ((memq system-type '(windows-nt ms-dos))
        (let ((ps-script (format
                          "[void] [System.Reflection.Assembly]::LoadWithPartialName('System.Windows.Forms'); \
                         $notification = New-Object System.Windows.Forms.NotifyIcon; \
                         $notification.Icon = [System.Drawing.SystemIcons]::Information; \
                         $notification.BalloonTipTitle = '%s'; \
                         $notification.BalloonTipText = '%s'; \
                         $notification.Visible = $true; \
                         $notification.ShowBalloonTip(5000); \
                         [System.Media.SystemSounds]::Asterisk.Play();" title msg)))
          (call-process "powershell" nil 0 nil "-Command" ps-script))))))
  ;; Also display message in echo area
  (ding)
  (message "🔔 Org: %s" msg))

;;; --------------------------------------------------------------------------
(provide 'pel-org)

;;; pel-org.el ends here
