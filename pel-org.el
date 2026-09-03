;;; pel-org.el --- PEL Org mode utilities  -*- lexical-binding: t; -*-

;; Created   : Saturday, August 29 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-09-03 14:31:32 EDT, updated by Pierre Rouleau>

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

;; ---------------------------------------------------------------------------
;; Archive Restoration
;; -------------------
;;
;; Provides logic to restore an archived item tree back to the Org file it
;; came from.  When the data is restored all ARCHIVE properties are removed
;; from the archived file.

(defvar pel-refile-is-archive-restore nil
  "Set to t by `pel-org-archive-restore' to activate archived tree restoration.")

(defun pel--org-clean-archive-properties-on-refile (&optional force)
  "Automatically clear archive context properties when a subtree is refiled."
  (require 'org 'noerror)
  (if (fboundp 'org-delete-property)
      (when (or pel-refile-is-archive-restore force)
        (dolist (prop '("ARCHIVE_TIME"
                        "ARCHIVE_FILE"
                        "ARCHIVE_OLPATH"
                        "ARCHIVE_CATEGORY"
                        "ARCHIVE_TODO"
                        "ARCHIVE_ITAGS"))
          (org-delete-property prop)))
    (error "org not loaded in `pel--org-clean-archive-properties-on-refile'")))

(defun pel--org-archive-file-first-property (property-name)
  "Return the property of the first headline defining a ARCHIVE_FILE property.
Search the entire buffer.
Return the expanded path string if one is found, nil otherwise."
  (require 'org 'noerror)
  (if (and (fboundp 'org-map-entries)
           (fboundp 'org-entry-get))
      (when (derived-mode-p 'org-mode)
        (let* ((all-matches (org-map-entries
                             (lambda ()
                               (org-entry-get nil property-name)))))
          ;; Remove all 'nil' entries from the list of headlines
          (car-safe (delq nil all-matches))))
    (error "org not loaded in `pel--org-archive-file-first-property'")))

(defun pel--org-archive-original ()
  "Return file name/location of first file entry if buffer is an Org archive.
Return nil otherwise."
  (when (buffer-file-name)              ; must be visiting a physical file
    (let ((fname (pel--org-archive-file-first-property "ARCHIVE_FILE")))
      (when fname
        (list (file-truename (expand-file-name fname))
              (pel--org-archive-file-first-property "ARCHIVE_OLPATH"))))))

(defun pel--org-heading-and-pos (outline-path-string)
  "Return final leaf heading and its buffer position from OUTLINE-PATH-STRING.
OUTLINE-PATH-STRING should look like \"Parent/Child/Grandchild\".

Returns a cons cell: (final-heading-string . buffer-position).
Returns nil if the structural path cannot be found."
  (require 'org 'noerror)
  (if (fboundp 'org-find-olp)
      (when (and (derived-mode-p 'org-mode)
                 (stringp outline-path-string)
                 (not (string-empty-p outline-path-string)))
        (let* (;; 1. Split the path by slashes into a list: ("Parent" "Child")
               (path-list (split-string outline-path-string "/"))
               ;; 2. Isolate the final leaf heading text
               (leaf-heading (car (last path-list))))
          (save-excursion
            (goto-char (point-min))
            ;; 3. CORRECT FUNCTION: org-find-olp takes a list of strings
            ;; It moves the cursor point directly to that heading if it exists.
            (condition-case nil
                (let ((pos (org-find-olp path-list 'this-buffer)))
                  ;; 4. Return a dotted pair: (Heading . Position)
                  (cons leaf-heading pos))
              ;; org-find-olp throws an error if it fails to find the path,
              ;; so we catch it and return nil instead of crashing.
              (error nil)))))
    (error "org not loaded in 'pel--org-heading-and-pos'")))

(defun pel-org-archive-restore (&optional silent)
  "Restore archived sub-tree back to its original org file.
Raise an error when failing to restore item unless SILENT is non-nil."
  (interactive)
  (require 'org-refile 'noerror)
  (if (fboundp 'org-refile)
      (let ((fname--location (pel--org-archive-original))
            done)
        (when fname--location
          (let ((orig-org-fname (nth 0 fname--location))
                (heading-path   (nth 1 fname--location))
                (rfloc nil))
            (with-current-buffer (find-file-noselect orig-org-fname)
              (let ((heading--location (pel--org-heading-and-pos heading-path)))
                (when heading--location
                  (setq rfloc (list  (car heading--location)
                                     orig-org-fname
                                     nil
                                     (cdr heading--location))))))
            (let ((pel-refile-is-archive-restore t))
              (org-refile nil nil rfloc))
            (setq done t)))
        (unless (or done silent)
          (user-error
           "Nothing to restore; use in valid/non-empty Org Archive buffer")))
    (error "Cannot load org-refile")))

;; ---------------------------------------------------------------------------
;; Org Notification that works in terminal-based Emacs
;; ---------------------------------------------------

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
