;;; pel-org.el --- PEL Org mode utilities  -*- lexical-binding: t; -*-

;; Created   : Saturday, August 29 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-09-11 18:01:23 EDT, updated by Pierre Rouleau>

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
(require 'pel--base)         ; use `pel-running-under-ssh-p'
(require 'pel--keys-macros)
(require 'cus-edit)          ; use: `customize-option'
(require 'org)               ; use: `org-get-outline-path', `org-entry-get',
;;                           ;     `org-archive-location'
(require 'org-archive)       ; use: `org-archive--compute-location'
(require 'org-macs)          ; use: `org-with-wide-buffer'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defvar pel-has-detected-org-file) ; prevent warning; defined in pel_keys.el
                                   ; and set by a hook function when an org
                                   ; file is opened.

;;-pel-autoload
(defun pel-org-open-pdf (&optional open-github-page-p)
  "Open Ɱ Org-Mode PEL PDF.

By default the function opens the local PDF file unless the
OPEN-GITHUB-PAGE-P is specified, in which case it opens the GitHub
hosted raw PDF file.  However, if the user-option variable
`pel-flip-help-pdf-arg' is set, it's the other way around: the
GitHub remote file is opened by default."
  (interactive "P")
  (pel-help-open-pdf "mode-org-mode" open-github-page-p))


;;-pel-autoload
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

;;  * `pel-org-archive-restore'
;;    - `pel--org-buffer-is-archive-p'
;;    - `pel--org-archive-original'
;;      - `pel--org-archive-file-first-property'
;;    - `pel--org-heading-and-pos'

(defvar pel-refile-is-archive-restore nil
  "Set to t by `pel-org-archive-restore' to activate archived tree restoration.")

;;-pel-autoload
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

(defun pel--org-buffer-is-archive-p ()
  "Return t if current buffer is an Org archive file."
  (let ((fname (buffer-file-name)))
    (and fname
         (string-suffix-p ".org_archive" fname)
         (eq major-mode 'org-mode))))

;; (defun pel-org-buffer-is-archive-p (&optional buffer)
;;   "Return t if BUFFER is an Org archive file."
;;   (if buffer
;;       (with-current-buffer buffer
;;         (pel--org-buffer-is-archive-p))
;;     (pel--org-buffer-is-archive-p)))

;;-pel-autoload
(defun pel-org-archive-restore (&optional silent)
  "Restore archived sub-tree at or above point back to its original org file.
Raise an error when failing to restore item unless SILENT is non-nil."
  (interactive)
  (unless (pel--org-buffer-is-archive-p)
    (user-error "This buffer (%s) is not an Org archive buffer!" (buffer-name)))
  (require 'org-refile 'noerror)
  (if (and (fboundp 'org-narrow-to-subtree)
           (fboundp 'org-refile))
      (let (orig-org-fname
            heading-path
            local-error)
        (save-restriction
          (condition-case err
              (progn
                ;; Narrow to the subtree at point to restore that tree and get
                ;; the information from that tree, not the first one in the file.
                (org-narrow-to-subtree)
                ;; Within the narrowed subtree, search and identify the properties.
                (let ((fname--location (pel--org-archive-original))
                      done)
                  (when fname--location
                    ;; if information found proceed with the refiling.
                    (setq orig-org-fname (nth 0 fname--location)
                          heading-path   (nth 1 fname--location))
                    (let ((rfloc nil))
                      (with-current-buffer (find-file-noselect orig-org-fname)
                        (let ((heading--location (pel--org-heading-and-pos heading-path)))
                          (when heading--location
                            (setq rfloc (list (car heading--location)
                                              orig-org-fname
                                              nil
                                              (cdr heading--location))))))
                      (let ((pel-refile-is-archive-restore t))
                        (org-refile nil nil rfloc))
                      (setq done t)))
                  (unless (or done silent)
                    (setq local-error t)
                    (user-error
                     "use in valid/non-empty Org Archive buffer"))))
            (error
             (user-error "Nothing to restore: %s"
                         (if local-error
                             (error-message-string err)
                           (format "%s
Could not find \"%s\" inside file: %s
Did you change the original heading text? If so, modify the archive
:ARCHIVE_OLDPATH: value to match what is in the original Org file."
                                   (error-message-string err)
                                   heading-path
                                   orig-org-fname)))))))
    (error "Cannot load org-refile")))


;; ---------------------------------------------------------------------------
;; Archive File Creation - Prevent Flattening
;; ------------------------------------------
;;
;; When archiving timed-tracked tasks, Org mode stores the tasks in the Org
;; archive in a flattened list by default.  It's fine, but when creating a
;; clocktable based report that includes the archived files, we loose the
;; parent/child information and that information can be useful to identify
;; those tasks.
;;
;; PEL will activate the following advice when
;; `pel-org-archive-with-hierarchy' user-option is turned on.


(defun pel--org-archive-preserve-hierarchy-adv (orig-fun &rest args)
  "Advise `org-archive-subtree' to recreate the original outline path
hierarchy inside the archive file before archiving the task."
  ;; (message "PEL pel--org-archive-preserve-hierarchy-adv: START with %S %S" orig-fun args)
  (let*
      ;; oldpath := list of task parent headings
      ((oldpath (org-get-outline-path))
       ;; archive-file. := name of the archive file
       (archive-location-string (or (org-entry-get nil "ARCHIVE" 'inherit)
                                    org-archive-location))
       (archive-file (car (org-archive--compute-location archive-location-string))))
    (if (and oldpath archive-file)
        ;;
        ;; Proceed with enhanced archiving.
        (progn
          ;; PHASE 1: Reconstruct the structural nodes inside the archive buffer.
          ;;  - open the archive file cleanly in the background.
          (with-current-buffer (find-file-noselect archive-file)
            (org-with-wide-buffer
             (goto-char (point-min))
             (let
                 ;; Track heading level and its scope.
                 ((current-level 1)
                  (scope-start   (point-min))
                  (scope-end     (point-max)))
               (dolist (heading oldpath)
                 (narrow-to-region scope-start scope-end)
                 (goto-char (point-min))
                 (let ((heading-regexp (format
                                        "^%s %s$"
                                        (regexp-quote (make-string current-level ?*))
                                        (regexp-quote heading))))
                   (if (re-search-forward heading-regexp nil t)
                       ;; Found end of parent heading.
                       (progn
                         ;; Find its tree boundaries for the next iteration loop.
                         (setq scope-start (point))
                         (org-end-of-subtree t t)
                         (setq scope-end (point)))
                     ;;
                     ;; Parent heading is missing from the archive.
                     ;; - Insert it cleanly at the end of the current scope.
                     (goto-char (point-max))
                     (unless (bolp) (insert "\n"))
                     (insert (format "%s %s\n" (make-string current-level ?*) heading))
                     ;; - Narrow the scope to this brand-new empty parent tree
                     (setq scope-start (point))
                     (setq scope-end (point))))
                 ;; Temporarily widen: allow next loop cycle to re-narrow correctly
                 (widen)
                 ;; and increment heading level
                 (setq current-level (1+ current-level))))))

          ;; PHASE 2: Back in the original buffer, set up the targeted override and
          ;; execute the original archiving with `org-archive-location' set
          ;; to the target location for this specific archive action to land
          ;; precisely under the newly verified/created parent hierarchy.
          ;; (message "PEL pel--org-archive-preserve-hierarchy-adv: About to invoke org-archive-subtree in %S" (current-buffer))
          (let* ((parent-depth (length oldpath))
                 (parent-stars (make-string parent-depth ?*))
                 (org-archive-location (format "%s::%s %s"
                                               archive-file
                                               parent-stars
                                               (car (last oldpath)))))
            (apply orig-fun args))

          ;; PHASE 3: return the Org Archive buffer in its startup view mode.
          (with-current-buffer (find-file-noselect archive-file)
            (when (require 'org-cycle :noerror)
              ;; Reset the visibility view back to the org archive file #+STARTUP preference
              ;; and cleanly save the file in the background.
              (org-cycle-set-startup-visibility)
              (save-buffer))))
      ;;
      ;; Could not find task parent headings: perform standard archiving.
      (apply orig-fun args))))

(defun pel-org-enhance-archiving ()
  "Enhance Org archiving: store the task hierarchy in the archive."
  (advice-add 'org-archive-subtree :around
              #'pel--org-archive-preserve-hierarchy-adv))

;; ---------------------------------------------------------------------------
;; Specialized Org Customization Commands
;; --------------------------------------

;;-pel-autoload
(defun pel-customize-org-agenda (&optional other-window)
  "Customize Org Agenda.
If OTHER-WINDOW is non-nil display in other window."
  (interactive "P")
  (pel--customize-group "org-agenda" other-window))

;; ---------------------------------------------------------------------------
;; Org Notification that works in terminal-based Emacs
;; ---------------------------------------------------

;;-pel-autoload
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

;; ---------------------------------------------------------------------------
;; Org Clock Table Report Support
;; ------------------------------

;;-pel-autoload
(defun pel-org-get-project-files ()
  "Return the list of project files identified by `pel-org-project-files'.

Use this inside an Org clocktable :scope argument.
See an example inside the file example/templates/org-mode/master-org.org"
  pel-org-project-files)

;;; --------------------------------------------------------------------------
(provide 'pel-org)

;;; pel-org.el ends here
