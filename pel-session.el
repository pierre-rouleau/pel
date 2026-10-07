;;; pel-session.el --- Session Utilities  -*- lexical-binding: t; -*-

;; Created   : Tuesday, September 29 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-10-06 22:45:33 EDT, updated by Pierre Rouleau>

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
;; This file holds the logic to provide UI-driven customization of the
;; selection of easysession.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)     ; use: `pel-toggle-mode-and-show', `pel-toggle-and-show'
(require 'pel--options)  ; use `pel-use-easysession', `pel--easysession-catalog-fname'
(require 'pel-prompt)    ; use: `pel-prompt'
(require 'cl-lib)        ; use: `cl-delete-if-not'
(require 'savehist)      ; use: `savehist-additional-variables'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; easysesion.el support
;; =====================

;; Since easysession might not be installed, we can't require it
;; at the top level.  To allow byte compilation of the code all
;; of the functions are declared here and the variable defined as defvar.

(declare-function easysession-get-session-name               "easysession")
(declare-function easysession-set-current-session-name       "easysession")
(declare-function easysession-load-including-geometry        "easysession")
(declare-function easysession-scratch-mode                   "easysession")
(declare-function easysession-magit-mode                     "easysession")
(declare-function easysession-mode-line-session-name-format  "easysession")
(declare-function easysession-reset                          "easysession")
(declare-function easysession-kill-all-buffers               "easysession")
(declare-function easysession-visible-buffer-list            "easysession")
(declare-function easysession-save                           "easysession")
(declare-function easysession-switch-to                      "easysession")
(declare-function easysession-switch-to-and-restore-geometry "easysession")
(declare-function easysession-save-mode                      "easysession")
(declare-function easysession-reset                          "easysession")

(defvar easysession-mode-line-misc-info)
(defvar easysession-new-session-hook)
(defvar easysession-switch-to-save-session)
(defvar easysession-save-mode-predicate)
(defvar easysession-buffer-list-function)
(defvar easysession-edit-read-only)
(defvar easysession-save-pretty-print)
(defvar easysession-confirm-new-session)
(defvar easysession-setup-load-predicate)
(defvar easysession-fontify)
(defvar easysession-quiet)
(defvar easysession-save-interval)
(defvar easysession-switch-to-exclude-current)

(defvar pel-emacs-launch-directory)


(defun pel--easysession-only-main-saved ()
  "Only save the main session."
  (when (equal "main" (easysession-get-session-name))
    t))



(defun pel--easysession-auto-load-if-env ()
  "Predicate: return t if PEL_SESSION_AUTO envvar is 1.

When activated by \\='auto-load-when-PEL_SESSION_AUTO-is-1, this
controls whether session is automatically loaded on start when the
PEL_SESSION_AUTO environment variable exists and its value is 1."
  (let* ((value (getenv "PEL_SESSION_AUTO")))
    (and value (string= value "1"))))

(defun pel--easysession-kill-all-buffers ()
  "Kill all buffers before switching sessions."
  (easysession-kill-all-buffers))

(defvar pel--easysession-filter-invisible-buffers nil
  "Set to t to exclude saving/restoring invisible buffers.

Note: this also exclude the remote Tramp-accessed file buffers.
      I reported this as bug 77.  See:
      https://github.com/jamescherti/easysession.el/issues/77")

(defvar pel--easysession-filter-remote-buffers nil
  "Set to t to exclude saving/restoring remote buffers accessed via Tramp.")

(defun pel--easysession-relevant-buffers ()
  "Return list of buffers to save/restore, given restrictions."
  (let ((all-buffers (buffer-list))
        (visible-buffers nil))
    ;;
    (when pel--easysession-filter-invisible-buffers
      (setq visible-buffers (easysession-visible-buffer-list))
      ;; Remove all non-visible buffers from all-buffers
      (setq all-buffers (seq-filter (lambda (x)
                                      (member x visible-buffers))
                                    all-buffers)))
    ;;
    (when pel--easysession-filter-remote-buffers
      ;; Remove all remote (Tramp) buffers from all-buffers
      (let ((local-buffers nil))
        (dolist (buffer all-buffers)
          (let ((dir (buffer-local-value 'default-directory buffer)))
            (unless (and dir (file-remote-p dir))
              (push buffer local-buffers))))
        (setq all-buffers (reverse local-buffers))))
    ;;
    ;; return remaining buffers
    all-buffers))

;; --

(defvar easysession-scratch-mode)

;;-pel-autoload
(defun pel-easysession-config (cfg)
  "Configure easysession according to its CFG.
The CFG argument is meant to be the value of `pel-use-easysession'
when it is set for extensions.  That function is meant to be called once
easysession is loaded."
  ;; Restore defaults before updating the values
  (require 'easysession)
  ;; (message "PEL pel-easysession-config: %S" cfg)
  (setq pel--easysession-filter-invisible-buffers nil
        pel--easysession-filter-remote-buffers    nil
        easysession-buffer-list-function          #'buffer-list)

  ;; The very first element is the ('save-interval . value)
  ;; where the car is a symbol to identify the purpose of the value
  ;; and the cdr is the interval integer in minutes.
  (let ((elm (car-safe cfg)))
    (when (eq (car elm) 'save-interval)
      (setq elm (cdr elm))
      (when (and (integerp  elm)
                 (> elm 0))
        (setq easysession-save-interval (* 60 elm)))))
  ;; Always activate automatic session save
  (easysession-save-mode 1)
  ;; The remainder of the list is a set of options.
  (dolist (elm (cadr cfg))
    ;; (message "PEL pel-easysession-config: check %S" elm)
    (cond
     ;; Save the current session when using `easysession-switch-to'
     ((eq elm 'save-current-session-when-switching)
      (setq easysession-switch-to-save-session t))
     ;;
     ;; Exclude current session name when loading or switch session
     ((eq elm 'exclude-current-session-when-switching)
      (setq easysession-switch-to-exclude-current t))
     ;;
     ;; Persist and restore the scratch buffer
     ((eq elm 'save-scratch-buffer)
      (require 'easysession-scratch)
      (easysession-scratch-mode 1))
     ;;
     ;; Persist and restore Magit buffers
     ((eq elm 'save-magit-buffers)
      (require 'easysession-magit)
      (easysession-magit-mode 1))
     ;;
     ;; Make the current session name appear in the mode-line
     ((eq elm 'show-name-in-mode-line)
      (setq easysession-mode-line-misc-info t))
     ;;
     ;; Display the session name in the tab bar
     ((eq elm 'show-name-in-tab-bar)
      ;; This is only available in some Emacs versions
      (when (boundp 'tab-bar-format)
        (setq tab-bar-format '(tab-bar-format-tabs
                               tab-bar-format-align-right
                               tab-bar-format-global))
        (add-to-list 'global-mode-string
                     '(:eval (easysession-mode-line-session-name-format))
                     'append)))
     ;;
     ;; Create an empty session setup
     ((eq elm 'create-minimal-sessions)
      (add-hook 'easysession-new-session-hook #'easysession-reset))
     ;;
     ;; Auto-save only main session, manually save others
     ((eq elm 'auto-save-main-session)
      (setq easysession-save-mode-predicate #'pel--easysession-only-main-saved))
     ;;
     ;; Kill all buffers/frames/windows before loading a session
     ((eq elm 'kill-all-before-loading-session)
      (add-hook 'easysession-before-load-hook #'easysession-reset))
     ;;
     ;; Make easysession-reset save all buffers without prompting before killing everything
     ((eq elm 'auto-save-all-buffer-before-loading-session)
      ;; Automatically save all buffers without prompting the user
      (add-hook 'easysession-before-reset-hook #'(lambda ()
                                                   (save-some-buffers t))))
     ;;
     ;; Kill all buffers when changing a session
     ((eq elm 'kill-all-buffers-when-changing-session)
      (add-hook 'easysession-before-load-hook
                #'pel--easysession-kill-all-buffers))
     ;;
     ;; Save/restore only visible buffers (see note #3)
     ((eq elm 'save-visible-buffers)
      ;; Restrict session persistence and restoration to buffers that are
      ;; visible A buffer is included if it satisfies any of the following:
      ;; - It is currently displayed in a visible window of a visible frame.
      ;; - It is associated with a visible tab in tab-bar-mode, if enabled.
      (setq pel--easysession-filter-invisible-buffers t
            easysession-buffer-list-function
            #'pel--easysession-relevant-buffers))
     ;;
     ;; Exclude restoring remote Tramp-accessed buffers
     ((eq elm 'dont-restore-remote-tramp-buffers)
      (setq pel--easysession-filter-remote-buffers t
            easysession-buffer-list-function
            #'pel--easysession-relevant-buffers))
     ;;
     ;; Open saved session file in read-only mode
     ((eq elm 'edit-saved-session-file-in-RO)
      (setq easysession-edit-read-only t))
     ;;
     ;; Save session file in human-readable format
     ((eq elm 'save-in-human-readable-format)
      (setq easysession-save-pretty-print t))
     ;;
     ;; Disable new session confirmation prompt
     ((eq elm 'no-prompt-on-new-session)
      (setq easysession-confirm-new-session nil))
     ;;
     ;;
     ;; Ensure restored buffers are properly fontified
     ((eq elm 'force-fontification)
      (setq easysession-fontify t))
     ;;
     ;; Suppress EasySession messages
     ((eq elm 'force-quietness)
      (setq easysession-quiet t))
     ;;
     ;; Add global variables
     ((and (listp elm) (eq (car elm) 'saved-global-variables))
      ;; Iterate in the provided list, adding the symbols to the
      ;; savehist-additional-variables; the order of storage inside that list
      ;; is not important.
      (dolist (em (cdr elm))
        (if (symbolp em)
            ;; It's one of the pre-defined variable symbols
            (push em savehist-additional-variables)
          ;; Is it a list of user-supplied variable symbols?
          (if (and (listp em) (eq (car em) 'user-defined-globals))
              (dolist (e (cdr em))
                (if (symbolp e)
                    (push e savehist-additional-variables)
                  (display-warning 'pel-session
                                   (format "\
Error in pel-use-easysession: Invalid global variable entry: %S" e)
                                   :warning)))
            (display-warning 'pel-session
                             (format "\
Error in pel-use-easysession: Invalid global variable entry: %S" em)
                             :warning))))))))


(defun pel-easysession-toggle-save-on-switch ()
  "Toggle saving session when switching."
  (interactive)
  (pel-toggle-and-show 'easysession-switch-to-save-session))

(defun pel-easysession-toggle-save-on-exit ()
  "Toggle the `easysession-save-mode’ mode global minor mode.

When the mode is active easysession stores the current session when
Emacs exists."
  (interactive)
  (pel-toggle-mode-and-show 'easysession-save-mode))

;; ---------------------------------------------------------------------------
;; Automatic Session Identification
;; ================================

;; Note: the following regexp-quote is a safeguard that is currently not
;; required because the separator string does not need to be escaped.
;; I keep it in case we decide to change the separator and the new one would
;; need escaping.  The escaping is done at compile time to minimize run time
;; impact.
(defconst pel--esasysession-separator (eval-when-compile
                                        (regexp-quote "\t;--pel--;\t"))
  "Pre-Escaped regex pattern for the session catalog file delimiter.")

(defconst pel--easysession-catalog-fname
  (locate-user-emacs-file pel-easysession-catalog-filename)
  "Fully expanded easysession catalog file name.")

;; --

(defun pel-easysession-name-for (dirpath)
  "Return the session name associated with DIRPATH.

DIRPATH may begin with ~.  However the content of the catalog file always
stores the directory names as absolute filename paths and the function will
expand it for the current user."
  ;; Use the fastest possible way to look-up the file contents. Don't try using
  ;; `insert-file-contents-literally' to bypass Emacs' heavy character-set
  ;; decoding, format conversions, and text property attachments, because then
  ;; the code would have to explicitly encode/decode to support non-ASCII
  ;; character and we'd end-up doing it in elisp code slower than what Emacs
  ;; does more efficiently in C.  So use insert-file-contents and let Emacs
  ;; deal with potential encoding.
  ;;
  (when (file-exists-p pel--easysession-catalog-fname)
    (with-temp-buffer
      (buffer-disable-undo)
      (insert-file-contents pel--easysession-catalog-fname)
      (goto-char (point-min))
      ;; Bound the pattern explicitly between your unique separator and the end of the line
      (let ((entry-regex (concat "^"
                                 (regexp-quote (expand-file-name dirpath))
                                 pel--esasysession-separator
                                 "\\(.*\\)$")))
        (when (re-search-forward entry-regex nil t)
          (match-string 1))))))


(defun pel-easysession-save-session-in-catalog (dir session)
  "Save a DIR and SESSION association at the TOP of session catalog safely.

If the file is locked or being written to by another Emacs process, then
loop and retry up to 5 times using exponential backoff."
  (let* ((catalog-fpath pel--easysession-catalog-fname)
         (max-attempts 5)
         (attempt 1)
         (success nil)
         (lock-file (concat (file-name-directory catalog-fpath)
                            ".#" (file-name-nondirectory catalog-fpath)))
         (entry-regex (concat "^"
                              (regexp-quote (expand-file-name dir))
                              pel--esasysession-separator
                              ".*$"
                              ))
         (new-catalog-line (concat (expand-file-name dir)
                                   pel--esasysession-separator
                                   session
                                   "\n")))

    (while (and (not success) (<= attempt max-attempts))
      (if (file-exists-p lock-file)
          ;; 1. FILE IS LISP-LOCKED: Wait and increase attempt count
          (let ((sleep-time (* 0.05 (expt 2 (1- attempt))))) ; 0.05s, 0.1s, 0.2s...
            (message "File %s locked by another process, retrying in %fs..." catalog-fpath sleep-time)
            (sit-for sleep-time)
            (setq attempt (1+ attempt)))
        ;;
        ;; 2. FILE IS FREE: proceed:
        ;;       Try save new association, placing at top of file.
        ;;       This is a read-modify-write operation done by loading the
        ;;       file's content inside a temporary buffer, modify the buffer
        ;;       content and writing the buffer back into the file.
        (condition-case err
            (progn
              (with-temp-buffer
                (buffer-disable-undo)
                ;; Read file content (if it exists)
                (when (file-exists-p catalog-fpath)
                  (insert-file-contents catalog-fpath))
                ;;
                (goto-char (point-min))
                ;; Loop to find and purge all existing lines for this directory
                (goto-char (point-min))
                (while (re-search-forward entry-regex nil t)
                  (replace-match "")
                  (unless (eobp)
                    (delete-char 1)))
                ;; Prepend the new entry to the top of the buffer
                (goto-char (point-min))
                (insert new-catalog-line)
                ;; Low-level write and close
                (write-region (point-min) (point-max) catalog-fpath nil 'quiet))
              ;; If we reached here without a file-error, it succeeded!
              (setq success t))

          ;; 3. WRITE FAILED: Catch OS level race conditions and retry
          (file-error
           (if (< attempt max-attempts)
               (let ((sleep-time (* 0.05 (expt 2 (1- attempt)))))
                 (sit-for sleep-time)
                 (setq attempt (1+ attempt)))
             ;; If we run out of attempts, re-throw the error to the user
             (error "Failed to write to session file after %d attempts: %s"
                    max-attempts (cdr err)))))))))

;; --

(defun pel--easysession-load (session-name)
  "Load SESSION-NAME with activated options."
  ;; (message "pel--easysession-load: name=%S" session-name)
  (when session-name
    (message "pel--easysession-load: Loading session %s" session-name)
    ;; (pel-easysession-config pel-use-easysession)
    (easysession-set-current-session-name session-name)
    ;; When loading for the first time, don't save session setup
    ;; before attempting to first load it.
    (let ((easysession-switch-to-save-session nil))
      (if (display-graphic-p)
          (easysession-load-including-geometry session-name)
        (easysession-switch-to session-name)))))

(defun pel-easysession-load-by-env ()
  "Load session identified by PEL_SESSION environment variable.

If the environment variable does not exist, no loading is done.
To auto-load the main session, set PEL_SESSION to \"main\"."
  (require 'easysession)
  (let* ((env-session-name (getenv "PEL_SESSION")))
    (unless (string-empty-p env-session-name)
      (if (string= env-session-name ".")
          ;; If PEL_SESSION is ".", extract the session name associated with
          ;; the current working directory from PEL session catalog.
          (pel--easysession-load (pel-easysession-name-for
                                  pel-emacs-launch-directory))
        ;; If the PEL_SESSION value is any other name use that name
        ;; as the session name.
        (pel--easysession-load env-session-name)))))

(defun pel-easysession-save ()
  "Save the easysession name, prompt for settings."
  (interactive)
  (require 'easysession)
  (let ((name (pel-prompt "Session name"
                          'pel-easysession-name
                          nil
                          (easysession-get-session-name)))
        (current-dir-session-name (pel-easysession-name-for  pel-emacs-launch-directory)))

    ;; When saving session info, update PEL session catalog
    ;; if the current session is known to be a current-directory-specific
    ;; session or the user wants to add this session to the catalog to be able
    ;; to auto-restore it later.
    (when (or (string= name current-dir-session-name)
              (y-or-n-p "Save as directory-specific session?"))
      (pel-easysession-save-session-in-catalog pel-emacs-launch-directory
                                               name))
    (easysession-save name)))

(defun pel-easysession-load ()
  "Load an easy session, prompt to load geometry in graphics mode.

When executed in graphics-mode Emacs, the command prompts user to
restore the complete frame geometry and does it on request.

When Emacs is running in terminal mode, all frames are in the same OS window
and geometry does not apply; the function does not prompt."
  (interactive)
  (require 'easysession)
  (if (and (display-graphic-p)
           (y-or-n-p "Restore geometry"))
      (call-interactively #'easysession-switch-to-and-restore-geometry)
    (call-interactively  #'easysession-switch-to)))

(defun pel-easysession-reset ()
  "Prompt before resetting session and closing all buffers."
  (interactive)
  (when (y-or-n-p "Reset the session and close all buffers/window/tab/frames?")
    (easysession-reset)))

;; ---------------------------------------------------------------------------
;; desktop support
;; ===============
;;
;; Since desktop+ might not be installed, declare the functions the code uses
;; when it is installed.

(declare-function desktop+-create-auto "desktop+")
(declare-function desktop+-create      "desktop+")
(declare-function desktop+-load-auto   "desktop+")
(declare-function desktop+-load        "desktop+")

(defun pel-desktop-save ()
  "Save a desktop with desktop+ command is used, or with desktop."
  (interactive)
  (if (eq pel-use-desktop 'with-desktop+)
      ;; Using desktop+ commands
      (progn
        (require 'desktop+)
        (if (y-or-n-p "Create directory-specific desktop")
            (desktop+-create-auto)
          (call-interactively #'desktop+-create)))
    ;; Using desktop command
    (call-interactively #'desktop-save)))

(defun pel-desktop-load ()
  "Load a desktop with desktop+ command is used, or with desktop."
  (interactive)
  (if (eq pel-use-desktop 'with-desktop+)
      ;; Using desktop+ commands
      (progn
        (require 'desktop+)
        (if (y-or-n-p "Load directory-specific desktop")
            (desktop+-load-auto)
          (call-interactively #'desktop+-load)))
    ;; Using desktop command
    (call-interactively #'desktop-read)))

;; ---------------------------------------------------------------------------
;; Adaptive Commands
;; =================

(defun pel-session-show ()
  "Display name of currently used desktop if any."
  (interactive)
  (if (bound-and-true-p desktop-dirname)
      (message "Last loaded desktop: %s" desktop-dirname)
    (user-error "No desktop currently loaded!")))

(defun pel--with-easysession-p ()
  "Return t to use easysession commands, nil otherwise.
Check the `pel-use-desktop' and `pel-use-easysession' user-options.
Prompt if both are active."
  (cond
   ((and pel-use-easysession pel-use-desktop)
    (y-or-n-p "Use easysession"))
   (pel-use-easysession t)
   (t nil)))

(defun pel-session-save ()
  "Save a session using the current session manager.
Use easysession or desktop operations, depending on which is available.
Prompt if both are available."
  (interactive)
  (if (pel--with-easysession-p)
      (pel-easysession-save)
    (pel-desktop-save)))

(defun pel-session-load ()
  "Load a session using the current session manager.
Use easysession or desktop operations, depending on which is available.
Prompt if both are available."
  (interactive)
  (if (pel--with-easysession-p)
      (pel-easysession-load)
    (pel-desktop-load)))

;;; --------------------------------------------------------------------------
(provide 'pel-session)

;;; pel-session.el ends here
