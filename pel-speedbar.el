;;; pel-speedbar.el --- PEL (Sr-)Speedbar support -*-lexical-binding: t; -*-

;; Copyright (C) 2020, 2021  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

;; This file is part of the PEL package
;; This file is not part of GNU Emacs.

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

;;; ------------------------------------------------------------------
;;; Commentary:
;;
;; A set of utilities to help work with the native Emacs speedbar and
;; sr-speedbar which displays in the same frame, which is useful in terminal
;; mode.
;;
;; In graphics mode, both are equally functional, but in terminal mode
;; Sr-speedbar is clearly superior because Speedbar will take over the entire
;; frame while SR-Speedbar uses only one of the windows.
;;
;; If SR-speedbar is not available, Speedbar is used.
;;
;; If SR-speedbar is available, when Emacs runs in graphics mode, then
;; `pel-open-close-speedbar' prompts the first time it's called to select
;; which one to use.
;;
;; When Emacs runs in terminal mode, `pel-open-close-speedbar' prompts only if
;; the user-option variable `pel-prefer-sr-speedbar-in-terminal' is nil,
;; otherwise it automatically selects Sr-Speedbar, which is more convenient.
;;
;; The mechanism selected is remembered inside the variable
;; `pel-speedbar-type-used'.  If you want to prevent the first prompt set this
;; variable to 'sr-speedbar or 'speedbar.
;;
;;
;; Control Behaviour on SR-Speedbar item selection
;; -----------------------------------------------
;;
;; SR-speedbar has a potentially annoying behaviour: when you select a file or
;; tag from the speedbar it opens the file in a buffer but returns the point
;; back to the speedbar instead of leaving it inside the file's buffer window.
;;
;; In some case this behaviour might be useful though.  PEL provides the
;; user-option `pel-sr-speedbar-move-point-to-target-on-select'.  By default
;; it is set to t, meaning that we want to leave point inside the window of
;; the target, just like Speedbar behaves.  You can set it nil to get
;; SR-standard behaviour of returning point into the SR-Speedbar buffer after
;; a selection.  The `pel--sr-speedbar-move-point-to-target-on-select' global
;; variable is initialized with its value and can be modified by the command
;; `pel-sr-speedbar-toggle-select-behaviour' dynamically.  Then
;; `el-sr-speedbar-visiting-control' is used as a call-back for the following
;; SR-Speedbar hooks:
;;
;;  - `speedbar-visiting-file-hook', and
;;  - `speedbar-visiting-tag-hook'.
;;
;; Focus SR-Speedbar on current file
;; ---------------------------------
;;
;; The command `pel-speedbar-focus-current-file' updates the SR-speedbar,
;; updating its content to show the content of the currently edited file at
;; the top of the SR-speedbar window.
;;
;;
;; Note:
;;
;; The 2014 version of the sr-speedbar.el file attempts to access the
;; `helm-alive-p' variable, but does not require helm nor check if that
;; variable is bounded, causing byte-compilation warnings in pel.el or here.
;; The 2016 version of sr-speedbar fixes this problem.
;;
;; If you get this warning while byte compiling any of the PEL code, make sure
;; to use the 2016 version of sr-speedbar, available at MELPA.

;;;---------------------------------------------------------------------------
;;; Dependencies:

(require 'speedbar)
(require 'pel--base)         ; use: pel-emacs-is-graphic-p
(require 'pel--macros)
(require 'pel--options)      ; use: pel-prefer-sr-speedbar-in-terminal
;; sr-speedbar may not be installed.
;; Allow compilation if it's not installed:
;; later code checks if its symbols are bound
(require 'sr-speedbar nil :noerror)

;;;---------------------------------------------------------------------------
;;; Code:

;; PEL: Speedbar/Sr-Speedbar management
;; ------------------------------------
;;
;; variables:
;; speedbar-frame     : holds frame of speedbar, nil if not opened.
;; sr-speedbar-window : nil when there is no sr-speedbar,
;;                      otherwise holds it window.

(defvar pel-window-before-sr-speedbar
  nil
  "If non-nil, holds the window to return to from sr-speedbar.")

(defvar pel-speedbar-type-used
  (if (not (fboundp 'sr-speedbar-toggle))
      'speedbar
    (if pel-emacs-is-graphic-p
        nil
      (when pel-prefer-sr-speedbar-in-terminal
        'sr-speedbar)))
  "Identifies the type of Speedbar component used.
The values are:
- nil:          nothing used so far, prompt first time used.
- 'speedbar:    Speedbar opened in another frame.
                Available in graphics mode only.
- 'sr-speedbar: Sr-Speedbar, a speedbar in the same frame.

'sr-speedbar is only allowed if available.
When available, 'sr-speedbar is preferred in terminal mode,
unless `pel-prefer-sr-speedbar-in-terminal' is nil.")

(defvar pel--speedbar-active
  nil
  "Identifies whether Speedbar was already opened.
t if Speedbar was opened, nil otherwise.
Do *not* change its value manually.")

;; --

(defun pel--speedbar-toggle ()
  "Open/close Speedbar appropriately, remember we're using it."
  (setq pel-speedbar-type-used 'speedbar)
  (if pel-emacs-is-graphic-p
      (speedbar)
    (speedbar-get-focus)))

(defun pel--sr-speedbar-toggle ()
  "Open/close Sr-Speedbar and remember we're using it."
  (pel-when-fbound 'sr-speedbar-toggle
    (setq pel-speedbar-type-used 'sr-speedbar)
    (sr-speedbar-toggle)))

;;-pel-autoload
(defun pel-open-close-speedbar ()
  "Use/close appropriate speedbar: Speedbar or Sr-Speedbar.
- If Sr-Speedbar is not available, use Speedbar.
- If Sr-Speedbar is available, then:
  - In text mode, if `pel-prefer-sr-speedbar-in-terminal'
    is non-nil use Sr-Speedbar (other window, same frame),
    otherwise prompt and use what user selects.
  - In graphics mode, always prompt, and use what user selects.
Once Speedbar or Sr-Speedbar has been used, keep using the same
in subsequent calls of the Emacs session."
  (interactive)
  (cond ((not pel-speedbar-type-used)
         (if (yes-or-no-p
              (if pel-emacs-is-graphic-p
                  "Use separate frame? "
                "Use entire terminal frame instead of a dedicated window? "))
             (pel--speedbar-toggle)
           (pel--sr-speedbar-toggle)))
        ((equal pel-speedbar-type-used 'speedbar)
         (pel--speedbar-toggle))
        ((equal pel-speedbar-type-used 'sr-speedbar)
         (pel--sr-speedbar-toggle)))
  (setq pel--speedbar-active t))

;; --
;;
;; * pel-toggle-to-speedbar
;;   - pel--toggle-to-sr-speedbar-window

(defun pel--toggle-to-sr-speedbar-window ()
  "Move point to/out of sr-speedbar.
If point is not in sr-speedbar and sr-speedbar exists move to it.
Otherwise return to previously visited window if it still exists,
otherwise move to the other window."
  (pel-when-fbound 'sr-speedbar-window-p
    (pel-when-fbound 'sr-speedbar-select-window
      (if pel--speedbar-active
          (if (sr-speedbar-window-p)
              (when pel-window-before-sr-speedbar
                ;; Return point back to window that had focus before
                ;; moving to sr-speedbar If that window is still
                ;; alive, otherwise move to other window.
                (if (window-live-p pel-window-before-sr-speedbar)
                    (select-window pel-window-before-sr-speedbar)
                  (other-window 1))
                (setq pel-window-before-sr-speedbar nil))
            ;; else: remember current window and move to sr-speedbar
            (setq pel-window-before-sr-speedbar (selected-window))
            (sr-speedbar-select-window))
        (user-error "Open SR-Speedbar first with pel-open-close-speedbar")))))

;;-pel-autoload
(defun pel-toggle-to-speedbar ()
  "Move point to speedbar frame or sr-speedbar window or back.
If no speedbar is used, open one."
  (interactive)
  (if (not pel-speedbar-type-used)
      (pel-open-close-speedbar)
    (if (equal pel-speedbar-type-used 'sr-speedbar)
        (pel--toggle-to-sr-speedbar-window)
      (user-error "The speedbar used is inside a separate frame "))))

;; --

;;-pel-autoload
(defun pel-speedbar-toggle-refresh ()
  "Toggle automatic refresh of used Speedbar."
  (interactive)
  (if pel--speedbar-active
      (cond ((equal pel-speedbar-type-used 'speedbar)
             (speedbar-toggle-updates)
             (message
              "Speedbar automatic refresh is now: %s"
              (pel-symbol-on-off-string 'speedbar-update-flag)))
            ((equal pel-speedbar-type-used 'sr-speedbar)
             (pel-when-fbound 'sr-speedbar-refresh-toggle
               (sr-speedbar-refresh-toggle)
               (message
                "Sr-Speedbar automatic refresh is now: %s"
                (pel-symbol-on-off-string 'sr-speedbar-auto-refresh))))
            (t
             (error "Logic error, please report")))
    (user-error "Open Speedbar first")))

;; --

;;-pel-autoload
(defun pel-speedbar-refresh ()
  "Force refresh of Speedbar content."
  (interactive)
  (if pel--speedbar-active
      (cond ((equal pel-speedbar-type-used 'speedbar)
             (speedbar-refresh t))
            ((equal pel-speedbar-type-used 'sr-speedbar)
             (pel-when-fbound 'sr-speedbar-refresh
               (sr-speedbar-refresh)))
            (t
             (error "Logic error, please report")))
    (user-error "Open Speedbar first")))

;;-pel-autoload
(defun pel-speedbar-toggle-show-all-files ()
  "Execute `speedbar-toggle-show-all-files' if loaded, warn otherwise."
  (interactive)
  (if (fboundp 'speedbar-toggle-show-all-files)
      (speedbar-toggle-show-all-files)
    (user-error "Open Speedbar first")))

;;-pel-autoload
(defun pel-speedbar-toggle-sorting ()
  "Execute `speedbar-toggle-sorting' if loaded, warn otherwise."
  (interactive)
  (if (fboundp 'speedbar-toggle-sorting)
      (let ((warning-msg (when (member 'speedbar-trim-words-tag-hierarchy
                                       speedbar-tag-hierarchy-method)
                           "\n⚠️ Speedbar trims and regroups symbols.  \
Sorting has no impact. \
Customize speedbar-tag-hierarchy-method to change.")))
      (message "Speedbar sorting now %s. \
Contract and re-expand parents to see the change.%s"
               (pel-on-off-string (speedbar-toggle-sorting))
               (or warning-msg "")))
    (user-error "Open Speedbar first")))

;;-pel-autoload
(when pel-emacs-is-graphic-p
  (defun pel-speedbar-toggle-images ()
    "Execute `speedbar-toggle-images' if loaded, warn otherwise."
    (interactive)
    (if (fboundp 'speedbar-toggle-images)
        (speedbar-toggle-images)
      (user-error "Open Speedbar first"))))

;; --
;; Control Behaviour on SR-Speedbar item selection
;; -----------------------------------------------
;;
;; The following logic determines what of the following 2 behaviours
;; are used by the speedbar when a speedbar item is selected from the
;; speedbar window, after the selected item is shown inside another
;; Emacs window:

;; 1) point is moved to the window where the item is shown,
;; 2) point remains inside the speedbar, allowing the user to select
;;   another item.
;;
;; The default behaviour is selected by the
;; `pel-sr-speedbar-move-point-to-target-on-select' user-option.  It
;; can also be modified dynamically during an editing session by
;; calling the command `pel-sr-speedbar-toggle-select-behaviour'.
;;
;; The heart of the control is the `pel-sr-speedbar-visiting-control'
;; call-back used by the `speedbar-visiting-file-hook' which Speedbar
;; invokes when a user selects a Speedbar file or tag item.


(defvar pel--sr-speedbar-move-point-to-target-on-select
  pel-sr-speedbar-move-point-to-target-on-select
  "Global behaviour of `pel-sr-speedbar-move-point-to-target-on-select.")

;;-pel-autoload
(defun pel-sr-speedbar-toggle-select-behaviour ()
  "Toggle SR-Speedbar selection behaviour."
  (interactive)
  (pel-toggle-and-show 'pel--sr-speedbar-move-point-to-target-on-select
                       "Move to target on SR-Speedbar select"
                       "Stay in SR-Speedbar buffer after select"))

(defun pel--select-sr-speedbar-buffer-window ()
  "Select SR-Speedbar buffer window."
  (let ((sp-win (get-buffer-window "*SPEEDBAR*")))
    (if sp-win
        (select-window sp-win)
      (error "Can't find buffer *SPEEDBAR*"))))

(defun pel-sr-speedbar-visiting-control ()
  "Hook callback: determine what buffer to select.

This function is used by SR-speedbar hooks
`speedbar-visiting-file-hook' and `speedbar-visiting-tag-hook'.
It returns point to the SR-Speedbar buffer window when
`pel--sr-speedbar-move-point-to-target-on-select' is nil,
otherwise it leaves it in the window of the file selected by the
SR-Speedbar selection user action."
  (unless pel--sr-speedbar-move-point-to-target-on-select
    (pel--select-sr-speedbar-buffer-window)))

;; --
;; Controlling the Speedbar item in focus
;; --------------------------------------
;;
;; It would be nice to be able to automatically make the currently
;; edited file (or tag) visible in the Speedbar window as soon as a
;; new window is selected or when the content of the current window is
;; changed to another buffer.
;;
;; At this point, at least with Emacs 26.3, I did not find an Emacs
;; hook that I could use to update the Speedbar window when visiting a
;; new file or changing window or the content of a window.
;;
;; So for now all I have is the command `pel--speedbar-focus-on' which
;; forces an update of the content of the Speedbar window to put the
;; current file content in focus: display it at the top of the
;; Speedbar window and expand its top-level list.

(defun pel--speedbar-focus-on (buffer-name sp-win &optional original-window)
  "Set speedbar focus on the content of specified BUFFER-NAME.
Put the content for that buffer at the top of the speedbar and
expand all first level elements.

The SP-WIN argument identifies the Speedbar window.
If optional ORIGINAL-WINDOW is not nil, return to that window,
otherwise leave focus inside speedbar."
  ;; move Speedbar line for current file at the top of
  ;; the speedbar
  (select-window sp-win)
  (goto-char (point-min))
  (let (file-name-position)
    ;; first expand first level (file content) if found
    (when (search-forward (concat "] " buffer-name)
                          nil :noerror)
      (setq file-name-position (point))
      (speedbar-expand-line)
      ;; then expand all found second level items
      (while
          (when (search-forward " {+}" nil :noerror)
            (speedbar-expand-line)))
      ;; put the first level at the top of the buffer
      (goto-char file-name-position)
      (recenter-top-bottom 0)))
  (when original-window
    (select-window original-window)))

;;-pel-autoload
(defun pel-speedbar-focus-current-file (&optional stay-in-speedbar)
  "Set SR-Speedbar focus to the content of current window buffer.

Place the its tag list at the top of the speedbar and expand all
first and second level items.

If optional STAY-IN-SPEEDBAR argument is non-nil, move point to
speedbar, otherwise don't move it."
  (interactive "P")
  (if (boundp 'speedbar-initial-expansion-list-name)
      (if (eq pel-speedbar-type-used 'sr-speedbar)
          (let ((original-window (unless stay-in-speedbar (selected-window)))
                (buf-name (buffer-name))
                (sp-win (get-buffer-window "*SPEEDBAR*")))
            (if sp-win
                (cond
                 ((string= speedbar-initial-expansion-list-name "files")
                  (let ((fname (pel-current-buffer-filename
                                :sans-directory nil :no-error)))
                    (if fname
                        (pel--speedbar-focus-on fname sp-win original-window)
                      (user-error "Current buffer %s is not visiting a file"
                                  buf-name))))
                 ((member speedbar-initial-expansion-list-name
                          '("buffers"
                            "quick buffers"))
                  (pel--speedbar-focus-on buf-name sp-win original-window))
                 (t (error "Unsupported speedbar content: %s"
                           speedbar-initial-expansion-list-name)))
              (user-error "Open speedbar first!")))
        (if (eq pel-speedbar-type-used 'speedbar)
            (user-error "This command only supports SR-speedbar!")
          (user-error "Please open a SR-Speedbar first!")))
    (error "Cannot load speedbar!")))

;;; --------------------------------------------------------------------------
(provide 'pel-speedbar)

;;; pel-speedbar.el ends here

;; LocalWords:  speedbar sr
