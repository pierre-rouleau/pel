;;; pel-buffer.el --- PEL buffer management utilities.  -*- lexical-binding: t; -*-

;; Created   : Thursday, May 27 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-06-01 11:52:04, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021  Pierre Rouleau
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
;; Provide ability to navigate through buffers in sorted order as specified by
;; the built-in bs.el library.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: `pel-buffers-in-mode',
;;                                      ;      `pel-major-mode-of'
(require 'pel-list)                     ; use: `pel-list-index'
(require 'bs)                           ; Emacs built-in

;;; --------------------------------------------------------------------------
;;; Code:
;;
;; This provides ability to iterate through buffers of the same major mode.
;; Two facilities are provided: an extension of the Buffer Selection built-in
;; Emacs `bs' feature and another, totally independent mechanism.
;;
;; The Buffer Selection extension:
;; - - - - - - - - - - - - - - - -
;;
;; The Buffer Selection extension mechanism adds buffer filter configurations,
;; named 'only-X' where X is the major mode name.  The code provides a new
;; command, the function `pel-bs-this-mode-only' that is mapped into the
;; Buffer Selection key map using the "." key.
;;
;; To add a new major-mode-specific configuration the user must first open the
;; Buffer Selection mode buffer, move point to a buffer line that lists a
;; buffer of the wanted mode and then press the "." key.  This invokes the
;; function `pel-bs-this-mode-only' that creates and activates a new
;; specialized Buffer Selection configuration.  That configuration can be
;; invoked later using the "c" or "C" Buffer Selection configuration.
;;
;; The name of each new specialized Buffer Selection configuration is stored
;; in the variable `pel--registered-bs-mode-configurations' to prevent
;; re-creation of the same closures and limit memory consumption.
;;
;; Additionally the file provides the command functions `pel-bs-next' and
;; `pel-bs-previous' that changes the current buffer to the next or previous
;; buffer in the currently active Buffer Selection list.  These allow a user
;; to quickly cycle through the buffers in that list without having to open
;; the Buffer Selection buffer itself.
;;
;; On startup PEL hooks the function `pel-bs-init' to the `bs-mode-hook' to
;; activate the "." key binding in the Buffer Selection key map.
;;
;; While the use of Buffer Selection mechanism described above work fine it
;; requires the user to open a Buffer Selection buffer and select the proper
;; configuration first.  That's useful in itself because you can get a list of
;; all buffers of the same mode quickly.  But if you're in a buffer of a
;; specific major mode and just want to go to the next one with the same major
;; mode, that's a lot of things to do.  It'd be nice to only have to hit a key
;; to cycle to the next buffer.  That's what the following code offers.
;;
;;
;; Independent cycling over buffers of the same major mode
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;
;; To quickly cycle through buffers of the same major mode, use the command
;; functions `pel-smb-next' and `pel-smb-previous'.  The "smb" acronym here
;; stand for "Same Mode Buffer".  When invoked, these functions create a list
;; of buffers in the same mode using the internal function `pel--smb-capture'
;; which updates the values of internal variables `pel--smb-list-mode',
;; `pel--smb-list',  `pel--smb-list-size' and `pel--smb-list-idx'.  Successive
;; calls use the index `pel--smb-list-idx' to identify the next or previous
;; buffer.  The function also call `pel--refresh-when-needed' to refresh these
;; variables when needed, when the index is back to 0 or when the major mode
;; of the current buffer is not the same as what was remembered.  Refreshing
;; the list at index 0 handle the cases when new buffers of the same mode were
;; added or removed.  The functions also detect killed buffers and skip them.
;; The list refreshing removes the killed buffers.

;; ---------------------------------------------------------------------------
;; Buffer Selection (bs) Extension
;; -------------------------------

;; - Commands that change buffer to next/previous in Buffer Selection list.

;;-pel-autoload
(defun pel-bs-next ()
  "Show next buffer in current window."
  (interactive)
  (bs-show nil)
  (bs-down 1)
  (bs-select))

;;-pel-autoload
(defun pel-bs-previous ()
  "Show previous buffer in current window."
  (interactive)
  (bs-show nil)
  (bs-up 1)
  (bs-select))

;; - Add Buffer Selection Configuration: list all buffer of same major mode

(defvar pel--registered-bs-mode-configurations nil
  "List of bs configuration names (strings) already created.
Used by function `pel-bs-set-for-this-mode-only' to prevent
multiple creation of the same closures used to filter buffers on
the value of their major mode.")

(defun pel-bs-set-for-this-mode-only (wanted-major-mode)
  "Create a new bs configuration: show only buffers in WANTED-MAJOR-MODE.

The creation of the configuration for a specific WANTED-MAJOR-MODE is done
once per Emacs editing session; they are remembered in the variable
`pel--registered-bs-mode-configurations'.

Return new configuration name."
  (let ((bs-config-name (format "only-%s" wanted-major-mode)))
    (unless (member bs-config-name pel--registered-bs-mode-configurations)
      ;; create new closures that filter buffers based on their major mode
      ;; for the specific wanted-major-mode
      (add-to-list 'bs-configurations
                   (list
                    bs-config-name
                    nil                  ; bs-must-show-regexp
                    (lambda (buffer-obj) ; bs-must-show-function
                      (with-current-buffer buffer-obj
                        (eq major-mode wanted-major-mode)))
                    nil                  ; bs-dont-show-regexp
                    (lambda (buffer-obj) ; bs-dont-show-function
                      (with-current-buffer buffer-obj
                        (not (eq major-mode wanted-major-mode))))
                    nil))               ; bs-buffer-sort-function
      (push bs-config-name pel--registered-bs-mode-configurations))
    bs-config-name))

(defun pel-bs-this-mode-only ()
  "Add a Buffer Selection configuration for buffer of this mode only.
Add a Buffer Selection that will be named \"only-X\" where X is the major
mode of the current line buffer.
This configuration will only show buffers that use the same major mode.

Refresh the  Buffer Selection buffer to show only buffer of selected mode,
which also includes the current buffer which may be of another mode.
Also set the default configuration to this new selected mode."
  (interactive)
  (let ((config-name (pel-bs-set-for-this-mode-only
                      (pel-major-mode-of (bs--current-buffer)))))
    (bs-set-configuration config-name)
    (setq bs-default-configuration config-name)
    (bs--redisplay t)
    (message "New configuration is: %s" config-name)))

;;-pel-autoload
(defun pel-bs-init ()
  "Initialize PEL addition commands to the Buffer Selection mode.
Add the dot command that adds a configuration for buffers in the same
major mode."
  (if (boundp 'bs-mode-map)
      (define-key bs-mode-map "." 'pel-bs-this-mode-only)
    (display-warning 'pel-bs-this-mode-only
                     "Failed installing PEL command in (unbound) bs-mode-map!"
                     :error)))

;; ---------------------------------------------------------------------------
;; Same Mode Buffer (smb) iteration
;; --------------------------------
;;
;; Iterate over buffers of same major modes.

(defvar pel--smb-list-mode nil
  "Currently active major mode.")

(defvar pel--smb-list nil
  "List of buffers of the same major modes.")

(defvar pel--smb-list-size nil
  "Size of the list `pel--smb-list'.")

(defvar pel--smb-list-idx nil
  "0-based index of the current buffer shown in the list, nil if none shown.")

(defun pel--smb-capture ()
  "Build a list of the buffers using the same major mode as the current one.
The other 2 commands will use that list."
  (setq pel--smb-list (pel-buffers-in-mode major-mode))
  (setq pel--smb-list-idx (pel-list-index (current-buffer) pel--smb-list))
  (setq pel--smb-list-size (length pel--smb-list))
  (setq pel--smb-list-mode major-mode))

(defun pel--show-buffer (idx)
  "Show buffer identified by index IDX in current window.
Return the displayed buffer, nil if the buffer no longer exists."
  (let ((buf-obj (nth idx pel--smb-list)))
    (when (buffer-live-p buf-obj)
      (switch-to-buffer buf-obj))))

(defun pel--refresh-when-needed (refresh)
  "Refresh buffer list when needed.
The REFRESH argument, when non-nil, forces it."
  (when (or refresh
            (not pel--smb-list)
            (not pel--smb-list-idx)
            (not (eq major-mode pel--smb-list-mode)))
    (pel--smb-capture)))

(defun pel--to-next-idx ()
  "Increment list index and wrap back to 0 at the end.
On index 0 refresh the list to handle buffer deletions and additions.
Return new idx value."
  (when (= pel--smb-list-idx 0)
    (pel--smb-capture))
  (setq pel--smb-list-idx (1+ pel--smb-list-idx))
  (when (>= pel--smb-list-idx pel--smb-list-size)
    (setq pel--smb-list-idx 0))
  pel--smb-list-idx)

;;-pel-autoload
(defun pel-smb-next (&optional refresh)
  "Open next buffer of same major mode from the registered list.
If the optional prefix argument is passed, REFRESH the list of buffers."
  (interactive "P")
  (pel--refresh-when-needed refresh)
  ;; Try to display next buffer.
  ;; It's possible buffers have been deleted, so try as many times
  ;; as the length of the list, no more.
  (let ((attempt-down-count pel--smb-list-size))
    (while
        (progn
          (setq attempt-down-count (1- attempt-down-count))
          (and (null (pel--show-buffer (pel--to-next-idx)))
               (> attempt-down-count 0))))))

(defun pel--to-previous-idx ()
  "Decrement list index and wrap at end.
On index 0 refresh the list to handle buffer deletions and additions.
Return new idx value."
  (when (= pel--smb-list-idx 0)
    (pel--smb-capture))
  (setq pel--smb-list-idx (1- pel--smb-list-idx))
  (when (< pel--smb-list-idx 0)
    (setq pel--smb-list-idx (- pel--smb-list-size 1)))
  pel--smb-list-idx)

;;-pel-autoload
(defun pel-smb-previous (&optional refresh)
  "Open previous buffer of same major mode from the registered list.
If the optional prefix argument is passed, REFRESH the list of buffers."
  (interactive "P")
  (pel--refresh-when-needed refresh)
  ;; Try to display next buffer.
  ;; It's possible buffers have been deleted, so try as many times
  ;; as the length of the list, no more.
  (let ((attempt-down-count pel--smb-list-size))
    (while
        (progn
          (setq attempt-down-count (1- attempt-down-count))
          (and (null (pel--show-buffer (pel--to-previous-idx)))
               (> attempt-down-count 0))))))

;;; --------------------------------------------------------------------------
(provide 'pel-buffer)

;;; pel-buffer.el ends here
