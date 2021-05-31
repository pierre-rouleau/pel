;;; pel-buffer.el --- PEL buffer management utilities.  -*- lexical-binding: t; -*-

;; Created   : Thursday, May 27 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-05-31 19:15:16, updated by Pierre Rouleau>

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

;; ---------------------------------------------------------------------------

(defvar pel--bs-buffer-mode nil
  "Major mode to search.")

(defun pel-include-this-buffer-p (buffer-spec)
  "Check if this buffer is the same mode as the wanted one."
  (let ((buffer-obj (if  (bufferp buffer-spec)
                       buffer-spec
                  (get-buffer buffer-spec))))
    (with-current-buffer buffer-obj
      (message "--> %S  -- %S" buffer-obj major-mode)
      (eq major-mode pel--bs-buffer-mode))))

(defun pel-exclude-this-buffer-p (buffer-spec)
  "Check if this buffer is the same mode as the wanted one."
  (not (pel-include-this-buffer-p buffer-spec)))

(defun pel-bs-set-for-this-mode-only (wanted-major-mode)
  "Activate new bs configuration: show only buffers in WANTED-MAJOR-MODE."
  (let ((bs-config-name (format "only-%s" wanted-major-mode)))
    (add-to-list 'bs-configurations
                 (list
                  bs-config-name
                  nil                                  ; bs-must-show-regexp
                  (function pel-include-this-buffer-p) ; bs-must-show-function
                  nil                                  ; bs-dont-show-regexp
                  (function pel-exclude-this-buffer-p) ; bs-dont-show-function
                  nil))))                              ; bs-buffer-sort-function

(defun pel-bs-this-mode-only ()
  "Add a Buffer Selection configuration for buffer of this mode only.
Add a Buffer Selection that will be named \"only-X\" where X is the major
mode of the current line buffer.
This configuration will only show buffers that use the same major mode."
  (interactive)
  (setq pel--bs-buffer-mode (pel-major-mode-of (bs--current-buffer)))
  (pel-bs-set-for-this-mode-only pel--bs-buffer-mode)
  (message "Configuration is: %s" pel--bs-buffer-mode))

;;-pel-autoload
(defun pel-bs-init ()
  "Initialize PEL addition commands to the bs-show mode.
Add the dot command that adds a configuration for buffers in the same
major mode."
  (if (boundp 'bs-mode-map)
      (define-key bs-mode-map "." 'pel-bs-this-mode-only)
    (user-error "Failed installing PEL command in bs-mode-map!")))


;; ---------------------------------------------------------------------------
;; Stand-alone iteration over buffers of same major modes
;; ------------------------------------------------------

(defvar pel--smb-list nil
  "List of buffers of the same major modes.")

(defvar pel--smb-list-mode nil
  "Currently active major mode.")

(defvar pel--smb-list-size nil
  "Size of pel--smb-list.")

(defvar pel--smb-list-idx nil
  "Zero-based index of the current buffer shown in the list.  Nil if none shown.")

;;-pel-autoload
(defun pel-smb-capture ()
  "Build a list of the buffers using the same major mode as the current one.
The other 2 commands will use that list."
  (setq pel--smb-list (pel-buffers-in-mode major-mode))
  (setq pel--smb-list-idx (pel-list-index (current-buffer) pel--smb-list))
  (setq pel--smb-list-size (length pel--smb-list))
  (setq pel--smb-list-mode major-mode))

(defun pel--show-buffer (idx)
  "Show buffer identified by index IDX in current window."
  (let ((buf-obj (nth idx pel--smb-list)))
    (switch-to-buffer buf-obj)))

(defun pel--refresh-when-needed (refresh)
  "Refresh buffer list when needed."
  (when (or refresh
            (not pel--smb-list)
            (not pel--smb-list-idx)
            (not (eq major-mode pel--smb-list-mode)))
    (pel-smb-capture)))

;;-pel-autoload
(defun pel-smb-next (&optional refresh)
  "Open next buffer of same major-mode from the registered list.
If the optional prefix argument is passed, REFRESH the list of buffers."
  (interactive "P")
  (pel--refresh-when-needed refresh)
  ;; increment index and wrap at end of list
  (setq pel--smb-list-idx (1+ pel--smb-list-idx))
  (when (>= pel--smb-list-idx pel--smb-list-size)
    (setq pel--smb-list-idx 0))
  (pel--show-buffer pel--smb-list-idx))

;;-pel-autoload
(defun pel-smb-previous (&optional refresh)
  "Open previous buffer of same major-mode from the registered list.
If the optional prefix argument is passed, REFRESH the list of buffers."
  (interactive "P")
  (pel--refresh-when-needed refresh)
  ;; increment index and wrap at end of list
  (setq pel--smb-list-idx (1- pel--smb-list-idx))
  (when (< pel--smb-list-idx 0)
    (setq pel--smb-list-idx (- pel--smb-list-size 1)))
  (pel--show-buffer pel--smb-list-idx))

;;; --------------------------------------------------------------------------
(provide 'pel-buffer)

;;; pel-buffer.el ends here
