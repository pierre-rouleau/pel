;;; pel-cc-linux-kernel.el --- Linux Kernel Coding Style Support.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, August 13 2024.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2024-08-13 21:54:59 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2024  Pierre Rouleau
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
;; This implements a C style that aims to conform to the Linux Kernel Coding
;; Style described in:
;; https://github.com/torvalds/linux/blob/master/Documentation/process/coding-style.rst
;;
;; The code was heavily influenced by the linux-kernel-coding-style.el
;; at http://github.com/coldnew/linux-kernel-coding-style.el.  I added PEL
;; specific logic to it and fixed the CC style creation logic.
;;
;; The hooks are installed inside pel_keys.el which is invoked by pel-init()
;; when the `pel-use-linux-kernel-code-style-support' user option is active
;; (the default) and `pel-use-c' is also active.
;;


;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--options)
(require 'cc-mode)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel--lkcs-lineup-arglist-by-tabs (langelem)
  "Line up argument lists by tabs, the `c-basic-offset', not spaces."
  (let* ((anchor-pos (c-langelem-pos langelem))
	     (column (c-langelem-2nd-pos langelem))
	     (offset (- (1+ column) anchor-pos))
	     (steps (floor offset c-basic-offset)))
    (* (max steps 1) c-basic-offset)))

(defun pel--lks-add-c-style ()
  "Add the Linux Kernel C style."

  (message "Adding the linux-kernel style...")
  (c-add-style "linux-kernel"
			   '("linux"
                 (c-offsets-alist
                  (arglist-cont-nonempty
                   . (first c-lineup-gcc-asm-reg
                            pel--lkcs-lineup-arglist-by-tabs)))))  )

;; Add Linux kernel style



(defun pel--lkcs ()
  "Activate the Linux Kernel Code Style in buffer."

  ;; Give the style a name and inform user.
  (c-set-style "linux-kernel")

  ;; impose that hard tabs are used for indentation (as opposed to spaces)
  (setq indent-tabs-mode t)
  (setq pel-c-use-tabs t)

  ;; Set the visual width of hard tabs to render as specified by the
  ;; pel-c-tab-width; the value you have configured and probably most
  ;; comfortable with.
  ;; Note: you can change the rendering at any time by executing the
  ;;       command `pel-set-tab-width'
  ;;       That does not modify the content of the file, it only modifies
  ;;       the way the buffer renders the hard tabs.  This way you can
  ;;       make the code look the way you want while still complying with
  ;;       the Linux Kernel Code Style guideline.
  ;;        Use one tab-width of indentation
  (setq tab-width pel-c-tab-width)
  (setq c-basic-offset pel-c-tab-width)

  ;; Impose maximum line width
  (setq fill-column 80)

  (message "Using Linux kernel style.\
 Use `pel-set-tab-width' to alter tab rendering."))

(defun pel--lkcs-maybe-activate ()
  "Activate the Linux Kernel Code Style in buffer if needed."
  (let ((filename (buffer-file-name)))
    ;; Enable kernel mode for the appropriate files
    (when
        (and filename
             (or
              (locate-dominating-file filename "Kbuild")
              (locate-dominating-file filename "Kconfig")
              (save-excursion
                (goto-char 0)
                (re-search-forward
                 "^ *# *include +<linux/\\(module\\|kernel\\)\\.h>" nil t))))
      (pel--lkcs))))


;;-pel-autoload
(defun pel-linux-kernel-code-style ()
  "Manually Activate Linux Kernel Coding Style regardless of file content."
  (interactive)
  (pel--lkcs))

;;-pel-autoload
(defun pel-linux-kernel-code-style-setup ()
  "Activate the Linux Kernel Code Style for C."
  (add-hook 'c-mode-common-hook (function pel--lks-add-c-style))
  (add-hook 'c-mode-hook 'pel--lkcs-maybe-activate))

;;; --------------------------------------------------------------------------
(provide 'pel-cc-linux-kernel)

;;; pel-cc-linux-kernel.el ends here
