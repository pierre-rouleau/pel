;;; pel-highlight.el --- PEL highlight support. -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2022, 2024, 2025  Pierre Rouleau

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

;; -----------------------------------------------------------------------------
;;; Commentary:
;;

;;; Code:
(require 'hl-line)
(require 'pel--base)
(require 'pel-prompt)                   ; uses: `pel-prompt-with-completion'
;; -----------------------------------------------------------------------------
;; highlight control
;; -----------------

;;-pel-autoload
(defun pel-show-paren-info (&optional append)
  "Show `paren-mode' control user-options in *pel-highlight-info* buffer.

With non-nil optional APPEND argument, append the information to
the buffer."
  (interactive "P")
  (pel-print-in-buffer
   "*pel-highlight-info*"
   "paren highlighting"
   (lambda ()
     "Print values."
     (insert "show-paren mode:")
     (pel-insert-symbol-content 'show-paren-highlight-openparen     nil :on-same-line)
     (pel-insert-symbol-content 'show-paren-style                   nil :on-same-line)
     (pel-insert-symbol-content 'show-paren-when-point-inside-paren nil :on-same-line)
     (pel-insert-symbol-content 'show-paren-when-point-in-periphery nil :on-same-line)
     (pel-insert-symbol-content 'show-paren-context-when-offscreen  nil :on-same-line)
     (insert "\n"))
   (unless append :clear-buffer)
   :use-help-mode))

;;-pel-autoload
(defun pel-set-highlight-color (colorname)
  "Select (or prompt) for COLOR and use it as new line highlight.
With <tab> provides completion of the colors and their names."
  (interactive (list (read-color (format "Color name [%s]: "
                                         (face-attribute 'highlight :background)))))
  (progn
    (set-face-background 'highlight colorname)
    (set-face-foreground 'highlight nil)
    (set-face-underline 'highlight nil)))

;;-pel-autoload
(defun pel-customize-highlight ()
  "Open the customize buffer to change the `highlight' color and attributes."
  (interactive)
  (customize-face 'highlight))

;;-pel-autoload
(defun pel-toggle-hl-line-sticky ()
  "Toggle current line highlight to all windows or just the current one.
Toggles the value of `hl-line-sticky-flag' between t and nil."
  (interactive)
  (progn
    (eval-when-compile
      (defvar hl-line-mode)) ; prevent warning about ref to free variable.
    (setq hl-line-sticky-flag (not hl-line-sticky-flag))
    (message "Current line highlighting %s done in %s."
             (if hl-line-mode
                 "is now"
               "(currently disabled) will be")
             (if hl-line-sticky-flag
                 "all windows showing current buffer"
               "current buffer only"))))

;; -----------------------------------------------------------------------------
;; Whitespace and empty lines
;; --------------------------

;;-pel-autoload
(defun pel-toggle-show-trailing-whitespace ()
  "Toggle highlight of the trailing whitespaces in current buffer."
  (interactive)
  (pel-toggle-and-show 'show-trailing-whitespace))

;;-pel-autoload
(defun pel-toggle-indicate-empty-lines ()
  "Toggle highlight of empty lines."
  (interactive)
  (pel-toggle-and-show 'indicate-empty-lines))

;; -----------------------------------------------------------------------------
;; Hard tabs
;; ---------

;;-pel-autoload
(defun pel-toggle-indent-tabs-mode (&optional arg)
  "Toggle use of hard tabs or spaces for indentation in current buffer.
If ARG is positive set to use hard tabs, otherwise force use of spaces only.
Beep on each change to warn user of the change and display new value."
  (interactive "P")
  (beep :dont-break-macro)
  (let ((pel-insert-symbol-content-context-buffer (current-buffer))
        (on-string  "t: using hard TABs for indentation in current buffer")
        (off-string "nil: using SPACEs only for indentation in current buffer"))
    (if (null arg)
        ;; toggle
        (pel-toggle-and-show 'indent-tabs-mode on-string off-string)
      ;; set value according to arg
      (if (< (prefix-numeric-value arg) 0)
          ;; negative; use space -> set to nil
          (setq indent-tabs-mode nil)
        (setq indent-tabs-mode t))
      (message "%s" (pel-symbol-text indent-tabs-mode on-string off-string)))))

;; ---------------------------------------------------------------------------
;; Highlight Lines
;; ---------------
;;
;; Credit: PascalIVKooten wrote the original version of the code

(defvar pel--highlight-color pel-highlight-color-default)

(defun pel--find-overlays-specifying (prop pos)
  (let ((overlays (overlays-at pos))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))

(defun pel-highlight-line (&optional change-color)
  "Toggle current line highlight.  With CHANGE-COLOR prompt for the new color.

The prompt has a buffer-specific history and supports tab completion."
  (interactive "P")
  (when change-color
    (let ((color-requested (pel-prompt-with-completion "Color: " (defined-colors))))
      (if (color-supported-p color-requested)
          (setq pel--highlight-color color-requested)
        (user-error "%s is not a supported color" color-requested))))
  (if (pel--find-overlays-specifying
       'line-highlight-overlay-marker
       (line-beginning-position))
      (remove-overlays (line-beginning-position) (+ 1 (line-end-position)))
    (let ((overlay-highlight (make-overlay
                              (line-beginning-position)
                              (+ 1 (line-end-position)))))
      (overlay-put overlay-highlight 'face (list :background pel--highlight-color))
      (overlay-put overlay-highlight 'line-highlight-overlay-marker t))))

(defun pel-remove-line-highlight ()
  "Prompt.  If proceeds, remove all active line highlighting.

Caution: also removes bm bookmarks!"
  (interactive)
  (when (y-or-n-p "Will also remove all bm bookmarks.  Proceed? ")
    (remove-overlays (point-min) (point-max))))

;; -----------------------------------------------------------------------------
(provide 'pel-highlight)

;;; pel-highlight.el ends here
