;;; pel-highlight.el --- PEL highlight support -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2022, 2024, 2025, 2026  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

;; This file is part of the PEL package.
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

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; User-controlled highlighting commands.
;;
;; Highlight Control
;;  * `pel-show-paren-info'
;;  * `pel-customize-highlight'
;;  * `pel-toggle-hl-line-sticky'
;;
;; Whitespace and empty lines
;;  * `pel-toggle-show-trailing-whitespace'
;;  * `pel-toggle-indicate-empty-lines'
;;
;; Hard tabs
;;  * `pel-toggle-indent-tabs-mode'
;;
;; Highlight Lines
;;  * `pel-set-highlight-color'
;;    - `pel--prompt-for-color'
;;      - `pel--color-completion-collection'
;;  * `pel-highlight-line'
;;    . `pel--prompt-for-color'
;;    - `pel--find-overlays-specifying'
;;  * `pel-remove-line-highlight'

;;; --------------------------------------------------------------------------
;;; Dependencies

(require 'pel--base)
(require 'pel--options)                 ; uses 'pel-highlight-color-default'
(require 'pel-prompt)                   ; uses: `pel-prompt-with-completion'
(require 'hl-line)

;;; --------------------------------------------------------------------------
;;; Code:


;;* Highlight Control
;;  =================

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

;; ---------------------------------------------------------------------------

;;-pel-autoload
(defun pel-customize-highlight ()
  "Open the customize buffer to change the `highlight' color and attributes."
  (interactive)
  (customize-face 'highlight))

;;-pel-autoload
(defun pel-toggle-hl-line-sticky ()
  "Toggle whether current-line highlighting is sticky across all windows.
It changes the value of `hl-line-sticky-flag'."
  (interactive)
  (setq hl-line-sticky-flag (not hl-line-sticky-flag))
  (message "Current line highlighting %s done in %s."
           (if hl-line-mode
               "is now"
             "(currently disabled) will be")
           (if hl-line-sticky-flag
               "all windows showing current buffer"
             "current buffer only")))

;; ---------------------------------------------------------------------------
;;* Whitespace and empty lines
;;  ==========================

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

;; ---------------------------------------------------------------------------
;;* Hard tabs
;;  =========

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
;;* Highlight Lines
;;  ===============
;;

(defvar pel--highlight-color pel-highlight-color-default
  "Currently used color for line/marked area highlighting.")

(defvar pel-set-highlight-color--history nil
  "Minibuffer history for `pel-set-highlight-color' and `pel-highlight-line'.")

(defun pel--color-completion-collection ()
  "Return a completion collection for color names with visual swatches.

On Emacs 27+, returns a metadata-bearing completion table whose
`:affixation-function' prefixes each candidate with a small color swatch.
On older Emacs, returns a plain list with an `:annotation-function' that
appends a colored swatch suffix — both avoid the `color-dark-p' crash
present in `read-color' on Emacs 30.2."
  (let* ((colors (defined-colors))
         (swatch-string (lambda (c)
                          ;; 5 spaces painted with the candidate color.
                          ;; Guard against nil: color-name-to-rgb returns nil
                          ;; for invalid or unavailable color names.
                          (if (color-name-to-rgb c)
                              (propertize "     "
                                          'font-lock-face
                                          (list :background c :foreground c))
                            "     "))))
    (if (>= emacs-major-version 27)
        ;; Emacs 27+: affixation-function gives us a colored PREFIX.
        ;; Each entry is (candidate prefix suffix).
        (let ((affix (lambda (cands)
                       (mapcar (lambda (c)
                                 (list c (funcall swatch-string c) " "))
                               cands))))
          (lambda (str pred action)
            (if (eq action 'metadata)
                `(metadata (affixation-function . ,affix))
              (complete-with-action action colors str pred))))
      ;; Emacs < 27: annotation-function appends a colored swatch SUFFIX.
      (let ((annotate (lambda (c) (funcall swatch-string c))))
        (lambda (str pred action)
          (if (eq action 'metadata)
              `(metadata (annotation-function . ,annotate))
            (complete-with-action action colors str pred)))))))

(defun pel--prompt-for-color ()
  "Prompt for a color name string.
Prompt supports completion which shows the color of each color name.
The prompt has its history maintained in `pel-set-highlight-color--history'
and is used by `pel-set-highlight-color' and `pel-highlight-line'."
  (let ((completion-ignore-case t))
    (completing-read
     (format "Highlight color [%s]: " pel--highlight-color)
     (pel--color-completion-collection)
     nil nil nil
     'pel-set-highlight-color--history
     pel--highlight-color)))

;;-pel-autoload
(defun pel-set-highlight-color (color-name)
  "Prompt for COLOR-NAME and set it as the new highlight color.

Each completion candidate is shown with a small color swatch so the color
is visible during selection.  Works correctly on Emacs 30+ (avoids the
`color-dark-p' / `min' crash present in `read-color' on that version).

Supports tab-completion with colors shown for each color name.
Prompt history is available.

Store new value in `pel--highlight-color'."
  (interactive
   (list (pel--prompt-for-color)))
  (setq pel--highlight-color color-name)
  (set-face-background 'highlight color-name)
  (set-face-foreground 'highlight nil)
  (set-face-underline  'highlight nil))

(defun pel--find-overlays-specifying (prop pos)
  "Return the list of overlays at position POS that have property PROP.
Return nil if there are none."
  (let ((overlays (overlays-at pos))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))

(defun pel-highlight-line (&optional change-color)
  "Toggle highlighting of current line or marked area.

If a marked area or a line is partly highlighted, the function toggles
the highlighting of the complete marked area or line, if something was
highlighted it becomes non-highlighted and if something was highlighted
it stops being highlighted.

With CHANGE-COLOR prompt for the new color and changes the highlighting
color to the selected one.  Previously highlighted text retains its
previously used highlighting color.

The color prompt supports tab-completion with colors shown and a selection
history."
  (interactive "P")
  (when change-color
    (let ((color-requested (pel--prompt-for-color)))
      (if (color-supported-p color-requested)
          (setq pel--highlight-color color-requested)
        (user-error "%s is not a supported color" color-requested))))
  (let* ((use-region (use-region-p))
         (zone-start (if use-region
                         (region-beginning)
                       (line-beginning-position)))
         (zone-end (if use-region
                       (region-end)
                     (1+ (line-end-position)))))
    (if (pel--find-overlays-specifying
         'line-highlight-overlay-marker zone-start)
        (remove-overlays zone-start zone-end
                         'line-highlight-overlay-marker t)
      (let ((overlay-highlight (make-overlay zone-start zone-end)))
        (overlay-put overlay-highlight
                     'face (list :background pel--highlight-color))
        (overlay-put overlay-highlight
                     'line-highlight-overlay-marker t)))))

(defun pel-remove-line-highlight ()
  "Remove all active line/marked-area highlighting."
  (interactive)
  (remove-overlays (point-min) (point-max) 'line-highlight-overlay-marker t))

;;; --------------------------------------------------------------------------
(provide 'pel-highlight)

;;; pel-highlight.el ends here
