;;; pel-window.el --- PEL Window Management Utilities

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau.swd@gmail.com>

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
;; This file contains a set of commands and functions used to create, delete,
;; query and overall manage Emacs windows in a way that uses the graphical
;; Cartesian position of the window.  For some of the commands the file uses the
;; windmove package.

;;; Code:

(require 'pel--base)      ; use: pel-current-buffer-filename

;; The windmove package is required for the following functions.
;; Instead of requiring windmove, and force its loading when pel-window is
;; loaded, just autoload its used functions.
(autoload 'windmove-up                 "windmove")
(autoload 'windmove-right              "windmove")
(autoload 'windmove-left               "windmove")
(autoload 'windmove-down               "windmove")
(autoload 'windmove-find-other-window  "windmove")

;; -----------------------------------------------------------------------------
;; Window buffer
;; -------------

;;-pel-autoload
(defun pel-show-window-previous-buffer ()
  "Show the name of previous buffer used in the current window."
  (interactive)
  (message "Previous buffer was: %S" (other-buffer (current-buffer))))

;;-pel-autoload
(defun pel-switch-to-last-used-buffer ()
  "Switch buffer in current window to the one previously seen in the window.
Used twice returns to the same buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

;; -----------------------------------------------------------------------------
;; Dedicated Windows
;; -----------------

;;-pel-autoload
(defun pel-show-window-dedicated-status ()
  "Display the dedicated status of the current window in the echo area."
  (interactive)
  (message "Window %S %s dedicated"
           (selected-window)
           (if (window-dedicated-p) "is" "is NOT")))

;;-pel-autoload
(defun pel-toggle-window-dedicated ()
  "Toggle the dedicated status of the current window.  Use with care."
  (interactive)
  (set-window-dedicated-p (selected-window) (not (window-dedicated-p)))
  (pel-show-window-dedicated-status))

;; -----------------------------------------------------------------------------
;; Create new Window
;; -----------------

;;-pel-autoload
(defun pel-create-window-down ()
  "Create a window below the current one and move point to it."
  (interactive)
  (select-window
   (split-window-below)))

;;-pel-autoload
(defun pel-create-window-right ()
  "Create a window right of the current one and move point to it."
  (interactive)
  (select-window
   (split-window-right)))

;;-pel-autoload
(defun pel-create-window-up ()
  "Create a window above the current one and move point to it."
  (interactive)
  (split-window-below))

;;-pel-autoload
(defun pel-create-window-left ()
  "Create a window left of the current one and move point to it."
  (interactive)
  (split-window-right))

;; -----------------------------------------------------------------------------
;; Window close by direction
;; -------------------------

(defun pel-move-to-window (direction)
  "Move point to the window identified by DIRECTION.
DIRECTION may be: 'up, 'down, 'left or 'right.
Raise an error if the DIRECTION is invalid."
  (cond ((eq direction 'down)  (windmove-down))
        ((eq direction 'up)    (windmove-up))
        ((eq direction 'left)  (windmove-left))
        ((eq direction 'right) (windmove-right))
        (t (error "Invalid direction %S" direction))))

(defun pel--close-window (direction)
  "Close the window identified by DIRECTION ('up, 'down, 'right or 'left).
Ensure that point remains in the window from where the function was called."
  (let ((original_window (selected-window)))
    (when (pel-move-to-window direction)
      (delete-window)
      (select-window original_window))))

;;-pel-autoload
(defun pel-close-window-down ()
  "Close the window below the current one if there is one."
  (interactive)
  (pel--close-window 'down))

;;-pel-autoload
(defun pel-close-window-up ()
  "Close the window above the current one if there is one."
  (interactive)
  (pel--close-window 'up))

;;-pel-autoload
(defun pel-close-window-left ()
  "Close the window left of the current one if there is one."
  (interactive)
  (pel--close-window 'left))

;;-pel-autoload
(defun pel-close-window-right ()
  "Close the window right of the current one if there is one."
  (interactive)
  (pel--close-window 'right))

;; -----------------------------------------------------------------------------
;; Split Current Window More Sensibly
;; ----------------------------------

;;-pel-autoload
(defun pel-split-window-sensibly ()
  "Split current window sensibly.  Keep same point in both.
Spit window more sensibly than `split-window-sensibly' by looking at the overall
sizes of the current window and preferring horizontal split when the width is
larger than height and when the width is 160 columns or more.
Do not select the new window.
Returns the new window."
  (let ( (w_height (window-size))
         (w_width  (window-size nil t)))
    (if (and (> w_width w_height)
             (or (> w_width 160) (> w_width (* 2 w_height)))
             (> w_width 30))
        (split-window-right)
      ;; ensure point is not moved in the split windows,
      ;; so we don't loose context.  Don't impact global
      ;; behaviour by binding variable split-window-keep-point
      ;; locally.
      (if (> w_height 5)
          (let ((split-window-keep-point t))
            (split-window-below))
        (user-error "%S too small for splitting" (selected-window))))))

;; -----------------------------------------------------------------------------
;; Re-orient 2 windows
;; -------------------
;;
;; The commands `pel-2-vertical-windows' and `pel-2-horizontal-windows' flip the
;; orientation of the current and next window from horizontal to vertical and
;; vice-versa.
;;
;;
;; Functions:
;; - pel-2-vertical-windows
;; - pel-2-horizontal-windows
;;   - pel-flip-2-windows

(defun pel-flip-2-windows-to (orientation)
  "Flip the ORIENTATION of 2 windows: current and next window.
ORIENTATION must be one of: `horizontal or `vertical."
  (let ((original-window (selected-window)))
    (other-window 1)
    (let ((otherwin-buf (buffer-name)))
      (delete-window)
      (cond ((eq orientation 'horizontal) (split-window-below))
            ((eq orientation 'vertical)   (split-window-right))
            (t (error "Invalid orientation: %S" orientation)))
      (other-window 1)
      (switch-to-buffer otherwin-buf))
  (select-window original-window)))

;;-pel-autoload
(defun pel-2-vertical-windows ()
  "Convert 2 horizontal windows into 2 vertical windows.
Flip the orientation of the current window and its next one."
  (interactive)
  (pel-flip-2-windows-to 'vertical))

;;-pel-autoload
(defun pel-2-horizontal-windows ()
  "Convert 2 vertical windows into 2 horizontal windows.
Flip the orientation of the current window and its next one."
  (interactive)
  (pel-flip-2-windows-to 'horizontal))

;; -----------------------------------------------------------------------------
;; Select window by direction and context
;; --------------------------------------
;;
;; Functions:
;; - pel-window-valid-for-editing-p
;; - pel-window-select
;;   - pel-find-window

(defun pel-find-window (direction)
  "Return the window for the specified DIRECTION.
The DIRECTION can be 'up, 'down, 'right, 'left, 'current, or 'other.
The value 'new is not accepted and raises an error.
See `pel-find-file-at-point' for a description of these values."
  (cond ((eq direction 'current) (selected-window))
        ((eq direction 'other)   (next-window nil 'no-minibuf))
        ((eq direction 'new)     (error "Invalid code: 'new not accepted by `pel-find-window'"))
        (t (windmove-find-other-window direction))))

;;-pel-autoload
(defun pel-window-valid-for-editing-p (direction)
  "Return t if window in DIRECTION is valid for editing.
.
DIRECTION identifies the direction from the current window.
It must be one of:  'up, 'down, 'right, 'left, 'current, 'other or 'new.
See `pel-find-file-at-point' for a description of these values.
When 'current is used, point remains in the currently edited window.
Return t if the identified window is valid for editing a file or buffer.
Return nil otherwise:
- if there is no window there, for example: 'up of the window positioned
  at the top of the current frame,
- if the window is improper for editing, such as:
  - it's the minibuffer window,
  - that window is dedicated to a specific buffer."
  (if (eq direction 'new)
      t                                 ; a new window will be created: so it's OK.
    (let ((a_window (pel-find-window direction)))
      (and a_window
           (not (window-minibuffer-p a_window))
           (not (window-dedicated-p a_window))))))

;;-pel-autoload
(defun pel-window-select (direction)
  "Move point to window identified by DIRECTION.
.
DIRECTION identifies the direction from the current window.
It must be one of:  'up, 'down, 'right, 'left, 'current, 'other or 'new.
When 'new is specified, the current window is split and point is moved
to that new window.
When 'current is used, point remains in the currently edited window.
If the point is moved to the window specified by direction, the point
can optionally be moved to a specified position identified by LINE and COLUMN.
Any or none of LINE and COLUMN may be set to integers or to nil.
If there is no window in the specified DIRECTION or if that window is
the minibuffer, the point is not moved and nil is returned.
Return the selected window, or nil if nothing is valid."
  (let ((a_window
         (if (eq direction 'new)
             (pel-split-window-sensibly)
           (pel-find-window direction))))
    (if (and a_window
             (not (window-minibuffer-p a_window)))
        (select-window a_window))))

;; -----------------------------------------------------------------------------
;; Move to previous window
;; -----------------------

;;-pel-autoload
(defun pel-other-window-backward (&optional n)
  "Select Nth previous window.

  - n defaults to 1 : meaning direct previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

;; -----------------------------------------------------------------------------
;; Show information about window
;; -----------------------------

;;-pel-autoload
(defun pel-show-window-filename-or-buffer-name ()
  "Show the name of the file or buffer of current window.
For file; display full file path.
Display in minibuffer."
  (interactive)
  (let ((fn (pel-current-buffer-filename)))
    (if fn
        (message "File:= %s --> %s" (current-buffer) fn )
      (message "Buffer:= %s" (buffer-name)))))

;;-pel-autoload
(defun pel-show-window-sizes ()
  "Show the height & width of the current window."
  (interactive)
  (message "Window %s: Height=%d, Width=%d"
           (selected-window)
           (window-size)
           (window-size nil t)))

;; -----------------------------------------------------------------------------
(provide 'pel-window)

;;; pel-window.el ends here
