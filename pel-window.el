;;; pel-window.el --- PEL Window Management Utilities -*-lexical-binding: t; -*-

;; Copyright (C) 2020, 2021, 2022  Pierre Rouleau

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

;;; Commentary:
;;
;; The file `pel-window.el' provides a set of window management utilities.
;; Some of these utility commands use or extend the features provided by the
;; `windmove' library, a library packaged with standard GNU Emacs.
;;
;; The file provides the following features:
;;
;; - Buffer management utilities:
;;
;;   - `pel-show-window-previous-buffer' shows the name of the buffer that was
;;      previously used in the current window.
;;   - `pel-switch-to-last-used-buffer' switch the buffer in current window to
;;      the buffer that was previously used.
;;
;;  - Dedicated window management utilities:
;;
;;    - `pel-show-window-dedicated-status' displays the dedicated status of
;;      the current window: ie. whether the current window is dedicated or
;;      not.
;;    - `pel-toggle-window-dedicated' toggles the dedicated status of the
;;      current window.  Use it to dedicate the current window or turn
;;      dedication off.
;;    - `pel-count-non-dedicated-windows' returns the number of non-dedicated
;;      windows.
;;
;;  - Creating new windows:
;;
;;    The following 4 commands allow creating cursor bindings to create
;;    windows pointed by a cardinal direction:
;;
;;    - `pel-create-window-down'
;;    - `pel-create-window-left'
;;    - `pel-create-window-right'
;;    - `pel-create-window-up'
;;
;;  - Move point to window specified by direction
;;
;;    - `pel-move-to-window' move point to a window up, down, left or right of
;;      the current window.
;;
;;  - Closing windows:
;;
;;    The following 4 commands allow creating cursor bindings to close windows
;;    pointed by a cardinal direction:
;;
;;    - `pel-close-window-down'
;;    - `pel-close-window-left'
;;    - `pel-close-window-right'
;;    - `pel-close-window-up'
;;
;;  - Window splitting:
;;
;;    - The function `pel-split-window-sensibly' attempts to improve window
;;      splitting logic by selecting an orienation that takes the frame size
;;      into account with a different heuristic than what is normally used by
;;      Emacs.  The function is used by other PEL commands when windows are
;;      created.  The logic gives priority to splitting vertically if the
;;      available area is wide *enough*.
;;
;;  - Changing orientation of 2 windows:
;;
;;    The commands `pel-2-vertical-windows' and `pel-2-horizontal-windows'
;;    flip the orientation of the current and next window from horizontal to
;;    vertical and vice-versa.
;;
;; - Moving to windows by direction or context:
;;
;;   Two functions provide services to move point to other window by direction
;;   or to create a new one.  These functions are used by other PEL commands.
;;   The functions are:
;;
;;   - `pel-window-valid-for-editing-p' return t is the window pointed by
;;     direction is valid for editing, nil otherwise.
;;     This excludes the minibuffer or any dedicated window.
;;   - `pel-find-window' return the window identified by a specified direction.
;;   - `pel-window-select' move point to the window specified by a direction
;;      argument which may also identify the *other* window (the next one) or
;;      create a new window.
;;
;; - Process numeric arguemtn to identify a direction
;;
;;   - `pel-window-direction-for' converts a numeric value to a window
;;     direction or action symbol used in other PEL windows processing.
;;
;; - Moving to other (next) or previous window:
;;
;;   - The `pel-other-window' is just explicitly calling the Emacs
;;     `other-window' command that might be hidden by the use of `ace-window'.
;;   - The `pel-other-window-backward' moves to the previous window.
;;
;; - Showing information about current window:
;;
;;   - `pel-show-window-filename-or-buffer-name' displays the name of the
;;     file or buffer used in the current window.
;;   - `pel-show-window-sizes' displays the height and width of the current
;;     window.


;;; --------------------------------------------------------------------------
;;; Dependencies:

(require 'pel--base)      ; use: pel-current-buffer-filename

;; The windmove package is required for the following functions.
;; Instead of requiring windmove, and force its loading when pel-window is
;; loaded, just autoload its used functions.
(autoload 'windmove-up                 "windmove")
(autoload 'windmove-right              "windmove")
(autoload 'windmove-left               "windmove")
(autoload 'windmove-down               "windmove")
(autoload 'windmove-find-other-window  "windmove")

;;; --------------------------------------------------------------------------
;;; Code:

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

;; ---------------------------------------------------------------------------
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

;;-pel-autoload
(defun pel-count-non-dedicated-windows ()
  "Count and return the number of non-dedicated windows in the current frame.
Exclude the minibuffer."
  (length (delq t (mapcar #'window-dedicated-p
                          (window-list nil :exclude-minibuf)))))

;; ---------------------------------------------------------------------------
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

;; ---------------------------------------------------------------------------
;; Move to specified window
;; ------------------------

;;-pel-autoload
(defun pel-move-to-window (direction &optional direction-for-new)
  "Move point to the window identified by DIRECTION.
DIRECTION may be: 'other, 'up, 'down, 'left or 'right.
Raise an error if the DIRECTION is invalid.
If direction is 'new, create a new window in the DIRECTION-FOR_NEW,
or down by default.
See `pel-window-direction-for' to convert a prefix number to a direction."
  (cond ((eq direction 'down)  (windmove-down))
        ((eq direction 'up)    (windmove-up))
        ((eq direction 'left)  (windmove-left))
        ((eq direction 'right) (windmove-right))
        ((eq direction 'other) (other-window 1))
        ((eq direction 'new)
         (let ((direction-for-new (or direction-for-new 'down)))
           (cond ((eq direction-for-new 'down)  (pel-create-window-down))
                 ((eq direction-for-new 'up  )  (pel-create-window-up))
                 ((eq direction-for-new 'left)  (pel-create-window-left))
                 ((eq direction-for-new 'right) (pel-create-window-right)))))
        (t (error "Invalid direction %S" direction))))

;; ---------------------------------------------------------------------------
;; Window close by direction
;; -------------------------

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

;;-pel-autoload
(defun pel-close-other-window ()
  "Close the other window.
Hide its buffer, does not kill it."
  (interactive)
  (let ((original_window (selected-window)))
    (delete-window (other-window 1))
    (select-window original_window)))

;; ---------------------------------------------------------------------------
;; Split Current Window More Sensibly
;; ----------------------------------

;;-pel-autoload
(defun pel-split-window-sensibly ()
  "Split current window sensibly.  Keep same point in both.

Spit window more sensibly than `split-window-sensibly' by looking
at the overall sizes of the current window and preferring
horizontal split when the width is larger than height and when
the width is 160 columns or more.

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

;; ---------------------------------------------------------------------------
;; Re-orient 2 windows
;; -------------------
;;
;; The commands `pel-2-vertical-windows' and `pel-2-horizontal-windows' flip
;; the orientation of the current and next window from horizontal to vertical
;; and vice-versa.
;;
;;
;; Functions:
;; - `pel-2-vertical-windows'
;; - `pel-2-horizontal-windows'
;;   - `pel-flip-2-windows-to'
;;     - `pel-other-flippable-window'
;;       - `pel-window-unflippable'

(defun pel-window-unflippable ()
  "Return string describing why window is unflippable.
Return nil if it can be flipped."
  (cond ((window-dedicated-p)
         "dedicated")
        ((window-minibuffer-p)
         "minibuffer")
        (t nil)))

(defun pel-other-flippable-window ()
  "Select another flippable window in cyclic ordering of windows.
The new window must be in the same frame.
If the skipped to window is dedicated skip one more."
  (let  ((reason  (pel-window-unflippable)))
    (when reason
      (user-error "Cannot flip %s window!" reason)))
  (let ((original-window (selected-window)))
    (select-window (next-window nil :skip-minibuffer :current-frame-only))
    (when (pel-window-unflippable)
      (select-window (next-window nil :skip-minibuffer :current-frame-only))
      (if (pel-window-unflippable)
          (user-error "Cannot flip dedicated window!")
        (when (eq (selected-window) original-window)
          (user-error "Cannot flip window: need another one!"))))))


(defun pel-flip-2-windows-to (orientation)
  "Flip the ORIENTATION of 2 windows: current and next window.
ORIENTATION must be one of: `horizontal or `vertical."
  (unless (> (pel-count-non-dedicated-windows) 1)
    (user-error "Need 2 flippable windows in current frame!"))
  (let ((original-window (selected-window)))
    (pel-other-flippable-window)
    (let ((otherwin-buf (buffer-name)))
      (delete-window)
      (cond ((eq orientation 'horizontal) (split-window-below))
            ((eq orientation 'vertical)   (split-window-right))
            (t (error "Invalid orientation: %S" orientation)))
      (pel-other-flippable-window)
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

;; ---------------------------------------------------------------------------
;; Select window by direction and context
;; --------------------------------------
;;
;; Functions:
;; - pel-window-valid-for-editing-p
;; - pel-window-select
;;   - pel-find-window

;;-pel-autoload
(defun pel-find-window (direction)
  "Return the window for the specified DIRECTION.
The DIRECTION can be 'up, 'down, 'right, 'left, 'current, or 'other.
The value 'new is not accepted and raises an error.
See `pel-find-file-at-point' for a description of these values."
  (cond ((eq direction 'current) (selected-window))
        ((eq direction 'other)   (next-window nil 'no-minibuf))
        ((eq direction 'new)
         (error "The direction new is not accepted by `pel-find-window'"))
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
      ;; a new window will be created: so it's OK.
      t
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

;; ---------------------------------------------------------------------------
;; Window direction argument processing utility
;; --------------------------------------------

;;-pel-autoload
(defun pel-window-direction-for (n &optional prefer for-editing)
  "Return direction of the target window identified by N and PREFER.
.
If FOR-EDITING is non-nil, only return direction of editable window:
if N identifies a non-editable window, return what is identified
by PREFER (if this is also OK) otherwise return 'new.
.
Window selection:
- If N is negative:    : return 'new
- If N is 0:           : return 'other
- If N is nil or 1     : the selection depends on PREFER:
  - If PREFER is non-nil then it should be set to one of the possible valid
    returned values because it is returned unchanged.
  - If PREFER is nil, then select the window according to the number
    of non-dedicated and non-minibuffer windows in the current frame:
    - 1 window  in frame: return 'new
    - 2 windows in frame: return 'other
    - 3 or more windows : return 'current
- If N is 3, 7, 9 or larger, with PREFER not nil, return: PREFER value.
  If PREFER is nil return as described above.
- If N in remaining [2,8] range, return the direction corresponding to the
  cursor in a numeric keypad:
  -             8 := 'up
  - 4 := 'left  5 := 'current  6 := 'right
  -             2 := 'down"
  (let ((direction (cond
                    ((eq n 2) 'down)
                    ((eq n 8) 'up)
                    ((eq n 4) 'left)
                    ((eq n 6) 'right)
                    ((eq n 5) 'current)
                    ((eq n 0) 'other)
                    ((< n 0)  'new)
                    (t (or prefer
                           (let ((nwindows (pel-count-non-dedicated-windows)))
                             (cond ((eq nwindows 2) 'other)
                                   ((eq nwindows 1) 'new)
                                   (t               'current))))))))
    (if for-editing
        (if (pel-window-valid-for-editing-p direction)
            direction
          (if (and prefer (pel-window-valid-for-editing-p prefer))
              prefer
            'new))
      direction)))

;; ---------------------------------------------------------------------------
;; Original other-window
;; ---------------------
;; The function other-window is replaced by ace-window and the keys
;; are re-mapped. The following provide access to that function anyway.

;;-pel-autoload
(defun pel-other-window (&optional all-frames)
  "Execute (other-window 1).
Useful when `other-window' has been remapped to something like `ace-window'
and want to see where the next window is."
  (interactive "P")
  (other-window 1 (if all-frames t nil)))

;; ---------------------------------------------------------------------------
;; Move to previous window
;; -----------------------

;;-pel-autoload
(defun pel-other-window-backward (&optional n)
  "Select Nth previous window.

  - n defaults to 1 : meaning direct previous window.
  - if n is negative: move backwards but consider all frames."
  (interactive "P")
  (let* ((n          (prefix-numeric-value n))
         (count      (- (abs n)))
         (all-frames (< n 0)))
    (other-window count all-frames)))

;; ---------------------------------------------------------------------------
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

;;; --------------------------------------------------------------------------
(provide 'pel-window)

;;; pel-window.el ends here
