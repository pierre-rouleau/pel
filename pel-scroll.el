;;; pel-scroll.el --- PEL Window Scrolling Utilities -*-lexical-binding: t; -*-

;; Copyright (C) 2020, 2021  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

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
;; The `pel-scroll' file provides a set of window scrolling facilities.
;;
;; The following 2 commands are used to scroll the current window, and
;; other windows that may be placed inside the PEL window scroll group:
;;
;; - `pel-scroll-up' which scrolls text up,
;; - `pel-scroll-down' which scrolls text down.
;;
;; The following commands scrool the 'other' window by one line (but do not
;; support the PEL window scroll group):
;;
;;  - `pel-scroll-up-other'
;;  - `pel-scroll-down-other'
;;
;; The file also provides the creation and management of a group of
;; windows into the *PEL window scroll sync* group, a list stored inside
;; the `pel-in-scroll-sync' variable identifying windows that will be
;; scrolled together.
;;
;; The following commands are used to activate and manage the
;; *PEL window scroll sync* group:
;;
;; - `pel-toggle-scroll-sync' toggles scroll lock on/off.  When turning it on
;;   it locks scrolling of the current and the next window.
;; - `pel-add-window-to-scroll-sync' adds the current window to the already
;;   existing group of scroll locked windows.  If there is none it locks
;;   scrolling of the current and the next window.
;; - `pel-remove-window-from-scroll-sync' removes the currenbt window from the
;;   group of scroll locked windows.  Removing the last one disables the
;;   window scroll sync.  If only one window is left in the group the command
;;   informs the user but allows it.  That way another window can be added to
;;   the group.
;;
;; The scrolling of multiple windows is currently only performed when the
;; following commands are used:
;;
;; -  `pel-scroll-up' which scrolls text up,
;; -  `pel-scroll-down' which scrolls text down,
;; -  `pel-home' and `pel-end', defined in `pel-navigation', which move
;;     point the the beginning or end of current field, line, window or buffer.
;;


;;; Code:

;; Uses scroll-other-window and scroll-other-window-down from window.el, which is
;; part of Emacs but taht file does not 'provide' itself, so the code does not
;; 'require' it.

(defvar pel-in-scroll-sync nil  "If non-nil, hold a list of windows to scroll.")

;;-pel-autoload
(defun pel-toggle-scroll-sync ()
  "Toggle window scroll sync mode.
Tie the current window and next window in the scroll."
  (interactive)
  (if (null pel-in-scroll-sync)
      (let ((window1 (selected-window))
            (window2 (progn
                       (other-window 1)
                       (selected-window))))
        (setq pel-in-scroll-sync (list window1 window2))
        (select-window window1)
        (message
         "Window scroll sync enabled between %s and %s"
         window1
         window2))
    (setq pel-in-scroll-sync nil)
    (message "Window scroll sync disabled.")))

;;-pel-autoload
(defun pel-add-window-to-scroll-sync ()
  "Add current window to window scroll sync.
If scroll sync is currently disabled, adds this window and the next."
  (interactive)
  (if (null pel-in-scroll-sync)
      (pel-toggle-scroll-sync)
    (let ((current-window (selected-window)))
      (if (member current-window pel-in-scroll-sync)
          (user-error
           "Window %s is already inside the scroll sync group"
           current-window)
        (setq pel-in-scroll-sync (cons current-window pel-in-scroll-sync))
        (message "Window scroll sync of %s" pel-in-scroll-sync)))))

;;-pel-autoload
(defun pel-remove-window-from-scroll-sync ()
  "Remove current window from window scroll sync."
  (interactive)
  (if (null pel-in-scroll-sync)
      (user-error "No scroll sync currently active")
    (let ((current-window (selected-window)))
      (if (member current-window pel-in-scroll-sync)
          (progn
            (setq pel-in-scroll-sync
                  (delete current-window pel-in-scroll-sync))
            (let ((group-size (length pel-in-scroll-sync)))
              (cond ((eq group-size 1)
                     (user-error
                      "Just 1 window left in scroll sync: %s"
                      pel-in-scroll-sync))
                    ((eq group-size 0)
                     (message "Window scroll sync disabled."))
                    (t (message
                        "Window removed from scroll sync list %s."
                        pel-in-scroll-sync)))))
        (user-error
         "Current window is not part of the window scroll sync group")))))

;; --

(defun pel-scroll-up-all-insync (including-current &optional n)
  "Scroll up N line(s) the scroll synced windows INCLUDING-CURRENT if is is t.
If INCLUDING-CURRENT is nil, then scroll all windows except current window.
Scrolling up means moving text up.
Default N is 1 line."
  (interactive "p")
  (let ((original-window (selected-window)))
    (when (member original-window pel-in-scroll-sync)
      (dolist (window pel-in-scroll-sync)
        (when (or including-current
                  (not (equal window original-window)))
          ;; Ignore error caused by one window being at the top
          ;; already: don't exit the loop & process other windows.
          (ignore-errors
            (select-window window)
            (scroll-up n))))
      (select-window original-window))))

;;-pel-autoload
(defun pel-scroll-up (&optional n)
  "Scroll up 1 line: move text up (same direction as forward).

Scrolling up is bringing text ahead into view.
If N is specified it identifies a repetition count.
If N is negative it means the other direction.
This command prevents screen re-centering done by the low level
scroll function."
  (interactive "P")
  (let ((n (prefix-numeric-value n))
        ;; (scroll-conservatively 10000)
        (scroll-step 1))    ; prevent screen re-centering by low level scroll
    (if (< n 0)
        (pel-scroll-down (abs n))
      (unless (and pel-in-scroll-sync
                   (pel-scroll-up-all-insync :all n))
        (scroll-up n)))))

;; --

;;-pel-autoload
(defun pel-scroll-down-other (&optional n)
  "Scroll the other window 1 line: move text down (same direction as backward).
Scrolling down is bringing text below into view.
If N is specified it identifies a repetition count.
If N is negative it means the other direction."
  (interactive "p")
  ;; flip the meaning of n and call scroll-other-window-down with it:
  ;; nil -> 1
  ;; x   -> x
  (scroll-other-window-down (or n 1)))

;;-pel-autoload
(defun pel-scroll-up-other (&optional n)
  "Scroll the other window 1 line: move text up (same direction as forward).
Scrolling up is bringing text ahead into view.
If N is specified it identifies a repetition count.
If N is negative it means the other direction."
  (interactive "p")
  ;; flip the meaning of n and call scroll-other-window with it:
  ;; nil -> 1
  ;; x   -> x
  (scroll-other-window (or n 1)))

;; --

(defun pel-scroll-down-all-insync (including-current  &optional n)
  "Scroll down N line(s) the scroll synced windows INCLUDING-CURRENT if it is t.
If INCLUDING-CURRENT is nil, then scroll all windows except current window.
Scrolling down means moving text down.
Default N is 1 line."
  (interactive)
  (let ((original-window (selected-window)))
    (when (member original-window pel-in-scroll-sync)
      (dolist (window pel-in-scroll-sync)
        (when (or including-current
                  (not (equal window original-window)))
          ;; Ignore error caused by one window being at the top
          ;; already: don't exit the loop & process other windows.
          (ignore-errors
            (select-window window)
            (scroll-down n))))
      (select-window original-window))))

;; TODO: in graphics mode on Emacs 26.3, I have seen the scroll-down and scroll-up by 1
;;       do nothing, even after several calls.  I have never seen this behaviour on
;;       Emacs in terminal mode (which I use much more often).
;;       I tried to fix it by forcing scroll-conservatively but that did not
;;       change anything.  I'd have to learn the low level Emacs scroll
;;       control code, which is written in C to understand why it's sometimes
;;       doing this.

;;-pel-autoload
(defun pel-scroll-down (&optional n)
  "Scroll down 1 line: move text down (same direction as backwards).

Scrolling down is bringing text behind into view.
If N is specified it identifies a repetition count.
If N is negative it means the other direction.
This command prevents screen re-centering done by the low level
scroll function."
  (interactive "P")
  (let ((n (prefix-numeric-value n))
        ;; (scroll-conservatively 10000)
        (scroll-step 1))     ; prevent screen re-centering by low level scroll
    (if (< n 0)
        (pel-scroll-up (abs n))
      (unless (and pel-in-scroll-sync
                   (pel-scroll-down-all-insync :all n))
        (scroll-down n)))))

;; -----------------------------------------------------------------------------
(provide 'pel-scroll)

;;; pel-scroll.el ends here
