;;; pel-mark.el --- PEL Mark Management Utilities -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2026  Pierre Rouleau

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

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; This file holds a collection of functions used to inspect, manipulate and
;; set the mark for various uses.  The file holds the following commands (the
;; ones with a * in the list below) and functions.
;;
;;
;; * `pel-mark-ring-stats'
;;   - `pel-global-mark-buffer-positions'
;;   - `pel-mark-ring-positions'
;;
;; * `pel-popoff-mark-ring'
;; * `pel-mark-line-up'
;; * `pel-mark-line-down'
;;
;; * `pel-push-mark-no-activate'
;; * `pel-jump-to-mark'
;; * `pel-exchange-point-and-mark-no-activate'

;; Credits:
;; The last 3 functions are copies/adaptation of code written by Mickey
;; Petersen.  See the complete attribution note below, just above the code of
;; these 3 functions.

;;; --------------------------------------------------------------------------
;;; Dependencies:
(require 'pel--base)   ; use: `pel-yes-no-string', `pel-symbol-on-off-string'

;;; --------------------------------------------------------------------------
;;; Code:

;; From simple.el (which is loaded even by emacs -Q)
;; The following is to prevent lint warnings.
(defvar global-mark-ring-max)
(defvar mark-ring-max)

;; --

(defun pel-global-mark-buffer-positions ()
  "Return a list of (buffer position) cons cells of the `global-mark-ring'.
If a buffer identified in the list has been killed, the entry cons cell is
modified to be ((buffer \\='deleted!) position)."
  (mapcar (lambda (m)
            (let ((buf (marker-buffer m))
                  (bname (buffer-name (marker-buffer m)))
                  (pos (marker-position m)))
              (if (and buf (buffer-live-p buf))
                  (cons bname pos)
                (cons (cons bname 'deleted!) pos))))
          global-mark-ring))

(defun pel-mark-ring-positions ()
  "Return a list of position integers corresponding to the `mark-ring' markers."
  (mapcar 'marker-position mark-ring))

;;-pel-autoload
(defun pel-mark-ring-stats ()
  "Show info about global and buffer local mark and mark rings.

Print their current and maximum size, buffer and positions for each
mark ring entry.
This function can be used to help understand the behaviour and impact
of commands on the mark and mark rings."
  (interactive)
  (message "\
%-40s %-24s  %s
%s: mark-ring size=%d/%d: %S
Global mark-ring size=%d/%d: %S"
           (format "Point, Mark: %S, %S.  Region:%s. "
                   (point) (mark 'force)
                   (pel-yes-no-string mark-active "active" "inactive"))
           (format "Transient mark mode: %s."
                   (pel-symbol-on-off-string 'transient-mark-mode))
           (format "Delete selection mode: %s."
                   (pel-symbol-on-off-string 'delete-selection-mode))
           ;; local
           (buffer-name)
           (length mark-ring) mark-ring-max (pel-mark-ring-positions)
           ;; global
           (length global-mark-ring) global-mark-ring-max
           (pel-global-mark-buffer-positions)))

;; --

;;-pel-autoload
(defun pel-popoff-mark-ring ()
  "Remove the top entry from the buffer's mark ring."
  (interactive)
  (when mark-ring
    (setq mark-ring (cdr mark-ring))
    (message "Mark-ring now has %d markers." (length mark-ring))))

;;-pel-autoload
(defun pel-mark-line-up (&optional n)
  "Mark current line or N previous lines for going up.

When mark is inactive:
 - No argument (or N=1): mark current line.
 - With argument N: mark current line and n-1 lines above.
When mark is active:  Change one end of the region to N+1 lines up.

Does nothing when N is 0.  The absolute value of N is used.

Mostly useful when mark is off to mark the current line or current set of
lines and allowing extension of the created region using navigation keys
without the need to use shift select."
  (interactive "P")
  (let ((n (abs (prefix-numeric-value n))))
    (unless (= n 0)
      (if mark-active
          ;; when mark is active issuing the command means moving 1 line up
          ;; than identified by n otherwise it would mean to mark current line.
          (pel+= n 1)
        ;; set mark only if it was not already active
        (set-mark (line-end-position)))
      (forward-line (- 1 n)))))

;;-pel-autoload
(defun pel-mark-line-down (&optional n)
  "Mark current line or N lines forward.
Set mark at beginning of line, move point to line end.
When mark is already active extend the region one more line down.

Does nothing when N is 0.  The absolute value of N is used.

Mostly useful when mark is off to mark the current line or current set of
lines and allowing extension of the created region using navigation keys
without the need to use shift select."
  (interactive "P")
  (let ((n (abs (prefix-numeric-value n))))
    (unless (= n 0)
      (if mark-active
          ;; when mark is active issuing the command means moving 1 line down
          ;; otherwise it means to mark current line.
          (pel+= n 1)
        ;; set mark only if it was not already active
        (set-mark (line-beginning-position)))
      (end-of-line n))))

;; ---------------------------------------------------------------------------
;; Attribution Notice for the code below:
;;   Code idea taken from the Mickey Petersen's great website at
;;   https://www.masteringemacs.org\
;;   /article/fixing-mark-commands-transient-mark-mode

;;-pel-autoload
(defun pel-push-mark-no-activate ()
  "Push `point' to the buffer's `mark-ring' without activating the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled.

Note that `push-mark' sets the mark at point but only pushes the
previous mark to the ring. With no prior mark, the ring stays empty."
  (interactive)
  (let ((current-mark (mark 'force)))
    (push-mark (point) t nil)
    (if current-mark
        (message "Set mark to point (%d). Pushed old mark (%d) to local mark ring."
                 (point) current-mark)
      (message "Set mark to point (%d). No old mark to push to mark ring."
               (point)))))

;;-pel-autoload
(defun pel-jump-to-mark ()
  "Jump to the next mark in the buffer's `mark-ring', then rotate the ring.
This is the same as using the `set-mark-command' via \\[set-mark-command] with
the prefix argument (but easier to type.)
When the `mark-ring' is empty, the function signals \"No mark set in this
buffer\"."
  (interactive)
  (set-mark-command 1)
  ;; replace the misleading "Mark popped" message by something
  ;; that really describe the operation.  Really nothing gets
  ;; removed from the local mark-ring.
  (message "Jumped to mark%s"
           (if mark-ring
               " and moved mark to the next position from \
local buffer's mark ring"
             "")))

;;-pel-autoload
(defun pel-exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but without activating the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

;; ---------------------------------------------------------------------------
(provide 'pel-mark)

;;; pel-mark.el ends here
