;;; pel-text-insert.el --- PEL Text Insertion Utilities

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

;;; Code:

(require 'pel-options)                  ; use: pel-linelen

;; -----------------------------------------------------------------------------
;; Direction conversion
;; --------------------

(defun pel--direction-for (n)
  "Return a symbol indicating direction corresponding to integer N.
Look at the location of keypad numbers to see the direction relationship."
  (cond ((eq n 2) 'down)
        ((eq n 8) 'up)
        ((eq n 4) 'left)
        ((eq n 6) 'right)
        (t        'current)))


;;-pel-autoload
(defun pel-insert-line (&optional linelen)
  "Insert a (commented) line before/at current line.
- If point is at the beginning of the line insert it there.
- If point is in the middle of a line, move point at beginning of line
  before inserting it.
The number of dash characters of the line is specified by LINELEN:
- If LINELEN is not specified the default (`pel-linelen' := 77) is used,
- otherwise the argument value is used."
  (interactive "*P")
  (let ((linelen (if linelen
                     (abs (prefix-numeric-value linelen))
                   pel-linelen)))
  (move-beginning-of-line nil)
  (insert-char ?- linelen))
  (insert "\n")
  (forward-line -1)
  (if (equal comment-start "-- ")
      ;; When comment-start is "-- " commenting a line that starts with
      ;; "--" erases it as it happens in haskell-mode.
      ;; In this case prepend it manually.
      (progn
        (move-beginning-of-line nil)
        (insert comment-start))
    (progn
      (move-end-of-line nil)
      (comment-line nil))))

;;-pel-autoload
(defun pel-insert-filename (&optional n)
  "Insert at point the name of a the file of the window identified by N.
N is a numeric argument that identifies the window that holds the file.
The numeric values correspond to the directions of the numeric keypad numbers:
.      8
.   4     6
.      2
That is:
 - 8: up
 - 4: left
 - 6: right
 - 2: down
Every other value, or no argument identifies the current window.
If the argument is positive, `pel-insert-filename'  inserts a filename with full
absolute path, if negative it omits the path."
  (interactive "*p")
  (let ((no-path (< n 0))
        (direction (pel--direction-for (abs n))))
    (if (eq direction 'current)
        (insert (pel-current-buffer-filename no-path))
      (let ((original-window (selected-window))
            fname)
        (require 'pel-window)
        (save-excursion
          (if (fboundp 'pel-move-to-window)
              (progn
                (pel-move-to-window direction)
                (setq fname (pel-current-buffer-filename no-path))
                (select-window original-window))
            (error "Internal loading error: failed loading pel-window")))
        (insert fname)))))

;;-pel-autoload
(defun pel-insert-current-date-time (&optional utc)
  "Insert current date and time at point.
Local by default, UTC if \\[universal-argument] prefix used."
  (interactive "*P")
  (insert
   (if utc
       (format-time-string "%A, %B %d, %Y @ %T (UTC)" nil t)
     (format-time-string "%A, %B %d, %Y @ %T"))))

;;-pel-autoload
(defun pel-insert-current-date (&optional utc)
  "Insert current date (only, no time) at point.
Local by default, UTC if \\[universal-argument] prefix used."
  (interactive "*P")
  (insert
   (if utc
       (format-time-string "%A, %B %d, %Y (UTC)" nil t)
     (format-time-string "%A, %B %d, %Y"))))

;;-pel-autoload
(defun pel-insert-iso8601-timestamp (&optional utc)
  "Insert ISO 8601 conforming abbreviated YYYY-MM-DD hh:mm:ss format timestamp.
Local by default, UTC if \\[universal-argument] prefix is used."
  (interactive "*P")
  (insert
   (if utc
       (format-time-string "%F %T (UTC)" nil t)
     (format-time-string "%F %T"))))

;; -----------------------------------------------------------------------------
(provide 'pel-text-insert)

;;; pel-text-insert.el ends here
