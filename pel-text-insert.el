;;; pel-text-insert.el --- PEL Text Insertion Utilities -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2021  Pierre Rouleau

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
;; This file holds a set of functions that insert generic text inside file
;; buffers.
;;
;; The commands ('*') and functions ('-') provided are the following:
;;
;; * `pel-insert-line'
;;   - `pel-separator-line'
;; * `pel-insert-filename'
;;   - `pel--direction-for'
;; * `pel-insert-current-date-time'
;; * `pel-insert-current-date'

;; -----------------------------------------------------------------------------
;;; Dependencies:

(require 'pel--base)       ; use: pel-require
;;                         ;      pel-current-buffer-filename
;;                         ;      pel-ends-with-space-p
;;                         ;      pel-comment-prefix
(require 'pel--macros)     ; use: pel-concat-to

;; -----------------------------------------------------------------------------
;;; Code:

;; Separator line


;;-pel-auto load
(defun pel-separator-line (&optional linelen char comment-prefix)
  "Return a (commented) line string.
The number of characters is identified by LINELEN.
If LINELEN is not specified the buffer's `fill-column' value is used.
The character used is identified by CHAR, otherwise '-' is used.
The string starts with the string specified by:
- COMMENT-PREFIX, if not nil, or
- the buffer-local variable `pel-comment-prefix' if not nil, or
- `comment-start' otherwise.
The string ends (if applicable) with the comment character(s).
The string does not end with a newline."
  (pel-require 'newcomment)
  (comment-normalize-vars)
  (let* ((linelen (if linelen
                      (abs (prefix-numeric-value linelen))
                    fill-column))
         ;; adjust the comment start to required style: programming languages
         ;; that use single chars like Lisp and Erlang double up these characters
         ;; for comments tart start at the beginning of the line. Other, like
         ;; Python or shells scripting, who use '#' don't.
         (cmt-start (or comment-prefix pel-comment-prefix comment-start))
         (len-comment-start (length cmt-start))
         (line-comment-start (if (and (= len-comment-start 1)
                                      (member cmt-start '(";" "%")))
                                 (concat cmt-start cmt-start)
                               cmt-start))
         (len-comment-start (length line-comment-start))
         (len-comment-end (length comment-end))
         (has-comment-end (> len-comment-end 1))
         (comment-start-ends-with-space (pel-ends-with-space-p cmt-start))
         (line line-comment-start))
    (unless comment-start-ends-with-space
      (pel-concat-to line " "))
    (pel-concat-to line (make-string (- linelen
                                        len-comment-start len-comment-end
                                        (if comment-start-ends-with-space 0 1))
                                     (or char ?-)))
    (when has-comment-end
      (pel-concat-to line comment-end))
    line))


;;-pel-auto load
(defun pel-insert-line (&optional linelen char)
  "Insert a (commented) line before/at current line.
- If point is at the beginning of the line insert it there.
- If point is in the middle of a line, move point at beginning of line
  before inserting it.
The number of dash characters of the line is specified by LINELEN:
If LINELEN is not specified the buffer's `fill-column' value is used.
The character used is identified by CHAR, otherwise '-' is used."
  (interactive "*P")
  (move-beginning-of-line nil)
  (insert (pel-separator-line linelen char))
  (insert "\n"))

;; -----------------------------------------------------------------------------
;; Inserting a filename
;; --------------------

;; -- Direction conversion

(defun pel--direction-for (n)
  "Return a symbol indicating direction corresponding to integer N.
Look at the location of keypad numbers to see the direction relationship."
  (cond ((eq n 2) 'down)
        ((eq n 8) 'up)
        ((eq n 4) 'left)
        ((eq n 6) 'right)
        (t        'current)))

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
        (require 'pel-window nil :no-error)
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
