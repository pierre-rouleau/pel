;;; pel-text-insert.el --- PEL Text Insertion Utilities -*-lexical-binding: t; -*-

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
;; * `pel-insert-dirname-wtilde'
;;   * `pel-insert-dirname'
;;   * `pel-insert-filename-wtilde'
;;     * `pel-insert-filename'
;;       - `pel-tilde-file-name'
;; * `pel-insert-current-date-time'
;; * `pel-insert-current-date'
;; * `pel-insert-todo-note'
;;
;;   - `pel-insert-commented'
;;     - `pel-comment-specs'

;; -----------------------------------------------------------------------------
;;; Dependencies:

(require 'pel--base)       ; use: pel-require
;;                         ;      pel-current-buffer-filename
;;                         ;      pel-ends-with-space-p
;;                         ;      pel-comment-prefix
(require 'pel--macros)     ; use: pel-concat-to
(require 'pel-window)      ; use: pel-window-direction-for
(require 'pel-syntax)      ; use: pel-inside-comment-p
;; -----------------------------------------------------------------------------
;;; Code:

(defun pel-comment-specs (&optional comment-prefix)
  "Return (comment-start . comment-end) cons cell required at point.

The two returned strings take the current mode into account and
the current position.  If the comment style has no comment end
the second element is nil."
  ;; Note: at the moment the second element is always equal to comment-end
  ;; because there's no reason to change it as far as I can tell with the
  ;; currently supported modes.  It returns it anyway in case I find some
  ;; condition to modify it.
  (pel-require 'newcomment)
  (comment-normalize-vars)
  (let* ((point-column         (current-column))
         (point-at-line-start  (or (= point-column 0)
                                   (= point-column (current-indentation))))
         ;; Adjust the comment start to required style: programming languages
         ;; that use single chars like Lisp and Erlang double up these characters
         ;; for comments that start at the beginning of the line. Other, like
         ;; Python or shells scripting, who use '#' don't modify it.
         (cmt-start (or comment-prefix pel-comment-prefix comment-start)))
    (cons (if (and point-at-line-start
                   (= (length cmt-start) 1)
                   (member cmt-start '(";" "%")))
              (concat cmt-start cmt-start)
            cmt-start)
          comment-end)))

(defun pel-insert-commented (text)
  "Insert the commented TEXT at point.

Return position of the last inserted TEXT character inside the comment.
If point is already inside a comment, just insert TEXT and return point."
  (if (pel-inside-comment-p)
      (progn
        (insert text)
        (point))
    (let* ((comment-start.comment-end (pel-comment-specs))
           (cmt-start (car comment-start.comment-end))
           (cmt-end (cdr comment-start.comment-end))
           pos)
      (insert cmt-start)
      (unless (pel-string-ends-with-p cmt-start " ")
        (insert " "))
      (insert text)
      (setq pos (point))
      (when cmt-end
        (insert cmt-end))
      pos)))

;; ---------------------------------------------------------------------------
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
- `comment-start otherwise.
The string ends (if applicable) with the comment character(s).
The string does not end with a newline."
  (let* ((linelen (if linelen
                      (abs (prefix-numeric-value linelen))
                    fill-column))
         (comment-start.comment-end (pel-comment-specs comment-prefix))
         (cmt-start (car comment-start.comment-end))
         (cmt-end   (cdr comment-start.comment-end))
         (len-comment-start (length cmt-start))
         (len-comment-end (length cmt-end))
         (has-comment-end (>= len-comment-end 1))
         (comment-start-ends-with-space (pel-ends-with-space-p cmt-start))
         (line cmt-start))
    (unless comment-start-ends-with-space
      (pel-concat-to line " "))
    (pel-concat-to line (make-string (- linelen
                                        len-comment-start len-comment-end
                                        (if comment-start-ends-with-space 0 1))
                                     (or char ?-)))
    (when has-comment-end
      (pel-concat-to line cmt-end))
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

(defun pel-tilde-file-name (filename)
  "Replace user home directory by ~ in FILENAME."
  (let ((tilde-expanded (expand-file-name "~")))
    (if (string= filename tilde-expanded)
        "~"
      (replace-regexp-in-string (format "^%s/" tilde-expanded) "~/"
                                filename))))

;;-pel-autoload
(defun pel-insert-filename (&optional n use-tilde dir-only)
  "Insert at point the name of a the file of the window identified by N.
N is a numeric argument that identifies the window that holds the file.

If N is not specified or is 1, use the current window.
Otherwise, the numeric values correspond to the directions of the
numeric keypad numbers:
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
absolute path, if negative it omits the path.

If USE-TILDE, the user home address is replaced by the single character ~.
If DIR-ONLY, only insert the directory (negative argument has no impact when
DIR-ONLY is non nil)."
  (interactive "*p")
  (let ((no-path (and (< n 0) (not dir-only)))
        (direction (if (eq (abs n) 1)
                       'current
                     (pel-window-direction-for (abs n))))
        fname)
    (if (eq direction 'current)
        (setq fname (pel-current-buffer-filename no-path))
      (let ((original-window (selected-window)))
        (save-excursion
          (pel-move-to-window direction)
          (setq fname (pel-current-buffer-filename no-path))
          (select-window original-window))))
    (when use-tilde
      (setq fname (pel-tilde-file-name fname)))
    (when dir-only
      (setq fname (file-name-directory fname)))
    (insert fname)))

;;-pel-autoload
(defun pel-insert-filename-wtilde (&optional n)
  "Insert directory name with ~ if user home is identified.
Use N the same way as `pel-insert-dirname'."
  (interactive "*p")
  (pel-insert-filename n :use-tilde))

;;-pel-autoload
(defun pel-insert-dirname (&optional n use-tilde)
  "Insert at point the name of a the file of the window identified by N.
N is a numeric argument that identifies the window that holds the file.

If N is not specified or is 1, use the current window.
Otherwise, the numeric values correspond to the directions of the
numeric keypad numbers:
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
absolute path, if negative it omits the path.

If USE-TILDE, the user home address is replaced by the single character ~."
  (interactive "*p")
  (pel-insert-filename n use-tilde :dir-only))

;;-pel-autoload
(defun pel-insert-dirname-wtilde (&optional n)
  "Insert directory name with ~ if user home is identified.
Use N the same way as `pel-insert-dirname'."
  (interactive "*p")
  (pel-insert-dirname n :use-tilde))

;; ----
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

;; ----

;;-pel-autoload
(defun pel-insert-todo-note ()
  "Insert a to-do note template with the creation date and author's name."
  (interactive)
  (goto-char (pel-insert-commented (format "[:todo %s, by %s: ]"
                                           (format-time-string "%F")
                                           user-full-name)))
  (left-char))

;; -----------------------------------------------------------------------------
(provide 'pel-text-insert)

;;; pel-text-insert.el ends here
