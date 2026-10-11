;;; pel-text-insert-base.el --- Text insertion base functions  -*- lexical-binding: t; -*-

;; Created   : Saturday, October 10 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-10-11 16:43:06 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2026  Pierre Rouleau
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
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)     ; use : `pel-ends-with-space-p'
(require 'pel--macros)   ; use: `pel-concat-to'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-comment-specs (&optional comment-prefix)
  "Return (comment-start . comment-end) cons cell required at point.

The two returned strings take the current mode into account and
the current position.  If the comment style has no comment end
the second element is nil."
  ;; Note: at the moment the second element is always equal to comment-end
  ;; because there's no reason to change it as far as I can tell with the
  ;; currently supported modes.  It returns it anyway in case I find some
  ;; condition to modify it.
  (require 'newcomment)
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
         (len-comment-end (length (or cmt-end "")))
         (has-comment-end (and (stringp cmt-end) (> len-comment-end 0)))
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

;;; --------------------------------------------------------------------------
(provide 'pel-text-insert-base)

;;; pel-text-insert-base.el ends here
