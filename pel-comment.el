;;; pel-comment.el --- PEL Comments Utilities -*-lexical-binding: t-*-

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

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; This file holds a collection of functions used to manipulate,insert
;; comments in the current buffer and provide information about the way
;; comments are handled.
;;
;; The file holds the following commands:

;; * pel-comment-start
;; * pel-comment-middle
;; * pel-comment-end
;; * pel-toggle-comment-auto-fill-only-comments
;; * pel-delete-all-comments
;; * pel-kill-all-comments

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;

;; Elisp functions taken from files always loaded
;; - from: newcomment                    ; use: comment-kill
;; - from: simple                        ; use kill-ring

(require 'pel--base)                     ; use: pel-toggle,
;;                                       ;       pel-print-in-buffer
(eval-when-compile (require 'subr-x))    ; use: dolist, with-current-buffer

;;; --------------------------------------------------------------------------
;;; Code:


;;-pel-autoload
(defun pel-comment-start (string)
  "Show and set comment start STRING for current mode."
  (interactive
   (let ((string
          (read-string
           (format "Comment start string 「%s」: " comment-start)
           comment-start)))
     (list string)))
  (setq comment-start string))

;;-pel-autoload
(defun pel-comment-middle (string)
  "Show and set comment continue/middle STRING for current mode."
  (interactive
   (let ((string
          (read-string
           (format "Comment middle/continue string 「%s」: " comment-continue)
           comment-continue)))
     (list string)))
  (setq comment-continue string))

;;-pel-autoload
(defun pel-comment-end (string)
  "Show and set comment end STRING for current mode."
  (interactive
   (let ((string
          (read-string
           (format "Comment end string 「%s」: " comment-end)
           comment-end)))
     (list string)))
  (setq comment-end string))


;;-pel-autoload
(defun pel-toggle-comment-auto-fill-only-comments ()
  "Toggle the variable `comment-auto-fill-only-comments'.
Activate/de-activate automatic filling in source code comments only."
  (interactive)
  (pel-toggle 'comment-auto-fill-only-comments))

;;-pel-autoload
(defun pel-kill-all-comments ()
  "Kill all comments in current (possibly narrowed) buffer or marked region.
Retain them in the variable `kill-ring'.
Each killed comment group is retained in the kill ring, as a separate kill
ring entry."
  (interactive "*")
  (let ((start-point (if (region-active-p) (region-beginning) (point-min)))
        (end-point   (if (region-active-p) (region-end) (point-max))))
    (save-excursion
      (goto-char start-point)
      (comment-kill (count-lines start-point end-point)))))

;;-pel-autoload
(defun pel-delete-all-comments ()
  "Delete all comments in current (possibly narrowed) buffer or marked region."
  (interactive "*")
  (let (kill-ring)
    (pel-kill-all-comments)))

;;-pel-autoload
(defun pel-comment-show-variables (&optional only-user-options)
  "Show the names and values of the comment related variables.

Open a *comment-vars* buffer and insert the information at the end of the
buffer. Move last line of text to the last line of window.

If the ONLY-USER-OPTIONS argument is non-nil, include only user option
variables."
  (interactive "P")
  (let ((comment-symbols '())
        (max-length 0)
        (local-values-alist '()))     ; maps symbol to it's buffer local value
    ;; Accumulate names of `comment-' variables and their values.
    (mapatoms (lambda (symbol)
                (when (and (boundp symbol)
                           (or (not only-user-options)
                               (get symbol 'custom-type))
                           (pel-string-starts-with
                            (symbol-name symbol) "comment-"))
                  (push symbol comment-symbols)
                  (push (cons symbol (symbol-value symbol))
                        local-values-alist))))
    ;; Sort their names.
    (setq comment-symbols
          (sort comment-symbols (lambda (s1 s2)
                                  (let ((n1 (symbol-name s1)))
                                    (when (> (length n1) max-length)
                                      (setq max-length (length n1)))
                                    (string< n1
                                             (symbol-name s2))))))
    ;; Print a report inside a specialized buffer.
    (let ((format-string (format "%%-%ds: %%S\n" max-length)))
      (pel-print-in-buffer
       "*comment-vars*"
       "Comment controlling variables"
       (lambda ()
         "Print comment control data."
         (insert (format "----- List of comment %s:\n"
                         (if only-user-options
                             "user options"
                           "variables")))
         (dolist (symbol comment-symbols)
           (insert (format format-string
                           (symbol-name symbol)
                           (cdr (assoc symbol local-values-alist))))))))))

;;; --------------------------------------------------------------------------
(provide 'pel-comment)

;;; pel-comment.el ends here
