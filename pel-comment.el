;;; pel-comment.el --- PEL Comments Utilities

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
(require 'pel-base)                      ; use: pel-toggle

;;-pel-autoload
(defun pel-comment-start (string)
  "Show and set comment start STRING for current mode."
  (interactive
   (let ((string (read-string
               (format "Comment start string 「%s」: " comment-start)
               comment-start)))
     (list string)))
  (setq comment-start string))

;;-pel-autoload
(defun pel-comment-middle (string)
  "Show and set comment continue/middle STRING for current mode."
  (interactive
   (let ((string (read-string
               (format "Comment middle/continue string 「%s」: " comment-continue)
               comment-continue)))
     (list string)))
  (setq comment-continue string))

;;-pel-autoload
(defun pel-comment-end (string)
  "Show and set comment end STRING for current mode."
  (interactive
   (let ((string (read-string
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

;; -----------------------------------------------------------------------------
(provide 'pel-comment)

;;; pel-comment.el ends here
