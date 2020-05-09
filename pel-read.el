;;; pel-read.el --- Read text from buffer -*-lexical-binding: t-*-

;; Copyright (C) 2020  Pierre Rouleau

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
;; Utility functions that read text from current buffer at point.
;;
;;  - pel-word-at-point
;;  - pel-sentence-at-point
;;  - pel-paragraph-at-point
;;    - pel-thing-at-point
;;

;;; Code:

(require 'thingatpt)
(require 'pel-navigate)

(defun pel-thing-at-point (thing)
  "Read and return the string of THING at point.
See `bounds-of-thing-at-point' for a list of possible THING symbols."
  (let ((bounds (bounds-of-thing-at-point thing))
        text)
    (if bounds
        (progn
          (setq text (buffer-substring-no-properties
                      (car bounds)
                      (cdr bounds)))
          (goto-char (cdr bounds))
          text)
      (error "No %s at point" thing))))

;;-pel-autoload
(defun pel-word-at-point ()
  "Read and return word at point, moving to next word."
  (let ((text (pel-thing-at-point 'word)))
    (ignore-errors
      (pel-forward-word-start))
    text))

;;-pel-autoload
(defun pel-sentence-at-point ()
  "Read and return sentence at point, moving to next sentence."
  (let ((text (pel-thing-at-point 'sentence)))
    (ignore-errors
      (pel-forward-word-start))
    text))

;;-pel-autoload
(defun pel-paragraph-at-point ()
  "Read and return paragraph at point, moving to next paragraph."
  (let ((text (pel-thing-at-point 'paragraph)))
    (ignore-errors
      (pel-forward-word-start))
    text))

;; -----------------------------------------------------------------------------
(provide 'pel-read)

;;; pel-read.el ends here
