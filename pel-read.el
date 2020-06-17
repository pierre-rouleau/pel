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
;;  - `pel-word-at-point'
;;  - `pel-sentence-at-point'
;;  - `pel-paragraph-at-point'
;;    - `pel-thing-at-point'
;;  - `pel-string-at-point'
;;

;;; Code:

(require 'thingatpt)      ; use: bounds-of-thing-at-point
(require 'pel-navigate)   ; use: pel-forward-word-start

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
;; Read string at point
;; --------------------


(defun pel-string-at-point (delimiters &optional always-allow-space)
  "Return the string at point delimited by DELIMITERS string.

The DELIMITERS string must NOT include a space:
- If point is currently NOT located at one of the DELIMITERS characters
  then the space character is automatically added to the DELIMITERS.
- When point is located at a delimiter, space is not added as a delimiter
  to allow space to be included in the extracted string.

If ALWAYS-ALLOW-SPACE is non-nil, then the space character is never included
in the delimiters so it becomes possible to capture a delimited string with
spaces even when point is located between the delimiters."
  (save-excursion
    (let* (p1
           p2
           (at-delimiter (string-match-p (regexp-quote (string (char-after))) delimiters))
           (delimiters (if (or at-delimiter always-allow-space)
                           delimiters
                         (concat " " delimiters)))
           (delimiters (concat "^" delimiters))) ; skip all BUT those delimiters
      (if at-delimiter
          ;; when at first delimiter move in the string.
          (forward-char)
        ;; otherwise search backward for the first delimiter
        (skip-chars-backward delimiters))
      (setq p1 (point))
      ;; move to the second delimiter
      (skip-chars-forward delimiters)
      (setq p2 (point))
      (buffer-substring-no-properties p1 p2))))

;; -----------------------------------------------------------------------------
(provide 'pel-read)

;;; pel-read.el ends here
