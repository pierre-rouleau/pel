;;; pel-read.el --- Read text from buffer -*- lexical-binding: t; -*-

;; Created   : Tuesday, May 25 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-04-17 09:33:31 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2021, 2022, 2025, 2026  Pierre Rouleau

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
;; Utility functions that read various text elements from current buffer at
;; point.
;;
;; The following call hierarchy uses '-' for functions and '.' when repeating
;; a function also called that has already been represented in the hierarchy.
;;
;;  Read "thing" at point
;;  - `pel-word-at-point'
;;    - `pel-thing-at-point'
;;  - `pel-sentence-at-point'
;;    . `pel-thing-at-point'
;;  - `pel-paragraph-at-point'
;;    . `pel-thing-at-point'
;;
;;  Read string at point
;;  - `pel-string-at-point'
;;
;;  Read customize symbol at point
;;  - `pel-customize-symbol-at-line'
;;    - `pel-move-to-face'
;;    - `pel-move-past-face'
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-navigate)   ; use: `pel-forward-word-start'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;* Read "thing" at point
;;  =====================

(defun pel-thing-at-point (thing)
  "Read and return the string of THING at point.
See `bounds-of-thing-at-point' for a list of possible THING symbols."
  ;; thingatpt: use: bounds-of-thing-at-point
  (unless (and (require 'thingatpt nil :noerror)
               (fboundp 'bounds-of-thing-at-point))
    (user-error "Failed loading thingatpt!"))
  (let ((bounds (bounds-of-thing-at-point thing))
        text)
    (if (and bounds
             (progn
               (setq text (buffer-substring-no-properties
                           (car bounds)
                           (cdr bounds)))
               ;; Emacs 26.1: bounds-of-thing-at-point for 'word 'sentence
               ;; or 'paragraph can return non-nil bounds covering only
               ;; whitespace; reject that case.
               (or (not (memq thing '(word sentence paragraph)))
                   (string-match-p "\\w" text))))
        (progn
          (goto-char (cdr bounds))
          text)
      (user-error "No %s at point" thing))))

;;-pel-autoload
(defun pel-word-at-point (&optional dont-move)
  "Return word at point, move to next word unless DONT-MOVE is non-nil."
  (let ((text (pel-thing-at-point 'word)))
    (unless dont-move
      (ignore-errors
        (pel-forward-word-start)))
    text))

;;-pel-autoload
(defun pel-sentence-at-point (&optional dont-move)
  "Return sentence at point, move to next sentence unless DONT-MOVE is non-nil."
  ;; pel-thing-at-point leaves point just after the period at the end of the
  ;; sentence.  pel-forward-word-start moves point to the beginning of the
  ;; next sentence.
  (let ((text (pel-thing-at-point 'sentence)))
    (unless dont-move
      (ignore-errors
        (pel-forward-word-start)))
    text))

;;-pel-autoload
(defun pel-paragraph-at-point (&optional dont-move)
  "Return paragraph at point, move to next paragraph unless DONT-MOVE is non-nil."
  ;; pel-thing-at-point leaves point just after the period at the end of the
  ;; paragraph.  pel-forward-word-start moves point to the beginning of the
  ;; next paragraph.
  (let ((text (pel-thing-at-point 'paragraph)))
    (unless dont-move
      (ignore-errors
        (pel-forward-word-start)))
    text))

;; ---------------------------------------------------------------------------
;;* Read string at point
;;  ====================

(defun pel-string-at-point (delimiters &optional allow-space)
  "Return the string at point delimited by DELIMITERS string.

The DELIMITERS string must NOT include a space:
- If point is currently NOT located at one of the DELIMITERS characters
  then the space character is automatically added to the DELIMITERS.
- When point is located at a delimiter, space is not added as a delimiter
  to allow space to be included in the extracted string.

If ALLOW-SPACE is non-nil, then the space character is never included in
the delimiters so it becomes possible to capture a delimited string with
spaces even when point is located between the delimiters.

If there is no character next, return an empty string."
  (save-excursion
    (let ((next-char (char-after)))
      (if next-char
          (let* (p1
                 p2
                 (at-delimiter (string-match-p (regexp-quote (string next-char))
                                               delimiters))
                 (delimiters (if (or at-delimiter allow-space)
                                 delimiters
                               (concat " " delimiters)))
                 ;; skip all BUT those delimiters
                 (delimiters (concat "^" delimiters)))
            (if at-delimiter
                ;; when at first delimiter move in the string.
                (forward-char)
              ;; otherwise search backward for the first delimiter
              (skip-chars-backward delimiters))
            (setq p1 (point))
            ;; move to the second delimiter
            (skip-chars-forward delimiters)
            (setq p2 (point))
            (buffer-substring-no-properties p1 p2))
        ;; If no next char return empty string
        ""))))

;; ---------------------------------------------------------------------------
;;* Read customize symbol at point
;;  ==============================

(defun pel-move-to-face (face limit)
  "Move to the first char with specified FACE, up to (but not including) LIMIT.
If point is already on FACE, return point immediately without moving.
Return the position of the first char with FACE, or nil if none is found
before LIMIT."
  (while (and (not (eq face
                       (get-char-property (point) 'face)))
              (< (point) limit))
    (forward-char 1))
  (if (eq (point) limit)
      nil
    (point)))

(defun pel-move-past-face (face limit)
  "Move point past contiguous region with FACE, up to (but not including) LIMIT.
If point is not on FACE, return point immediately.
Return the new point position if a non-FACE char is reached before LIMIT,
nil if the FACE region extends all the way to LIMIT."
  (while (and (eq face
                  (get-char-property (point) 'face))
              (< (point) limit))
    (forward-char 1))
  (if (eq (point) limit)
      nil
    (point)))

(defun pel-customize-symbol-at-line ()
  "Return the string for the customize symbol on current line.

Customize symbols are taken from Customize buffers *only*.
They are identified by their `custom-variable-tag' face.
The text may include space characters.
The text must be on a single line.
Return nil if no customize symbol is present on the current line."
  ;; Identify the symbol by the face of text.
  ;; Start from the beginning o the line.
  (save-excursion
    (let (p1                            ; point of first char
          p2                            ; point of last char
          (pe (progn
                (move-end-of-line 1)
                (point))))
      ;; move to the beginning of line, unconstrained by fields
      (forward-line 0)
      (setq p1 (pel-move-to-face 'custom-variable-tag pe))
      (when p1
        (setq p2 (pel-move-past-face 'custom-variable-tag pe))
        (when p2
          (buffer-substring-no-properties p1 p2))))))

;;; --------------------------------------------------------------------------
(provide 'pel-read)

;;; pel-read.el ends here
