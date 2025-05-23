;;; pel-text-transform.el --- PEL Text Transformation -*-lexical-binding: t; -*-

;; Copyright (C) 2020, 2021, 2022, 2023, 2024, 2025  Pierre Rouleau

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
;; The Emacs native case conversion functions only operate on a
;; specific syntactic unit and don't operate on all words in a region.
;; The functions below can operate on both and are good candidate for
;; command replacement.

;;; Code:
(require 'pel--base)     ; use: `pel-at-lowercase-p', `pel-at-uppercase-p'
(require 'pel--options)  ; use: `pel-modes-activating-align-on-return'
;;(require 'paragraphs)  ; use: `sentence-end-double-space', `paragraph-start'
;;                       ;      and `paragraph-separate'.
;;                       ; However, paragraphs.el has no provide form,
;;                       ; probably because it's always part of emacs.
;; *declare* its variables to prevent lint warnings.
(defvar sentence-end-double-space)
(defvar paragraph-start)
(defvar paragraph-separate)

;; PEL: Text Changing Case
;; -----------------------
;;
;; Call hierarchy:
;;
;; - pel-capitalize-first-letter
;; * pel-capitalize-word-or-region
;; * pel-upcase-word-or-region
;; * pel-downcase-word-or-region
;;   - pel-casechg-word-or-region
;; * pel-upcase-letter
;; * pel-downcase-letter

(defun pel-capitalize-first-letter (text)
  "Return TEXT string with first letter capitalized."
  (if (> (length text) 0)
      (concat (upcase (substring text 0 1))
              (substring text 1))
    ""))

(defun pel-casechg-word-or-region (operation n)
  "Detect marked region and perform case convert OPERATION N times.
When N is 0 check the case of current character to determine the case
change operation.
When N is positive check the case of the current char (or the next letter
char) to determine the case change operation.
When N is negative, check the case of the first letter of the previous
word to determine the case change operation.
The case change operation is determined according to the requested
OPERATION (a symbol that must be one of upcase, downcase or capitalize)
and the case of the checked letter, as described below:

----------------------------------------------------
Requested OPERATION  letter case Performed operation
-------------------  ----------- -------------------
upcase               lower case  up-cased
upcase               upper case  capitalized
downcase             lower case  capitalized
downcase             upper case  down-cased
capitalize           lower case  capitalized
capitalize           upper case  capitalized
----------------------------------------------------

For all cases except when N is -1, check the case of character at point
(or next/previous letter character depending of the sign of N) to check
the current case and identify the case change action.
When N is -1, check the case of the first letter of the previous word
to determine the case change action.
When N is 0, perform the operation once on the beginning of the current word."
  (let ((backward (< n 0))
        (byword n))
    (if (use-region-p)
        (let (p1 p2)
          (progn
            (setq p1 (region-beginning) p2 (region-end))
            (cond
             ((eq 'capitalize operation) (if (< n 0)
                                             (upcase-initials-region p1 p2)
                                           (capitalize-region p1 p2)))
             ((eq 'upcase operation)     (if (pel-at-uppercase-p 2 nil nil p1)
                                             (capitalize-region p1 p2)
                                           (upcase-region p1 p2)))
             ((eq 'downcase operation)   (if (pel-at-lowercase-p nil p1)
                                             (capitalize-region p1 p2)
                                           (downcase-region p1 p2)))
             (t
              (error "Unsupported region operation!")))))
      ;; No active region visible
      (when (eq n 0)
        ;; when n is 0: affect current word, from its first
        ;; letter: so move to the beginning of the word.
        (forward-word)
        (backward-word)
        (setq n 1))
      (if (< n 0)
          ;; when going backward, use the case of the previous word
          (setq byword -1)
        ;; otherwise we use the case of the current word.
        (setq byword 0))
      (cond
       ((eq 'capitalize operation) (capitalize-word n))
       ((eq 'upcase operation)     (if (pel-at-uppercase-p 2 nil backward nil)
                                       (capitalize-word n)
                                     (upcase-word n)))
       ((eq 'downcase operation)   (if (pel-at-lowercase-p nil nil byword)
                                       (capitalize-word n)
                                     (downcase-word n)))
       (t
        (error "Unsupported word operation!"))))))


;;-pel-autoload
(defun pel-capitalize-word-or-region (&optional n)
  "Capitalize N word(s) or all words in marked region.
- If no region marked, with:
  - no argument: capitalize current word (from current position).
  - N = 0:  capitalize current word (from start of current word)
  - N > 0: capitalize current word, N-1 following words and move
    forward.
  - N < 0: capitalize current word and N-1 previous words.
  For non-zero N, the conversion starts at point, not at the
  beginning of the word (unless point is at beginning of word).
  Uses: `capitalize-word'.
- If region is marked :
  - if no prefix (N is nil): convert first letter of each word to
    upper/title case and convert all other word letters to lower
    case, using `capitalize-region'.
  - if N <0: convert first letter of each word to upper/title
    case but leave the rest unchanged.
    Uses `upcase-initials-region'."
  (interactive "P")
  (pel-casechg-word-or-region
   'capitalize
   (prefix-numeric-value n)))

;;-pel-autoload
(defun pel-upcase-word-or-region (&optional n)
  "Upcase N word(s) or all words in marked region.
- If no region marked, with:
  - no argument: upcase current word (from current position).
  - N = 0:  upcase current word (from start of current word)
  - N > 0: upcase current word, N-1 following words and move
    forward.
  - N < 0: upcase current word and N-1 previous words.
  For non-zero N, the conversion starts at point, not at the
  beginning of the word (unless point is at beginning of word).
- If region is marked: upcase all words in region.
- Note:
 If 2 \"criteria\" characters are already upper-case letters,
 the function capitalizes the word(s) instead.
 The two \"criteria\" characters are the first (or last) two
 letter characters at point of after (before) point depending
 of N's sign.
- This uses:
   `upcase-word'."
  (interactive "P")
  (pel-casechg-word-or-region
   'upcase
   (prefix-numeric-value n)))

;;-pel-autoload
(defun pel-downcase-word-or-region (&optional n)
  "Down-case N word(s) or all words in marked region.
- If no region marked, with:
  - no argument: downcase current word (from current position).
  - N = 0:  downcase current word (from start of current word)
  - N > 0: downcase current word, N-1 following words and move
    forward.
  - N < 0: downcase current word and N-1 previous words.
  For non-zero N, the conversion starts at point, not at the
  beginning of the word (unless point is at beginning of word).
- If region is marked: downcase all words in region.
- Note:
 If \"criteria\" character is already a lower-case letter,
 the function capitalizes the word(s) instead.
 The \"criteria\" character is the first (or last) letter
 character at point of after (before) point depending
 of N's sign.
- This uses:
   `downcase-word'."
  (interactive "P")
  (pel-casechg-word-or-region
   'downcase
   (prefix-numeric-value n)))

;; --------------------------
;; single letter case control

;;-pel-autoload
(defun pel-upcase-letter (&optional n)
  "upcase n letters at point or all region.

n defaults to 1 unless a region already exists, in which case
all region is up-cased.
With negative N: convert that many letters before point."
  (interactive "P")
  (if (use-region-p)
      (upcase-region (region-beginning) (region-end))
    (let ((beg (point)))
      (right-char (prefix-numeric-value n))
      (upcase-region beg (point)))))

;;-pel-autoload
(defun pel-downcase-letter (&optional n)
  "Down-case N letters at point or all region.

N defaults to 1 unless a region already exists, in which case
all region is down-cased.
With negative N: convert that many letters before point."
  (interactive "P")
  (if (use-region-p)
      (downcase-region (region-beginning) (region-end))
    (let ((beg (point)))
      (right-char (prefix-numeric-value n))
      (downcase-region beg (point)))))

;; Potential future improvement: when N is negative and no are is marked,
;; toggle the case of the letters.

;; -----------------------------------------------------------------------------
;; Control the meaning of a sentence
;; ---------------------------------

(defun pel--sentence-end-description ()
  "Return a string describing end of sentence."
  (if sentence-end-double-space
      "2 space characters"
    "1 space character"))

;;-pel-autoload
(defun pel-toggle-sentence-end ()
  "Toggle sentence spacing between 1 and 2 space characters.  Show status."
  (interactive)
  (progn
    (setq sentence-end-double-space (not sentence-end-double-space))
    (message "Sentence now ends with %s."
             (pel--sentence-end-description))))

;; -----------------------------------------------------------------------------
;; Display information about active text modes
;; -------------------------------------------
;;-pel-autoload
(defun pel-show-text-modes ()
  "Show the status of the various text modes in the mini buffer."
  (interactive)
  (message "Text Modes Status:
- Local indent-tabs-mode   : %-10s, Tab width = %d
- Local newline does align : %-10s. Automatically activated by modes (<f11> t a <f2>): %s
- Local electric-quote-mode: %-10s, electric-quote-local-mode: %s.
- whitespace-mode          : %-10s, show-trailing-whitespace : %-10s, indicate-empty-lines: %s.
- enriched-mode            : %-10s, prettify-symbols-mode    : %s.
- overwrite mode           : %-10s, delete-selection-mode    : %s.
- case-fold-search         : %-10s, sort-fold-case           : %s.
- subword mode             : %-10s, superword mode           : %-10s, glass-mode: %s.
- visible-mode             : %-10s, smart-dash-mode          : %s.
- Sentences end with %s.
- paragraph-start   : %S
- paragraph-separate: %S"
           (pel-symbol-on-off-string 'indent-tabs-mode
                                     "on: use hard-tab"
                                     "off: use spaces"
                                     "void: use spaces")
           tab-width
           (pel-symbol-on-off-string 'pel-newline-does-align)
           (or  pel-modes-activating-align-on-return "none.")
           (pel-symbol-on-off-string 'electric-quote-mode)
           (pel-symbol-on-off-string 'electric-quote-local-mode nil nil "not loaded")
           (pel-symbol-on-off-string 'whitespace-mode nil nil "not loaded")
           (pel-symbol-on-off-string 'show-trailing-whitespace nil nil "not loaded")
           (pel-symbol-on-off-string 'indicate-empty-lines nil nil "not loaded")
           (pel-symbol-on-off-string 'enriched-mode nil nil "not loaded")
           (pel-symbol-on-off-string 'prettify-symbols-mode)
           (pel-symbol-on-off-string 'overwrite-mode)
           (pel-symbol-on-off-string 'delete-selection-mode nil nil "not loaded")
           (pel-symbol-on-off-string 'case-fold-search)
           (pel-symbol-on-off-string 'sort-fold-case nil nil "not loaded")
           (pel-symbol-on-off-string 'subword-mode)
           (pel-symbol-on-off-string 'superword-mode)
           (pel-symbol-on-off-string 'glasses-mode nil nil "not loaded")
           (pel-symbol-on-off-string 'visible-mode)
           (pel-symbol-on-off-string 'smart-dash-mode nil nil "not loaded")
           (pel--sentence-end-description)
           paragraph-start
           paragraph-separate))

;; -----------------------------------------------------------------------------
(provide 'pel-text-transform)

;;; pel-text-transform.el ends here
