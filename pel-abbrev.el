;;; pel-abbrev.el --- Abbreviation and spell checking utilities.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, June  8 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-06-08 14:59:26, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021  Pierre Rouleau
;;
;; Credits: The content of this file is heavily based on © work
;;          by Artur Malabarba, available at the following location:
;;          http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
;;          The code is almost the same. I added and modified docstrings,
;;          modified names and argument and placed the code inside its own
;;          file.

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; This file provides an extension to spell checking with ispell: the function
;; `pel-ispell-word-then-abbrev' corrects the spelling of the previous spell
;; checking error and adds a conversion in the abbrev table.  This way, when
;; the `abbrev-mode' is active these typos will be corrected automatically.

;; Note that it is possible to achieve the same functionality without the code
;; here if you use flyspell and customize the `flyspell-abbrev-p' use-option
;; to t and optionally set `flyspell-use-global-abbrev-table-p' to t.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

(require 'ispell)                       ; use: ispell-get-word, ispell-word
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-current-or-previous--word ()
  "Return word at point or word before.
Return the word at point if point is on a word.
If word is between two words, return the previous word before point."
  (car-safe (save-excursion (ispell-get-word nil))))


;;-pel-autoload
(defun pel-ispell-word-then-abbrev (&optional locally)
  "Fix spelling mistake in text before point.
Create an `abbrev' abbreviation for it.
Store the abbreviation globally unless the LOCALLY argument is non-nil.

If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer.  You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (original-word corrected-word)
    (save-excursion
      (while (if (setq original-word (pel-current-or-previous--word))
                 (if (ispell-word nil 'quiet)
                     nil    ; stop loop when word was corrected or user quits.
                   ;; Also stop at beginning of buffer
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until beginning of buffer.
               (not (bobp)))
        ;; move point before previous word
        (backward-word)
        (backward-char))
      (setq corrected-word (pel-current-or-previous--word)))
    ;; If a word was corrected add the correction -> original
    ;; in the manual abbreviation list.
    (if (and corrected-word
             original-word
             (not (equal corrected-word original-word)))
        (let ((corrected-word (downcase corrected-word))
              (original-word (downcase original-word)))
          (define-abbrev
            (if locally local-abbrev-table global-abbrev-table)
            original-word corrected-word)
          (message "\"%s\" now expands to \"%s\" %sally"
                   original-word corrected-word (if locally "loc" "glob")))
      (user-error "No typo at or before point"))))


;;; --------------------------------------------------------------------------
(provide 'pel-abbrev)

;;; pel-abbrev.el ends here
