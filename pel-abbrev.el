;;; pel-abbrev.el --- Abbreviation and spell checking utilities.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, June  8 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2023-02-05 11:41:56 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2023  Pierre Rouleau
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

(require 'pel--base)
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
looking for a typo until the beginning of.  You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (original-word corrected-word)
    (save-excursion
      (while (if (setq original-word (pel-current-or-previous--word))
                 (if (ispell-word nil 'quiet)
                     nil    ; stop loop when word was corrected or user quits.
                   ;; Also stop at beginning of
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until beginning of.
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

;;-pel-autoload
(defun pel-abbrev-info ()
  "Display current status of abbreviation control."
  (interactive)
  (let ((pel-insert-symbol-content-context-buffer (current-buffer)))
    (pel-print-in-buffer
     "*pel-abbrev-info*"
     "Abbreviation control"
     (lambda ()
       "Print abbreviation control variables."
       (pel-insert-symbol-content-line 'abbrev-mode)
       (pel-insert-symbol-content-line 'save-abbrevs)
       (insert (format "\n\n* Dynamic abbreviation done with %s as specified by:"
                       (if (bound-and-true-p pel-use-hippie-expand)
                           "Hippie Expand"
                         "DAbbrev Mode")))
       (pel-insert-symbol-content-line 'pel-use-hippie-expand)
       (pel-insert-symbol-content-line 'pel-modes-activating-abbrev-mode)
       (pel-insert-symbol-content-line 'pel-use-origami)
       (when (bound-and-true-p pel-use-origami)
         (insert "
   - NOTE: When origami-mode is active PEL binds
           the abbreviation command to ")
         (insert (propertize "M-/ M-/" 'face 'bold))
         (insert " instead of the standard ")
         (insert (propertize "M-/" 'face 'bold)))
       (insert "\n\n* Abbreviations are stored in the file specified by:")
       (pel-insert-symbol-content-line 'abbrev-file-name)
       (pel-insert-symbol-content-line 'abbrev-table-name-list)
       (insert "\n\n* Spell Checking:")
       (pel-insert-symbol-content-line 'flyspell-auto-correct-word)
       (pel-insert-symbol-content-line 'flyspell-abbrev-p)
       (pel-insert-symbol-content-line 'flyspell-use-global-abbrev-table-p))
     :clear-buffer
     :use-help-mode)))
;;; --------------------------------------------------------------------------
(provide 'pel-abbrev)

;;; pel-abbrev.el ends here
