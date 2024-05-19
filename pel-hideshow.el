;;; pel-hideshow.el --- PEL Hide/Show -*- lexical-binding: t -*-

;; Copyright (C) 2020, 2024  Pierre Rouleau

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

;;; Credits:
;;   Several ideas came from the source of hideshow.el.
;;   I wanted to use a single key to toggle hiding/showing all blocks
;;   and that was already proposed by Joseph Eydelnant who has the idea
;;   a little more than 19 years before (I wish I had been using Emacs then).
;;   Unfortunately his code is not yet in hideshow.el and I don't want to
;;   modify that code, so I implemented something similar here.
;;

;;; Commentary:
;;
;; This is just a simple collection of functions that complement the hideshow
;; functionality.  Several of these functions activate the minor mode if it is
;; not active.

;;; Code:
(require 'hideshow)
(require 'pel--base)                    ; uses: pel-toggle, pel-dec, pel-inc


(defvar pel--hs-all-hidden nil
  "Current state of hiding all blocks.
Value is t when hiding all blocks, nil otherwise.
LIMITATION:
- No attempt is made by pel-hideshow.el to ensure that
  this variable is set to nil when function `hs-minor-mode' is called.
  The impact is negligible: in the worst case the value becomes out of sync with
  the real state and it will take 2 toggle commands to change the state of
  hiding.")

(make-variable-buffer-local 'pel--hs-all-hidden)

(defvar pel--hs-hide-relative-block nil
  "Number of blocks to hide below current block level.")

(make-variable-buffer-local 'pel--hs-hide-relative-block)


;;-pel-autoload
(defun pel-show-hide-state ()
  "Display state of pel-hideshow in current buffer."
  (interactive)
  (message "All blocks: %s.%s"
           (if pel--hs-all-hidden "hidden" "shown")
           (if pel--hs-hide-relative-block
               (format " Hide blocks %d level deeper than current."
                       pel--hs-hide-relative-block)
             "")))


(defun pel--activate-hideshow-mode (&optional use-relative-block)
  "Activate the Hide/Show minor mode if it is not already active."
  (unless use-relative-block
    (setq pel--hs-hide-relative-block nil))
  (unless hs-minor-mode
    (setq pel--hs-all-hidden nil)
    (hs-minor-mode)))

;;-pel-autoload
(defun pel-toggle-hide-all ()
  "Toggle hiding of all blocks.
Activate Hide/Show minor mode if not already active."
  (interactive)
  (pel--activate-hideshow-mode)
  (if (pel-toggle 'pel--hs-all-hidden)
      (hs-hide-all)
    (hs-show-all)))

;;-pel-autoload
(defun pel-toggle-hide-block ()
  "Toggle hiding of current block.
Activate Hide/Show minor mode if not already active."
  (interactive)
  (pel--activate-hideshow-mode)
  (hs-toggle-hiding))

;;-pel-autoload
(defun pel-hide-block (&optional end)
  "Select a block and hide it.
With prefix arg, reposition to END.
Activate Hide/Show minor mode if not already active."
  (interactive "P")
  (pel--activate-hideshow-mode)
  (hs-hide-block end))

;;-pel-autoload
(defun pel-show-block (&optional end)
  "Select a block and show it.
With prefix arg, reposition to END.
Activate Hide/Show minor mode if not already active."
  (interactive "P")
  (pel--activate-hideshow-mode)
  (hs-show-block end))

;;-pel-autoload
(defun pel-hide-all ()
  "Hide all top level blocks, displaying only first and last lines.
Activate Hide/Show minor mode if not already active."
  (interactive)
  (pel--activate-hideshow-mode)
  (hs-hide-all)
  (setq pel--hs-all-hidden t))

;;-pel-autoload
(defun pel-show-all ()
  "Show all blocks.
Activate Hide/Show minor mode if not already active."
  (interactive)
  (pel--activate-hideshow-mode)
  (hs-show-all)
  (setq pel--hs-all-hidden nil))

;;-pel-autoload
(defun pel-hide-level-1 ()
  "Hide all blocks 1 level below the current block.
Activate Hide/Show minor mode if not already active."
  (interactive)
  (pel--activate-hideshow-mode)
  (hs-hide-level 1))

;;-pel-autoload
(defun pel-hide-level-2 ()
  "Hide all blocks 2 levels below the current block.
Activate Hide/Show minor mode if not already active."
  (interactive)
  (pel--activate-hideshow-mode)
  (hs-hide-level 2))

;;-pel-autoload
(defun pel-hide-level-3 ()
  "Hide all blocks 3 levels below the current block.
Activate Hide/Show minor mode if not already active."
  (interactive)
  (pel--activate-hideshow-mode)
  (hs-hide-level 3))

;;-pel-autoload
(defun pel-hide-level-4 ()
  "Hide all blocks 4 levels below the current block.
Activate Hide/Show minor mode if not already active."
  (interactive)
  (pel--activate-hideshow-mode)
  (hs-hide-level 4))

;;-pel-autoload
(defun pel-hs-hide-block-below-inc ()
  "Hide one more block level below this block."
  (interactive)
  (pel--activate-hideshow-mode :use-relative-block)
  (unless pel--hs-hide-relative-block
    (setq pel--hs-hide-relative-block 1))
  (if (pel-inc 'pel--hs-hide-relative-block 10)
      (progn
        (hs-hide-level pel--hs-hide-relative-block)
        (message "Hiding level %d below current level" pel--hs-hide-relative-block))
    (message "Nested block limit of 10 reached")
    (beep)))

;;-pel-autoload
(defun pel-hs-hide-block-below-dec ()
  "Hide one less block level below this block."
  (interactive)
  (pel--activate-hideshow-mode :use-relative-block)
  (if pel--hs-hide-relative-block
    (if (pel-dec 'pel--hs-hide-relative-block 1 1)
        (progn
          (hs-hide-level pel--hs-hide-relative-block)
          (message "Hiding level %d below current level" pel--hs-hide-relative-block))
      (setq pel--hs-hide-relative-block nil))
    (message "All blocks below current are shown")
    (beep)))

;; -----------------------------------------------------------------------------
;; Column-based Selective display
;; ------------------------------

(defun pel--set-vline (column)
  "Set the vertical line position when vline-mode is available."
  (when (bound-and-true-p vline-mode)
    (move-to-column (max 0 (- column 1)))))

;;-pel-autoload
(defun pel-selective-display-column-inc (n)
  "Increment variable `selective-display' by N (defaults to 1).
This hides text indented by that many columns."
  (interactive "p")
    (let ((sd-value (or selective-display 1)))
      (setq sd-value (+ sd-value (or n 1)))
      (set-selective-display sd-value)
      (pel--set-vline sd-value)))

;;-pel-autoload
(defun pel-selective-display-column-dec (n)
  "Decrement variable `selective-display' by N (defaults to 1).
This hides text indented by that many columns."
  (interactive "p")
  (let ((sd-value selective-display))
    (when sd-value
      (setq sd-value (pel-dec sd-value (or n 1) 0)))
    (set-selective-display sd-value)
    (pel--set-vline sd-value)))


(defun pel--indent-size ()
  "Return the indentation size used by the current buffer if any, 1 otherwise."
  ;; for bracket-type programming languages: use c-basic-offset
  (if (boundp 'c-basic-offset)
      c-basic-offset
    1))

;;-pel-autoload
(defun pel-selective-display-indent-inc (n)
  "Increment variable `selective-display' by N indentation levels.
N defaults to 1.  If indentation level is not known, use N columns.
This hides text indented a the new value of variable `selective-display'."
  (interactive "p")
  (let ((indent-columns (pel--indent-size))
        (sd-value       (or selective-display 1)))
    (setq sd-value (+ sd-value (* (or n 1) indent-columns)))
    (set-selective-display sd-value)
    (pel--set-vline sd-value)))

;;-pel-autoload
(defun pel-selective-display-indent-dec (n)
    "Decrement variable `selective-display' by N indentation levels.
N defaults to 1.  If indentation level is not known, use N columns.
This hides text indented a the new value of variable `selective-display'."
  (interactive "p")
  (let ((indent-columns (pel--indent-size))
        (sd-value       (or selective-display 0)))
    (when sd-value
      (setq sd-value (pel-dec sd-value (* (or n 1) indent-columns) 0))
      (set-selective-display sd-value)
      (pel--set-vline sd-value))))

;;-pel-autoload
(defun pel-selective-display-unhide ()
  "Selective display un-hide column."
  (interactive)
  (set-selective-display nil))

;;-pel-autoload
(defun pel-selective-display-at-1 ()
  "Selective display hide at column 1."
  (interactive)
  (set-selective-display 1))

;; ---------------------------------------------------------------------------
;; Hide indented lines only
;; ------------------------

;;-pel-autoload
(defun pel-toggle-hide-indent ()
  "Toggle hiding lines more indented than current line."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (set-selective-display
     (if selective-display nil (+ 1 (current-column))))))

;; -----------------------------------------------------------------------------
(provide 'pel-hideshow)

;;; pel-hideshow.el ends here
