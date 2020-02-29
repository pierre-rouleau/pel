;;; pel-scroll.el --- PEL Window Scrolling Utilities

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
;; Scrolling up & down without moving point
;; ----------------------------------------

;; - Implement dual-window lock scrolling:
;;   Activated by (pel-toggle-dual-scroll).


;;; Code:

(defvar pel-in-scroll-sync nil  "If t, scroll this and other window.")

;;-pel-autoload
(defun pel-toggle-dual-scroll ()
  "Toggle pel dual window scroll sync mode."
  (interactive)
  (setq pel-in-scroll-sync (not pel-in-scroll-sync))
  (if pel-in-scroll-sync
      (message "Enabled dual window scroll sync.")
    (message "Disabled dual window scroll sync.")))

;; --

(defun pel-scroll-both-up ()
  "Scroll up (ahead) 1 line current and other window: move text up."
  (interactive)
  (scroll-up 1)
  (scroll-other-window 1))

;;-pel-autoload
(defun pel-scroll-up (&optional n)
  "Scroll up 1 line: move text up (same direction as forward).
Scrolling up is bringing text ahead into view.
If N is specified it identifies a repetition count.
If N is negative it means the other direction."
  (interactive "P")
  (let ((n (prefix-numeric-value n)))
    (if (< n 0)
        (pel-scroll-down (abs n))
      (while (> n 0)
        (setq n (1- n))
        (if pel-in-scroll-sync
            (pel-scroll-both-up)
          (scroll-up 1))))))

;; --

(defun pel-scroll-both-down ()
  "Scroll down 1 line current and other window: move text down."
  (interactive)
  (scroll-down 1)
  (scroll-other-window -1))

;;-pel-autoload
(defun pel-scroll-down (&optional n)
  "Scroll down 1 line: move text down (same direction as backwards).
Scrolling down is bringing text behind into view.
If N is specified it identifies a repetition count.
If N is negative it means the other direction."
  (interactive "P")
  (let ((n (prefix-numeric-value n)))
    (if (< n 0)
        (pel-scroll-up (abs n))
      (while (> n 0)
        (setq n (1- n))
        (if pel-in-scroll-sync
            (pel-scroll-both-down)
          (scroll-down 1))))))

;; -----------------------------------------------------------------------------
(provide 'pel-scroll)

;;; pel-scroll.el ends here
