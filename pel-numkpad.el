;;; pel-numkpad.el --- PEL Numeric Keypad Key Control -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2021, 2022, 2024  Pierre Rouleau

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
;; Dynamic control of the keypad keys to allow both numlocking and non
;; numlocking use.

;; Note: when using Emacs inside a macOS Terminal application, running
;; `emacs -nw`, the key labelled 'clear' which is labelled 'Numlock' in my
;; keypad layout below will effectively behave as a numlock key, but does not
;; register as a key inside Emacs.
;;
;;; --------------------------------------------------------------------------
;;; Dependencies:
(require 'pel--base)      ; use pel-toggle
;; pel-scroll functions are only needed if the pel-3 or pel-9 commands
;; are used.  Avoid loading pel-scroll until those commands are used.
(eval-when-compile
  (require 'pel-scroll))  ; use: pel-scroll-up, pel-scroll-down

;;; --------------------------------------------------------------------------
;;; Code:

(defvar pel-mac-keypad-numlocked nil
  "Identify whether macOS numeric keypad is used as numlocked or not.

If t, macOS keypad is used as the original numlocked keypad,
otherwise, it's a cursor centric keypad.  By default, the value
is t to identify the native behaviour: NumLocked.

This is valid for:
 - macOS when Emacs is running in graphics mode,
 - Other OS that support a NumLock key.
When using terminal Emacs under macOS, or Linux under a macOS hosted VM,
the NumLock key does not exists.  Terminal.app can be configured to
map the <clear> key to NumLock but Emacs only sees the impact to the
keypad keys; it does not see the <clear> key events.

The following keys are used in each mode:

+==============================+==========================+
|  Not NumLocked               | NumLocked                |
+==============================+==========================+
|NumLock  =      /      *      |Numlock  =     /    *     |
|home     up     ScrlUp -      |   7     8     9    -     |
|right    center right  +      |   4     5     6    +     |
|End      down   ScrlDn Enter  |   1     2     3   Enter  |
|---insert----   Del    Enter  |   ---0---     .   Enter  |
+==============================+==========================+

where the keys for '/', '*', '-' and '+' have their natural
meaning in Numlock mode but have other meanings in Non Numlocked
mode.

Use `pel-toggle-mac-numlock' to activate/deactivate numlock on
environments that can't bind the clear key such as macOS
terminal.")


;;-pel-autoload
(defun pel-show-mac-numlock ()
  "Display state of PEL Keypad num-lock mode.

In macOS TTY mode, the function shows an error as Emacs does not
receive NumLock keys events for the <clear> key used as NumLock.

BUG: If running Linux in a VM hosted on macOS, the code does not
     detect macOS and thinks it receives NumLock notification, allowing
     the function to display potentially invalid information."
  (interactive)
  (if (and pel-system-is-macos-p
           pel-emacs-is-a-tty-p)
      (user-error
       "NumLock state unknown: can't detect <clear> key events in macOS TTY")
    (if pel-mac-keypad-numlocked
        (message "Num-Lock ON")
      (message "Num-Lock off; cursor mode available"))))

;;-pel-autoload
(defun pel-toggle-mac-numlock ()
  "Toggle PEL numlock mode.

Nothing done in macOS TTY.

BUG: If running Linux in a VM hosted on macOS, the code does not
     detect macOS and thinks it receives NumLock notification, allowing
     the function to proceed which may cause out-of-sync status and
     prevent proper handling of the NumPad keys."
  (interactive)
  (if (and pel-system-is-macos-p
           pel-emacs-is-a-tty-p)
      (user-error
       "Can't toggle NumLock on macOS TTY: use <clear> & switch blindly.")
      (pel-toggle 'pel-mac-keypad-numlocked)
    (pel-show-mac-numlock)) )

;;-pel-autoload
(defun pel-0 (&optional n)
  "Keypad 0 key handler, with N as numeric prefix to support repetition."
  (interactive "*P")
  (if pel-mac-keypad-numlocked
      (insert-char ?0 (abs (prefix-numeric-value n)) t)
    (yank)))

;;-pel-autoload
(defun pel-1 (&optional n)
  "Keypad 1 key handler, with N as numeric prefix to support repetition."
  (interactive "P")
  (if pel-mac-keypad-numlocked
      (insert-char ?1 (abs (prefix-numeric-value n)) t)
    (if (and (require 'pel-navigate nil :no-error)
             (fboundp 'pel-end))
        (pel-end)
      (error "Function pel-end is not loaded"))))

;;-pel-autoload
(defun pel-2 (&optional n)
  "Keypad 2 key handler, with N as numeric prefix to support repetition."
  (interactive "P")
  (let ((n (prefix-numeric-value n)))
    (if pel-mac-keypad-numlocked
        (insert-char ?2 (abs n) t)
      (forward-line n))))

;;-pel-autoload
(defun pel-3 (&optional n)
  "Keypad 3 key handler, with N as numeric prefix to support repetition."
  (interactive "P")
  (let ((n (prefix-numeric-value n)))
    (if pel-mac-keypad-numlocked
        (insert-char ?3 (abs n) t)
      (if (require 'pel-scroll nil :no-error)
          (if (> 0)
              (when (fboundp 'pel-scroll-up)
                (pel-scroll-up  n))
            (when (fboundp 'pel-scroll-down)
              (pel-scroll-down (abs n))))
        (error "File not loaded: pel-scroll")))))

;;-pel-autoload
(defun pel-4 (&optional n)
  "Keypad 4 key handler, with N as numeric prefix to support repetition."
  (interactive "P")
  (let ((n (prefix-numeric-value n)))
    (if pel-mac-keypad-numlocked
        (insert-char ?4 (abs n) t)
      (left-char n))))

;;-pel-autoload
(defun pel-5 (&optional n)
  "Keypad 5 key handler, with N as numeric prefix to support repetition."
  (interactive "P")
  (let ((n (prefix-numeric-value n)))
    (if pel-mac-keypad-numlocked
        (insert-char ?5 (abs n) t)
      (recenter-top-bottom))))

;;-pel-autoload
(defun pel-6 (&optional n)
  "Keypad 6 key handler, with N as numeric prefix to support repetition."
  (interactive "P")
  (let ((n (prefix-numeric-value n)))
    (if pel-mac-keypad-numlocked
        (insert-char ?6 (abs n) t)
      (right-char n))))

;;-pel-autoload
(defun pel-7 (&optional n)
  "Keypad 7 key handler, with N as numeric prefix to support repetition."
  (interactive "P")
  (let ((n (prefix-numeric-value n)))
    (if pel-mac-keypad-numlocked
        (insert-char ?7 (abs n) t)
      (if (and (require 'pel-navigate nil :no-error)
               (fboundp 'pel-home))
          (pel-home)
        (error "Function pel-home is not loaded")))))

;;-pel-autoload
(defun pel-8 (&optional n)
  "Keypad 8 key handler, with N as numeric prefix to support repetition."
  (interactive "P")
  (let ((n (prefix-numeric-value n)))
    (if pel-mac-keypad-numlocked
        (insert-char ?8 (abs n) t)
      (forward-line (- n)))))

;;-pel-autoload
(defun pel-9 (&optional n)
  "Keypad 9 key handler, with N as numeric prefix to support repetition."
  (interactive "P")
  (let ((n (prefix-numeric-value n)))
    (if pel-mac-keypad-numlocked
        (insert-char ?9 (abs n) t)
      (if (require 'pel-scroll nil :no-error)
          (if (> 0)
              (when (fboundp 'pel-scroll-down)
                (pel-scroll-down n))
            (when (fboundp 'pel-scroll-up)
              (pel-scroll-up (abs n))))
        (error "File not loaded: pel-scroll")))))

;;-pel-autoload
(defun pel-kp-decimal (&optional n)
  "Keypad '.' key handler, with N as numeric prefix to support repetition."
  (interactive "P")
  (let ((n (prefix-numeric-value n)))
    (if pel-mac-keypad-numlocked
        (insert-char ?. (abs n) t)
      (delete-char n))))

;;-pel-autoload
(defun pel-kp-subtract (&optional n)
  "Keypad '-' key handler, with N as numeric prefix to support repetition."
  (interactive "*P")
  (let ((n (prefix-numeric-value n)))
    (if pel-mac-keypad-numlocked
        (insert-char ?- (abs n) t)
      (if (and (require 'pel-ccp nil :no-error)
               (fboundp 'pel-kill-or-delete-marked-or-whole-line))
          (pel-kill-or-delete-marked-or-whole-line n)
        (error
         "Function pel-kill-or-delete-marked-or-whole-line not loaded")))))

;;-pel-autoload
(defun pel-kp-add (&optional n)
  "Keypad '+' key handler, with N as numeric prefix to support repetition."
  (interactive "P")
  (let ((n (prefix-numeric-value n)))
    (if pel-mac-keypad-numlocked
        (insert-char ?+ (abs n) t)
      (if (and (require 'pel-ccp nil :no-error)
               (fboundp 'pel-copy-marked-or-whole-line))
          (pel-copy-marked-or-whole-line)))))

;;; --------------------------------------------------------------------------
(provide 'pel-numkpad)

;;; pel-numkpad.el ends here
