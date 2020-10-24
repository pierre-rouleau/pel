;;; pel-cc.el --- PEL support for CC modes.  -*- lexical-binding: t; -*-

;; Created   : Friday, October 23 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2020-10-23 18:50:57, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020  Pierre Rouleau
;;
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
;;  This file contains utilities to help manage the various CC modes.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-cc-key-electric-p (key)
  "Return non-nil if KEY is electric, nil otherwise."
  ;; Work only with keys that may be electric.
  (local-key-binding key))

(defun pel-cc-filter-electric-key (char)
  "Return CHAR if it is electric, space otherwise."
  (if (pel-cc-key-electric-p (kbd char))
      char
    nil))

(defun pel-cc-electric-keys ()
  "Return a string with the electric keys."
  (seq-filter 'pel-cc-filter-electric-key
          (mapcar 'string "#*/<>(){}:;,")))

(defun pel-cc-c-default-style-for (mode)
  "Return styles identified in variable `c-default-style' for MODE.
Return a list of style strings in order of entry in the
variable `c-default-style' if bound.  If it is unbound
return \"void\"."
  (if (boundp 'c-default-style)
      (let ((styles '()))
        (dolist (mode.style c-default-style (reverse styles))
          (if (eq (car mode.style) mode)
              (push (cdr mode.style) styles))))
    "void"))

(defun pel-cc-bracket-style-for (mode)
  "Return the name of the PEL requested bracket style for MODE."
  (let* ((mode-str (substring (symbol-name mode) 0 -5)) ; strip trailing "-mode"
         (symbol-name (intern (format "pel-%s-bracket-style" mode-str))))
    (if (boundp symbol-name)
        (symbol-value symbol-name)
      (format "unknown - %s is void" symbol-name))))

;;-pel-autoload
(defun pel-cc-mode-info ()
  "Display information about current CC mode derivative."
  (interactive)
  (let ((not-avail-msg "not available for this mode"))
    (message
     "%s state:
- c-default-style     : %s
- active style        : %s
- Indent width        : %s
- Tab width           : %s
- Indenting with      : %s
- c-indentation-style : %s
- PEL Bracket style   : %s
- Comment style       : %s
- Electric chars      : %s
- Auto newline        : %s
- Syntactic indent    : %s
- Hungry delete       : %s"
     major-mode                                 ; 1
     (pel-cc-c-default-style-for major-mode)    ; 2
     (if (boundp 'c-default-style)              ; 3
         (alist-get major-mode c-default-style)
       "Unknown - c-default-style not loaded")
     (pel-symbol-on-off-string 'c-basic-offset) ; 4
     tab-width                                  ; 5
     (pel-on-off-string indent-tabs-mode        ; 6
                        "hard-tabs and spaces"
                        "spaces only")
     (pel-symbol-value-or 'c-indentation-style) ; 7
     (pel-cc-bracket-style-for major-mode)      ; 8
     (if (and (boundp 'c-block-comment-flag)    ; 9
              (boundp 'c-block-comment-starter)
              (boundp 'c-block-comment-ender)
              (boundp 'c-block-comment-prefix))
         (if c-block-comment-flag
             (format
              "Block comments: %s %s , continued line start with %s"
              c-block-comment-starter
              c-block-comment-ender
              c-block-comment-prefix)
           (format "Line comments: %s" (pel-symbol-value-or
                                        'c-line-comment-starter)))
       not-avail-msg)
     (pel-symbol-on-off-string 'c-electric-flag ; 10
                               (format "active: %s"
                                       (pel-concat-strings-in-list
                                        (pel-cc-electric-keys)))
                               "inactive"
                               not-avail-msg)
     (pel-symbol-on-off-string 'c-auto-newline nil nil not-avail-msg) ; 11
     (pel-symbol-on-off-string                                        ; 12
      'c-syntactic-indentation nil nil not-avail-msg)
     (pel-symbol-on-off-string 'c-hungry-delete-key                   ; 13
                               nil
                               "off, but the \
F11-⌦  and F11-⌫  keys are available."
                               not-avail-msg))))

;;; --------------------------------------------------------------------------
(provide 'pel-cc)

;;; pel-cc.el ends here
