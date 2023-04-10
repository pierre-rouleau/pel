;;; pel-man.el --- Man page support extension.  -*- lexical-binding: t; -*-

;; Created   : Sunday, February 19 2023.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2023-04-10 18:54:40 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2023  Pierre Rouleau
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
;; Extension to the man command by allowing the ability to select a default
;; manual section for a given major mode.  Useful for properly invoking the
;; man page of some systems, like TCL man pages which use a specific section
;; for their man pages.   The name of the section is controlled by a
;; major-mode-specific user option whose name is patterned after
;; "pel-%s-man-section" with the '%s' replaced by the name of the major mode.


;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--base)
(declare-function  Man-getpage-in-background "man")
(declare-function  Man-default-man-entry     "man")

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-man-at-point ()
  "Open Man page for text at point.  No prompt.

If the current major mode supports a section string via a user-option named
pel-MODE-man-section then that string is used to identify the Man section.
If there's no topic at point, invoke the man command interactively."
  (interactive)
  (require 'man)
  (let ((man-section (pel-major-mode-symbol-value-or "pel-%s-man-section" ""))
        (man-topic (if (and (require 'thingatpt nil :noerror)
                            (fboundp 'thing-at-point))
                       (thing-at-point 'symbol :no-properties)
                     (error "Function thing-at-point not loaded!"))))
    (if man-topic
        (Man-getpage-in-background (if (string= man-section "")
                                       man-topic
                                     (format "%s %s" man-section man-topic)))
      (call-interactively 'man))))

;;; --------------------------------------------------------------------------
(provide 'pel-man)

;;; pel-man.el ends here
