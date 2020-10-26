;;; pel-cc.el --- PEL support for CC modes.  -*- lexical-binding: t; -*-

;; Created   : Friday, October 23 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2020-10-26 12:11:19, updated by Pierre Rouleau>

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

(defvar-local pel-cc-newline-mode 'context-newline
  "Operation mode of the RET key in CC mode.
One of:
- context-newline       : use function `c-context-line-break'
- newline-and-indent    : use function `newline'
- just-newline-no-indent: use function `electric-indent-just-newline'")

;;-pel-autoload
(defun pel-cc-newline (&optional n)
  "Insert a newline and perhaps align.

With argument N repeat N times.

For newline insertion, operate according to the value of the
variable `pel-cc-newline-mode'.

If the variable `pel-newline-does-align' is t, then perform the
text alignment done by the function `align'.

For Emacs Lisp code: return value of effective variable
`pel-cc-newline-mode' used.  The function may use something
different then expected if the function it should call is not
loaded. That would only occur if this function is called from a
mode not fully supported."
  (interactive "*P")
  (pel-require 'align)
  (pel-require 'pel-align)
  (let ((separate (or (when (boundp 'align-region-separate)
                        (if (and (symbolp align-region-separate)
                                 (boundp align-region-separate))
                            (symbol-value align-region-separate)
                          align-region-separate))
		      'entire))
	(end      (point))
        (mode-used
         (cond ((and (eq pel-cc-newline-mode 'context-newline)
                     (fboundp 'c-context-line-break))
                (dotimes (_i (prefix-numeric-value n))
                  (c-context-line-break))
                'context-newline)
               ((and (eq pel-cc-newline-mode 'just-newline-no-indent)
                     (fboundp 'electric-indent-just-newline))
                (electric-indent-just-newline n)
                'just-newline-no-indent)
               (t ; simple.el is always loaded so default to newline
                (newline n :interactive)
                'newline))))
    (when (and (boundp 'pel-newline-does-align)
               pel-newline-does-align
               (fboundp 'align-new-section-p)
               (fboundp 'align))
      (save-excursion
        (forward-line -1)
        (while (not (or (bobp)
                        (align-new-section-p (point) end separate)))
          (forward-line -1))
        (align (point) end)))
    mode-used))

;;-pel-autoload
(defun pel-cc-change-newline-mode ()
  "Change behaviour of function `pel-cc-newline'.

Changes the mode to the next mode with the following order:
+-> context newline -> newline and indent -> just newline no indent ->+
|----------------------<----------------------------------------------|

Display and return the new value of the mode."
  (interactive)
  (prog1
      (setq pel-cc-newline-mode (cond ((eq pel-cc-newline-mode 'context-newline)
                                       'newline-and-indent)
                                      ((eq pel-cc-newline-mode 'newline-and-indent)
                                       'just-newline-no-indent)
                                      (t
                                       'context-newline)))
    (message "Return key now does: %S" pel-cc-newline-mode)))

;; --

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
- active style        : %s. c-default-style: %s
- RET mode            : %S%s
- Electric characters : %s
- Auto newline        : %s
- fill column         : %s
- Tab width           : %s, using %s
- Indent width        : %s%s
- Syntactic indent    : %s
- c-indentation-style : %s
- PEL Bracket style   : %s
- Comment style       : %s
- Hungry delete       : %s"
     major-mode                                 ; 1
     (if (boundp 'c-default-style)              ; 2
         (alist-get major-mode c-default-style)
       "Unknown - c-default-style not loaded")
     (pel-cc-c-default-style-for major-mode)    ; 3
     pel-cc-newline-mode                        ; 4
     (pel-symbol-on-off-string 'pel-newline-does-align
                               ", and aligns (comments, assignments, etc...)"
                               ""
                               "")              ; 5
     (pel-symbol-on-off-string 'c-electric-flag ; 6
                               (format "active on: %s"
                                       (pel-concat-strings-in-list
                                        (pel-cc-electric-keys)))
                               "inactive"
                               not-avail-msg)
     (pel-symbol-on-off-string 'c-auto-newline nil nil not-avail-msg) ; 7
     fill-column                                ; 8
     tab-width                                  ; 9
     (pel-on-off-string indent-tabs-mode        ; 10
                        "hard-tabs and spaces"
                        "spaces only")
     (pel-symbol-value-or 'c-basic-offset) ; 11
     (pel-symbol-on-off-string 'c-syntactic-indentation ; 12
                               ", using syntactic indentation"
                               ""
                               "")
     (pel-symbol-on-off-string                  ; 13
      'c-syntactic-indentation nil nil not-avail-msg)
     (pel-symbol-value-or 'c-indentation-style) ; 14
     (pel-cc-bracket-style-for major-mode)      ; 15
     (if (and (boundp 'c-block-comment-flag)    ; 16
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
     (pel-symbol-on-off-string 'c-hungry-delete-key ; 17
                               nil
                               "off, but the \
F11-⌦  and F11-⌫  keys are available."
                               not-avail-msg))))

;;; --------------------------------------------------------------------------
(provide 'pel-cc)

;;; pel-cc.el ends here
