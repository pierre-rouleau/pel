;;; pel-modeline.el --- Interpretation of Emacs Modeline  -*- lexical-binding: t; -*-

;; Created   : Friday, May  1 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-05-01 11:18:23 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2026  Pierre Rouleau
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
;; This provides a function that provide plain-English information about the
;; modeline, which is, for most people, a little cryptic, as it tries to
;; represent as much information in as little space as possible.
;;
;; The `pel-modeline-describe' command describes the first 8 characters of the
;; modeline.  It may not identify everything yet.  If you happen to see
;; something missing please let me know via the PEL GitHub project.
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--base)
;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-modeline-describe ()
  "Decodes the first set of characters at left of buffer modeline.
Print an detailed description of the characters representing the coding
system, line ending and modification status. "
  (interactive)
  (let* ((coding buffer-file-coding-system)
         (eol (coding-system-eol-type coding))
         (p1 (if enable-multibyte-characters "-  → multibyte capable" "n  → unibyte") )
         (p2 (if (and current-input-method-title
                      (> (length current-input-method-title) 0))
                 (format "%s → %s." current-input-method-title current-input-method)
               "U"))
         ;; Terminal output (usually U in modern UTF-8 terminals)
         (p4 (let ((base (coding-system-get coding :base)))
               (cond ((eq base 'no-conversion) "=")
                     ((eq base 'undecided) "-")
                     (t "U"))))
         (p5 (cond ((eq eol 0) ": (Unix/LF)")
                   ((eq eol 1) "\\ (DOS/CRLF)")
                   ((eq eol 2) "/ (Mac/CR)")
                   (t ":")))
         (p6 (if buffer-read-only "%" "-"))
         (p7 (if (buffer-modified-p) "*" "-"))
         (p8 (if (file-remote-p default-directory) "@" "-")))

    (if pel-emacs-is-graphic-p
        ;; In graphics mode
        (message "\
- (Multibyte):   %s (not shown in modeline)
- (Input Meth):  %s (U = None/UTF-8, otherwise show name abbreviation)
- (File Coding): %s (= is Raw, U is UTF-8)
- (EOL):         %s
- (Read-Only):   %s (%% is Read-Only, - is writable)
- (Modified):    %s (* is Modified,   - is unchanged)
- (Remote):      %s (@ is Remote,     - is local)"
                 p1 p2 p4 p5 p6 p7 p8)
      ;; in terminal mode
      (let ((p3 (terminal-coding-system)))
        (message "\
Pos 1 (Multibyte):   %s
Pos 2 (Input Meth):  %s (U = None/UTF-8, otherwise show name abbreviation)
Pos 3 (Terminal):    %s (U = UTF-8)
Pos 4 (File Coding): %s (= is Raw, U is UTF-8)
Pos 5 (EOL):         %s
Pos 6 (Read-Only):   %s (%% is Read-Only, - is writable)
Pos 7 (Modified):    %s (* is Modified,   - is unchanged)
Pos 8 (Remote):      %s (@ is Remote,     - is local)"
                 p1 p2 p3 p4 p5 p6 p7 p8)))))

;;; --------------------------------------------------------------------------
(provide 'pel-modeline)

;;; pel-modeline.el ends here
