;;; pel-modeline.el --- Interpretation of Emacs Modeline  -*- lexical-binding: t; -*-

;; Created   : Friday, May  1 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-05-01 16:30:19 EDT, updated by Pierre Rouleau>

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
;; The helper function `pel-emacsclient-description' returns nil when Emacs is
;; a normal process, and returns a string describing the emacs client type,
;; name and client count when Emacs is an Emacs daemon client.
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--base)
(require 'seq)         ; use: `seq-filter'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-emacsclient-description ()
  "Return a string describing the emacsclient status.
Return nil if Emacs is a normal Emacs frame (no server involved)."
  (let* ((e-type (frame-parameter nil 'client))
         (c-count (if (boundp 'server-clients)
                      (length server-clients)
                    0))
         (dp (daemonp))
         (c-name (if (stringp dp) dp "default")))
    (cond
     ;; nil: normal
     ((not e-type) nil)
     ;;
     ;;  nowait emacs daemon
     ((eq e-type 'nowait)
      ;; The c-count computed before is 0 for nowait servers.
      ;; Count open frames that non-nil client parameter instead:
      (setq c-count
            (length (seq-filter
                     (lambda (f)
                       (frame-parameter f 'client))
                     (frame-list))))
      (format "Emacs daemon%s with %s"
              (if c-name (format " %s" c-name) "?")
              (pel-count-string c-count "client")))
     ;;
     ;;  waiting emacs daemon
     ((eq e-type t)
      (format "Emacs daemon%s with %s"
              (if c-name (format " %s" c-name) "?")
              (pel-count-string c-count "client")))
     ;;
     ;; emacsclient process
     (t
      (format "emacsclient of %s daemon which has %s."
              (or c-name "")
              (pel-count-string c-count "client"))))))

;;-pel-autoload
(defun pel-modeline-describe ()
  "Decodes the first set of characters at left of buffer modeline.
Print an detailed description of the characters representing the coding
system, the input method, the Emacs process type (normal or daemon with client
count), the line ending format and the buffer modification status. "
  (interactive)
  (let* ((coding buffer-file-coding-system)
         (eol (coding-system-eol-type coding))
         (is-client (frame-parameter nil 'client))
         (p1 (if enable-multibyte-characters "-  → multibyte capable" "n  → unibyte") )
         (use-named-input-method (and current-input-method-title
                                      (> (length current-input-method-title) 0)))
         (p2 (if use-named-input-method
                 (format "%s → %s." current-input-method-title current-input-method)
               "U"))
         ;; Terminal output. Usually U in modern UTF-8 terminals, but its
         ;; different under Windows, so get the real value.
         (p4 (char-to-string (coding-system-get coding :mnemonic)))

         (p5 (cond ((eq eol 0) ": (Unix/LF)")
                   ((eq eol 1) "\\ (DOS/CRLF)")
                   ((eq eol 2) "/ (Mac/CR)")
                   (t ":")))
         (csetm (coding-system-eol-type-mnemonic coding))
         (p5a (if (and (eq eol 0) (equal csetm ":"))
                  ""
                (format ", %s" csetm)))
         (p6 (if buffer-read-only "%" "-"))
         (p7 (if (buffer-modified-p) "*" "-"))
         (p8 (if (file-remote-p default-directory) "@" "-"))
         (ept (pel-emacsclient-description))
         (emacs-process-type (if ept
                                 (format "@ %s" ept)
                               "Normal Emacs process")))
    (pel-print-in-buffer
     "*modeline-description*"
     "Description of the modeline information"
     (if (display-graphic-p)
         ;; In graphics mode
         (let* ((c2 (if use-named-input-method "- c1 " "     "))
                (c3 (if use-named-input-method 2 1))
                (c4 (1+ c3))
                (ce (if is-client (format "- c%d " (1+ c4))   "     "))
                (c6 (if is-client (+ c4 2) (1+ c4)))
                (c7 (1+ c6))
                (c8 (1+ c7)))
           (format "\
     (Multibyte):   %s
%s(Input Meth):  %s (U = None/UTF-8, otherwise show name abbreviation)
- c%d (File Coding): %s (= is Raw, U is UTF-8)
- c%d (EOL):         %s
%sEmacs process: %s
- c%d (Read-Only):   %s (%% is Read-Only, - is writable)
- c%d (Modified):    %s (* is Modified,   - is unchanged)
- c%d (Remote):      %s (@ is Remote,     - is local)"
                   p1
                   c2 p2
                   c3 p4
                   c4 (format "%s%s" p5 p5a)
                   ce emacs-process-type
                   c6 p6
                   c7 p7
                   c8 p8))
       ;; in terminal mode
       (let* ((p3 (terminal-coding-system))

              (ce (if is-client "- c6 "   "     "))
              (c6 (if is-client 7 6))
              (c7 (1+ c6))
              (c8 (1+ c7)))
         (format "\
- c1 (Multibyte):   %s
- c2 (Input Meth):  %s (U = None/UTF-8, otherwise show name abbreviation)
- c3 (Terminal):    %s (U = UTF-8)
- c4 (File Coding): %s (= is Raw, U is UTF-8)
- c5 (EOL):         %s
%sEmacs process: %s
- c%d (Read-Only):   %s (%% is Read-Only, - is writable)
- c%d (Modified):    %s (* is Modified,   - is unchanged)
- c%d (Remote):      %s (@ is Remote,     - is local)"
                 p1 p2 p3 p4
                 (format "%s%s" p5 p5a)
                 ce emacs-process-type
                 c6 p6
                 c7 p7
                 c8 p8)))
     'clear-buffer
     'use-help-mode
     'show-top)))

;;; --------------------------------------------------------------------------
(provide 'pel-modeline)

;;; pel-modeline.el ends here
