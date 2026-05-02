;;; pel-modeline.el --- Interpretation of Emacs Modeline  -*- lexical-binding: t; -*-

;; Created   : Friday, May  1 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-05-02 11:25:06 EDT, updated by Pierre Rouleau>

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
(defun pel-modeline-describe (&optional append)
  "Print a detailed description on modeline left-most information.

Print the information inside a help-mode buffer. Clear the help buffer unless
the APPEND argument is specified.

Print a detailed description of the characters representing the coding
system, the input method, the Emacs process type (normal or daemon with client
count), the line ending format, the buffer modification status, the remote
file access state  and the window dedication state."
  (interactive "P")
  (let* ((coding buffer-file-coding-system)
         (eol (coding-system-eol-type coding))
         (is-client (frame-parameter nil 'client))
         (p1 (if enable-multibyte-characters
                 "-   ⇶ multibyte capable."
               "n   ⇶ unibyte.") )
         (use-named-input-method (and
                                  current-input-method-title
                                  (> (length current-input-method-title) 0)))
         (p2 (format
              "%-3s ⇶ %s.  (U → None/UTF-8, or its name)"
              (if use-named-input-method current-input-method-title "U")
              (if use-named-input-method current-input-method "None/UTF-8")))
         ;; Terminal output. Usually U in modern UTF-8 terminals, but its
         ;; different under Windows, so get the real value.
         (p4
          (format "%s   ⇶ %s. (U → UTF-8, = → raw, D → DOS cpNNN)"
                  (char-to-string (coding-system-get coding :mnemonic))
                  coding))

         (p5-char (cond ((eq eol 0) (format "%s" eol-mnemonic-unix))
                        ((eq eol 1) (format "%s" eol-mnemonic-dos))
                        ((eq eol 2) (format "%s" eol-mnemonic-mac))
                        (t          (format "%s" eol-mnemonic-undecided))))
         (p5 (format "%s   ⇶ %s." p5-char
                     (cond ((eq eol 0) "Unix/LF")
                           ((eq eol 1) "DOS/CRLF")
                           ((eq eol 2) "Mac/CR")
                           (t          "Undecided"))))

         (p67 (cond ((and buffer-read-only (buffer-modified-p))
                     "%*  ⇶ read-only but externally modified.")
                    (buffer-read-only
                     "%%  ⇶ read-only.")
                    ((buffer-modified-p)
                     "**  ⇶ modified and writable.")
                    (t
                     "--  ⇶ unchanged and writable.")))
         (p8 (format "%s   ⇶ %s.  (@ → remote, - → local)"
                     (if (file-remote-p default-directory) "@" "-")
                     (if (file-remote-p default-directory) "remote" "local")))
         (ept (pel-emacsclient-description))
         (emacs-process-type (if ept
                                 (format "@   ⇶ %s" ept)
                               "    ⇶ Normal Emacs process"))
         (ded (window-dedicated-p))
         (p-ded (cond ((eq ded t) "D   ⇶ window strongly dedicated to buffer")
                      (ded        "d   ⇶ window weakly dedicated to buffer")
                      (t          nil))) ; nil = not dedicated
         (last-c 0)
         (description nil))
    (setq description
          (if (display-graphic-p)
              ;; In graphics mode
              (let* ((c2 (if use-named-input-method "- c1 " "     "))
                     (c3 (if use-named-input-method 2 1))
                     (c4 (1+ c3))
                     (ce (if is-client (format "- c%d " (1+ c4))   "     "))
                     (c6 (if is-client (+ c4 2) (1+ c4)))
                     (c7 (1+ c6))
                     (c8 (1+ c7)))
                (setq last-c c8)
                (format "\
     (Multibyte):   %s
%s(Input Meth):  %s
- c%d (File Coding): %s
- c%d (EOL):         %s
%sEmacs process: %s
- c%d/%d (Mod state): %s
- c%d (File System): %s"
                        p1
                        c2 p2
                        c3 p4
                        c4 p5
                        ce emacs-process-type
                        c6 c7 p67
                        c8 p8))
            ;; in terminal mode
            (let* ((p3 (format "%s   ⇶ %s.  (U → UTF-8, = → raw, D → DOS cpNNN)"
                               (char-to-string
                                (coding-system-get
                                 (terminal-coding-system) :mnemonic))
                               (terminal-coding-system)))
                   (ce (if is-client "- c6 "   "     "))
                   (c6 (if is-client 7 6))
                   (c7 (1+ c6))
                   (c8 (1+ c7)))
              (setq last-c c8)
              (format "\
- c1 (Multibyte):   %s
- c2 (Input Meth):  %s
- c3 (Trm. coding): %s
- c4 (File Coding): %s
- c5 (EOL):         %s
%sEmacs process: %s
- c%d/%d (Mod state): %s
- c%d (File System): %s"
                      p1
                      p2
                      p3
                      p4
                      p5
                      ce emacs-process-type
                      c6 c7 p67
                      c8 p8))))
    (when p-ded
      (setq description (concat description
                                (format "\n- c%d Window Ded.:   %s"
                                        (1+ last-c)
                                        p-ded))))
    (pel-print-in-buffer
     "*modeline-description*"
     "Description of the modeline information"
     description
     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-modeline)

;;; pel-modeline.el ends here
