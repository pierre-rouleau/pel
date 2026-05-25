;;; pel--process.el --- PEL Emacs Type Detection Logic  -*- lexical-binding: t; -*-

;; Created   : Monday, May 25 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-05-25 18:13:51 EDT, updated by Pierre Rouleau>

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
;; Defines a function to detect whether a GUI Emacs was launched directly from
;; the OS instead of a shell.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--options)
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defconst pel--known-shells
  '("bash" "zsh" "sh" "dash" "fish" "ksh" "tcsh" "csh" "ash" "mksh" "pdksh"
    "elvish" "nu" "ion" "yash" "rbash" "rzsh")
  "Common Unix shell executable names used by `pel-is-os-launched-gui-p'.")

(defun pel--linux-parent-process-name ()
  "Return the parent process executable name on Linux via /proc.
Reads /proc/self/status to obtain the PPid, then reads /proc/<ppid>/comm
for the executable name.  No subprocess is spawned; this is fast and
has no side-effects.  Returns nil if the information is unavailable."
  (let ((ppid
         (with-temp-buffer
           (insert-file-contents "/proc/self/status")
           (when (re-search-forward "^PPid:[ \t]+\\([0-9]+\\)" nil t)
             (match-string 1)))))
    (when ppid
      (let ((comm-file (format "/proc/%s/comm" ppid)))
        (when (file-readable-p comm-file)
          (with-temp-buffer
            (insert-file-contents comm-file)
            (string-trim (buffer-string))))))))

(defvar pel--is-os-launched-gui-p 'unset
  "Cached value for `pel-is-os-launched-gui-p' returned value.")

(defun pel-is-os-launched-gui-p ()
  "Predicate: t when Emacs is a GUI Emacs launched from the OS, not a shell.
Returns nil when Emacs is running in terminal mode, or when it was launched
from a shell (interactively or via a shell script such as bin/ec).

If `pel-emacs-gui-programs' is non-nil, the current executable path is
checked against it first; if it matches, t is returned without heuristics.

The computation is done once and its result is cached.  The cached value
returned in subsequent calls."
  (if (eq pel--is-os-launched-gui-p 'unset)
      (setq
       pel--is-os-launched-gui-p
       (when (display-graphic-p)
         (or
          ;; Stage 1 – user-configured executable paths (100 % reliable)
          (when pel-emacs-gui-programs
            (let ((exe (expand-file-name invocation-name invocation-directory)))
              (and (member exe pel-emacs-gui-programs) t)))
          ;; Stage 2 – OS/environment heuristics (fallback)
          (cond
           ;; Windows
           ((eq system-type 'windows-nt)
            ;; PROMPT    → active cmd.exe session
            ;; PSHOME    → active PowerShell session (session-scoped env var)
            ;; SHLVL     → Git Bash / MSYS2 / Cygwin
            ;; WT_SESSION → Windows Terminal (any shell inside it)
            (not (or (getenv "PROMPT")
                     (getenv "PSHOME")
                     (getenv "SHLVL")
                     (getenv "WT_SESSION"))))
           ;;
           ;; GNU/Linux — exact check via /proc (no subprocess, O(1) cost)
           ((eq system-type 'gnu/linux)
            (let ((parent (pel--linux-parent-process-name)))
              (if parent
                  ;; Definitive: true only when the parent is NOT a known shell
                  (not (member parent pel--known-shells))
                ;; /proc/<ppid>/comm unreadable (container / restricted namespace):
                ;; fall back to env-var heuristics
                (let ((shlvl (getenv "SHLVL")))
                  (and (or (null shlvl) (equal shlvl "0"))
                       (not (or (getenv "TERM_PROGRAM")
                                (getenv "COLORTERM")
                                (getenv "SSH_TTY"))))))))
           ;;
           ;; macOS, *BSD, and other Unix-like systems
           ;; A ps-based parent-process check would be exact but spawns a
           ;; subprocess at startup; env-var heuristics are used instead.
           ;; macOS launchd sets TERM=dumb for GUI apps launched from the desktop;
           ;; every real terminal emulator sets a meaningful value (xterm-256color,
           ;; alacritty, xterm-kitty, …).
           (t
            (let ((shlvl (getenv "SHLVL"))
                  (term  (getenv "TERM")))
              (and (or (null shlvl) (equal shlvl "0"))
                   (or (null term)  (equal term "dumb"))
                   (not (or (getenv "TERM_PROGRAM") ; Terminal.app, iTerm2, VSCode…
                            (getenv "COLORTERM") ; Alacritty, Kitty (true-colour)
                            (getenv "SSH_TTY"))))))))))
    pel--is-os-launched-gui-p))

;;; --------------------------------------------------------------------------
(provide 'pel--process)

;;; pel--process.el ends here
