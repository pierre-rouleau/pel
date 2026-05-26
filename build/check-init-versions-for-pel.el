;;; check-init-versions-for-pel.el --- PEL pre-build check of early-init and init files  -*- lexical-binding: t; -*-

;; Created   : Tuesday, May 26 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-05-26 08:07:44 EDT, updated by Pierre Rouleau>

;; This file is part of the CHECK package.
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
;; Purpose   : Verify that the user's init.el and early-init.el (when present)
;;             carry the version numbers expected by this PEL release.
;; Called by : The PEL Makefile target `check-user-init-files', before any
;;             byte or native compilation.
;; Usage     : emacs --batch -Q -l build/check-init-versions-for-pel.el
;;             (the Makefile sets PEL_EXAMPLE_DIR and PEL_USER_EMACS_D via
;;             --eval "(setenv ...)" before loading this file.)
;; Notes     : - Does NOT load user or example files; scans them as text.
;;             - Therefore requires no external packages.
;;             - Reports all mismatches before exiting non-zero so the user
;;               sees every problem at once.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;; None: use standard Emacs code.

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;; ---------------------------------------------------------------------------
;;; Helper

(defun pel--check-init-extract-version (file varname)
  "Return the string value of a (defconst VARNAME \"...\") form in FILE.
Scans FILE as plain text without executing it.  Returns nil when FILE is
not readable or VARNAME is not found."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward
             (concat "(defconst\\s-+" (regexp-quote varname)
                     "\\s-+\"\\([^\"]+\\)\"")
             nil t)
        (match-string 1)))))

;;; ---------------------------------------------------------------------------
;;; Main check

(let* ((example-dir      (or (getenv "PEL_EXAMPLE_DIR")  "example/init"))
       (user-emacs-d     (or (getenv "PEL_USER_EMACS_D")
                             (expand-file-name "~/.emacs.d")))
       (example-init     (expand-file-name "init.el"       example-dir))
       (example-early    (expand-file-name "early-init.el" example-dir))
       (user-init        (expand-file-name "init.el"       user-emacs-d))
       (user-early-init  (expand-file-name "early-init.el" user-emacs-d))
       (errors           nil))

  ;; -- Check init.el
  (if (not (file-readable-p user-init))
      (push (format "Cannot read user init.el: %s" user-init) errors)
    (let ((expected (pel--check-init-extract-version example-init "pel-init-file-version"))
          (actual   (pel--check-init-extract-version user-init    "pel-init-file-version")))
      (cond
       ((null expected)
        (push (format "Cannot determine expected pel-init-file-version from %s."
                      example-init)
              errors))
       ((null actual)
        (push (format "%s does not define `pel-init-file-version'.\n\
  Update it from %s (expected version: \"%s\")."
                      user-init example-init expected)
              errors))
       ((not (string= actual expected))
        (push (format "init.el version mismatch:\n\
  Your version    : \"%s\"\n\
  Required version: \"%s\"\n\
  Please update   : %s\n\
  Using template  : %s"
                      actual expected user-init example-init)
              errors)))))

  ;; -- Check early-init.el (present only on Emacs >= 27)
  ;; Absence of the file is not an error: Emacs 26 users have no
  ;; early-init.el.
  ;;  If PEL users using Emacs >= 27 do not have an early-init.el and they
  ;;  try to activate quickstart mode PEL will detect that and can create the
  ;;  file for the user.
  (cond
   ;;
   ((and (file-exists-p user-early-init)
         (not (file-readable-p user-early-init)))
    (push (format "Cannot read user early-init.el: %s" user-early-init)
          errors))
   ;;
   ((file-readable-p user-early-init)
    (let ((expected (pel--check-init-extract-version example-early
                                                     "pel-early-init-file-version"))
          (actual   (pel--check-init-extract-version user-early-init
                                                     "pel-early-init-file-version")))
      (cond
       ((null expected)
        (push (format "Cannot determine expected pel-early-init-file-version from %s."
                      example-early)
              errors))
       ((null actual)
        (push (format "%s does not define `pel-early-init-file-version'.\n\
  Update it from %s (expected version: \"%s\")."
                      user-early-init example-early expected)
              errors))
       ((not (string= actual expected))
        (push (format "early-init.el version mismatch:\n\
  Your version    : \"%s\"\n\
  Required version: \"%s\"\n\
  Please update   : %s\n\
  Using template  : %s"
                      actual expected user-early-init example-early)
              errors))))))

  ;; ── Report and exit ───────────────────────────────────────────────────────
  (if errors
      (progn
        (message "\nPEL BUILD ERROR - Init file version check FAILED")
        (message   "=================================================")
        (dolist (e (nreverse errors))
          (message "\n* %s" e))
        (message "\nResolve the above issue(s) before rebuilding PEL.
  Please compare your version of the file(s) with the versions stored
  inside the example/init directory.\n")
        (kill-emacs 1))
    (message "PEL init file version check: OK.")))

;;; --------------------------------------------------------------------------
(provide 'check-init-versions-for-pel)

;;; check-init-versions-for-pel.el ends here
