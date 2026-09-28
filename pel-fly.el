;;; pel-fly.el --- PEL support for Flymake and Flycheck.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, December 10 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-09-22 14:14:21 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2025, 2026  Pierre Rouleau
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
;; This file provides PEL integration with both Emacs built-in flymake and the
;; external flycheck package.  It provides dispatcher commands that use
;; commands from the currently selected syntax checker, providing a uniform
;; command interface to syntax checking that works with both systems.

;;  * `pel-fly-show-setup-info'
;;  * `pel-fly-toggle-engine'
;;    - `pel-fly--check-buffer-settings'
;;  * `pel-fly-toggle-syntax-check'
;;    - `pel-fly--check-buffer-settings'
;;  * `pel-fly-toggle-diag-at-eol'
;;    - `pel-fly--check-buffer-settings'
;;  * `pel-fly-list-diagnostics'
;;    - `pel-fly--check-buffer-settings'


;; The `pel-fly--check-buffer-settings' utility function is used by all
;; commands except `pel-fly-show-setup-info' which prints information related
;; to syntax checking.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)             ; use: `pel-symbol-text'
(require 'pel--options)          ; use: `pel-use-flycheck',
;;                               ;      `pel-use-flycheck-inline'
;;                               ;      `pel-toggle-mode-and-show'
(require 'flymake)
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defvar-local pel-fly--buffer-fly-engine nil
  "Identify the syntax check engine used for the buffer.
The values can be:
- nil     : use nothing.
- flymake : use `flymake-mode'.
- flycheck: use `flycheck-mode'.")

;;-pel-autoload
(defun pel-fly-show-setup-info (&optional append)
  "Print syntax check tools setup information in specialized buffer.
The buffer name is *pel-fly-info*.
If APPEND is non-nil, append information to the buffer, otherwise clear
it and display information from the top."
  (interactive "P")
  (let* ((pel-insert-symbol-content-context-buffer (current-buffer))
         (fly--buffer-fly-engine pel-fly--buffer-fly-engine)
         (buffer-flymake-mode  (bound-and-true-p flymake-mode))
         (buffer-flycheck-mode (bound-and-true-p flycheck-mode))
         (title (format "PEL setup for %s"
                        pel-insert-symbol-content-context-buffer)))
    (pel-print-in-buffer
     "*pel-fly-info*"
     title
     (lambda ()
       "Print syntax check setup control."
       (pel-insert-bold "* Flymake/Flycheck current status:")
       (insert (format "\n- Current buffer uses: %s"
                       (cond
                        ((null fly--buffer-fly-engine) "nothing")
                        ((eq fly--buffer-fly-engine 'flymake)
                         (propertize "Flymake" 'face 'bold))
                        ((eq fly--buffer-fly-engine 'flycheck)
                         (propertize "Flycheck" 'face 'bold))
                        (t (format "?: the variable is set to: %S"
                                   fly--buffer-fly-engine)))))
       (pel-insert-symbol-content-line 'flymake-mode)
       (pel-insert-symbol-content-line 'flycheck-mode)
       (when buffer-flymake-mode
         (pel-insert-symbol-content-line 'flymake-diagnostic-functions)
         (when pel-emacs-27-or-later-p
           (pel-insert-symbol-content-line 'flymake-proc-ignored-file-name-regexps))
         (when pel-emacs-30-or-later-p
           (pel-insert-symbol-content-line 'flymake-autoresize-margins))
         (pel-insert-symbol-content-line 'flymake-compilation-prevents-syntax-check)
         (pel-insert-symbol-content-line 'flymake-mode-hook)
         (pel-insert-symbol-content-line 'flymake-no-changes-timeout)
         (pel-insert-symbol-content-line 'flymake-proc-master-file-count-limit)
         (pel-insert-symbol-content-line 'flymake-start-on-flymake-mode)
         (when pel-emacs-30-or-later-p
           (pel-insert-symbol-content-line
            'flymake-show-diagnostics-at-end-of-line))
         (if (boundp 'flymake-collection-hook-config)
             (pel-insert-list-content 'flymake-collection-hook-config
                                      nil nil nil
                                      'on-same-line)
           (pel-insert-list-content 'flymake-collection-config
                                    nil nil nil
                                    'on-same-line))
         (pel-insert-list-content 'flymake-proc-allowed-file-name-masks
                                  nil nil nil
                                  'on-same-line))
       (when buffer-flycheck-mode
         (pel-insert-symbol-content-line 'flycheck-emacs-lisp-load-path)
         (pel-insert-symbol-content-line 'flycheck-inline-mode)
         (pel-insert-symbol-content-line 'flycheck-eglot-mode))

       (pel-insert-bold "\n\n* Flymake/Flycheck activation control:")
       (pel-insert-list-content 'pel-fly-engine-for-modes
                                nil nil nil
                                'on-same-line)
       (pel-insert-list-content 'pel-modes-activating-syntax-check
                                nil nil nil
                                'on-same-line)
       (pel-insert-list-content 'pel-files-activating-syntax-check
                                nil nil nil
                                'on-same-line)

       (pel-insert-bold "\n\n* Flymake tools activation control:")
       (pel-insert-symbol-content-line 'pel-use-flymake-collection)

       (pel-insert-bold "\n\n* Flycheck activation control:")
       (pel-insert-symbol-content-line 'pel-use-flycheck)
       (pel-insert-symbol-content-line 'pel-force-flycheck-emacs-lisp-load-path-inherit)
       (pel-insert-symbol-content-line 'pel-use-flycheck-eglot)
       (pel-insert-symbol-content-line 'pel-use-flycheck-inline)
       (pel-insert-symbol-content-line 'pel-use-flycheck-projectile)
       (pel-insert-bold "\n\n* Flycheck major mode tools activation control:")
       (pel-insert-symbol-content-line 'pel-use-flycheck-golangci-lint)
       (pel-insert-symbol-content-line 'pel-use-flycheck-odin)
       (pel-insert-symbol-content-line 'pel-use-flycheck-plantuml)
       (pel-insert-symbol-content-line 'pel-use-flycheck-rebar3)
       (pel-insert-symbol-content-line 'pel-use-flycheck-rust)
       (pel-insert-symbol-content-line 'pel-use-shellcheck))

     (unless append :clear-buffer)
     :use-help-mode)))

(defun pel-fly--check-buffer-settings ()
  "Check buffer settings.  Issue user error if invalid."
  ;; When eglot is active and `pel-use-flycheck-eglot' is on,
  ;; then eglot uses flycheck, so override any requested settings
  ;; and identify flycheck as being active.
  (when (and pel-use-flycheck-eglot
             (fboundp 'eglot-managed-p)
             (eglot-managed-p))
    (setq-local pel-fly--buffer-fly-engine 'flycheck))
  ;; If not set, check which one is required by customization.
  ;; Check `pel-fly-engine-for-modes'
  (unless pel-fly--buffer-fly-engine
    (setq-local pel-fly--buffer-fly-engine
                (car-safe
                 (cdr-safe
                  (assoc (intern (pel-file-type-for major-mode))
                         pel-fly-engine-for-modes)))))
  ;; When still not set, and flymake-vale is used, check TODO
  (unless pel-fly--buffer-fly-engine
    (user-error "\
No syntax check engine used in this buffer!
Check `pel-fly-engine-for-modes' & `pel-files-activating-syntax-check'")))


;;-pel-autoload
(defun pel-fly-toggle-engine ()
  "Toggle between using Flymake or Flycheck when one is used.
Refresh the '*pel-fly-info*' buffer if it is opened."
  (interactive)
  (pel-fly--check-buffer-settings)
  (cond
   ;; Switch flymake -> flycheck
   ((and (fboundp 'flycheck-mode)
         (eq pel-fly--buffer-fly-engine 'flymake))
    (flymake-mode -1)
    (flycheck-mode 1)
    (setq-local pel-fly--buffer-fly-engine 'flycheck)
    (message "Now using flycheck"))
   ;; Switch flycheck -> flymake
   ((and (fboundp 'flycheck-mode)
         (eq pel-fly--buffer-fly-engine 'flycheck))
    (flycheck-mode -1)
    (flymake-mode 1)
    (setq-local pel-fly--buffer-fly-engine 'flymake)
    (message "Now using flymake"))
   ;;
   (t (user-error "Command flycheck-mode not known.  Is it installed?")))
  ;; If the information buffer is opened; update it.
  (when (get-buffer "*pel-fly-info*")
    (pel-fly-show-setup-info)))

;;-pel-autoload
(defun pel-fly-toggle-syntax-check (&optional globally)
  "Toggle the current syntax check engine (flymake|flycheck) on/off.
It changes the buffer local state of the syntax check.
You can also toggle the global state of flycheck with the optional GLOBALLY
parameter.  That parameter is ignored for flymake.
Refresh the '*pel-fly-info*' buffer if it is opened."
  (interactive "P")
  (pel-fly--check-buffer-settings)
  (cond
   ;; Toggle flymake on/off
   ((eq pel-fly--buffer-fly-engine 'flymake)
    (pel-toggle-mode-and-show 'flymake-mode))
   ;; Toggle flycheck on/off
   ((and (fboundp 'flycheck-mode)
         (eq pel-fly--buffer-fly-engine 'flycheck))
    (if globally
        (pel-toggle-mode-and-show 'global-flycheck-mode)
      (pel-toggle-mode-and-show 'flycheck-mode)))
   ;;
   (t (user-error "Command flycheck-mode not known.  Is it installed?")))
  ;; If the information buffer is opened; update it.
  (when (get-buffer "*pel-fly-info*")
    (pel-fly-show-setup-info)))

;;-pel-autoload
(defun pel-fly-toggle-diag-at-eol (&optional only-error)
  "Toggle diagnostics display at end of line.
If optional ONLY-ERROR is specified affect only the display of errors when
activating otherwise activate display of all diagnostics at the end of line."
  (interactive "P")
  (pel-fly--check-buffer-settings)
  (cond
   ;; Toggle display of diagnostics at end of line with flymake.
   ((eq pel-fly--buffer-fly-engine 'flymake)
    (when (boundp 'flymake-show-diagnostics-at-end-of-line)
      (flymake-mode -1)
      (if flymake-show-diagnostics-at-end-of-line
          ;; Disable showing flymake diagnostics at end of line
          (progn
            (setq-local flymake-show-diagnostics-at-end-of-line nil)
            (message "Stop display of diagnostics at end of line."))
        ;; Activate showing flymake diagnostics at end of line
        (if only-error
            (progn
              (setq-local flymake-show-diagnostics-at-end-of-line 'short)
              (message "Display most severe diagnostics at end of line."))
          (setq-local flymake-show-diagnostics-at-end-of-line t)
          (message "Display all diagnostics at end of line.")))
      (flymake-mode 1))
    )
   ;; Toggle display of diagnostics at end of line with flycheck
   ;; - toggle the flycheck-inline-mode
   ((and (fboundp 'flycheck-mode)
         (eq pel-fly--buffer-fly-engine 'flycheck))
    (if (fboundp 'flycheck-inline-mode)
        (progn
          (flycheck-inline-mode 'toggle)
          (message (pel-symbol-text 'flycheck-inline-mode)))
      (user-error "Command flycheck-inline-mode not bound.  Is it installed?
Turn `pel-use-flycheck-inline' and restart Emacs: PEL will install it!")))
   ;;
   (t (user-error "Command flycheck-mode not known.  Is it installed?")))
  (when (get-buffer "*pel-fly-info*")
    (pel-fly-show-setup-info)))

;;-pel-autoload
(defun pel-fly-list-diagnostics (&optional all-files)
  "List all syntax diagnostics in a specialized buffer.
If ALL-FILES optional prefix used, list diagnostics of all project files."
  (interactive "P")
  (pel-fly--check-buffer-settings)
  (cond
   ;; When using flymake
   ;; [:todo 2025-12-11, by Pierre Rouleau: Add better support for old Emacs versions]
   ((eq pel-fly--buffer-fly-engine 'flymake)
    (if all-files
        (when (fboundp 'flymake-show-project-diagnostics)
          (flymake-show-project-diagnostics))
      (when (fboundp 'flymake-show-buffer-diagnostics)
        (flymake-show-buffer-diagnostics))))
   ;; When using flycheck
   ((and (fboundp 'flycheck-mode)
         (eq pel-fly--buffer-fly-engine 'flycheck))
    (if (and all-files
             pel-use-flycheck-projectile
             (fboundp 'flycheck-projectile-list-errors))
        (flycheck-projectile-list-errors)
      (when (fboundp 'flycheck-list-errors)
        (flycheck-list-errors))))
   ;;
   (t (user-error "Command flycheck-mode not known.  Is it installed?"))))

;;; --------------------------------------------------------------------------
(provide 'pel-fly)

;;; pel-fly.el ends here
