;;; pel-dart.el --- PEL Dart support extension.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, November  4 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-04-13 22:30:47 EDT, updated by Pierre Rouleau>

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
;; Support for the Dart programming language.
;;
;; Two modes support Dart: the classic `dart-mode' and the `dart-ts-mode'.
;;
;; * `dart-mode':
;;   - Unfortunately uses `tab-width' to identify the indentation width,
;;     and forces it to 2.  It does not define a user-option for the indent
;;     width.
;;   - Forces `indent-tabs-mode' to nil to disable the use of hard-tabs.
;;   - Forces `fill-column' to 80
;;   - Only support // style comment, even though /* */ style is supported.
;;
;; * `indent-tabs-mode':
;;   - Indentation width is controlled by `dart-ts-mode-indent-offset'
;;     user-option which defaults to 2.
;;

;; Dart takes a very strong approach to indentation, forcing indentation to be
;; 2 spaces, always.  Dart tools force this convention.
;;
;; Several people complained about the lack of flexibility of this approach
;; that causes them personal pain as well as causing problem in team
;; environments.
;;
;; See:
;;  - https://github.com/dart-lang/dart_style/issues/534
;;  - https://github.com/dart-lang/dart_style/issues/1683
;;
;; IMHO, the selected approach is too inflexible.  Some people find it
;; difficult to read code that uses a 2 column width indentation.
;;
;; A very strict yet better and respectful approach would have been to impose
;; a 2 column indentation with a 2-column width hard-tab with line length
;; computing the hard-tab as a 2 column width.  That would have allowed people
;; to increase tab width to a larger value to widen the rendering of
;; indentation on their screen without any impact on the file's content.
;;
;; PEL implements a solution to this problem: an automated conversion of
;; space-indented files into tab-indented buffers with ability to change the
;; tab width.  For Dart, this special minor mode is activated by the
;; `pel-indent-with-tabs-mode-for-dart' user-option.
;;

;;  Major Mode Dispatcher: `pel-dart-mode'
;;  * `pel-dart-mode'
;;  - `pel--dart-ts-mode-fixer'
;;
;;  Indentation Control Information
;;  * `pel-dart-indent-tab-info'
;;    - `pel-dart-insert-indent-info'
;;    - `pel-dart-insert-tab-info'
;;
;;  Major Mode Setup Information
;;  * `pel-dart-setup-info'
;;    - `pel--dart-minor-mode-info'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)        ; use:
(require 'pel--options)     ; use:
(require 'pel-indent)       ; use: `pel-indent-insert-control-info',
;;                          ;      `pel-indent-control-context'
;;                          ;      `pel-tab-insert-control-info',
;;                          ;      `pel-tab-control-context'
(require 'pel-modes)        ; use: `pel-insert-minor-mode-activation-info'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;* Major Mode Dispatcher: `pel-dart-mode'
;;  ======================================

;;-pel-autoload
(defun pel-dart-mode ()
  "Major mode dispatcher for editing Dart source text.
Uses `dart-mode' or `dart-ts-mode' depending on what is available
and required by `pel-use-dart'."
  (interactive)
  (cond
   ;; When `pel-use-dart` is t, PEL has downloaded and installed dart-mode.el
   ;; that provides the `dart-mode'.  Use that.
   ((eq pel-use-dart t)
    (when (fboundp 'dart-mode)
      (dart-mode)))

   ;; The `dart-ts-mode' is not built-in Emacs
   ((eq pel-use-dart 'with-tree-sitter)
    (if (and (pel-treesit-ready-p 'dart)
             (require 'dart-ts-mode nil :noerror)
             (fboundp 'dart-ts-mode))
        (dart-ts-mode)
      (display-warning 'pel-dart-with-tree-sitter
                       (format "Can't use dart-ts-mode: %s"
                               (if (pel-treesit-ready-p 'dart)
                                   "error loading dart-ts-mode"
                                 "no grammar for dart")))

      (if (fboundp 'dart-mode)
          (dart-mode)
        (user-error
         "Can't use `dart-ts-mode' nor `dart-mode': check installation!"))))))

;; Emacs tree-sitter handling fixer function.
;; Identified by `pel--ts-mode-with-fixer' and used by `pel-eval-after-load'.
;;-pel-autoload
(defun pel--dart-ts-mode-fixer ()
  "Remove `dart-ts-mode' entries from `auto-mode-alist'.
It removes what was entered when `dart-ts-mode' loads to ensure that the
`pel-dart-mode' mode dispatcher remains used."
  ;; There are several file extensions for Dart and the dart-ts-mode
  ;; adds several entries (entries for .dart, .zon).
  ;; Delete them all from auto-mode-alist.
  (setq auto-mode-alist
        (rassq-delete-all 'dart-ts-mode auto-mode-alist)))

;; ---------------------------------------------------------------------------
;;* Indentation Control Information
;;  ===============================
;;
;; Define a set of major-mode specific call back functions that are called
;; by the `pel-show-indent' command.  The call backs are invoked indirectly.
;; The `pel-indent-control-context' infer the names of the call back functions
;; using the name of the major mode and a format string.
;;
;; The call back functions are:
;; - `pel-dart-insert-indent-info' : Insert non-inferred indentation control.
;; - `pel-dart-insert-tab-info'    : Insert tab control info.
;; - `pel-dart-indent-tab-info'    : Tab information command.

;; Define the function called by `pel-indent-control-context' used by the
;; command `pel-show-indent' to insert the indentation control user option.

;;-pel-autoload
(defun pel-dart-insert-indent-info ()
  "Insert Dart indentation and hard tab setup info in current context.
Return a list of generic symbols described by this function."
  ;; `pel-indent-insert-control-info' identifies the dart indentation control
  ;; variable via a call to `pel-mode-indent-control-vars' which uses
  ;; `pel-mode-or-ts-mode-indent-control-vars' to identify
  ;; `dart-ts-indent-offset'.  Therefore no user-option value/link is
  ;; inserted here, just a note that only Tree-Sitter mode variable is used.
  (insert "\nDart has no classic mode.")
  ;; Return the list of generic symbols this function inserts.
  ;; These symbols are later used to identify the information already
  ;; inserted.
  '(indent-description-info))


;;-pel-autoload
(defun pel-dart-insert-tab-info ()
  "Insert Dart indentation and hard tab setup info in current context.
Return a list of generic symbols described by this function."
  (insert (substitute-command-keys "
 The Dart designers are pushing for an 2-column indentation
 made of space only. No tabs.

 See https://github.com/dart-lang/dart/discussions/3633

 However, several people find the 2-column indentation too small.
 If this is the case for you, and you work on official Dart code,
 you could use a temporary use a `tab-width' of 2, tabify the code
 with \\[tabify], then change the `tab-width' to a larger value
 for wider indentation.

 You can disable the format on save while you work on that file.
 When you're done just untabify the code with \\[untabify].

 PEL can automate all that: simply activate the
 `pel-indent-with-tabs-mode-for-dart'  user-option below.
 This does the same as the tbindent-mode implemented
 as a separate package.
"))
  (pel-insert-symbol-content-line 'pel-dart-tab-width)
  (pel-insert-symbol-content-line 'pel-dart-use-tabs
                                  nil #'pel-on-off-string)
  ;; Return the list of generic symbols described here.
  '(tab-description-intro
    pel-MM-tab-width
    pel-MM-use-tabs))

;;-pel-autoload
(defun pel-dart-indent-tab-info (&optional append)
  "Display Dart Indentation and hard tab control information."
  (interactive "P")
  (pel-major-mode-must-be 'dart-ts-mode)
  (let ((indent-control-context (pel-indent-control-context))
        (tab-control-context (pel-tab-control-context))
        (format-on-save (pel--symbol-value 'dart-ts-format-on-save :quiet)))
    (pel-print-in-buffer
     "*pel-indent-info*"
     "Indentation Width Control and Space/Tab Insertion Rendering"
     (lambda ()
       (pel-indent-insert-control-info indent-control-context)
       (pel-tab-insert-control-info tab-control-context)
       (when format-on-save
         (insert "\n
     Remember: the Dart formatter imposes its own style and is executed
               when the buffer is saved.  If you want to use a different
               format you will need to turn it off:")
         (pel-insert-symbol-content-line 'dart-ts-format-on-save)))
     (unless append :clear-buffer)
     :use-help-mode)))

;; ---------------------------------------------------------------------------
;;* Major Mode Setup Information
;; =============================

;;-pel-autoload
(defun pel--dart-minor-mode-info ()
  "Insert information related to Dart minor modes."
  (insert "
Automatic activation of minor mode is also controlled by the
following user-options:")
  (pel-insert-list-content 'pel-dart-activates-minor-modes
                           nil nil nil :1line))

;;-pel-autoload
(defun pel-dart-setup-info (&optional append)
  "Display Dart setup information."
  (interactive "P")
  (pel-major-mode-must-be '(dart-mode dart-ts-mode))
  (let ((pel-insert-symbol-content-context-buffer (current-buffer))
        (current-major-mode major-mode)
        (active-modes (pel-active-minor-modes))
        (indent-control-context (pel-indent-control-context))
        (tab-control-context (pel-tab-control-context)))
    (pel-print-in-buffer
     "*pel-dart-info*"
     "PEL setup for Dart programming language"
     (lambda ()
       "Print Dart setup"
       (insert (propertize "* Major Mode Control:" 'face 'bold))
       (pel-insert-symbol-content 'major-mode nil :on-same-line nil
                                  "major mode currently used")
       (when pel-use-tree-sitter
         (insert (format "\n- %s" (pel-ts-language-grammar-status-for
                                   'dart "\n- "))))
       (insert "\n\n")
       ;; -- List of minor modes
       (pel-insert-list-of-minor-modes active-modes)
       (insert "\n\n")
       (pel-insert-minor-mode-activation-info current-major-mode
                                              #'pel--dart-minor-mode-info)
       ;; --
       (insert "\n\n")
       (pel-indent-insert-control-info indent-control-context)
       (pel-tab-insert-control-info tab-control-context)
       )
     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-dart)

;;; pel-dart.el ends here
