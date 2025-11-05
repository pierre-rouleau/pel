;;; pel-dart.el --- PEL Dart support extension.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, November  4 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-11-05 15:32:21 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2025  Pierre Rouleau
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
;; Dart takes a very strong approach to indentation, forcing indentation to be
;; 2 spaces, always.  Their tools force this convention.  Several people
;; complained about the lack of flexibility of this approach that causes them
;; personal pain as well as causing problem in team environments.
;;
;; See:
;;  - https://github.com/dart-lang/dart_style/issues/534
;;  - https://github.com/dart-lang/dart_style/issues/1683
;;
;; IMHO, the selected approach is quite misguided and way too inflexible.
;; Several people find it difficult to read code that has a 2 column width
;; indentation.  Perhaps one day, several people that do not see a problem
;; with it now will find themselves in a similar situation and will have to
;; change their views on this.
;;
;; A very strict yet better and respectful approach would have been to
;; impose a 2 column indentation with a 2-column width hard-tab with line
;; length computing the hard-tab as a 2 column width.  That would have allowed
;; people to increase tab width to a larger value to widen the rendering of
;; indentation on their screen without any impact on the file's content.
;;
;; I will try to provide a way to deal with this limitation in Emacs, by
;; dynamically replacing 2-space indentation with hard-tabs that can then be
;; expanded to any width to help make it look wider.
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

;;-pel-autoload
(defun pel--dart-ts-mode-fixer ()
  "Remove `dart-ts-mode' entries from `auto-mode-alist'.
It removes what entered when `dart-ts-mode' loads."
  ;; There are several file extensions for Dart and the dart-ts-mode
  ;; adds several entries (entries for .dart, .zon).
  ;; Delete them all from auto-mode-alist.
  (setq auto-mode-alist
        (rassq-delete-all 'dart-ts-mode auto-mode-alist)))

;;; --------------------------------------------------------------------------
(provide 'pel-dart)

;;; pel-dart.el ends here
