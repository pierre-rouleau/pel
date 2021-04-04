;;; pel-file-recent.el --- Manage and access recently visited files.  -*- lexical-binding: t; -*-

;; Created   : Friday, April  2 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-04-02 15:36:42, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021  Pierre Rouleau
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
;;
;; ido-recentf-open
;; counsel-recentf
;; psw-switch-recentf
;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'recentf)
(require 'ido)
(require 'pel--options)
(require 'pel-prompt)
;;; --------------------------------------------------------------------------
;;; Code:
;;


;; Credits for ido-recentf-open: Mickey Petersen
;; https://www.masteringemacs.org/article/find-files-faster-recent-files-package

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; --

(defvar pel--recentf-function nil
  "Function used to display recently opened files.")

;; --
(defconst pel--recentf-function-method-alist
  '((nil                . "Ido")
    (ido-recentf-open   . "Ido")
    (consel-recentf     . "Ivy/Counsel")
    (psw-switch-recentf . "Popup Switcher"))
  "Associates function symbol to method name string.")

(defun pel--recentf-function-method-name (recentf-function)
  "Return the name of the method for given RECENTF-FUNCTION."
  (cdr (assoc recentf-function pel--recentf-function-method-alist)))

(defun pel-show-recentf-function (&optional after-selection-p)
  "Display what function is used to visit recently opened files.
If AFTER_SELECTION-P is non-nil the message is issued after changing it."
  (interactive)
  (let ((msg (if (or after-selection-p
                     pel--recentf-function)
                 (format "%s using %s"
                         (if after-selection-p "Now" "Currently")
                         (pel--recentf-function-method-name
                          pel--recentf-function))
               (format "Using the default, Ido,"))))
    (message "%s to visit recently opened files." msg)))

;; --

(defun pel--recentf-selection ()
  "Return a list of (char prompt symbol) of available recent file functions."
  (let ((selection '((?d "Ido" ido-recentf-open))))
    (when pel-use-counsel
      (setq selection (append selection '((?c "Ivy/Counsel" counsel-recentf)))))
    (when pel-use-popup-switcher
      (push '(?p "Popup-switcher" psw-switch-recentf) selection))
    (reverse selection)))

(defun pel-select-recentf-function (&optional recentf-function silent)
  "Select the function to visit recently opened files.
Non interactively accepts a RECENTF-FUNCTION value.
Prints a message describing the new selected value unless SILENT is non-nil."
  (interactive)
  (let ((recentf-function recentf-function))
    (unless recentf-function
      ;; prompt user
      (setq recentf-function
            (pel-select-from "Function to visit recently opened files"
                             (pel--recentf-selection)
                             pel--recentf-function
                             nil
                             "default - ido")))
    ;; validate selection: leave unchanged if choice cannot be applied: issue
    ;; a user-error.
    (cond
     ((eq recentf-function 'counsel-recentf)
      (unless pel-use-counsel
        (user-error "\
Please activate pel-use-counsel to list recent opened file with Counsel")))
     ((eq recentf-function 'psw-switch-recentf)
      (unless pel-use-popup-switcher
        (user-error "\
Please activate pel-use-popup-switcher to list recent opened file with\
 popup-switcher"))))
    (setq pel--recentf-function recentf-function))
  (unless silent
    (pel-show-recentf-function :after-selection)))

;; --

(defun pel-find-recent-file ()
  "Open the recent file prompt using the currently active function."
  (interactive)
  ;; If not already selected, select the function from the user-option
  (unless pel--recentf-function
    (pel-select-recentf-function pel-initial-recentf-function :silently))
  (funcall pel--recentf-function))

;;; --------------------------------------------------------------------------
(provide 'pel-file-recent)

;;; pel-file-recent.el ends here
