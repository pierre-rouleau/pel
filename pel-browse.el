;;; pel-browse.el --- Browser support utilities.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, May 11 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-05-11 16:54:11, updated by Pierre Rouleau>

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
;; This file provides the `pel-browse-url' command that can use a mechanism
;; identified by Emacs browse-url custom variable but can also be overridden
;; to force the use of a browser different than the system default browser.
;; It handles macOS specially: in macOS the normal Emacs mechanism does not
;; work and the macOS scripting application osascript is used.
;;
;; The PEL user options variables `pel-browser-used' selects the mechanism.
;;
;; The code hierarchy is:
;;
;; * `pel-browse-url'
;;   - `pel--macos-browse'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)
(require 'pel--options)
(require 'browse-url)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel--macos-browse (browser url)
  "Browse URL using specified BROWSER in macOS.

BROWSER must be a string; the name macOS application has,
like \"Firefox\"."
  (let ((cmd (format "osascript -e 'tell application \"%s\" \
to open location \"%s\"'" browser url)))
    (shell-command cmd)))

;;-pel-autoload
(defun pel-browse-url (url &rest args)
  "Open the URL using the PEL selected web browser.
Optionally pass extra arguments ARGS, but only when the
user-option `pel-browser-used' is nil.
Use the browser identified by the user-option variable
`pel-browser-used'."
  (interactive (browse-url-interactive-arg "URL: "))
  ;; On macOS, the normal browser-url- functions do not work.
  ;; So use the macOS scripting osascript to launch the browser
  ;; when firefox or chrome is selected by the user-option.
  (cond
   ((null pel-browser-used)
    (browse-url url args))
   ((eq pel-browser-used 'firefox)
    (if pel-system-is-macos-p
        (pel--macos-browse "Firefox" url)
      (browse-url-firefox url)))
   ((eq pel-browser-used 'chrome)
    (if pel-system-is-macos-p
        (pel--macos-browse "Google Chrome" url)
      (browse-url-chrome url)))
   (t (user-error "Invalid value for pel-browser-used: %s" pel-browser-used))))

;;; --------------------------------------------------------------------------
(provide 'pel-browse)

;;; pel-browse.el ends here
