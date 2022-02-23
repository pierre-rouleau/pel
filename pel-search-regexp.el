;;; pel-search-regexp.el --- Search regxp selection control  -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2022  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

;; This file is part of the PEL package
;; This file is not part of GNU Emacs.

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


;;; Commentary:
;;

;; Control Search Commands used
;; ----------------------------
;;
;; * pel-select-search-regexp-engine
;;   - pel--prompt-for-search-regexp-engine
;;   - pel-set-search-regexp-engine
;;     - pel--available-search-regexp-engines
;;       - pel--vr-engines
;;
;; * pel-query-replace-regexp
;; * pel-replace-regexp

;;; Code:

(eval-when-compile
  (require 'cl-macs))    ; use: cl-case.

(require 'pel--options)  ; use: pel-use-visual-regexp,
;;                       ;      pel-use-visual-regexp-steroids
;;                       ;      pel-initial-regexp-engine

(defvar pel--search-regexp-initialized nil
  "Set to t when search regexp engine management is initialized.
Modified by pel-search-regexp code ONLY.")

(defvar pel--search-regexp-engine 'emacs
  "Search regexp engine used by various search commands.
Possible values are:
- emacs         : default: use Standard Emacs search regexp
- vr            : use visual-regexp.
- vr/emacs      : use visual-regexp-steroids emacs.
- vr/emacs-plain: use visual-regexp-steroids emacs-plain
- vr/pcre2el    : use visual-regexp-steroids pcre2el.
- vr/python     : use visual-regexp-steroids python.
- vr/custom     : use visual-regexp-steroids custom.

Change it with function `pel-select-search-regexp-engine'.")

;; --

;;-pel-autoload
(defun pel-replace-regexp ()
  "Unconditional regexp replace - use active search regexp engine."
  (interactive)
  (cl-case pel--search-regexp-engine
    (emacs (call-interactively 'replace-string))
    (vr    (call-interactively 'vr/replace))
    (t     (call-interactively 'vr/select-replace))))

;;-pel-autoload
(defun pel-query-replace-regexp ()
  "Query regexp replace - use active search regexp engine."
  (interactive)
  (cl-case pel--search-regexp-engine
    (emacs (call-interactively 'query-replace))
    (vr    (call-interactively 'vr/query-replace))
    (t     (call-interactively 'vr/select-query-replace))))

;; --

(defun pel--vr-engines ()
  "Return PEL adjusted available Visual Regexp engines strings."
  (when (and (boundp 'vr--engines)
             (boundp 'vr/command-custom))
    (let ((choices vr--engines))
      (unless (string= "" vr/command-custom)
        (setq choices (cons 'custom choices)))
      ;; choices hold the symbols for available VR regexp engines
      ;; Get a list of string for these, and prepend "vr/" to their names.
      (mapcar (lambda (symb)
                (intern (format "vr/%s" (symbol-name symb))))
              choices))))

(defun pel--available-search-regexp-engines ()
  "Return a list of available search engine names (symbols).
Available choices depend on what PEL has available via its configuration.
The names that can be in the return list, and their meanings are:
- emacs          : plain emacs
- vr             : visual-regexp
- vr/emacs       : visual-regexp-steroids emacs
- vr/emacs-plain : visual-regexp-steroids emacs-plain
- vr/pcre2el     : visual-regexp-steroids pcre2el
- vr/python      : visual-regexp-steroids python
- vr/custom      : visual-regexp-steroids custom"
  (let ((choices '(emacs)))
    (when (and pel-use-visual-regexp-steroids
             (require 'visual-regexp-steroids nil :noerror))
        (setq choices (append choices (pel--vr-engines))))
    (when (and pel-use-visual-regexp
             (require 'visual-regexp nil :noerror))
      (setq choices (append choices '(vr))))
    choices))

(defun pel-set-search-regexp-engine (choice)
  "Set the search regexp engine to CHOICE if it is valid.
Valid choices are:
emacs | vr | vr/emacs | vr/emacs-plain | vr/pcre2el | vr/python | vr/custom
Display a message for invalid values and ignore it.
Return the new value if changed, nil if not changed."
  (if (memq choice (pel--available-search-regexp-engines))
      (progn
        (setq pel--search-regexp-engine choice)
        ;; when selecting one of the visual-regexp-steroids engine, then
        ;; also set the engine used by the visual-regexp-steroids library.
        (when (and (boundp 'vr/engine)
                   (memq choice '(vr/emacs
                             vr/emacs-plain
                             vr/pcre2el
                             vr/python
                             vr/custom)))
          (setq vr/engine choice)))
    (message "Invalid/unavailable search regexp engine: %S" choice)
    nil))

(defun pel--prompt-for-search-regexp-engine ()
  "Prompt for available search engine. Return selected string."
  (let ((default (symbol-name pel--search-regexp-engine)))
    (intern (completing-read
             (format "Select engine (default: %s): "
                     default)
             (mapcar 'symbol-name (pel--available-search-regexp-engines))
             nil t nil nil default))))

;;-pel-autoload
(defun pel-select-search-regexp-engine ()
  "Select the search/replace and regexp engine to use."
  (interactive)
  (pel-set-search-regexp-engine (pel--prompt-for-search-regexp-engine)))

;; --

(defun pel--active-search-regexp-engine ()
  "Return description of currently used regexp engine."
  (cl-case pel--search-regexp-engine
    (emacs "Emacs standard")
    (vr    "Visual-Regexp")
    (vr/emacs       "Visual-Regexp-Steroids emacs")
    (vr/emacs-plain "Visual-Regexp-Steroids emacs-plain")
    (vr/pcre2el     "Visual-Regexp-Steroids pcre2el")
    (vr/python      "Visual-Regexp-Steroids python")
    (vr/custom      "Visual-Regexp-Steroids custom")
    (t              (format "Unknown: %S" pel--search-regexp-engine))))

(defvar pel--active-search-regexp-engine-shown-already nil
  "Count number of times the message was shown.")


(defun pel-active-search-regexp-engine-str (&optional with-details)
  "Return a string describing the used search regexp engine."
  (let* ((with-details (or (not pel--active-search-regexp-engine-shown-already)
                           with-details))
         (text         (format "Search/replace regexp engine is: %s.%s"
                               (pel--active-search-regexp-engine)
                               (if with-details
                                   (format "
- Possible values:
  - emacs         : always available.
  - vr            : pel-use-visual-regexp (%s) must be t to use this.
  - For the following, pel-use-visual-regexp-steroids (%s) must be t:
    - vr/emacs
    - vr/emacs-plain
    - vr/pcre2el
    - vr/python
    - vr/custom
  Change the values of the pel-use-visual-regexp... user option variables
  than execute pel-init to activate or deactivate them."
                                          pel-use-visual-regexp
                                          pel-use-visual-regexp-steroids)
                                ""))))
    (setq pel--active-search-regexp-engine-shown-already t)
    text))

;; -----------------------------------------------------------------------------
(provide 'pel-search-regexp)

;;; pel-search-regexp.el ends here
