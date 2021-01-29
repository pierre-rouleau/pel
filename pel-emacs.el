;;; pel-emacs.el --- Emacs stats utilities -*- lexical-binding: t -*-

;; Copyright (C) 2020, 2021  Pierre Rouleau

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

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; A set of of utilities related to Emacs itself.
;;
;; - `pel-emacs-executable' identifies the Emacs executable path and prints it
;;    on the echo area.
;; - `pel-open-emacs-refcard' prompts for the name of an Emacs reference card
;;   and opens the PDF file if it can locate it.  It attempts to locate the
;;   directory unless the `pel-emacs-refcard-dirpath' user option identify the
;;   directory where the reference card files are stored.
;;   - It uses the `pel-emacs-refcard-dirpath' utility to identify the
;;     location of the directory and topic specific PDF files.
;; - `pel-emacs-load-stats' print Emacs Lisp load statistics in the echo area.
;; - `pel-emacs-mem-stats' print Emacs Lisp memory statistics in a buffer.
;;   Those might be useful when developing Emacs Lisp code.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--base)                    ; use: pel-print-in-buffer
(require 'pel--options)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-emacs-executable ()
  "Display Emacs executable path in echo area."
  (interactive)
  (message "Emacs := %s"
           (file-truename
            (expand-file-name invocation-name invocation-directory))))

;; --

(defvar pel--prompt-history-for-emacs-refcard nil
  "History list for function `pel-open-emacs-refcard'.")

(defun pel-emacs-refcard-dirpath (&optional topic)
  "Compute and return the path of Emacs refcard directory.
If TOPIC is non-nil, return the full path  of the specified topic PDF file."
  (let ((refcard-dirpath (or pel-emacs-refcard-dirpath
                             (expand-file-name
                              (format "../../share/emacs/%s/etc/refcards"
                                      emacs-version)
                              (file-truename
                               (expand-file-name
                                invocation-name
                                invocation-directory))))))
    (unless (file-exists-p refcard-dirpath)
      (user-error
       (if pel-emacs-refcard-dirpath
           "Directory identified by pel-emacs-refcard-dirpath is invalid!\
 Please fix it!"
         "Cannot locate Emacs refcard directory!
Please identify a directory with PDF refcard files in the \
pel-emacs-refcard-dirpath user option!")))
    (if topic
        (expand-file-name (format "%s.pdf" topic) refcard-dirpath)
      refcard-dirpath)))

;;-pel-autoload
(defun pel-open-emacs-refcard ()
  "Prompt for an Emacs REFCARD and open it.

Attempts to find the directory where the Emacs PDF reference card
files are stored.  Failing to detect them it uses the directory identified by
the variable `pel-emacs-refcard-dirpath' user option."
  (interactive)
  (let* ((topics (mapcar
                  (lambda (fn)
                    (substring fn 0 -4))
                  (directory-files
                   (pel-emacs-refcard-dirpath) nil  "\\.pdf\\'")))
         (topic  (completing-read
                  "Refcard: "
                  topics
                  nil                   ; predicate
                  t                     ; require-match
                  nil                   ; initial
                  'pel--prompt-history-for-emacs-refcard)))
    (browse-url (format "file:%s" (pel-emacs-refcard-dirpath topic)))))

;;-pel-autoload
(defun pel-emacs-load-stats ()
  "Display number of loaded files & features."
  (interactive)
  (message "\
# loaded files: %d
# features    : %d"
           (length load-history)
           (length features)))


;; Prevent byte-compiler warning by declaring variables that are
;; always available.
(defvar cons-cell-consed)
(defvar float-consed)
(defvar vector-cells-consed)
(defvar string-chars-consed)
(defvar misc-objects-consed)
(defvar intervals-consed)
(defvar strings-consed)

;;-pel-autoload
(defun pel-emacs-mem-stats ()
  "Display Emacs memory statistics inside an *emacs-mem-stats* buffer."
  (interactive)
  (pel-print-in-buffer "*emacs-mem-stats*"
                       "Emacs Memory"
                       (format "\
On %s:
 - cons-cells-consed  : %19d
 - floats-consed      : %19d
 - vector-cells-consed: %19d
 - symbols-consed     : %19d
 - string-chars-consed: %19d
 - misc-objects-consed: %19d
 - intervals-consed   : %19d
 - strings-consed     : %19d"
               (format-time-string "%A, %B %d, %Y @ %T")
               cons-cells-consed
               floats-consed
               vector-cells-consed
               symbols-consed
               string-chars-consed
               misc-objects-consed
               intervals-consed
               strings-consed)))


(defun pel--emacs-command-count ()
  "Return number of available commands."
  (let ((command-count 0))
  (mapatoms
   (lambda (symbol)
     (when (commandp symbol)
       (setq command-count (1+ command-count)))))
  command-count))

;;-pel-autoload
(defun pel-emacs-command-stats ()
  "Display number of available commands."
  (interactive)
  ;; TODO: find a way to detect count of commands bound to key
  (message "Number of currently available commands: %d"
           (pel--emacs-command-count)))

;;; --------------------------------------------------------------------------
(provide 'pel-emacs)

;;; pel-emacs.el ends here
