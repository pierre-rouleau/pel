;;; pel-uuid.el --- PEL UUID Generation  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Pierre Rouleau

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

;; -----------------------------------------------------------------------------
;;; Commentary:

;; UUID generation utilities.
;;
;; This file provides the following functions tha generate or insert generated
;; UUID strings:
;;
;; - (pel-uuid)       : returns a UUID string (36 characters)
;; * (pel-insert-uid) : insert a UUID string (36 characters) in current buffer
;;                      at  point.
;; * (pel-insert-c-include-guard) : inserts a C pre-processor #include guard
;;                                  specialized for the file in the current
;;                                  buffer that uses the file name and a UUID.
;;
;; pel-uuid is a function, the others are interactive functions (commands).
;;

;; References & Credits:
;;
;; Aside from Emacs manual, the following sites contain information that
;; provided inspirations during the creation of this file:
;;
;; - Wikipedia: URL https://en.wikipedia.org/wiki/Universally_unique_identifier
;; - Chris Wellons Emacs UUID: URL https://nullprogram.com/blog/2010/05/11/
;; - Martin Blais: URL http://furius.ca/pubcode/pub/conf/lib/elisp/blais/uuid.el



;;; Code:

;; Internal utilities
;; ------------------
;; - pel--simple-uuid: a simple function that can genearet UUID from variable
;;                     data and a MD5 hash.
;; - pel--sys-uuid:    a more secure function that uses the system's uuidgen.
;;

(defun pel--simple-uuid ()
  "Return a manually generated UUID string.
Uses a simple hashing of variable data.
Used when the uuuidgen is not available."
  (let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s%s%s"
                        (current-time)
                        (emacs-init-time)
                        (emacs-pid)
                        (emacs-uptime)
                        (emacs-version)
                        (garbage-collect)
                        (pwd)
                        (random)
                        (recent-keys)
                        (system-name)
                        (user-full-name)
                        (user-uid)))))
    (upcase (format "%s-%s-3%s-%s-%s"
                    (substring s 0 8)
                    (substring s 8 12)
                    (substring s 13 16)
                    (substring s 16 20)
                    (substring s 20 32)))))


(defun pel--sys-uuid ()
  "Generate and return UUID string.
Uses the uuidgen facility to generate a high-quality UUID string.
The returned string does not include the terminating newline nor
any whitespace."
  (substring
   (shell-command-to-string "uuidgen")
   0 -1))

;; Public Interface
;; ----------------

;; Select the internal implementation based on what is available on the
;; computer.
(defalias 'pel-uuid (if (executable-find "uuidgen")
                        'pel--sys-uuid
                      'pel--simple-uuid)
  "Generate and return a UUID string of 36 uppercase characters.
To insert a UUID in the current buffer use `pel-insert-uuid'.")

(defun pel-insert-uuid ()
  "Insert UUID at point in current buffer.
Leaves point after the inserted UUID."
  (interactive)
  (insert (pel-uuid)))

(defun pel-insert-c-include-guard ()
  "Insert a C pre-processor include guard.
The C pre-processor symbol created uses the following components:
- File name body (without directory name and without extension) in upper case.
- Two underscores.
- The file extension in upper-case.
- One underscore character.
- The UUID string, with dash characters replaced by underscores.

For a file \"utils.h\", the text inserted in the buffer looks
like this:

    #ifndef UTILS__H_B89D59EF_9CDE_45CD_BB83_242AD937BF08
    #define UTILS__H_B89D59EF_9CDE_45CD_BB83_242AD937BF08 /* include guard */

    #endif

The file name is placed in the first part of the symbol followed by a UUID with
all dash characters replaced by underscore to be compatible with the C
pre-processor."
  (interactive)
  (let* ((fn (file-name-nondirectory
              (file-name-sans-versions
               (buffer-file-name))))
         (fn-extension (file-name-extension fn))
         (fn-body (file-name-base fn))
         (preproc-symbol (format
                          "%s__%s_%s"
                          (upcase fn-body)
                          (upcase fn-extension)
                          (replace-regexp-in-string "-" "_" (pel-uuid)))))
         (insert (format "\
#ifndef %s
#define %s /* include guard */

#endif"
                         preproc-symbol
                         preproc-symbol))
         (forward-line -1)))

;; -----------------------------------------------------------------------------
(provide 'pel-uuid)

;;; pel-uuid.el ends here
