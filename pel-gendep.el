;;; pel-generate-dependencies.el --- Generate list of Emacs Lisp dependencies.  -*- lexical-binding: t; -*-

;; Created   : Monday, March  2 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-02 09:22:39 EST, updated by Pierre Rouleau>

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
;; This file defines `pel-generate-elisp-dependencies'; a tool function
;; that analyzes the content of an Emacs Lisp and returns dependency
;; information.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defconst pel-gendep-require-re
  "[[:blank:]]*(require[[:blank:]]'\\(\\S_[[:alnum:]-]+\\) ?"
  "Regular expression that extracts feature required by file in group 1.")

(defconst pel-gendep-load-re
  "[[:blank:]]*(load\\(?:-library\\)? \"\\([[:alnum:]-]+\\)\""
  "Regular expression that extracts feature loaded by file in group 1.")

(defconst pel-gendep-provide-re
  "^[[:blank:]]*(provide[[:blank:]]'\\(\\S_[[:alnum:]-]+\\)"
  "Regular expression that extracts feature provided by file in group 1.")

(defun pel-generate-elisp-dependencies (file)
  "Identifies and return dependencies of Emacs Lisp FILE.
Return an association list with:
- :requires : list of strings: name of required library.
- :loads    : list of strings: name of loaded files.
- :provides : list of provided feature(s)."
  (let ((dependencies ())
        (loaded ())
        (provides ())
        feature)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      ;; Find 'require' dependencies
      (while (re-search-forward pel-gendep-require-re nil t)
        (setq feature (match-string 1))
        (unless (member feature dependencies)
          (push feature dependencies)))
      ;; Find loaded dependencies
      (goto-char (point-min))
      (while (re-search-forward pel-gendep-load-re nil t)
        (setq feature (match-string 1))
        (unless (member feature loaded)
          (push feature loaded)))
      ;; Find 'provide'
      (goto-char (point-min))
      (while (re-search-forward pel-gendep-provide-re nil t)
        (setq feature (match-string 1))
        (unless (member feature provides)
          (push feature provides))))
    (list :requires (nreverse dependencies)
             :loads (nreverse loaded)
             :provides (nreverse provides))))

;;; --------------------------------------------------------------------------
(provide 'pel-generate-dependencies)

;;; pel-generate-dependencies.el ends here
