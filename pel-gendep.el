;;; pel-gendep.el --- Generate list of Emacs Lisp dependencies.  -*- lexical-binding: t; -*-

;; Created   : Monday, March  2 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-02 12:47:08 EST, updated by Pierre Rouleau>

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
(require 'pel--base)  ; use `pel-inside-code'

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


(defun pel-gendep-elisp-file-dependencies (file)
  "Identifies and return dependencies of Emacs Lisp FILE.
Return a property list with:
- :file     : name of file (passed by FILE)
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
      ;; Identify the major mode to allow syntax checking
      ;; and discriminate code from comments and strings.
      (emacs-lisp-mode)
      ;; Find 'require' dependencies
      (while (re-search-forward pel-gendep-require-re nil t)
        (setq feature (match-string 1))
        (when (pel-inside-code (point))
          (unless (member feature dependencies)
            (push feature dependencies))))
      ;; Find loaded dependencies
      (goto-char (point-min))
      (while (re-search-forward pel-gendep-load-re nil t)
        (setq feature (match-string 1))
        (when (pel-inside-code)
          (unless (member feature loaded)
            (push feature loaded))))
      ;; Find 'provide'
      (goto-char (point-min))
      (while (re-search-forward pel-gendep-provide-re nil t)
        (setq feature (match-string 1))
        (when (pel-inside-code)
          (unless (member feature provides)
            (push feature provides)))))
    (list :file file
          :requires (nreverse dependencies)
          :loads (nreverse loaded)
          :provides (nreverse provides))))

(defun pel-gendep-elisp-dependencies-in-dir (directory)
  "Return a list of dependencies of all Emacs Lisp files in DIRECTORY."
  (let ((files-dep ()))
    (dolist (fn (sort (directory-files directory t "\\.el$" t)) (nreverse files-dep))
      (push (pel-gendep-elisp-file-dependencies fn) files-dep))))

(defun pel-string-starts-with-any-of (text prefixes)
  "Return non-nil if TEXT starts with any of the PREFIXES."
  (catch 'match
    (dolist (prefix prefixes)
      (when (pel-string-starts-with-p text prefix)
        (throw 'match prefix)))))

(defun pel-gendep-insert-make-elisp-dependencies (directory &optional local-prefix)
  "Insert Make code expressing elisp file dependencies for files in DIRECTORY.

Insert it in current buffer.

If LOCAL-PREFIX is non-nil it is a string or a list of strings that
identifies the prefix that identifies files that must be retained in the
dependency lists."
  (dolist (dep (pel-gendep-elisp-dependencies-in-dir directory))
    (let ((fname (file-name-nondirectory (plist-get dep :file)))
          (reqs  (cl-union (plist-get dep :requires) (plist-get dep :requires)
                           :test #'string= )))
      (when local-prefix
        (setq reqs (seq-filter (lambda (fn)
                                 (and fn
                                      (pel-string-starts-with-any-of fn (pel-list-of local-prefix))))
                               reqs)))
      (when (and reqs
                 (or (null local-prefix)
                     (pel-string-starts-with-any-of fname (pel-list-of local-prefix))))
        ;; Insert a .elc dependency to a list of .elc files.
        (insert (format "%sc: %s.elc\n"
                        fname
                        (string-join reqs ".elc ")))))))

;;-pel-autoload
(defun pel-gendep-insert-make-elisp-dependencies-for-pel ()
  "Generate the elisp file dependencies for PEL in current buffer."
  ;; Note that it will generate a circular dependency via pel--options.elc
  ;; pel--options.elc really only depends on pel--base.elc.
  (interactive)
  (if (boundp 'pel-home-dirpath-name)
      (pel-gendep-insert-make-elisp-dependencies pel-home-dirpath-name
                                                 '("pel-" "pel_"))
    (message "PLease update your init.el to define `pel-home-dirpath-name'!")))

;;; --------------------------------------------------------------------------
(provide 'pel-gendep)

;;; pel-gendep.el ends here
