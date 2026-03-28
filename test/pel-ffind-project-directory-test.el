;;; pel-ffind-project-directory-test.el --- Test the pel-ffind-project-directory  -*- lexical-binding: t; -*-

;; Created   : Friday, March 27 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-27 16:09:21 EDT, updated by Pierre Rouleau>

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
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel-ffind)
(require 'pel--base)      ;; if needed for utility macros
(require 'cl-lib)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel--test-touch (file)
  "Create FILE and ensure its parent directories exist."
  (make-directory (file-name-directory (expand-file-name file)) t)
  (with-temp-file (expand-file-name file) (insert "")))

(ert-deftest pel-ffind-project-directory/outermost-without-restricted ()
  "When multiple normal anchors exist, the outermost (shortest path) root is used."
  (let* ((root (make-temp-file "pel-ffind-t-" t))
         (inner (expand-file-name "a/b/c" root))
         (default-directory inner)
         ;; Use only the anchors we control here for determinism.
         (pel-project-root-identifiers '(".git" ".hg"))
         (pel-project-restricted-root-identifiers '(".pel-restricted-project")))
    (unwind-protect
        (progn
          ;; Create both anchors: .git at root (outermost), .hg one level down.
          (pel--test-touch (expand-file-name ".git" root))
          (pel--test-touch (expand-file-name ".hg" (expand-file-name "a" root)))
          (should (equal (pel-ffind-project-directory)
                         (directory-file-name (expand-file-name root)))))
      (delete-directory root t))))

(ert-deftest pel-ffind-project-directory/stops-at-restricted-anchor ()
  "Stops at the nearest restricted anchor if present."
  (let* ((root (make-temp-file "pel-ffind-t-" t))
         (a-dir (expand-file-name "proj/a" root))
         (deep (expand-file-name "proj/a/deep/x" root))
         (default-directory deep)
         (pel-project-root-identifiers '(".git" ".hg" ".projectile" ".pel-project"))
         ;; Default includes ".pel-restricted-project" — keep explicit for clarity.
         (pel-project-restricted-root-identifiers '(".pel-restricted-project")))
    (unwind-protect
        (progn
          (make-directory deep t)
          (pel--test-touch (expand-file-name ".git" root)) ; outermost
          (pel--test-touch (expand-file-name ".pel-restricted-project" a-dir)) ; restricted nearer
          (let ((expected (directory-file-name (expand-file-name a-dir))))
            (should (equal (pel-ffind-project-directory) expected))))
      (delete-directory root t))))

(ert-deftest pel-ffind-project-directory/fallback-to-default-directory ()
  "When no anchors are found, return nil; callers fall back to default-directory."
  (let* ((root (make-temp-file "pel-ffind-t-" t))
         (default-directory (expand-file-name "no-anchors/here" root)))
    (unwind-protect
        (progn
          (make-directory default-directory t)
          (should (equal (pel-ffind-project-directory) nil))
          ;; If this signals an error, the test fails automatically.
          (should (null (pel-generic-find-file "no-such-file"))))
      (delete-directory root t))))

;;; --------------------------------------------------------------------------
(provide 'pel-ffind-project-directory-test)

;;; pel-ffind-project-directory-test.el ends here
