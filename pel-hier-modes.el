;;; pel-hier-modes.el --- Represent how major-modes inherit from each other  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Adapted to PEL by: Pierre Rouleau <prouleau001@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This code was written by Damien Cassou as an example for his
;; excellent hierarchy library.  That library was integrated in Emacs 28.1
;; - Original project: https://github.com/DamienCassou/hierarchy
;; - Original example file:
;;    https://github.com/DamienCassou/hierarchy/blob/master/examples/hierarchy-examples-major-modes.el
;;
;; The example in itself is very useful but unfortunately not included in
;; Emacs.  This provides it.
;;

;;; Code:

;; Starting with Emacs 28.1, hierarchy.el is built-in Emacs, before it was
;; provided by Damien Cassou's GitHub based project.
;; PEL supports Emacs 26 and later, therefore we need to ensure that
;; it is installed from GitHub on earlier version *and* silence all
;; byte-compilation warnings.  The `pel-require' function ensure that
;; it is present and installs it if not but that's unknown to the byte
;; compiler which would report its functions as unknown.  Explicitly declare
;; them after to prevent the byte compiler warnings.

(require 'pel--base)
(pel-require 'hierarchy :install-when-missing
             "DamienCassou/hierarchy/refs/heads/master"
             "hierarchy.el")

(declare-function hierarchy-new "hierarchy.el")
(declare-function hierarchy-add-trees "hierarchy.el")
(declare-function hierarchy-sort "hierarchy.el")
(declare-function hierarchy-tabulated-display "hierarchy.el")
(declare-function hierarchy-labelfn-indent "hierarchy.el")
(declare-function hierarchy-labelfn-button "hierarchy.el")

;; --

(defun pel-hier-modes--major-mode-p (f)
  "Return non-nil if F is a major-mode function."
  ;; copy-edited from counsel.el (in the swiper/ivy repository)
  (and (commandp f) (string-match "-mode$" (symbol-name f))
       (null (help-function-arglist f))))

(defun pel-hier-modes--all-major-modes ()
  "Return a list of all major modes."
  (let ((major-modes (list)))
    (mapatoms
     (lambda (symbol)
       (when (pel-hier-modes--major-mode-p symbol)
         (setq major-modes (cons symbol major-modes)))))
    major-modes))

(defun pel-hier-modes--major-mode-parent (f)
  "Return the major mode F derive from.
If F doesn't derive from any major-mode, return `root-mode'."
  (let ((parent-mode (or (get f 'derived-mode-parent))))
    (cond
     ((eq f 'root-mode) nil)
     ((null parent-mode) 'root-mode)
     (t parent-mode))))

(defun pel-hier-modes--major-mode-build-hierarchy ()
  "Return a hierarchy of all major modes."
  (let ((hierarchy (hierarchy-new)))
    (hierarchy-add-trees hierarchy
                         (pel-hier-modes--all-major-modes)
                         #'pel-hier-modes--major-mode-parent)
    (hierarchy-sort hierarchy)
    hierarchy))

;;-pel-autoload
(defun pel-hier-modes ()
  "Display all loaded major modes and their inheritance relationship.
The major mode hierarchy is printed inside a hierarchy-tabulated buffer.
Each mode name is a link to its implementation code."
  (interactive)
  (let* ((hierarchy (pel-hier-modes--major-mode-build-hierarchy))
         (buffer (hierarchy-tabulated-display
                  hierarchy
                  (hierarchy-labelfn-indent
                   (hierarchy-labelfn-button
                    (lambda (item _) (insert (format "%s" item)))
                    (lambda (item _) (find-function item)))))))
    (switch-to-buffer buffer)))


(provide 'pel-hier-modes)

;;; pel-hier-modes.el ends here
