;;; pel-hier-face.el --- Represent how faces inherit from each other  -*- lexical-binding: t; -*-

;; Created   : Saturday, November 22 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-11-24 17:05:57 EST, updated by Pierre Rouleau>

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
;;  The `pel-hier-face' command prints the hierarchy of the faces currently
;;  loaded in a buffer that provides quick access to the code that defines
;;  them.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)
(require 'hierarchy)
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-hier-face--all-faces ()
  "Return a list of all faces."
  (let ((all-faces (list)))
    (mapatoms
     (lambda (symbol)
       (when (facep symbol)
         (setq all-faces (cons symbol all-faces)))))
    all-faces))

(defun pel-hier-face-parent (f &optional frame)
  "Return the face(s) that FACE-NAME inherits from, or nil if none."
  ;; (message "(pel-hier-face-parent %S)" f)
  (let ((parent-face (if (eq f 'root)
                         'root
                       (condition-case nil
                           (face-attribute f :inherit (or frame
                                                          (selected-frame)))
                         (error 'unspecified)))))
    (cond
     ((eq f 'root) nil)
     ((eq parent-face 'unspecified) 'root)
     (t (if (stringp parent-face)
            (format "*%s*"  parent-face)
          (format "%s" parent-face)
          )))))

(defun pel-hier-face-faces-hierarchy ()
  "Return a hierarchy of all faces."
  (let ((hierarchy (hierarchy-new)))
    (hierarchy-add-trees hierarchy
                         (pel-hier-face--all-faces)
                         #'pel-hier-face-parent)
    (hierarchy-sort hierarchy)
    hierarchy))

(defun pel-hier-face--requested-symbol-for (text column)
  "Return symbol in TEXT at COLUMN"
  (let* ((words (split-string (substring text 1 -1)))
         (word-len (mapcar #'length words))
         (begin-pos 2)
         (end-pos nil)
         (max-n (length words))
         (n 0)
         (found nil))
    (while (and (not found)
                (< n max-n))
      (setq n (1+ n))
      (setq end-pos (+ begin-pos (car word-len)))
      (setq word-len (cdr word-len))
      (when (<= begin-pos column end-pos)
        (setq found (intern (car words))))
      (setq begin-pos (+ end-pos 2))
      (setq words (cdr words)))
    found))

;;-pel-autoload
(defun pel-hier-face ()
  "Display all loaded faces in current frame and their inheritance relationship.
The faces hierarchy is printed inside a hierarchy-tabulated buffer.
Each face name is a link to its implementation code."
  (interactive)
  (let* ((hierarchy (pel-hier-face-faces-hierarchy))
         (buffer (hierarchy-tabulated-display
                  hierarchy
                  (hierarchy-labelfn-indent
                   (hierarchy-labelfn-button
                    (lambda (item _)
                      (condition-case nil
                          (cond
                           ((listp item)
                            (dolist (it item)
                              (insert (format "%s" it))))
                           ((stringp item)
                            (cond
                             ((pel-string-starts-with-p item "'")
                              (insert (substring item 1)))
                             ;;
                             ((pel-string-starts-with-p item "(")
                              (let ((the-list (split-string (substring item 1
                                                                       -1))))
                                (dolist (it the-list)
                                  (insert (format "%s, " it)))))
                             ;;
                             (t (insert item))))
                           (t
                            (insert (format "%s" item))))
                        (error "Internal ERROR 1 for: %S" item))
                      )
                    (lambda (item _)
                      ;; (message "--10: item: %S : %s, column=%d"
                      ;;          item
                      ;;          (type-of item)
                      ;;          (current-column))
                      (cond
                       ((stringp item)
                        (cond
                         ((pel-string-starts-with-p item "'")
                          (find-face-definition (intern (substring item 1))))
                         ((pel-string-starts-with-p item "(")
                          (find-face-definition
                           (pel-hier-face--requested-symbol-for item (current-column))))
                         (t (find-face-definition (intern item)))))

                       ((listp item)
                        (find-face-definition (car item)))
                       (t
                        (condition-case nil
                            (find-face-definition item)
                          (error "ERROR 2 for %S. Point=%d"
                                          item
                                          (point)))))))))))
    (switch-to-buffer buffer)))

;;; --------------------------------------------------------------------------
(provide 'pel-hier-face)

;;; pel-hier-face.el ends here
