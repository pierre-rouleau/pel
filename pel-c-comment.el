;;; pel-c-comment.el --- PEL C comment extension.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, November 10 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-11-16 21:48:37, updated by Pierre Rouleau>

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
;; When commenting several lines of C code `comment-dwim' comments each line
;; independently. It does not create a single multi-line comment.
;;
;; The command `pel-c-comment-dwim' behaves like `comment-dwim' in every way
;; except when a region spanning more than one line is marked.  In that case,
;; the behaviour of the command depends on the value of the
;; `pel-c-multiline-comments' user-option:
;;
;;  - if nil:  behave like `comment-dwim'
;;  - if set to 1: create a multi-line comment with the 1 star format.
;;  - if set to 2: create a multi-line comment with the 2 star format.
;;
;; These 2 formats look like the following:
;;
;; - 1-star   /* One star
;;             * continuation
;;             * comment.
;;             */
;;
;; - 2-stars  /* Two stars
;;            ** continuation
;;            ** comment.
;;            */
;;
;; The `pel-c-comment-dwim' is meant to be used as a replacement for
;; `comment-dwim' in C buffers.
;;
;;
;; Code Hierarchy:
;;
;; * `pel-c-comment-dwim'
;;   - `pel-c-comment-in-comment-p'
;;   - `pel--c-comment-marked'
;;   - `pel--c-comment-escape-comments'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: `pel-same-line-p'
(require 'pel--options)                 ; use: `pel-c-multiline-comments'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel--c-comment-escape-comments ()
  "Escape C comments in buffer.
Utility : meant to be used in narrowed region."
  (while (progn (goto-char (point-min))
                (search-forward "/*" nil :noerror))
    (replace-match "/\\*" nil :literal))

  (while (progn (goto-char (point-min))
                (search-forward "*/" nil :noerror))
    (replace-match "*\\/" nil :literal)))

(defun pel--c-comment-marked (column)
  "Comment entire buffer area with multi-line C comment.
The COLUMN argument identifies the comment of the area beginning.
Utility : meant to be used in narrowed region."
  (save-excursion
    (goto-char (point-min))
    (insert "/* ")
    (left-char 3)
    (forward-line 1)
    (right-char column)
    (while (not (eobp))
      (if (eq pel-c-multiline-comments 2)
          (insert "** ")
        (insert " * "))
      (left-char 3)
      (forward-line 1)
      (right-char column))
    (if (eq pel-c-multiline-comments 2)
        (insert "*/\n")
      (insert " */\n"))))

(defun pel-c-comment-in-comment-p (pos)
  "Return t if point is in a comment, nil otherwise."
  (save-excursion
    (goto-char pos)
    (nth 4 (syntax-ppss))))

;;-pel-autoload
(defun pel-c-comment-dwim (&optional kill)
  "Comment/un-comment C code.  Handle multiple line comments.

When no area is selected this function uses `comment-dwim' and provides the
same flexible commenting mechanism.
When an area is selected it creates a single multi-line comments when the
`pel-c-multiline-comments' user-option is t, otherwise it uses the standard
behaviour of `comment-dwim'."
  (interactive "P")
  (let (start end)
    (if (and pel-c-multiline-comments
             (use-region-p)
             (setq start (region-beginning))
             (setq end   (region-end))
             (not (pel-same-line-p start end))
             (not (pel-c-comment-in-comment-p (save-excursion
                                                (goto-char start)
                                                (forward-line 1)
                                                (point)))))
        (let ((column (save-excursion
                        (goto-char start)
                        (current-column))))
          (save-restriction
            (narrow-to-region start end)
            (pel--c-comment-escape-comments)
            (pel--c-comment-marked column)
            (goto-char (point-max))))

      (if (and
           (require 'newcomment nil :noerror)
           (fboundp 'comment-dwim))
          (comment-dwim kill)
        (error "comment-dwim is void")))))

;;; --------------------------------------------------------------------------
(provide 'pel-c-comment)

;;; pel-c-comment.el ends here
