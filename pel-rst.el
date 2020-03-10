;;; pel-rst.el --- PEL reStructuredText support

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau.swd@gmail.com>

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
;;


;;; Code:
(require 'thingatpt)                    ; uses: bounds-of-thing-at-point
(require 'pel--base)                    ; uses: pel-whitespace-in-str-p

;;-pel-autoload
(defun pel-rst-set-adornment (style)
  "Set the reStructuredText adornment STYLE.
Set it to one of: 'CRiSPeR, 'Sphinx-Python, or 'default."
  (when (boundp 'rst-preferred-adornments)
    (setq rst-preferred-adornments (cond ((eq style 'default)
                                          '((?= over-and-under 1)
                                            (?= simple 0)
                                            (?- simple 0)
                                            (?~ simple 0)
                                            (?+ simple 0)
                                            (?` simple 0)
                                            (?# simple 0)
                                            (?@ simple 0)))
                                         ;; ref: http://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html#sections
                                         ((eq style 'Sphinx-Python)
                                          '((?# over-and-under 0)  ; for parts
                                            (?* over-and-under 0)  ; for chapters
                                            (?= simple 0)          ; for sections
                                            (?- simple 0)          ; for subsections
                                            (?^ simple 0)          ; for subsubsections
                                            (?\" simple 0)))       ; for paragraph
                                         ((eq style 'CRiSPer)
                                          '((?= over-and-under 0)    ; level  0 : title
                                            (?= simple 0)            ; level  1
                                            (?- simple 0)            ; level  2
                                            (?~ simple 0)            ; level  3
                                            (?^ simple 0)            ; level  4
                                            (?+ simple 0)            ; level  5
                                            (?* simple 0)            ; level  6
                                            (?> simple 0)            ; level  7
                                            (?< simple 0)            ; level  8
                                            (?_ simple 0)            ; level  9
                                            (?# simple 0)))))))      ; level 10


;; -----------------------------------------------------------------------------
;; reStructuredText reference location bookmark management
;; -------------------------------------------------------
;;
;; Call hierarchy:
;;
;; - pel-rst-set-ref-bookmark
;; - pel--rst-bookmark-exists-p
;;   - pel-rst-ref-bookmark-name

;; * pel-rst-makelink
;;   - pel-rst-anchor-escaped
;;   - pel-goto-next-empty-line
;;   - pel-rst-goto-ref-bookmark
;;   - pel--rst-bookmark-exists-p

(defun pel-rst-ref-bookmark-name ()
  "Return the bookmark name string used for the current file.
This bookmark identifies the location for the next reStructuredText reference."
  (format "RST-%s" (pel-current-buffer-filename)))

;;-pel-autoload
(defun pel-rst-set-ref-bookmark ()
  "Set the reference bookmark for the currently edited file at point.
Used to identify the location where the next invocation of \\[pel-rst-makelink]
inserts fully expanded links.
Ensures the bookmark is at the beginning of an empty line which is followed
by another empty line, by inserting 2 lines and placing the point at the
beginning of the first of the 2 lines."
  (interactive "*")
  (move-beginning-of-line nil)
  (unless  (or (equal (pel-chars-at-point 2) "\n\n")
               (equal (pel-chars-at-point 4) ".. _"))
    (insert "\n\n")
    (forward-line -2))
  (bookmark-set (pel-rst-ref-bookmark-name)))

(defun pel--rst-bookmark-exists-p ()
  "Return non-nil if the RST bookmark exists, nil otherwise."
  (if (and (require 'pel-bookmark nil :noerror)
           (fboundp 'pel-bookmark-in-current-file-p))
      (pel-bookmark-in-current-file-p (pel-rst-ref-bookmark-name))
    (error "pel-bookmark-in-current-file-p not loaded")))

;;-pel-autoload
(defun pel-rst-goto-ref-bookmark ()
  "Move point to the reference bookmark.
Useful to see where the bookmark for storing the hyperlink are currently
located."
  (interactive)
  (if (pel--rst-bookmark-exists-p)
      (bookmark-jump (pel-rst-ref-bookmark-name))
    (user-error "The bookmark does not exists yet.  Please create it!")))

(defun pel-goto-next-empty-line ()
  "Move point to the beginning of the next empty line."
  (if (search-forward "\n\n" nil :noerror)
      (forward-line -1)
    (user-error "No (next) empty line found")))

(defun pel-rst-anchor-escaped (text)
  "Return the TEXT escaped, ready to be used in a reStructuredText anchor.
- Replaces each colon with a back-slash escaped colon."
  (replace-regexp-in-string ":" "\\:" text nil :literal))

;;-pel-autoload
(defun pel-rst-makelink (&optional arg)
  "Create reStructuredText hyperlink prefix for word at point or region's text.
- If a region is active, use the text of the region to make the link, otherwise
  use the word at point.
- If an argument (ARG, which can be a \\[universal-argument]) is specified,
  use the embedded URI format.
- If no argument is specified, use the named hyperlink format:
  - if the region is a single word, just append an underscore to make the link
  - if the region is several words, surround the region with the \"`\" start
    string and the \"`_\" end string.
- The named link is placed in the location of bookmark named \"RST\" if it
  exists and points to same file, otherwise the link is placed at the
  beginning of the next empty line.
- The cursor is placed where the URL is to be written.
- You can return to the location of the link by typing \\[pel-jump-to-mark].

Reference: see reStructuredText hyperlink format at URL
`https://docutils.sourceforge.io/docs/ref/rst/restructuredtext.html#hyperlink-references'"
  (interactive "*P")
  (let* ((p_begin (if (region-active-p) (region-beginning) (car (bounds-of-thing-at-point 'word))))
         (p_end   (if (region-active-p) (region-end) (cdr (bounds-of-thing-at-point 'word))))
         (anchor (buffer-substring-no-properties p_begin p_end))
         (anchor_isa_single_word (not (pel-whitespace-in-str-p anchor))))
    (deactivate-mark)
    (if arg
        ;; Use embedded URI format
        (progn
          (goto-char p_begin)
          (insert "`")
          (goto-char p_end)
          (right-char 1)
          (insert (format " <>`_"))
          (left-char 3))
      ;; Use the separate reference format.
      ;; For that, require the presence of the bookmark.
      (if (not (pel--rst-bookmark-exists-p))
          (user-error "Please set location of hyperlink bookmark first!"))
      (if anchor_isa_single_word
          (progn
            (goto-char p_end)
            (insert "_"))
        (goto-char p_begin)
        (insert "`")
        (goto-char p_end)
        (right-char 1)
        (insert "`_"))
      ;; place references starting at the bookmark, one after the other, with
      ;; first reference created at the top.  All references are placed after
      ;; the bookmark so that the bookmark never moves, making it easier to
      ;; undo editing without damaging the bookmark location.
      ;; The bookmark must be set to an area in the buffer with 2 empty
      ;; lines, with the bookmark at the beginning of the first one.
      (pel-rst-goto-ref-bookmark)
      (forward-line 1)
      (if (not (eq (char-after) 10))
          (pel-goto-next-empty-line))
      (insert (format ".. _%s: \n" (pel-rst-anchor-escaped anchor)))
      (move-end-of-line 0))))

;; -----------------------------------------------------------------------------
(provide 'pel-rst)

;;; pel-rst.el ends here
