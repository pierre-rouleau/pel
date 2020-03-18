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
(require 'rst)                          ; rst-mode code. Use rst-backward-section

;; --
;; Section Adornment Control
;; -------------------------

(defun pel-rst-set-adornment (style)
  "Set the reStructuredText adornment STYLE.
Set it to one of: 'CRiSPeR, 'Sphinx-Python, or 'default.
STYLE identifies the number of levels supported and their adornment.
- `default' is Emacs rst-mode default. A title and 7 levels.
- `Sphinx-Python' is what Sphinx uses: 6 levels:
  - parts,
  - chapters,
  - sections,
  - subsections,
  - subsubsections,
  - paragraphs.
- `CRiSPer', a title and 12-level mode previously developed for CRiSP."
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
                                          '((?= over-and-under 0)  ; level  0 : title
                                            (?= simple 0)          ; level  1
                                            (?- simple 0)          ; level  2
                                            (?~ simple 0)          ; level  3
                                            (?^ simple 0)          ; level  4
                                            (?+ simple 0)          ; level  5
                                            (?* simple 0)          ; level  6
                                            (?> simple 0)          ; level  7
                                            (?< simple 0)          ; level  8
                                            (?_ simple 0)          ; level  9
                                            (?# simple 0)          ; level 10
                                            (?` simple 0)          ; level 11
                                            (?@ simple 0)))        ; level 12
                                         (t (user-error "Unsupported style %S" style))))
    (setq pel-rst-adornment-style style)
    (message "Now using the %s adornment style with %d levels supported."
             style
             (length rst-preferred-adornments))))


;;-pel-autoload
(defun pel-rst-adorn-default ()
  "Set the default section adornment style.
This is Emacs `rst-mode' default: a title with 7 levels."
  (interactive)
  (pel-rst-set-adornment 'default))

;;-pel-autoload
(defun pel-rst-adorn-Sphinx-Python ()
  "Set the Sphinx-Python section adornment style.
This is what Sphinx supports: 6 levels:
- parts,
- chapters,
- sections,
- subsections,
- subsubsections,
- paragraphs."
  (interactive)
  (pel-rst-set-adornment 'Sphinx-Python))

;;-pel-autoload
(defun pel-rst-adorn-CRiSPer ()
  "Set the CRiSPer section adornment style.
A title level with another 10 levels."
  (interactive)
  (pel-rst-set-adornment 'CRiSPer))

;; --

(defun current-line-length (&optional n)
  "Return number of characters inside the current line or N-1 lines forward.
If N is not specified, is nil or 1: return the length of the current line.
Otherwise return the line N-1 lines forward: so if N is 2 use next line,
if N is 0 use previous line, etc..."
  (- (line-end-position n) (line-beginning-position n)))

;; NOTE: The following could be an interactive command but then it should go inside
;; pel-whitespace.el or pel-ccp.el.  Since it's probably not going to be that
;; useful as a stand-alone command (because we can set automatic deletion of all
;; trailing whitespace inside current buffer), I'm leaving it here until it's needed
;; somewhere else.  Then I'll move it and put all infrastructure to autoload it and
;; then making it a stand-alone command will be worthwhile: I want to reduce the
;; dependencies to the maximum to reduce extra loading.
;;
(defun pel-delete-trailing-whitespace ()
  "Delete trailing whitespace on current line."
  (let ((line-start-pos (progn (forward-line 0) (point)))
        (line-end-pos   (progn (move-end-of-line nil))))
    (delete-trailing-whitespace line-start-pos line-end-pos)))

(defun pel-rst-adorn (&optional level)
  "Adorn the current line as a reStructuredText section at the specified LEVEL.
Leave the cursor unmoved, on the title line."
  (interactive "p")
  (if (>= level (length rst-preferred-adornments))
      (user-error
       "Level %d not available in %s adornment style"
       level
       pel-rst-adornment-style))
  (save-excursion
    (pel-delete-trailing-whitespace)
    (let* ((linelen (current-line-length))
           (adorn-level (nth level rst-preferred-adornments))
           (adorn-char (car adorn-level))
           (adorn-style (nth 1 adorn-level))
           (indent-steps (nth 2 adorn-level)))
      (move-end-of-line nil)
      (insert (format "\n%s\n"
                      (make-string linelen adorn-char)))
      (when (eq adorn-style 'over-and-under)
        (forward-line -2)
        (insert (format "\n%s\n"
                        (make-string linelen adorn-char))))
      ;; if the style requires indentation, indent the 3 lines
      (when (> indent-steps 0)
        (dotimes (i indent-steps)
          (dolist (rel-line  '(0 2 2))
            (beginning-of-line rel-line)
            (insert " ")))))))

;; The following convenience functions defined to ease execution from keys

;;-pel-autoload
(defun pel-rst-adorn-title ()
  "Adorn current line with level-0 (title) reStructuredText section adornment."
  (interactive)
  (pel-rst-adorn 0))

;;-pel-autoload
(defun pel-rst-adorn-1 ()
  "Adorn current line with level-1 reStructuredText section adornment."
  (interactive)
  (pel-rst-adorn 1))

;;-pel-autoload
(defun pel-rst-adorn-2 ()
  "Adorn current line with level-2 reStructuredText section adornment."
  (interactive)
  (pel-rst-adorn 2))

;;-pel-autoload
(defun pel-rst-adorn-3 ()
  "Adorn current line with level-3 reStructuredText section adornment."
  (interactive)
  (pel-rst-adorn 3))

;;-pel-autoload
(defun pel-rst-adorn-4 ()
  "Adorn current line with level-4 reStructuredText section adornment."
  (interactive)
  (pel-rst-adorn 4))

;;-pel-autoload
(defun pel-rst-adorn-5 ()
  "Adorn current line with level-5 reStructuredText section adornment."
  (interactive)
  (pel-rst-adorn 5))

;;-pel-autoload
(defun pel-rst-adorn-6 ()
  "Adorn current line with level-6 reStructuredText section adornment."
  (interactive)
  (pel-rst-adorn 6))

;;-pel-autoload
(defun pel-rst-adorn-7 ()
  "Adorn current line with level-7 reStructuredText section adornment."
  (interactive)
  (pel-rst-adorn 7))

;;-pel-autoload
(defun pel-rst-adorn-8 ()
  "Adorn current line with level-8 reStructuredText section adornment."
  (interactive)
  (pel-rst-adorn 8))

;;-pel-autoload
(defun pel-rst-adorn-9 ()
  "Adorn current line with level-9 reStructuredText section adornment."
  (interactive)
  (pel-rst-adorn 9))

;;-pel-autoload
(defun pel-rst-adorn-10 ()
  "Adorn current line with level-10 reStructuredText section adornment."
  (interactive)
  (pel-rst-adorn 10))

(defun pel--rst-level-for (char)
  "Return the level number for a specific CHAR, nil if not found.
Ignore the title level."
  (let ((level-number 1)
        (level-detected nil))
    (dolist (adorn-level (cdr rst-preferred-adornments))
      (when (not level-detected)
        (let ((adorn-char (car adorn-level)))
          (if (eq adorn-char char)
              (setq level-detected level-number)
            (setq level-number (1+ level-number))))))
    level-detected))

(defun pel--rst-adorn-level-of-previous-section ()
  "Return the adornment level of the previous section in the text."
  (save-excursion
    (rst-backward-section 1)
    (forward-line 1)
    (pel--rst-level-for (char-after))))

(defun pel--rst-adorn-relative-to-other (level)
  "Adorn current line with section relative LEVEL.
- If LEVEL is 0: use same level as previous section.
- If LEVEL is 1: use a higher level to create a sub-section.
- If LEVEL is -1: use a lower level to create a parent section."
  (let ((other-level (pel--rst-adorn-level-of-previous-section)))
    (if other-level
        (pel-rst-adorn (+ other-level level))
      (user-error "Cannot detect section level of previous section"))))

(defun pel--rst-delete-whole-line ()
  "Local copy of delete-whole-line: delete the current line."
  (delete-region (line-beginning-position)
                 (min (point-max) (1+ (line-end-position)))))

(defun pel--rst-adorn-change (step)
  "Change adornment of current line by STEP level.
- If STEP is 1: increase level.
- If STEP is -1: decrease level."
  (save-excursion
    (let ((current-level (progn
                           (forward-line 1)
                           (pel--rst-level-for (char-after)))))
      (pel--rst-delete-whole-line)
      (forward-line -1)
      (pel-rst-adorn (+ current-level step))
      ;; prevent adding another line each time the level is changed
      (end-of-line 2)
      (delete-char 1 nil))))


(defun pel--rst-adorn-same-as-previous  (step)
  "Adorn current line at the STEP level compared to the previous adornment."
  (let ((level (pel--rst-adorn-level-of-previous-section)))
    (if level
        (pel-rst-adorn (+ level step))
      (user-error "Cannot detect section level of previous section!"))))

(defun pel--line-adorned-p ()
  "Return t if current line is section-adorned, nil otherwise.
REQUIREMENT: the line must not have trailing whitespaces."
  (save-excursion
    ;; the line has no trailing whitespace
    ;; check a character 2 char before end of line to be safe
    ;; since line might be indented (for some styles) and the
    ;; rst-level-x property goes up to end of line (but not on
    ;; the end-of-line character).
    (move-end-of-line nil)
    (backward-char 2)
    (let ((point-text-face-property (get-text-property (point) 'face)))
      (if (and point-text-face-property
               (listp point-text-face-property))
          ;; current line is adorned with the style supported by rst-mode
          ;; so just check the adornment text face property to get the level
          (car (memq
                (car point-text-face-property)
                '(rst-level-1
                  rst-level-2
                  rst-level-3
                  rst-level-4
                  rst-level-5
                  rst-level-6)))
        ;; The level is not adorned, check character in the underline
        ;; to detect the level (if there is one). Check that the underline
        ;; is the same length as the title line and check its last char.
        ;; It's not foolproof, but probably OK for most cases.
        (let ((title-line-length (current-line-length))
              (underline-length (progn (forward-line 1) (current-line-length))))
          (when (equal title-line-length underline-length)
            (move-end-of-line nil)
            (backward-char 2)
            (pel--rst-level-for (char-after))))))))

;;-pel-autoload
(defun pel-rst-adorn-same-level ()
  "Adorn current line with the same level as the previous section.
If the line is already adorned, update the adornment: adjust to previous section level."
  (interactive)
  (let ((previous-level (pel--rst-adorn-level-of-previous-section)))
    (if previous-level
        (progn
          (pel-delete-trailing-whitespace)
          (when (pel--line-adorned-p)
            (save-excursion
              (forward-line 1)
              (pel--rst-delete-whole-line)))
          (pel-rst-adorn previous-level))
      (user-error "Cannot detect section level of previous section!"))))

;;-pel-autoload
(defun pel-rst-adorn-increase-level ()
  "Adorn current line at a higher-level that current if already adorned.
If the line is not already adorned, adorn it with a level higher than previous section."
  (interactive)
  (pel-delete-trailing-whitespace)
  (if (pel--line-adorned-p)
      (pel--rst-adorn-change 1)
    (pel--rst-adorn-same-as-previous 1)))

;;-pel-autoload
(defun pel-rst-adorn-decrease-level ()
  "Adorn current line at a lower-level than current if already adorned.
If the line not already adorned, adorn it with a level lower than previous section."
  (interactive)
  (pel-delete-trailing-whitespace)
  (if (pel--line-adorned-p)
      (pel--rst-adorn-change -1)
    (pel--rst-adorn-same-as-previous -1)))

;; -----------------------------------------------------------------------------
;; reStructuredText reference location bookmark management
;; -------------------------------------------------------
;;
;; Call hierarchy:
;;
;; * pel-rst-set-ref-bookmark
;; * pel-rst-goto-ref-bookmark
;;   - pel--rst-bookmark-exists-p
;;     - pel-rst-ref-bookmark-name
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
    (error "Function pel-bookmark-in-current-file-p not loaded")))

;;-pel-autoload
(defun pel-rst-goto-ref-bookmark ()
  "Move point to the reference bookmark.
Useful to see where the bookmark for storing the hyperlink are currently
located."
  (interactive)
  (if (pel--rst-bookmark-exists-p)
      (progn
        (push-mark)
        (bookmark-jump (pel-rst-ref-bookmark-name)))
    (user-error "The bookmark does not exists yet.  \
Use pel-set-ref-bookmark to create it!")))

(defun pel-forward-empty-line-p ()
  "Return position right after next empty line below, nil otherwise."
    (save-excursion
      (search-forward "\n\n" nil :noerror)))

(defun pel-goto-next-empty-line (&optional no-error)
  "Move point to the beginning of the next empty line if there is one.
If there is no empty line, don't move point and raise an error unless
NO-ERROR is non-nil."
  (let ((original-pos (point)))
    (if (search-forward "\n\n" nil :noerror)
        (forward-line -1)
      (goto-char original-pos)
      (unless no-error
        (user-error "No (next) empty line found")))))

(defun pel-rst-anchor-escaped (text)
  "Return the TEXT escaped, ready to be used in a reStructuredText anchor.
- Replaces each colon with a back-slash escaped colon."
  (replace-regexp-in-string ":" "\\:" text nil :literal))


(defun pel--space-for-extra-link-p ()
  "Return t if there is spec for extra link, nil otherwise."
  (save-excursion
    (pel-rst-goto-ref-bookmark)
    (forward-line 1)
    (if (not (eq (char-after) 10))
        (pel-forward-empty-line-p)
      t)))

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
  (let* ((p_begin (if (region-active-p)
                      (region-beginning)
                    (car (bounds-of-thing-at-point 'word))))
         (p_end   (if (region-active-p)
                      (region-end)
                    (progn
                      ;; if no region already exists, set the mark to allow quick return
                      (push-mark)
                      ;; then return end of word as the end position
                      (cdr (bounds-of-thing-at-point 'word)))))
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
      (if (pel--space-for-extra-link-p)
          (progn
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
            (move-end-of-line 0))
        (user-error "Bookmarked reference link area has no space for new entry!  \
Move there with pel-rst-goto-ref-bookmark then add lines!")))))

;; -----------------------------------------------------------------------------
(provide 'pel-rst)

;;; pel-rst.el ends here
