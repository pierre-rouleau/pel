;;; pel-rst.el --- PEL reStructuredText support -*-lexical-binding: t; -*-

;; Copyright (C) 2020, 2021, 2022  Pierre Rouleau

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
;;
;; This file contains defintions to extend the support of reStructuredText
;; files.
;;
;;
;; - Section Adornment Control
;;
;;  * `pel-rst-adorn-decrease-level'
;;  * `pel-rst-adorn-increase-level'
;;  * `pel-rst-adorn-refresh'
;;  * `pel-rst-adorn-same-level'
;;    - `pel--line-adorned-p'
;;    - `pel--rst-adorn-same-as-previous'
;;      - `pel--rst-adorn-level-of-previous-section'
;;    - `pel--rst-adorn-change'
;;      - `pel--rst-delete-whole-line'
;;        - `pel--rst-level-for'
;;  * `pel-rst-adorn-10'
;;  * `pel-rst-adorn-9'
;;  * `pel-rst-adorn-8'
;;  * `pel-rst-adorn-7'
;;  * `pel-rst-adorn-6'
;;  * `pel-rst-adorn-5'
;;  * `pel-rst-adorn-4'
;;  * `pel-rst-adorn-3'
;;  * `pel-rst-adorn-2'
;;  * `pel-rst-adorn-1'
;;  * `pel-rst-adorn-title'
;;    * `pel-rst-adorn'
;;  - `pel-delete-trailing-whitespace'
;;      - `pel-current-line-length'
;;  * `pel-rst-adorn-CRiSPer'
;;  * `pel-rst-adorn-Sphinx-Python'
;;  * `pel-rst-adorn-default'
;;    - `pel--rst-activate-adornment-style'
;;      - `pel-rst-set-adornment'
;;
;;
;; - Link/reference location bookmark management:
;;
;;   * `pel-rst-set-ref-bookmark'
;;   * `pel-rst-goto-ref-bookmark'
;;     - `pel--rst-bookmark-exists-p'
;;       - `pel-rst-ref-bookmark-name'
;;   * `pel-rst-makelink'
;;     - `pel-rst-anchor-escaped'
;;     - `pel-goto-next-empty-line'
;;     - `pel-rst-goto-ref-bookmark'
;;     - `pel--rst-bookmark-exists-p'
;;
;;
;; - Emphasis markup support:
;;
;;   * `pel-rst-interpreted'
;;   * `pel-rst-literal'
;;   * `pel-rst-italic'
;;   * `pel-rst-bold'
;;     - `pel--rst-emphasize-with'
;;
;; - Open link URL:
;;
;;   * `pel-rst-open-target'
;;     - `pel--move-to-rst-target'
;;     - `pel--rst-reference-target'
;;       - `pel-at-rst-reference-p'

;; -----------------------------------------------------------------------------
;;; Dependencies:

(require 'pel--base)        ; uses: pel-whitespace-in-str-p
;;                          ;       pel-chars-at-point
(require 'pel--options)
(require 'pel-whitespace)   ; uses: pel-delete-trailing-whitespace
(require 'pel--macros)
(require 'rst)              ; rst-mode code. Use rst-backward-section

;; -----------------------------------------------------------------------------
;;; Code:

;; Utility
;; -------
(defun pel--rst-require-thingatpt ()
  "Load thingatpt/."
  (message "pel--rst-require-thingatpt")
  (unless (and (require 'thingatpt nil :noerror)
               (fboundp 'bounds-of-thing-at-point))
    (user-error "Failed loading thingatpt!")))

;; ---------------------------------------------------------------------------
;; rst-mode character syntax control
;; ---------------------------------
;;

(defvar pel--rst-underscore-as-symbol nil
  "Remember if the underscore syntax is a symbol or not.")

(defun pel--rst-set-underscore-as-symbol ()
  "Set syntax of underscore character as symbol."
  (modify-syntax-entry ?_ "_" rst-mode-syntax-table))

(defun pel--rst-restore-underscore-syntax ()
  "Restore syntax of underscore character to punctuation."
  (modify-syntax-entry ?_ "." rst-mode-syntax-table))

;;-pel-autoload
(defun pel-rst-set-underscore-syntax (&optional action)
  "Set syntax of underscore to punctuation or symbol according to superword-mode.

If superword-mode is active then the function can be used to change the
syntax of the underscore character.

The optional ACTION  argument controls whether the underscore syntax is
toggled, activated or de-activated:
- ACTION not set or nil : toggles the underscore syntax.
- ACTION set positive:  activates underscore syntax.
- ACTIVATES set negative: de-activates underscore syntax.

By default the syntax of an underscore in rst-mode is a
punctuation.  To use the superword-mode the syntax of the
underscore must be symbol instead.

This function checks if the superword-mode is active and changes the syntax of
the underscore character to symbol if superword-mode is on, otherwise sets it
to the default: symbol."
  (interactive "P")
  (if (bound-and-true-p superword-mode)
      (cond
       ((not action)
        ;; Toggle
        (if (not pel--rst-underscore-as-symbol)
            (progn
              (pel--rst-set-underscore-as-symbol)
              (setq pel--rst-underscore-as-symbol t)
              (message "Underscore syntax is now: symbol"))
          (pel--rst-restore-underscore-syntax)
          (setq pel--rst-underscore-as-symbol nil)
          (message "Underscore syntax is now: punctuation")))
       ((> (prefix-numeric-value action) 0)
        ;; Activate underscore syntax
        (pel--rst-set-underscore-as-symbol)
        (setq pel--rst-underscore-as-symbol t)
        (message "Underscore syntax is now: symbol"))
       ((<= (prefix-numeric-value action) 0)
        ;; De-activate underscore syntax
        (pel--rst-restore-underscore-syntax)
        (setq pel--rst-underscore-as-symbol nil)
        (message "Underscore syntax is now: punctuation")))
    ;; superword-mode is off
    (user-error "superword-mode is turned off.  First turn it on!")))

;; ---------------------------------------------------------------------------
;; Section Adornment Control
;; -------------------------

;; Sphinx-Python style ref:
;; www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html#sections

(defvar pel--rst-used-adornment-style nil
  "Remember last adornment style value set by `pel-rst-set-adornment'.")

(defun pel-rst-set-adornment (style)
  "Set the reStructuredText adornment STYLE.
Set it to one of: 'CRiSPeR, 'Sphinx-Python, or 'default.
STYLE identifies the number of levels supported and their adornment.
- `default' is Emacs `rst-mode' default.  A title and 7 levels.
- `Sphinx-Python' is what Sphinx uses: 6 levels:
  - parts,
  - chapters,
  - sections,
  - subsections,
  - subsubsections,
  - paragraphs.
- `CRiSPer', a title and 12-level mode previously developed for CRiSP."
  (when
      (or
       (not pel--rst-used-adornment-style)
       (not (eq pel--rst-used-adornment-style style)))
    (pel-when-bound
     'rst-preferred-adornments
     (setq
      rst-preferred-adornments
      (cond ((eq style 'default)
             '((?= over-and-under 1)
               (?= simple 0)
               (?- simple 0)
               (?~ simple 0)
               (?+ simple 0)
               (?` simple 0)
               (?# simple 0)
               (?@ simple 0)))
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
     ;; re-set the value in case it was called from user explictly.
     (setq pel-rst-adornment-style style)
     ;; and remember last set value independently to reduce the number
     ;; of times it is set to only when it changes, which happens when
     ;; the function is called explicitly or when pel-rst-adornment-style
     ;; is set via file local variable assignment.
     (setq pel--rst-used-adornment-style style)
     (message "Now using the %s adornment style with %d levels supported."
              style
              (length rst-preferred-adornments)))))

(defun pel--rst-activate-adornment-style ()
  "Activate the adornment style selected by `pel-rst-adornment-style'."
  (pel-rst-set-adornment pel-rst-adornment-style))

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

(defun pel-current-line-length (&optional n)
  "Return number of characters inside the current line or N-1 lines forward.
If N is not specified, is nil or 1: return the length of the current line.
Otherwise return the line N-1 lines forward: so if N is 2 use next line,
if N is 0 use previous line, etc..."
  (- (line-end-position n) (line-beginning-position n)))

;; NOTE: The following could be an interactive command but then it should go
;; inside pel-whitespace.el or pel-ccp.el.  Since it's probably not going to
;; be that useful as a stand-alone command (because we can set automatic
;; deletion of all trailing whitespace inside current buffer), I'm leaving
;; it here until it's needed somewhere else.
;; Then I'll move it and put all infrastructure to autoload it and
;; then making it a stand-alone command will be worthwhile:
;; I want to reduce the dependencies to the maximum to reduce extra loading.
;;

(defun pel-rst-adorn (&optional level update)
  "Adorn the current line as a reStructuredText section at the specified LEVEL.
When UPDATE is non-nil do not add a new line after the underlining line,
but when UPDATE is nil, it adds a new line after the underlining.
`pel-rst-adorn' leaves the cursor unmoved, on the title line."
  (interactive "*p")
  (pel--rst-activate-adornment-style)
  (if (>= level (length rst-preferred-adornments))
      (user-error
       "Level %d not available in %s adornment style"
       level
       pel-rst-adornment-style))
  (save-excursion
    (pel-delete-trailing-whitespace)
    (let* ((linelen (pel-current-line-length))
           (adorn-level (nth level rst-preferred-adornments))
           (adorn-char (car adorn-level))
           (adorn-style (nth 1 adorn-level))
           (indent-steps (nth 2 adorn-level)))
      (move-end-of-line nil)
      (insert (format "\n%s"
                      (make-string linelen adorn-char)))
      (unless update
        (insert "\n"))
      (when (eq adorn-style 'over-and-under)
        (forward-line (if update -1 -2))
        (insert (format "\n%s\n"
                        (make-string linelen adorn-char))))
      ;; if the style requires indentation, indent the 3 lines
      (when (> indent-steps 0)
        (dotimes (_i indent-steps)
          (dolist (rel-line  '(0 2 2))
            (beginning-of-line rel-line)
            (insert " ")))))))

;; The following convenience functions defined to ease execution from keys

;;-pel-autoload
(defun pel-rst-adorn-title ()
  "Adorn current line with level-0 (title) reStructuredText section adornment.
If point is at the top of the file, the top adorn line is placed
on the first line of the file.

In all cases, a mark is left at the end of the title text line
and point is placed 2 lines below."
  (interactive "*")
  (let ((orig-line (line-number-at-pos)))
    (pel-rst-adorn 0)
    (when (< orig-line 2)
      ;; delete the empty line this creates when done at the top of the file
      (forward-line -2)
      (delete-char 1))
    ;; move point to end of title
    (forward-line 1)
    (end-of-line)
    (push-mark)
    (forward-line 2)
    (insert "\n")))

;;-pel-autoload
(defun pel-rst-adorn-1 ()
  "Adorn current line with level-1 reStructuredText section adornment."
  (interactive "*")
  (pel-rst-adorn 1))

;;-pel-autoload
(defun pel-rst-adorn-2 ()
  "Adorn current line with level-2 reStructuredText section adornment."
  (interactive "*")
  (pel-rst-adorn 2))

;;-pel-autoload
(defun pel-rst-adorn-3 ()
  "Adorn current line with level-3 reStructuredText section adornment."
  (interactive "*")
  (pel-rst-adorn 3))

;;-pel-autoload
(defun pel-rst-adorn-4 ()
  "Adorn current line with level-4 reStructuredText section adornment."
  (interactive "*")
  (pel-rst-adorn 4))

;;-pel-autoload
(defun pel-rst-adorn-5 ()
  "Adorn current line with level-5 reStructuredText section adornment."
  (interactive "*")
  (pel-rst-adorn 5))

;;-pel-autoload
(defun pel-rst-adorn-6 ()
  "Adorn current line with level-6 reStructuredText section adornment."
  (interactive "*")
  (pel-rst-adorn 6))

;;-pel-autoload
(defun pel-rst-adorn-7 ()
  "Adorn current line with level-7 reStructuredText section adornment."
  (interactive "*")
  (pel-rst-adorn 7))

;;-pel-autoload
(defun pel-rst-adorn-8 ()
  "Adorn current line with level-8 reStructuredText section adornment."
  (interactive "*")
  (pel-rst-adorn 8))

;;-pel-autoload
(defun pel-rst-adorn-9 ()
  "Adorn current line with level-9 reStructuredText section adornment."
  (interactive "*")
  (pel-rst-adorn 9))

;;-pel-autoload
(defun pel-rst-adorn-10 ()
  "Adorn current line with level-10 reStructuredText section adornment."
  (interactive "*")
  (pel-rst-adorn 10))

(defun pel--rst-level-for (char)
  "Return the level number for a specific CHAR, nil if not found.
Ignore the title level."
  (let ((level-number 1)
        (level-detected nil))
    (dolist (adorn-level (cdr rst-preferred-adornments))
      (unless level-detected
        (let ((adorn-char (car adorn-level)))
          (if (eq adorn-char char)
              (setq level-detected level-number)
            (setq level-number (1+ level-number))))))
    level-detected))

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

(defun pel--rst-adorn-level-of-previous-section ()
  "Return the adornment level of the previous section in the text."
  (save-excursion
    (rst-backward-section 1)
    (forward-line 1)
    (pel--rst-level-for (char-after))))

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
    ;; the line should have no trailing whitespace
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
        (let ((title-line-length (pel-current-line-length))
              (underline-length (progn
                                  (forward-line 1)
                                  (pel-current-line-length))))
          (when (equal title-line-length underline-length)
            (move-end-of-line nil)
            (backward-char 2)
            (pel--rst-level-for (char-after))))))))

;;-pel-autoload
(defun pel-rst-adorn-same-level ()
  "Adorn current line with the same level as the previous section.
If the line is already adorned, update the adornment:
adjust to previous section level."
  (interactive "*")
  (pel--rst-activate-adornment-style)
  (let ((previous-level (pel--rst-adorn-level-of-previous-section)))
    (if previous-level
        (progn
          (pel-delete-trailing-whitespace)
          (if (pel--line-adorned-p)
              (progn
                (save-excursion
                  (forward-line 1)
                  (pel--rst-delete-whole-line))
                (pel-rst-adorn previous-level :update))
            (pel-rst-adorn previous-level)))
      (user-error "Cannot detect section level of previous section!"))))

;;-pel-autoload
(defun pel-rst-adorn-refresh ()
  "Refresh the adornment of the current line.
This helps when the length of the line changes."
  (interactive "*")
  (pel--rst-activate-adornment-style)
  (let ((current-level (save-excursion
                         (forward-line 1)
                         (pel--rst-level-for (char-after)))))
    (if current-level
        (progn
          (pel-delete-trailing-whitespace)
          (save-excursion
            (forward-line 1)
            (pel--rst-delete-whole-line))
          (pel-rst-adorn current-level :update))
      (user-error "Cannot detect section level!"))))

;;-pel-autoload
(defun pel-rst-adorn-increase-level ()
  "Adorn current line at a higher-level that current if already adorned.
If the line is not already adorned, adorn it with a level higher than
previous section."
  (interactive)
  (pel--rst-activate-adornment-style)
  (pel-delete-trailing-whitespace)
  (if (pel--line-adorned-p)
      (pel--rst-adorn-change 1)
    (pel--rst-adorn-same-as-previous 1)))

;;-pel-autoload
(defun pel-rst-adorn-decrease-level ()
  "Adorn current line at a lower-level than current if already adorned.
If the line not already adorned, adorn it with a level lower than
previous section."
  (interactive)
  (pel--rst-activate-adornment-style)
  (pel-delete-trailing-whitespace)
  (if (pel--line-adorned-p)
      (pel--rst-adorn-change -1)
    (pel--rst-adorn-same-as-previous -1)))

;; -----------------------------------------------------------------------------
;; Link/reference location bookmark management
;; -------------------------------------------
;;
;; Call hierarchy:
;;
;; * `pel-rst-set-ref-bookmark'
;;   - `pel-rst-ref-bookmark-name'
;;   - `pel--bookmark-exists-p'
;;
;; * `pel-rst-goto-ref-bookmark'
;;   - `pel--rst-bookmark-exists-p'
;;     - `pel-rst-ref-bookmark-name'
;;
;; * `pel-rst-makelink'
;;   - `pel--rst-bookmark-exists-p'
;;   - `pel--space-for-extra-link-p'
;;     - `pel-forward-empty-line-p'
;;   - `pel-rst-goto-ref-bookmark'
;;   - `pel-goto-next-empty-line'
;;   - `pel-rst-anchor-escaped'

(defvar-local pel--rst-file-was-saved-once-p nil
  "Remember if the rst file was saved once when creating a bookmark.")

(defvar pel--bookmark-file-loaded-p nil
  "Remember if the bookmark file was loaded once explicitly.")

(defun pel-rst-ref-bookmark-name ()
  "Return the bookmark name string used for the current file.
This bookmark identifies the location for the next reStructuredText reference."
  ;; Make sure the file has been saved once when creating a RST bookmark for
  ;; it, otherwise trying to create a hyperlink will prompt to rename the bookmark.
  ;; Also on the very first call, force loading the bookmarks to ensure that an old
  ;; bookmark for that file will be known.
  (when (and (buffer-modified-p)
             (not pel--rst-file-was-saved-once-p))
    (save-buffer)
    (setq pel--rst-file-was-saved-once-p t))
  ;; Although the docs states that the bookmark file is loaded automatically
  ;; for some reason sometimes it's not.  So make sure it is because the file
  ;; may have a bookmark for the edited reStructuredText file.
  ;; If it was loaded don't load it again as this would create duplicates.
  (unless pel--bookmark-file-loaded-p
    (when (and (require 'bookmark nil :no-error)
               (boundp 'bookmark-default-file)
               (fboundp 'bookmark-load)
               (fboundp 'bookmark-all-names))
      ;; if there are bookmark names, its' already loaded.
      (unless (bookmark-all-names)
        (bookmark-load bookmark-default-file))
      (setq pel--bookmark-file-loaded-p t)))
  (format "RST-%s" (pel-current-buffer-filename)))

(defun pel--bookmark-exists-p (bookmark-name)
  "Return non-nil if BOOKMARK-NAME is already present in the bookmark file.
Return nil otherwise."
  (if (and (require 'bookmark ` nil :noerror)
           (fboundp 'bookmark-all-names))
      (member bookmark-name (bookmark-all-names))
    (error "Cannot load bookmark.el to access bookmark-all-names!")))

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
  ;; If the bookmark already exists, replace it,
  ;; otherwise create a new one.
  (let ((rst-bookmark-name (pel-rst-ref-bookmark-name)))
    (when (pel--bookmark-exists-p rst-bookmark-name)
      (bookmark-delete rst-bookmark-name))
    (bookmark-set rst-bookmark-name)))

;; --

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

;; --

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

(defun pel-forward-empty-line-p ()
  "Return position right after next empty line below, nil otherwise."
  (save-excursion
    (search-forward "\n\n" nil :noerror)))

(defun pel--space-for-extra-link-p ()
  "Return t if there is space for an extra link, nil otherwise."
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
`https://docutils.sourceforge.io/docs/ref/rst/restructuredtext.html\
#hyperlink-references'"
  (interactive "*P")
  (pel--rst-require-thingatpt)
  (let (p_begin p_end)
    (if (region-active-p)
        (progn
          (setq p_begin (region-beginning))
          (setq p_end   (region-end)))
      (setq p_begin (car (bounds-of-thing-at-point 'word)))
      (setq p_end   (cdr (bounds-of-thing-at-point 'word))))
    (if (and (not p_begin) (not p_end))
        (user-error "Please select an anchor word or a region!")
      (let* ((anchor (buffer-substring-no-properties p_begin p_end))
             (anchor-isa-alnum (pel-alnum-p anchor)))
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
              (user-error "Please set location of hyperlink bookmark first \
with pel-rst-set-ref-bookmark!")
            (if (pel--space-for-extra-link-p)
                (progn
                  (if anchor-isa-alnum
                      (progn
                        (goto-char p_end)
                        (insert "_"))
                    (goto-char p_begin)
                    (insert "`")
                    (goto-char p_end)
                    (right-char 1)
                    (insert "`_"))
                  ;; place references starting at the bookmark, one after the
                  ;; other, with first reference created at the top.  All
                  ;; references are placed after the bookmark so that the
                  ;; bookmark never moves, making it easier to undo editing
                  ;; without damaging the bookmark location.  The bookmark must
                  ;; be set to an area in the buffer with 2 empty lines, with
                  ;; the bookmark at the beginning of the first one.
                  (pel-rst-goto-ref-bookmark)
                  (forward-line 1)
                  (if (not (eq (char-after) 10))
                      (pel-goto-next-empty-line))
                  (insert (format ".. _%s: \n" (pel-rst-anchor-escaped anchor)))
                  (move-end-of-line 0))
              (user-error "Bookmarked reference link area has no \
space for new entry!
Move there with pel-rst-goto-ref-bookmark then add lines!"))))))))

;; -----------------------------------------------------------------------------
;; Emphasis markup support
;; -----------------------

(defun pel--rst-emphasize-with (str)
  "Emphasize the current word or marked area using STR.
Leave point right after the emphasized text."
  (pel--rst-require-thingatpt)
  (let* ((p-begin (if (region-active-p)
                      (region-beginning)
                    (car (bounds-of-thing-at-point 'word))))
         (p-end (if (region-active-p)
                    (region-end)
                  (cdr (bounds-of-thing-at-point 'word)))))
    (deactivate-mark)
    (goto-char p-end)
    (insert str)
    (goto-char p-begin)
    (insert str)
    (goto-char p-end)
    (forward-char (* 2 (length str)))))


(defun pel-rst-bold ()
  "Mark current word or marked region bold.
Leave point after to the next character."
  (interactive "*")
  (pel--rst-emphasize-with "**"))

(defun pel-rst-italic ()
  "Mark current word or marked region italic.
Leave point after to the next character."
  (interactive "*")
  (pel--rst-emphasize-with "*"))

(defun pel-rst-literal ()
    "Mark current word or marked region literal.
Leave point after to the next character."
  (interactive "*")
  (pel--rst-emphasize-with "``"))

(defun pel-rst-interpreted ()
    "Mark current word or marked region interpreted.
Leave point after to the next character."
  (interactive "*")
  (pel--rst-emphasize-with "`"))

;; -----------------------------------------------------------------------------
;; Open Link URL
;; -------------
;;
;; Open the URL identified by the reStucturedText link.
;; This way, user does not have to move point to the URL: the URL can be opened
;; right from the link.

(defun pel-at-rst-reference-p (&optional pos)
  "Return t if POS (or point) is at a rst-reference character, nil otherwise."
  (setq pos (or pos (point)))
  (eq (get-text-property pos 'face) 'rst-reference))

(defun pel-at-rst-valid-ref-char-p (&optional pos)
  "Return t if character at POS is a valid reST reference character.

Return nil otherwise.  The '<' and '>' are invalid."
  (not (memq (char-after pos) '(?< ?>))))

(defun pel--rst-reference-target (&optional pos noerror)
  "Return the target string of the current reference if any.
Search at specified POS or at current point location.
If there is no reference issue a `user-error' unless NOERROR is non-nil,
in that case return nil instead.

Return a cons of 2 elements: ref-type and string.
- ref-type is either 'target or 'path.  It describes what the string
represents."
  (setq pos (or pos (point)))
  (if (pel-at-rst-reference-p pos)
      ;; find boundaries of this reference, go back to first char then to the
      ;; end
      (let ((beg pos)
            (end (1+ pos))
            (ref-type nil))
        (while (and (pel-at-rst-reference-p (1- beg))
                    (pel-at-rst-valid-ref-char-p (1- beg)))
          (setq beg (1- beg)))
        (when (equal (char-after beg) ?`)
          (setq beg (1+ beg)))
        (while (and (pel-at-rst-reference-p (1+ end))
                    (pel-at-rst-valid-ref-char-p (1+ end)))
          (setq end (1+ end)))
        (cond
         ((equal (char-before end) ?`)
          (setq end (1- end))
          (setq ref-type 'target))
         ((equal (char-after (1+ end)) ?>)
          (setq end (1+ end))
          (setq ref-type 'path)))
        (cons ref-type
              (buffer-substring-no-properties beg end)))
    (unless noerror
      (user-error "Point is not located over a rst-reference!"))))

(defun pel--move-to-rst-target (target)
  "Move point to the rst definition link for TARGET.

Return a list ('rst-title (point)) if the TARGET is a section title
hyperlink, non-nil if it is another hyperlink with a found target.
Return nil if no hyperlink target found."
  (goto-char (point-min))
  (let ((regexp (format "^\\.\\. _%s:" (pel-rst-anchor-escaped target))))
    ;; search for a complete reference (target and URL on the same line) first
    (if (re-search-forward (concat regexp " +") nil :noerror)
        t
      ;; if that fails search for a line that only has the target)
      (goto-char (point-min))
      (if (re-search-forward regexp nil :noerror)
          ;; if that's found move to the first complete reference line
          (re-search-forward ": +.+$" nil :noerror)
        ;; otherwise try to find a line that begins with the target
        ;; that might be a title
        (goto-char (point-min))
        (when (re-search-forward (format "^%s$" target)  nil :noerror)
          ;; It's a title target! Not a file target!
          ;; Just move point there and leave it there!
          (list 'rst-title (point)))))))


(defun pel--rst-target-regxp (target)
  "Transform TARGET string into a regular expression string to search for it."
  ;; In a reStructuredText section target, spaces, dashes and underscores are
  ;; replaced by a single dash.  So to search for the original section text
  ;; search for the string that begins at the beginning of the line and
  ;; expand the dashes with an expression that searches for any of the
  ;; possible original characters.
  (format "^%s$" (replace-regexp-in-string "-" "[ _-]+" target)))

(defun pel-html-to-rst (filename)
  "Extract source filename and potential target from HTML FILENAME.

Return a cons of (source-filename . target-regexp) where the
source-filename is the name of the file to visit, and target-regexp is a
regexp use to search the location of the target to move point to.  It is set
to nil if there is no target specification in the FILENAME.  A target
specification in FILENAME is the text that follows the '#' character.

The function removes the FILENAME extension if the extension is \"html\".
If the resulting FILENAME has an extension it tries to use that file name.
Otherwise it tries to append the \"rst\", \"txt\" or \"stxt\" extension and
uses the first one it finds. If it finds nothing it returns the FILENAME
unchanged.  If FILENAME extension is not \"html\" it returns FILENAME
unchanged."
  (let* ((filename.target  (split-string filename "#"))
         (filename (car filename.target))
         (target (cadr filename.target))
         (ext (file-name-extension filename)))
    (when target
      (setq target (pel--rst-target-regxp target)))
    (cons (if (string= ext "html")
              (let ((fn (file-name-sans-extension filename)))
                (if (file-exists-p fn)
                    fn
                  (let ((fn-ext nil)
                        (found-fn nil))
                    (dolist (new-ext '(".rst" ".txt" ".stxt"))
                      (unless found-fn
                        (setq fn-ext (concat fn new-ext))
                        (when (file-exists-p fn-ext)
                          (setq found-fn fn-ext))))
                    (if found-fn
                        found-fn
                      filename))))
            filename)
          target)))

(defun pel-rst-open-target (&optional n noerror)
  "Open the target of rst-reference at point.
If there is no target issue a `user-error' unless NOERROR is non-nil.
In that case just return nil.
Optionally identify a window to open a file reference with the argument N.
See `pel-find-file-at-point-in-window' for more information."
  (interactive "P")
  ;; It's possible the file visited by the current buffer is located in a
  ;; directory that is not the current directory. The links in the
  ;; reStructuredText file may refer to other files, with paths relative to
  ;; the location of the file in the buffer.  Therefore to process this
  ;; request we temporary change the current directory to the directory of the
  ;; file in the current buffer.
  (let ((original-cwd default-directory)
        (new-position nil))
    (save-excursion
      (unwind-protect
          (cd (file-name-directory (pel-current-buffer-filename)))
        ;; A reStructuredText link may have to be escaped in the reference,
        ;; therefore search for the potentially escaped reference target to
        ;; ensure we're able to handle all types of links.
        (let* ((reftype.string (pel--rst-reference-target))
               (reftype (car reftype.string))
               (result (if (eq reftype 'target)
                           (pel--move-to-rst-target
                            (pel-rst-anchor-escaped
                             (cdr reftype.string)))
                         (cdr reftype.string))))
          (if (eq reftype 'target)
              (cond
               ;; if found a ref to a title jump to it
               ((and (listp result) (eq (car result) 'rst-title))
                (setq new-position (cadr result)))
               ;; otherwise it's a link to a file: try to find it
               (t
                (if result
                    (if (and (require 'pel-file nil :noerror)
                             (fboundp 'pel-find-file-at-point-in-window))
                        (pel-find-file-at-point-in-window n (function pel-html-to-rst))
                      (user-error "Cannot load pel-file!"))
                  (unless noerror
                    (user-error "No reference target found!")))))
            ;; reftype is a path: use that path directly
            (if (and (require 'pel-file nil :noerror)
                     (fboundp 'pel-find-file-at-point-in-window))
                (pel-find-file-at-point-in-window n (function pel-html-to-rst))
              (user-error "Cannot load pel-file!"))))
        (cd original-cwd)))
    (when new-position
      (goto-char new-position))))

;; -----------------------------------------------------------------------------
(provide 'pel-rst)

;;; pel-rst.el ends here
