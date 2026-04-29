;;; pel-rst.el --- PEL reStructuredText support -*-lexical-binding: t; -*-

;; Copyright (C) 2020-2026  Pierre Rouleau

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

;; ---------------------------------------------------------------------------
;;; Commentary:
;;
;; This file contains defintions to extend the support of reStructuredText
;; files. The following call hierarchy list the commands (*), and the
;; functions (-) they call.  The (.) markers identify function hierarchy
;; reference that are not expanded further because that's done somewhere else.
;;
;; - rst-mode character syntax control
;;
;;   * `pel-rst-set-underscore-syntax'
;;     - `pel--rst-set-underscore-as-symbol'
;;     - `pel--rst-set-underscore-as-punctuation'
;;
;;
;; - Section Adornment Control
;;
;;  - Adornment Style Selection Control
;;  * `pel-rst-adornment-style-info'
;;    - `pel-rst-used-adornment-style'
;;
;;  * `pel-rst-adorn-CRiSPer'        -> `pel-rst-set-adornment'
;;  * `pel-rst-adorn-Sphinx-Python'  -> `pel-rst-set-adornment'
;;  * `pel-rst-adorn-default'        -> `pel-rst-set-adornment'
;;
;;  - Line Adornment Control - Adorn line at specified level
;;  * `pel-rst-adorn-title'          -> `pel-rst-adorn'
;;  * `pel-rst-adorn-1'              -> `pel-rst-adorn'
;;  * `pel-rst-adorn-2'              -> `pel-rst-adorn'
;;  * `pel-rst-adorn-3'              -> `pel-rst-adorn'
;;  * `pel-rst-adorn-4'              -> `pel-rst-adorn'
;;  * `pel-rst-adorn-5'              -> `pel-rst-adorn'
;;  * `pel-rst-adorn-6'              -> `pel-rst-adorn'
;;  * `pel-rst-adorn-7'              -> `pel-rst-adorn'
;;  * `pel-rst-adorn-8'              -> `pel-rst-adorn'
;;  * `pel-rst-adorn-9'              -> `pel-rst-adorn'
;;  * `pel-rst-adorn-10'             -> `pel-rst-adorn'
;;    - `pel-rst-adorn'
;;      - `pel--rst-level-valid-p'
;;      - `pel--rst-level-adorn-level-style'
;;
;;  - Line Adornment Control - Add/Adjust Adornment Line
;;    - `pel--rst-adorn-change'
;;      - `pel--rst-level-at'
;;      . `pel--rst-level-adorn-level-style'
;;      - `pel--rst-adorn-level+='
;;        - `pel--rst-level-valid-p'
;;      - `pel--rst-delete-whole-line'
;;    - `pel--rst-adorn-same-as-previous'
;;      . `pel--rst-adorn-level-of-previous-section'
;;      . `pel--rst-adorn-level+='
;;      . `pel-rst-adorn'
;;  * `pel-rst-adorn-same-level'
;;    - `pel--rst-adorn-level-of-previous-section'
;;      - `pel--rst-level-at'
;;    - `pel--rst-level-at'
;;    . `pel--rst-adorn-change'
;;    . `pel-rst-adorn'
;;  * `pel-rst-adorn-refresh'
;;    . `pel--rst-adorn-change'
;;  * `pel-rst-adorn-increase-level'
;;    - `pel--line-adorned-p'
;;    . `pel--rst-adorn-change'
;;    . `pel--rst-adorn-same-as-previous'
;;  * `pel-rst-adorn-decrease-level'
;;    - `pel--line-adorned-p'
;;    . `pel--rst-adorn-change'
;;    . `pel--rst-adorn-same-as-previous'
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
;;
;; - Open link URL:
;;
;;   - `pel-rst-open-file-at-point'
;;     - `pel-at-rst-reference-p'
;;     * `pel-rst-open-target'
;;       - `pel--move-to-rst-target'
;;       - `pel--rst-reference-target'
;;         . `pel-at-rst-reference-p'
;;
;; - Table Helper Utility:
;;
;;   * `pel-rst-table-dup-separator-lines'
;;
;; - Output generation: compilation support
;;
;;   * `pel-rst-compile'

;; ---------------------------------------------------------------------------
;;; Dependencies:

(require 'pel--base)        ; use: `pel-whitespace-in-str-p'
;;                          ;      `pel-chars-at-point'
(require 'pel--options)     ; use: `pel-rst-adornment-style'
(require 'pel-whitespace)   ; use: `pel-delete-trailing-whitespace'
(require 'pel-ccp)          ; use: `pel-delete-whole-line', `pel-duplicate-line'
(require 'pel--macros)
(require 'pel-file)         ; use: `pel-find-file-at-point-in-window'
(require 'rst)              ; rst-mode code. Use `rst-backward-section', `rst-preferred-adornments'
(eval-when-compile
  (require 'subr-x))        ; use: split-string

;; ---------------------------------------------------------------------------
;;; Code:

;; Utility
;; -------
(defun pel--rst-require-thingatpt ()
  "Load thingatpt when necessary."
  (message "pel--rst-require-thingatpt")
  (unless (and (require 'thingatpt nil :noerror)
               (fboundp 'bounds-of-thing-at-point))
    (user-error "Failed loading thingatpt!")))

;; ---------------------------------------------------------------------------
;;* rst-mode character syntax control
;;  =================================
;;

(defun pel--rst-set-underscore-as-symbol (&optional quiet)
  "Set syntax of underscore character as symbol."
  (modify-syntax-entry ?_ "_" rst-mode-syntax-table)
  (unless quiet
    (message "Underscore syntax is now: symbol")))

(defun pel--rst-set-underscore-as-punctuation (&optional quiet)
  "Restore syntax of underscore character to punctuation."
  (modify-syntax-entry ?_ "." rst-mode-syntax-table)
  (unless quiet
    (message "Underscore syntax is now: punctuation")))

;;-pel-autoload
(defun pel-rst-set-underscore-syntax (&optional action)
  "Set underscore syntax to punctuation or symbol according to superword-mode.

If superword-mode is active then the function can be used to change the
syntax of the underscore character from punctuation (the default) to symbol
and vice versa.  If superword-mode is not active the function signals a
user-error.

The optional ACTION  argument controls whether the underscore syntax is
toggled, activated or de-activated:
- ACTION not set or nil : toggles the underscore between symbol and
                          punctuation syntax.
- ACTION set positive   : activates symbol syntax where underscore is part of
                          words.
- ACTIVATES set negative: Activate punctuation syntax where underscore is not
                          part of words.

Activate underscore as symbol to make underscore part of words."
  (interactive "P")
  (if (bound-and-true-p superword-mode)
      (let ((und-syntax-is-symbol (eq (char-syntax ?_) ?_)))
        (cond
         ;; Toggle underscore syntax
         ((not action)
          (if und-syntax-is-symbol
              (pel--rst-set-underscore-as-punctuation)
            (pel--rst-set-underscore-as-symbol)))
         ;;
         ;; Activate underscore as symbol syntax
         ((> (prefix-numeric-value action) 0)
          (pel--rst-set-underscore-as-symbol))
         ;;
         ;; De-activate underscore as punctuation syntax
         ((<= (prefix-numeric-value action) 0)
          (pel--rst-set-underscore-as-punctuation))))
    ;; superword-mode is off
    (user-error "superword-mode is turned off.  First turn it on!")))

;; ---------------------------------------------------------------------------
;;* Section Adornment Control
;;  =========================
;;
;; The section adornment control is buffer specific, allowing each
;; reStructuredText buffer to use a different scheme if necessary.  The
;; default adornment style used by PEL is set by the `pel-rst-set-adornment'
;; user-option.  PEL applies it to the `rst-mode' `rst-preferred-adornments'
;; user-option dynamically: in the local buffer value for the current buffer.
;;
;; PEL provides commands to further change the adornment style in the current
;; buffer and also globally.  The global change does not affect the style used
;; in the currently opened rst-mode buffers but affects the adornment style of
;; the rst-mode buffers opened later in the current Emacs editing session.
;; The change does not persist across Emacs editing session.
;;
;; PEL supports 3 different style:
;;
;; - The default rst-mode style, identified by the value of the
;;   `rst-preferred-adornments' user-option.  The `rst-mode' default for this
;;   is a style with a title level with 7 other levels.  The user can change
;;   this (since it can be customized).  PEL honors the setting when the
;;   default style is selected.
;;
;; - The Sphinx-Python style, consisting of 6 levels.
;;   See:
;;   www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html#sections
;;
;; - The CRiSPer style; a style with a title level and 12 other levels.  This
;;   is a style I wrote long ago for CRiSPer, a extension package for the
;;   CRiSP editor (a successor of Underware Brief editor).  Something I used
;;   before using Emacs.

;; --
;;** Adornment Style Selection Control
;;   ---------------------------------

(defvar-local pel--rst-used-adornment-style nil
  "Adornment style currently used in the buffer.

Activation of the section adornment can only be changed by a call to the
`pel-rst-set-adornment' function because real activation is done by changing
the value of the rst-mode `rst-preferred-adornments' user-option.")


(defun pel-rst-used-adornment-style ()
  "Return adornment style used by current buffer."
  pel--rst-used-adornment-style)

;;-pel-autoload
(defun pel-rst-adornment-style-info ()
  "Display which adornment style is used."
  (interactive)
  (message "Used PEL adornment style: %s, with %d levels."
           pel--rst-used-adornment-style
           (length rst-preferred-adornments)))


;; --

;;-pel-autoload
(defun pel-rst-set-adornment (style &optional globally quiet)
  "Set the reStructuredText adornment STYLE.

STYLE identifies the number of levels supported and their adornment.
- \\='default is Emacs `rst-mode' default identified by
  `rst-preferred-adornments' user-option which defaults to a title and 7
  levels.
- \\='Sphinx-Python is what Sphinx uses: 6 levels:
  - parts,
  - chapters,
  - sections,
  - subsections,
  - subsubsections,
  - paragraphs.
- \\='CRiSPer, a title and 12-level mode previously developed for CRiSP.
- \\='pel-default identifies one of the above 3 styled as selected by the
  `pel-rst-adornment-style' user-option.

When GLOBALLY is non-nil, apply the change to all buffers by updating
the global default value of `rst-preferred-adornments'.  When GLOBALLY
is nil (the default), the change is buffer-local and affects only the
current buffer.

The function prints a message when the style changes unless the QUIET argument
is non-nil."
  ;; Validate style eagerly so an unknown STYLE always raises user-error,
  ;; independent of whether rst-preferred-adornments is currently bound.
  (unless (memq style '(CRiSPer Sphinx-Python default pel-default))
    (user-error "Unsupported style %S" style))
  ;; Change it only if the requested value differs from what is currently
  ;; used.
  (when (eq style 'pel-default)
    (setq style pel-rst-adornment-style))

  ;; Ensure rst is loaded and the current buffer has a concrete local slot
  ;; for `rst-preferred-adornments` before we assign to it. This avoids
  ;; Emacs-version/platform timing differences in how rst-mode localizes
  ;; the user option (observed on Emacs 26.1 and some macOS builds).
  (require 'rst nil :noerror)
  (when (and (not globally) (boundp 'rst-preferred-adornments)
             (not (local-variable-p 'rst-preferred-adornments)))
    (make-local-variable 'rst-preferred-adornments)
    (setq rst-preferred-adornments (default-value 'rst-preferred-adornments)))

  (let ((old-adornments rst-preferred-adornments)
        (new-adornments
         (cond ((eq style 'default)
                '((?= over-and-under 1) ; level 0 : title
                  (?= simple 0)         ; level 1
                  (?- simple 0)         ; level 2
                  (?~ simple 0)         ; level 3
                  (?+ simple 0)         ; level 4
                  (?` simple 0)         ; level 5
                  (?# simple 0)         ; level 6
                  (?@ simple 0)))       ; level 7
               ((eq style 'Sphinx-Python)
                '((?# over-and-under 0) ; 0: for parts
                  (?* over-and-under 0) ; 1: for chapters
                  (?= simple 0)         ; 2: for sections
                  (?- simple 0)         ; 3: for subsections
                  (?^ simple 0)         ; 4: for subsubsections
                  (?\" simple 0)))      ; 5: for paragraph
               ((eq style 'CRiSPer)
                '((?= over-and-under 0) ; level  0 : title
                  (?= simple 0)         ; level  1
                  (?- simple 0)         ; level  2
                  (?~ simple 0)         ; level  3
                  (?^ simple 0)         ; level  4
                  (?+ simple 0)         ; level  5
                  (?* simple 0)         ; level  6
                  (?> simple 0)         ; level  7
                  (?< simple 0)         ; level  8
                  (?_ simple 0)         ; level  9
                  (?# simple 0)         ; level 10
                  (?` simple 0)         ; level 11
                  (?@ simple 0))))))    ; level 12
    (if globally
        (progn
          ;; rst-preferred-adornments is a defcustom user-option
          ;; dynamically change its global and local-buffer value
          (pel-setq-user-option 'rst-preferred-adornments new-adornments)
          ;; Do the same for pel-rst-adornment-style user-options
          (pel-setq-user-option 'pel-rst-adornment-style style))
      (setq-local rst-preferred-adornments new-adornments))
    (setq-local pel--rst-used-adornment-style style)
    (unless quiet
      (message "%ssing the %s adornment style with %d levels supported."
               (if (equal new-adornments old-adornments)
                   "U"
                 "Now u")
               style
               (length new-adornments)))))

;;-pel-autoload
(defun pel-rst-adorn-default (&optional globally)
  "Set the default section adornment style.
This is Emacs `rst-mode' default: a title with 7 levels.
With prefix argument GLOBALLY, change style to all `rst-mode' buffers."
  (interactive "P")
  (pel-rst-set-adornment 'default globally))

;;-pel-autoload
(defun pel-rst-adorn-Sphinx-Python (&optional globally)
  "Set the Sphinx-Python section adornment style.
This is what Sphinx supports: 6 levels:
- parts,
- chapters,
- sections,
- subsections,
- subsubsections,
- paragraphs.
With prefix argument GLOBALLY, change style to all `rst-mode' buffers."
  (interactive "P")
  (pel-rst-set-adornment 'Sphinx-Python globally))

;;-pel-autoload
(defun pel-rst-adorn-CRiSPer (&optional globally)
  "Set the CRiSPer section adornment style.
A title level with another 12 levels.
With prefix argument GLOBALLY, change style to all `rst-mode' buffers."
  (interactive "P")
  (pel-rst-set-adornment 'CRiSPer globally))

;; --

;; Ensure our adornment style is applied after rst-mode initializes in every
;; buffer.

(defun pel--rst-activate-adornment-style ()
  "Activate chosen RST adornment style buffer-locally after rst-mode init."
  (when (boundp 'rst-preferred-adornments)
    ;; Make sure we have a concrete local slot to avoid Emacs-version timing
    ;; differences.
    (unless (local-variable-p 'rst-preferred-adornments)
      (make-local-variable 'rst-preferred-adornments)
      (setq rst-preferred-adornments
            (default-value 'rst-preferred-adornments))))
  ;;
  ;; Re-apply the selected style for the current  active rst-mode buffer,
  ;; quietly.
  (when (boundp 'pel-rst-adornment-style)
    (ignore-errors
      (pel-rst-set-adornment pel-rst-adornment-style nil 'quiet))))

;; Install the hook with APPEND = t so we run after upstream hooks that may
;; reset defaults.
(add-hook 'rst-mode-hook #'pel--rst-activate-adornment-style t)

;; --
;;** Line Adornment Control - Adorn line at specified level
;;   ------------------------------------------------------
;;
;; -- Utilities

(defun pel--rst-level-adorn-level-style (level)
  "Return the adorn level style for the LEVEL used by the current style.
Return \\='simple or \\='over-and-under."
  (when (>= level (length rst-preferred-adornments))
    (error "Level %d not available in %s adornment style"
           level
           pel-rst-adornment-style))
  (cadr (nth level rst-preferred-adornments)))

(defun pel--rst-level-valid-p (level)
  "Return non-nil when LEVEL is a valid adorn level for current adorn style.
Return nil otherwise."
  (< -1 level (length rst-preferred-adornments)))

;; --

(defun pel-rst-adorn (&optional level)
  "Adorn the current line as a reStructuredText section at the specified LEVEL.
When UPDATE is non-nil do not add a new line after the underlining line,
but when UPDATE is nil, it adds a new line after the underlining.
`pel-rst-adorn' leaves the cursor unmoved, on the title line."
  (unless (pel--rst-level-valid-p level)
    (user-error "Level %d not available in %s adornment style"
                level
                pel-rst-adornment-style))
  ;; proceed if level is OK.
  (let* ((orig-line (line-number-at-pos))
         (has-over-line (eq (pel--rst-level-adorn-level-style level)
                            'over-and-under))
         (need-space-at-top
          (and (< orig-line 2)
               has-over-line)))
    (when need-space-at-top
      ;; Create empty line above title text to allow for overlining
      (goto-char (point-min))
      (insert "\n"))
    (save-excursion
      (pel-delete-trailing-whitespace)
      (let* ((linelen (pel-line-length))
             (adorn-level (nth level rst-preferred-adornments))
             (adorn-char (car adorn-level))
             (adorn-style (nth 1 adorn-level))
             (indent-steps (nth 2 adorn-level)))
        (move-end-of-line nil)
        (insert (format "\n%s"
                        (make-string linelen adorn-char)))
        (when (eq adorn-style 'over-and-under)
          (forward-line -2)
          (insert (format "\n%s"
                          (make-string linelen adorn-char))))
        ;; if the style requires indentation, indent the 3 lines
        (when (> indent-steps 0)
          (let ((indentation (make-string indent-steps ?\s)))
            (dotimes (_i 3)
              (beginning-of-line)
              (insert indentation)
              (forward-line 1))))))
    (when need-space-at-top
      ;; delete the empty line previously added
      (forward-line -2)
      (delete-char 1)
      (forward-line 1))
    ;; For title-like over and under-linking: add lines after.
    (when has-over-line
      ;; move point to end of title
      (end-of-line)
      (push-mark)
      (forward-line 3)
      (insert "\n"))))

;; The following convenience functions defined to ease execution from keys

;;-pel-autoload

(defun pel-rst-adorn-title ()
  "Adorn current line with level-0 (title) reStructuredText section adornment.
If point is at the top of the file, the top adorn line is placed
on the first line of the file.

In all cases, a mark is left at the end of the title text line
and point is placed 2 lines below."
  (interactive "*")
  (pel-rst-adorn 0))

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

;; --
;;** Line Adornment Control - Add/Adjust Adornment Line
;;   --------------------------------------------------

(defun pel--rst-level-at (&optional pos)
  "Return the adornment level number at POS or point.
The POS (or point) is assumed to be on the text, title, line.
Return nil if none."
  (save-excursion
    (when pos
      (goto-char pos))
    ;; First move to the underlining line
    (forward-line 1)
    ;; Read first non-whitespace character on lines to get the
    ;; underlining and over-lining characters.
    (let* ((underline-char (save-excursion
                             (back-to-indentation)
                             (char-after)))
           (overline-char (save-excursion
                            (forward-line -2)
                            (back-to-indentation)
                            (char-after)))
           (adorn-line-style (if (eq underline-char overline-char)
                                 'over-and-under
                               'simple))
           (level-number -1)
           (level-detected nil))
      (catch 'pel-rst-break
        (dolist (adorn-level  rst-preferred-adornments)
          (pel+= level-number 1)
          (when (and (eq (cadr adorn-level) adorn-line-style)
                     (eq (car adorn-level)  underline-char))
            (setq level-detected level-number)
            (throw 'pel-rst-break nil))))
      level-detected)))

(defun pel--rst-delete-whole-line ()
  "Delete the current line."
  ;; A copy of `pel-delete-whole-line' from pel-ccp.  Avoid loading that file.
  (delete-region (line-beginning-position)
                 (min (point-max) (1+ (line-end-position)))))

(defun pel--rst-adorn-level+= (level step)
  "Increase adorn LEVEL by STEP.

LEVEL must be a valid adorn level for the current adorn style.
STEP is a positive or negative number.

Return the value (+ level step) if the result is a valid adorn level for the
current adorn style, signal a user error otherwise."
  (let ((new-level (pel+= level step)))
    (if (pel--rst-level-valid-p new-level)
        new-level
      (user-error "Invalid new level %d for %s adornment style"
                  new-level pel--rst-used-adornment-style))))

(defun pel--rst-adorn-change (step &optional quiet)
  "Change adornment of current line by STEP level.
- If STEP is 1: increase level.
- If STEP is -1: decrease level."
  (save-excursion
    ;; read level from underlining.
    ;; Restore title without adornment
    (let* ((current-level (pel--rst-level-at))
           (current-level-style
            (pel--rst-level-adorn-level-style current-level))
           (new-level (pel--rst-adorn-level+= current-level step)))
      ;; remove the current underlining line
      (forward-line 1)
      (pel--rst-delete-whole-line)
      (forward-line -1)
      ;; remove the overlining if there's one
      (when (eq current-level-style 'over-and-under)
        (forward-line -1)
        (pel--rst-delete-whole-line))
      (pel-rst-adorn new-level)
      (unless quiet
        (unless (= current-level new-level)
          (message "Section now at level %d" new-level))))))

(defun pel--rst-adorn-level-of-previous-section ()
  "Return the adornment level of the previous section in the text."
  (save-excursion
    (rst-backward-section 1)
    (pel--rst-level-at)))

(defun pel--rst-adorn-same-as-previous  (step)
  "Adorn current line at the STEP level compared to the previous adornment."
  (let ((level (pel--rst-adorn-level-of-previous-section)))
    (if level
        (pel-rst-adorn (pel--rst-adorn-level+= level step))
      (user-error "Cannot detect section level of previous section!"))))

(defun pel--line-adorned-p ()
  "Return t if current line is section-adorned, nil otherwise."
  (save-excursion
    ;; the line should have no trailing whitespace
    ;; check a character 2 char before end of line to be safe
    ;; since line might be indented (for some styles) and the
    ;; rst-level-x property goes up to end of line (but not on
    ;; the end-of-line character).
    (back-to-indentation)
    (let ((point-text-face-property (get-text-property (point) 'face)))
      (if (and point-text-face-property
               (listp point-text-face-property)
               nil)
          ;; current line is adorned with the style supported by rst-mode
          ;; so just check the adornment text face property to get the level
          (not (not (memq
                     (car point-text-face-property)
                     '(rst-level-1
                       rst-level-2
                       rst-level-3
                       rst-level-4
                       rst-level-5
                       rst-level-6))))
        ;; The level is not using an adornment face, but it could still be
        ;; underlined properly if the title line and the underlining have the
        ;; same length. Check those and then check the character used in the
        ;; underlining and possibly the line over the title.
        ;; It's not foolproof, but probably OK for most cases.
        (let ((title-line-length (pel-line-length))
              (underline-length (progn
                                  (forward-line 1)
                                  (pel-line-length))))
          (when (= title-line-length underline-length)
            ;; back to the title line
            (forward-line -1)
            ;; and check if the underlining is correct
            (not (not (pel--rst-level-at)))))))))

;;-pel-autoload
(defun pel-rst-adorn-same-level ()
  "Adorn current line with the same level as the previous section.
If the line is already adorned, update the adornment:
adjust to previous section level."
  (interactive "*")
  (let ((previous-level (pel--rst-adorn-level-of-previous-section))
        (current-level  (pel--rst-level-at)))
    (cond
     ;;
     ;; if both are adorned; adjust adornment
     ((and (numberp previous-level)
           (numberp current-level))
      (unless (= previous-level current-level)
        (pel--rst-adorn-change (- previous-level current-level))))
     ;;
     ;; Current line is not adorned, but there's a section before
     ((and (numberp previous-level))
      (pel-rst-adorn previous-level))
     ;;
     ;; There's no previous section, regardless of current line state
     (t (user-error "Cannot detect section level of previous section!")))))

;;-pel-autoload
(defun pel-rst-adorn-refresh ()
  "Refresh the adornment of the current line.
This helps when the length of the line changes."
  (interactive "*")
  (pel--rst-adorn-change 0))

;;-pel-autoload
(defun pel-rst-adorn-increase-level ()
  "Adorn current line at a higher-level that current if already adorned.
If the line is not already adorned, adorn it with a level higher than
previous section."
  (interactive "*")
  (if (pel--line-adorned-p)
      (pel--rst-adorn-change 1)
    (pel--rst-adorn-same-as-previous 1)))

;;-pel-autoload
(defun pel-rst-adorn-decrease-level ()
  "Adorn current line at a lower-level than current if already adorned.
If the line not already adorned, adorn it with a level lower than
previous section."
  (interactive "*")
  (if (pel--line-adorned-p)
      (pel--rst-adorn-change -1)
    (pel--rst-adorn-same-as-previous -1)))

;; ---------------------------------------------------------------------------
;;* Link/reference location bookmark management
;;  ===========================================
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
  ;; it, otherwise trying to create a hyperlink will prompt to rename the
  ;; bookmark.  Also on the very first call, force loading the bookmarks to
  ;; ensure that an old bookmark for that file will be known.
  (when (and (buffer-modified-p)
             (not pel--rst-file-was-saved-once-p))
    (save-buffer)
    (setq pel--rst-file-was-saved-once-p t))
  ;; Although the docs states that the bookmark file is loaded automatically
  ;; for some reason sometimes it's not.  So make sure it is because the file
  ;; may have a bookmark for the edited reStructuredText file.
  ;; If it was loaded don't load it again as this would create duplicates.
  (unless pel--bookmark-file-loaded-p
    (when (and (require 'bookmark nil :noerror)
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

Used to identify the location where the next invocation of
\\[pel-rst-makelink] inserts fully expanded links.  Ensures the
bookmark is at the beginning of an empty line which is followed
by another empty line, by inserting 2 lines and placing the point
at the beginning of the first of the 2 lines."
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
    (user-error "The bookmark does not exist yet.  \
Use `pel-rst-set-ref-bookmark' to create it!")))

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

- If a region is active, use the text of the region to make the
  link, otherwise use the word at point.
- If an argument (ARG, which can be a \\[universal-argument]) is
  specified, use the embedded URI format.
- If no argument is specified, use the named hyperlink format:
  - if the region is a single word, just append an underscore to
    make the link
  - if the region is several words, surround the region with the
    \"`\" start string and the \"`_\" end string.
- The named link is placed in the location of bookmark named
  \"RST\" if it exists and points to same file, otherwise the
  link is placed at the beginning of the next empty line.
- The cursor is placed where the URL is to be written.
- You can return to the location of the link by typing \\[pel-jump-to-mark].

Reference: see reStructuredText hyperlink format at URL
`https://docutils.sourceforge.io/docs/ref/rst/restructuredtext.html\
#hyperlink-references'"
  (interactive "*P")
  (pel--rst-require-thingatpt)
  (let (p_begin p_end)
    (if (use-region-p)
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
              (insert " <>`_")
              (left-char 3))
          ;; Use the separate reference format.
          ;; For that, require the presence of the bookmark.
          (if (not (pel--rst-bookmark-exists-p))
              (user-error "Please set location of hyperlink bookmark first \
with pel-rst-set-ref-bookmark!")
            (if (pel--space-for-extra-link-p)
                (progn
                  (if (and anchor-isa-alnum
                           pel-rst-use-single-underscore-for-single-word-ref)
                      (progn
                        (goto-char p_end)
                        (insert "_"))
                    (goto-char p_begin)
                    (insert "`")
                    (goto-char p_end)
                    (right-char 1)
                    (insert "`_"))
                  ;; Place references starting at the bookmark, one after the
                  ;; other, with first reference created at the top.  All
                  ;; references are placed after the bookmark so that the
                  ;; bookmark never moves, making it easier to undo editing
                  ;; without damaging the bookmark location.  The bookmark
                  ;; must be set to an area in the buffer with 2 empty lines,
                  ;; with the bookmark at the beginning of the first one.
                  (pel-rst-goto-ref-bookmark)
                  (forward-line 1)
                  (if (not (eq (char-after) 10))
                      (pel-goto-next-empty-line))
                  (insert (format ".. _%s: \n"
                                  (pel-rst-anchor-escaped anchor)))
                  (move-end-of-line 0))
              (user-error "Bookmarked reference link area has no \
space for new entry!
Move there with pel-rst-goto-ref-bookmark then add lines!"))))))))

;; ---------------------------------------------------------------------------
;;* Emphasis markup support
;;  =======================

(defconst pel--rst-whitespace-chars  '(?\s ?\t ?\n ?\r)
  "Supported whitespace surrounding characters.")

(defun pel--rst-emphasize-escape-for (point &optional other-separator-chars)
  "Return string to provide escaping for emphasis around POINT if necessary.

Return an empty string if no escaping is required.
Supported separators are white-space characters and OTHER-SEPARATOR-CHARS."

  (if (or (memq (char-after point) pel--rst-whitespace-chars)
          (memq (char-after point) other-separator-chars))
      ""
    "\\ "))

(defun pel--rst-emphasize-with (str)
  "Emphasize the current word or marked area using STR.
Leave point right after the emphasized text.

The function supports emphasis of text inside of a word, which
must, in that case, be surrounded by an escaped space
character. "
  (pel--rst-require-thingatpt)
  (let* ((p-begin (if (use-region-p)
                      (region-beginning)
                    (car (bounds-of-thing-at-point 'word))))
         (p-end (if (use-region-p)
                    (region-end)
                  (cdr (bounds-of-thing-at-point 'word))))
         (prefix (when p-begin (pel--rst-emphasize-escape-for (1- p-begin)
                                                              '(?\( ?\[ ?\{ ?\<
                                                                    ?\: ?\/ ?\-
                                                                    ?\' ?\"))))
         (suffix (when p-end (pel--rst-emphasize-escape-for p-end
                                                            '(?\) ?\} ?\] ?\>
                                                                  ?\: ?\/ ?\-
                                                                  ?\' ?\"
                                                                  ?\. ?\, ?\;
                                                                  ?\? ?\!)))))
    (deactivate-mark)
    (when p-end
      (goto-char p-end))
    (insert str)
    (when suffix
      (insert suffix)
      (goto-char p-begin)
      (insert prefix))
    (insert str)
    (when p-end
      (goto-char p-end))
    (if (or suffix prefix)
        (forward-char (* 2 (length str)))
      (backward-char (length str)))))

(defun pel-rst-bold ()
  "Mark current word or marked region bold.
Leave point after to the next character.
If nothing marked or not in or after a word insert markup
and leave point inside it."
  (interactive "*")
  (pel--rst-emphasize-with "**"))

(defun pel-rst-italic ()
  "Mark current word or marked region italic.
Leave point after to the next character.
If nothing marked or not in or after a word insert markup
and leave point inside it."
  (interactive "*")
  (pel--rst-emphasize-with "*"))

(defun pel-rst-literal ()
  "Mark current word or marked region literal.
Leave point after to the next character.
If nothing marked or not in or after a word insert markup
and leave point inside it."
  (interactive "*")
  (pel--rst-emphasize-with "``"))

(defun pel-rst-interpreted ()
  "Mark current word or marked region interpreted.
Leave point after to the next character.
If nothing marked or not in or after a word insert markup
and leave point inside it."
  (interactive "*")
  (pel--rst-emphasize-with "`"))

;; ---------------------------------------------------------------------------
;;* Open Link URL
;;  ============
;;
;; Open the URL identified by the reStucturedText link.
;; This way, user does not have to move point to the URL: the URL can be
;; opened right from the link.

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
- ref-type is either \\='target or \\='path.  It describes what the string
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
          (pel-= beg 1))
        (when (equal (char-after beg) ?`)
          (pel+= beg 1))
        (while (and (pel-at-rst-reference-p (1+ end))
                    (pel-at-rst-valid-ref-char-p (1+ end)))
          (pel+= end 1))
        (cond
         ((equal (char-before end) ?`)
          (pel-= end 1)
          (setq ref-type 'target))
         ((equal (char-after (1+ end)) ?>)
          (pel+= end 1)
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
  (let ((regexp (format "^\\.\\. _%s:"
                        (regexp-quote (pel-rst-anchor-escaped target)))))
    ;; search for a complete reference (target and URL on the same line) first
    (if (re-search-forward (concat regexp " +") nil :noerror)
        t
      ;; [:todo 2026-04-23, by Pierre Rouleau: add logic to support multi-line
      ;;                                       links]

      ;; If that fails there are 2 other possibilities:
      ;; 1) The current line is the first of a chain of references which ends
      ;;    with a line that holds the reference with the URL.
      ;; 2) The reference target is a section inside this file or another
      ;;    reStructured file;
      ;; Handle case 2: search for a line that only has the target: a section
      ;; title.  The search must start from the top of the file.
      (goto-char (point-min))
      (if (re-search-forward regexp nil :noerror)
          ;; if that's found move to the first complete reference line
          (re-search-forward ": +.+$" nil :noerror)
        ;; otherwise try to find a line that begins with the target
        ;; that might be a title
        (goto-char (point-min))
        (when (re-search-forward (format "^%s$" target) nil :noerror)
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
  ;; NOTE: to allow a double quote to surround a word inside a title we
  ;;       need to escape it for the Emacs Lisp read syntax. That would
  ;;       be \\\"  but then `replace-regexp-in-string' processes it
  ;;       so the first 2 backslash must also be escaped, therefore
  ;;       we have to use \\\\\" inside the string below.
  (format "^%s$" (replace-regexp-in-string "-" "[ \\\\\"'`*_-]+" target)))

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
When the point is located at a reStructuredText link, follow the link
and open what it points to, unless N>= 100.  In that case use N-100 as the
window number and don't interpret the link, just use it as a normal path.
See `pel-find-file-at-point-in-window' for more information."
  (interactive "P")
  ;; It's possible the file visited by the current buffer is located in a
  ;; directory that is not the current directory. The links in the
  ;; reStructuredText file may refer to other files, with paths relative to
  ;; the location of the file in the buffer.  Therefore to process this
  ;; request we temporary change the current directory to the directory of the
  ;; file in the current buffer.
  (let* ((original-cwd default-directory)
         (new-position nil)
         (n-value (prefix-numeric-value n))
         (ignore-rest-link (when (> n-value 99)
                             (progn
                               (setq n-value (- n-value 100))
                               t))))
    (save-excursion
      (unwind-protect
          (progn
            (when (and (buffer-file-name)
                       (not ignore-rest-link))
              (cd (file-name-directory (pel-current-buffer-filename))))
            ;; A reStructuredText link may have to be escaped in the
            ;; reference, therefore search for the potentially escaped
            ;; reference target to ensure we're able to handle all types of
            ;; links.
            (let* ((reftype.string (pel--rst-reference-target))
                   (reftype (car reftype.string))
                   (result (if (and (eq reftype 'target)
                                    (not ignore-rest-link))
                               (pel--move-to-rst-target
                                (pel-rst-anchor-escaped
                                 (cdr reftype.string)))
                             (cdr reftype.string))))
              (if (and (eq reftype 'target)
                       (not ignore-rest-link))
                  (cond
                   ;; if found a ref to a title jump to it
                   ((and (listp result) (eq (car result) 'rst-title))
                    (setq new-position (cadr result)))
                   ;; otherwise it's a link to a file: try to find it
                   (t
                    (if result
                        (pel-find-file-at-point-in-window n-value #'pel-html-to-rst)
                      (unless noerror
                        (user-error "No reference target found!")))))
                ;; reftype is a path: use that path directly
                (pel-find-file-at-point-in-window n-value #'pel-html-to-rst))))
        (cd original-cwd)))
    (when new-position
      (push-mark)
      (goto-char new-position))))

;;-pel-autoload
(defun pel-rst-open-file-at-point (&optional n)
  "Open a file at point if it is a rst reference.

Optionally identify a window to open a file reference with the argument N.
See `pel-rst-open-target' for more information.

The function opens the reSTucturedText reference at point and returns t.
If point is not on a reference, the function does nothing and returns nil."
  (when (pel-at-rst-reference-p)
    (pel-rst-open-target n)
    t))

;; ---------------------------------------------------------------------------
;;* Table Helper Utility
;;  ====================

(defun pel-rst-table-dup-separator-lines (&optional update)
  "Complete the table separator line to the top and the bottom of the table.

With the optional UPDATE argument the table lines as a copy of
the current one.  This command *must* be issued from the line
under the table title line."
  (interactive "*P")
  (save-excursion
    (pel-duplicate-line 2)
    (forward-line -2)
    (transpose-lines 1)
    (when update
      (save-excursion
        (forward-line -3)
        (pel-delete-whole-line)))
    (forward-line 1)
    (kill-whole-line 1)
    (if update
        (progn
          (search-forward "\n=")
          (pel-delete-whole-line))
      (search-forward "\n\n")
      (forward-line -1))
    (yank)))

;; ---------------------------------------------------------------------------
;;* Output generation: compilation support
;;  =====================================

(defvar pel-home-dirpath-name)          ; Prevent byte-compiler warning
;;                                      ; This is defined in PEL init.el
(defun pel-rst-compile ()
  "Generate the output file from the current reStructuredText file."
  (interactive)
  (let* ((command-line pel-rst-compiler)
         (pgm  (car (split-string pel-rst-compiler))))
    ;; The default is pel-rst2html, located in PEL bin directory.
    ;; It might not be on PATH, so prefix it automatically.
    (when (string= pgm "pel-rst2html")
      (setq pgm (format "%s/bin/pel-rst2html" pel-home-dirpath-name))
      (setq command-line (format "%s/bin/%s"
                                 pel-home-dirpath-name
                                 command-line)))
    (if (executable-find pgm)
        (compile (format "%s %s"
                         command-line (buffer-file-name)))
      (user-error "Specified command line is invalid: %s is not on PATH.\
 Update pel-rst-compiler!" pgm))))

;; ---------------------------------------------------------------------------
(provide 'pel-rst)

;;; pel-rst.el ends here
