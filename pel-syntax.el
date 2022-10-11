;;; pel-syntax.el --- Syntax processing helper functions.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, September 29 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-10-11 13:13:09 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2022  Pierre Rouleau
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
;;  This provides a set of utilities to provide syntax-table based utilities
;;  which may be used to enhance features such as electric behaviour of keys.

;; Internal Macros for self documenting code:
;; - `pel--inside-string-p'
;; - `pel--inside-block-p'
;; - `pel--inside-comment-p'
;; - `pel--open-parens-pos'

;; Predicate utilities:
;; - `pel-inside-block-p'
;; - `pel-inside-comment-p'
;; - `pel-inside-string-p'

;; Development Tools
;; - `pel-get-text-property'
;; - `pel-get-syntax-prop'
;; - `pel-get-face'
;;
;; * `pel-syntax-at-point'

;; Syntax Utilities:
;; - `pel-syntax-at'
;; - `pel-syntax-matching-parens-position'

;; Electric keys helper:
;; - `pel-insert-space-in-enclosing-block'

;; Syntax skipping utilities
;; - `pel-syntax-skip-string-forward'
;; - `pel-syntax-skip-string-backward'
;; - `pel-syntax-skip-string-and-comment-forward'
;; - `pel-syntax-skip-string-and-comment-backward'

;; Block syntax fixer:
;; - `pel-syntax-fix-block-content'
;;   - `pel-syntax-block-text-at'
;;   - `pel-replace'
;;     - `pel---replace-with'

;; Navigation across conditionals
;; - `pel-syntax-conditional-forward'
;; - `pel-syntax-conditional-backward'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)               ; use: `pel-=', `pel+=',
;;                                 ;      `pel-string-starts-with-p'
;;                                 ;      `pel-string-ends-with-p'
(require 'pel--options)            ; use: `pel-syntax-text-properties'
(require 'syntax)     ; syntax always available, even in emacs -Q
(eval-when-compile (require 'subr-x))   ; use: `string-join'

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; Macros for self documenting code
;; --------------------------------
;;
;; These macros are useful to reduce the need to call functions that create a
;; let-bound variable while providing meaningful names.

(defmacro pel--inside-string-p (syntax)
  "Return non-nil if point is inside string according to SYNTAX list."
  `(nth 3 ,syntax))

(defmacro pel--inside-block-p (syntax)
  "Return non-nil if point is inside matching pair block according to SYNTAX."
  `(> (nth 0 ,syntax) 0))

(defmacro pel--inside-comment-p (syntax)
  "Return non-nil if point is inside comment according to SYNTAX."
  `(nth 4 ,syntax))

(defmacro pel--open-parens-pos (syntax)
  "Return list of position of open parens according to SYNTAX.

Each integer in the list is the position of the open parens,
starting with the outermost one.  Return nil if not outside parens."
  `(nth 9 ,syntax))

;; Predicate utilities
;; -------------------

(defun pel-inside-block-p (&optional pos)
  "Return non-nil if POS, or point, is between a code matched-pair block.

Return nil otherwise.  Return nil when point is inside string or comment."
  (let ((syntax (syntax-ppss pos)))
    (unless (pel--inside-string-p syntax)
      (pel--inside-block-p syntax))))

(defun pel-inside-comment-p (&optional pos)
  "Return non-nil if POS, or point, is inside a comment, nil otherwise.

When inside comment, return t if inside non-nestable comment,
otherwise return an integer indicating the current comment nesting."
  (pel--inside-comment-p (syntax-ppss pos)))

(defun pel-inside-string-p (&optional pos)
  "Return non-nil if POS, or point, is inside a string, nil otherwise."
  (pel--inside-string-p (syntax-ppss pos)))

;; Development Tools
;; -----------------

(defun pel-get-text-property (property &optional pos)
  "Return specific text PROPERTY of character at POS or point."
  (get-text-property (or pos (point)) property))

(defun pel-get-syntax-prop (&optional pos)
  "Return syntax property of character at POS of point."
  (get-text-property (or pos (point)) 'syntax-table))

(defun pel-get-face (&optional pos)
  "Return syntax property of character at POS of point."
  (get-text-property (or pos (point)) 'face))

;;-pel-autoload
(defun pel-syntax-at-point ()
  "Display complete information for character at point.
If `pel-syntax-text-properties' is nil list all properties,
otherwise list only the ones specified by it."
  (interactive)
  (what-cursor-position t)
  (if pel-syntax-text-properties
      (let ((prop-msgs nil))
        (setq prop-msgs
              (dolist (prop pel-syntax-text-properties (reverse prop-msgs))
                (push (format "%-20s: %S"
                              prop
                              (get-text-property (point) prop))
                      prop-msgs)))
        (message (string-join prop-msgs "\n")))
    (message "%S" (text-properties-at (point)))))


;; Syntax Utilities
;; ----------------

(defun pel-syntax-at (&optional pos)
  "Return the syntax information for the character at POS or point."
  (list
   (char-to-string (char-syntax (char-after (or pos (point)))))
   (syntax-after (or pos (point)))))

(defun pel-syntax-matching-parens-position (&optional parens-pos)
  "Return the parens position that match PARENS-POS.

Caution: this function uses `forward-sexp' and `backward-sexp'
and therefore assumes that they handle the block pairing of the
current major mode properly."
  (setq parens-pos (or parens-pos (point)))
  (save-excursion
    (let ((parens-char (char-after (goto-char parens-pos))))
      (if (memq parens-char '(?\( ?\[ ?\{ ?<))
          (progn
            (forward-sexp)
            (backward-char))
        (if (memq parens-char '(?\) ?\] ?\} ?>))
            (progn
              (forward-char)
              (backward-sexp))
          (error "Invalid sexp character: %S" parens-char)))
      (point))))


;; Electric keys helper
;; --------------------

(defun pel-insert-space-in-enclosing-block ()
  "Insert a space if point is in between a block pair."
  (when (pel-inside-block-p)
    (insert " ")))


;; Syntax skipping utilities
;; -------------------------

(defun pel-syntax-skip-string-forward (&optional pos syntax)
  "Move point to character just after end of string.
Start from POS or current point.
The SYNTAX argument may be specified to re-use a caller syntax object,
reducing the need to create a new one."
  (or pos (setq pos (point)))
  (goto-char pos)
  (or syntax (setq syntax (syntax-ppss pos)))
  (while (and (pel--inside-string-p syntax)
              (not (eobp)))
    (forward-char)
    (setq pos (1+ pos))
    (setq syntax (syntax-ppss pos))))

(defun pel-syntax-skip-string-backward (&optional pos syntax)
  "Move point to character just before beginning of string.
Start from POS or current point.
The SYNTAX argument may be specified to re-use a caller syntax object,
reducing the need to create a new one."
  (or pos (setq pos (point)))
  (goto-char pos)
  (or syntax (setq syntax (syntax-ppss pos)))
  (while (and (pel--inside-string-p syntax)
              (not (bobp)))
    (backward-char)
    (setq pos (1- pos))
    (setq syntax (syntax-ppss pos))))

(defun pel-syntax-skip-string-and-comment-forward (&optional pos syntax)
  "Move point to character just after end of string or comment.
Start from POS or current point.
The SYNTAX argument may be specified to re-use a caller syntax object,
reducing the need to create a new one.
Return the new position."
  (setq pos (or pos (point)))
  (goto-char pos)
  (setq syntax (or syntax (syntax-ppss pos)))
  (while (and (or (pel--inside-string-p syntax)
                  (pel--inside-comment-p syntax))
              (not (eobp)))
    (forward-char)
    (setq pos (1+ pos))
    (setq syntax (syntax-ppss pos)))
  pos)

(defun pel-syntax-skip-string-and-comment-backward (&optional pos syntax)
  "Move point to character just before beginning of string or comment.
Start from POS or current point.
The SYNTAX argument may be specified to re-use a caller syntax object,
reducing the need to create a new one.
Return the new position"
  (setq pos (or pos (point)))
  (goto-char pos)
  (setq syntax (or syntax (syntax-ppss pos)))
  (while (and (or (pel--inside-string-p syntax)
                  (pel--inside-comment-p syntax))
              (not (bobp)))
    (backward-char)
    (setq pos (1- pos))
    (setq syntax (syntax-ppss pos)))
  pos)


;; Block syntax fixer
;; ------------------
;;

(defun pel-syntax-block-text-at (&optional pos)
  "Return text of block at POS or current point.
Return a list of (open-position close-position text)."
  (setq pos (or pos (point)))
  (let* ((syntax       (syntax-ppss pos))
         (open-p-pos  (car (pel--open-parens-pos syntax)))
         (close-p-pos (pel-syntax-matching-parens-position open-p-pos)))
    (list open-p-pos
          close-p-pos
          (buffer-substring-no-properties open-p-pos (+ 1 close-p-pos)))))

(defun pel---replace-with (from replacer)
  "Replace text in current buffer.
FROM is the regex identifying the text to change.
REPLACER is a closure that identifies the new text, the
REPLACER has access to the information from a `re-search-forward'
such as the result of `match-string'.

The function returns the number of replacements done."
  (let ((found-pos nil)
        (change-count 0)
        (syntax nil))
    (while
        (progn
          (goto-char (point-min))
          (setq found-pos (re-search-forward from nil :noerror))
          ;; if found item is in string, skip the string and search again:
          ;; do not transform the content of strings.
          (while (and found-pos
                      (progn
                        (setq syntax (syntax-ppss found-pos))
                        (or (pel--inside-string-p syntax)
                            (pel--inside-comment-p syntax))))
            (pel-syntax-skip-string-and-comment-forward found-pos syntax)
            (setq found-pos (re-search-forward from nil :noerror)))
          (when found-pos
            (replace-match (funcall replacer) :fixedcase :literal)
            (setq change-count (1+ change-count))
            t)))
    change-count))

(defmacro pel-replace (from &rest to)
  "Replace text in buffer.

FROM must be a regex string.
TO must be a form that produces a replacement string.
That form runs in the context of the string replacement code performed
by the function `re-search-forward'."
  `(pel---replace-with ,from (lambda () ,@to)))

(defun pel-syntax-fix-block-content (&optional pos)
  "Comma-separate all expressions inside the block-pair at POS or point.

Does not transform text inside any string located inside the matched-pair
block, but it may transform other text.

Returns the number of text modifications performed."
  (save-excursion
    (save-restriction
      (let* ((open.close.text (pel-syntax-block-text-at pos))
             (open-pos  (nth 0 open.close.text))
             (close-pos (nth 1 open.close.text))
             (total-changes 0)
             (changes 1))
        ;; TODO: I'm not sure I need the multi checks loop, for the moment
        ;;       it's there for safety but it might not be needed and will
        ;;       only slow down operation.
        ;; (message "%d, %d: %s" open-pos close-pos  (nth 2 open.close.text))
        (narrow-to-region open-pos (1+ close-pos))
        (while (> changes 0)
          (setq changes 0)
          ;;
          ;; -> ensure one space between comma and next element.
          ;;    Also eliminate multiple commas between 2 symbols.
          (pel+= changes (pel-replace "\\(\\w\\),+\\(\\w\\)"
                                      (format "%s, %s"
                                              (match-string 1)
                                              (match-string 2))))
          ;;
          ;; -> remove spaces between word, closing parens or quotes and the
          ;;    following comma
          (pel+= changes (pel-replace "\\(\\w\\|\\s)\\|\\\"\\|'\\) +,"
                                      (format "%s," (match-string 1))))
          ;;
          ;; -> replace multiple commas by a single one
          (pel+= changes (pel-replace "\\(\\w\\),,+ "
                                      (format "%s, " (match-string 1))))
          ;;
          ;; -> add a comma after word or closing parens if there is none
          ;;    before the next word or opening parens
          (pel+= changes (pel-replace "\\(\\w\\|\\s)\\) +\\(\\w\\|\\s(\\)"
                                      (format "%s, %s"
                                              (match-string 1)
                                              (match-string 2))))
          ;;
          ;; -> remove trailing commas placed just before the closing parens
          (pel+= changes (pel-replace ",\\s-*\\(\\s)\\)"
                                      (match-string 1)))
          ;;
          ;; -> replace multiple spaces after a comma by 1 space after comma
          (pel+= changes (pel-replace ",  +\\([^ ]\\)"
                                      (format ", %s" (match-string 1))))
          ;;
          ;; -> remove isolated commas not separating anything
          (pel+= changes (pel-replace ", +," ","))
          ;;
          ;; -> In erlang buffers move period after closing parens
          ;;    if it is before
          (when (eq major-mode 'erlang-mode)
            (pel+= changes (pel-replace "\\.\\(\\s)\\)"
                                        (format "%s." (match-string 1)))))
          ;;
          (pel+= total-changes changes)
          ;; (message "%d changes" changes)
          )
        total-changes))))

;; ---------------------------------------------------------------------------
;; Navigation across conditionals
;; ------------------------------
;;
;; to-do: once code lived for a while and have been used for several
;; programming languages, try to combine the 2 functions into a single one
;; since their code is almost the same.

(defun pel-syntax-conditional-forward (regexp
                                       match-to-token-f
                                       match-to-pos-f
                                       current-nesting
                                       to-else
                                       conditional)
  "Move point forward to matching end of conditional.

Search using specified REGEXP which should identify several
groups: if token, else token (if at all possible for the syntax),
and end token.

Search from nesting-level specified by CURRENT-NESTING.
Use 0 to search forward to the matching end (or else) when point
is outside of the if statement.  Use 1 to move outward forward to
the end of the if statement where point is in.  Use higher value
to move outward several statement nesting levels.

The MATCH-TO-TOKEN-F argument must be a function that takes the
match-data result of a successful REGEXP search and returns one
of the following symbols that identifies the token found: if,
else or end.

The MATCH-TO-POS-F argument must be a function that takes the
match -data result of a successful REGEXP search and returns the
position of the beginning of the token found.

Search for end statement unless TO-ELSE is non-nil: in that case search for
else statement.

CONDITIONAL is a string message used to describe the target searched.

On success, push the original position on the mark ring and
return the new position. On error, issue user error on mismatch."
  (let ((nesting-level current-nesting)
        (found-pos nil)
        (syntax nil)
        (mdata nil)
        (token nil)
        (searching t)
        (original-pos (point)))
    ;; (message "===================pel-syntax-conditional-forward %s: point=%S ================" to-else (point))
    (when (< nesting-level 0)
      (error "Invalid current-nesting value of %d specified" current-nesting))
    (while
        (progn
          ;; (message "-----At: %S --------------" (point))
          (setq found-pos
                (re-search-forward regexp nil t))
          (setq mdata (match-data))
          ;; (message "-----Searching: nesting=%d, found-pos=%S, point=%S" nesting-level found-pos (point))
          (if found-pos
              ;; get syntax of the beginning of the token found
              (if (progn
                    (setq syntax (syntax-ppss (funcall match-to-pos-f mdata)))
                    (or (pel--inside-string-p syntax)
                        (pel--inside-comment-p syntax)))
                  ;; skip over token found inside string or comment
                  (setq found-pos (pel-syntax-skip-string-and-comment-forward
                                   found-pos syntax))
                ;; not in string or comment: process token
                (setq token (funcall match-to-token-f mdata))
                ;; (message "%s found at %s" token found-pos)
                (cond
                 ((eq token 'if)
                  (pel+= nesting-level 1))
                 ((eq token 'else)
                  (when (and to-else
                             (<= nesting-level 1))
                    (setq nesting-level 0)
                    (setq searching nil)))
                 ((eq token 'end)
                  (pel-= nesting-level 1)
                  (unless to-else
                    (when (eq nesting-level 0)
                      (setq searching nil)))))
                (right-char 1))
            (setq searching nil))
          (and searching found-pos (> nesting-level 0))))
    (if (> nesting-level 0)
        (progn
          (goto-char original-pos)
          (user-error "Can't find matching conditional %s: \
missing %d nested levels" conditional nesting-level))
      (when (and found-pos (/= found-pos original-pos))
        (left-char 1)
        (push-mark original-pos)))
    found-pos))


(defun pel-syntax-conditional-backward (regexp
                                        match-to-token-f
                                        match-to-pos-f
                                        current-nesting
                                        to-else
                                        conditional)
  "Move point backward to matching beginning of conditional.

Search using specified REGEXP which should identify several
groups: if token, else token (if at all possible for the syntax),
and end token.

Search from nesting-level specified by CURRENT-NESTING.
Use 0 to search backward to the matching if (or else) when point
is outside of the if statement.  Use 1 to move outward backward
to the beginning of the if statement where point is in.  Use
higher value to move outward several statement nesting levels.

The MATCH-TO-TOKEN-F argument must be a function that takes the
match-data result of a successful REGEXP search and returns one
of the following symbols that identifies the token found: if,
else or end.

The MATCH-TO-POS-F argument must be a function that takes the
match -data result of a successful REGEXP search and returns the
position of the beginning of the token found.

Search for end statement unless TO-ELSE is non-nil: in that case search for
else statement.

CONDITIONAL is a string message used to describe the target searched.

On success, push the original position on the mark ring and
return the new position. On error, issue user error on mismatch."
  (let ((nesting-level current-nesting)
        (found-pos nil)
        (syntax nil)
        (mdata nil)
        (token nil)
        (searching t)
        (original-pos (point)))
    ;; (message "<<<<<<--pel-syntax-conditional-backward %s---------------" to-else)
    (when (< nesting-level 0)
      (error "Invalid current-nesting value of %d specified" current-nesting))
    (when (pel-string-ends-with-p regexp "$")
      (end-of-line nil))
    (while
        (progn
          ;; (message "-----At: %S --------------" (point))
          (setq found-pos
                (re-search-backward regexp nil t))
          (setq mdata (match-data))
          ;; (message "-----Searching: nesting=%d, found-pos=%S" nesting-level found-pos)
          (if found-pos
              ;; get syntax of the beginning of the token found
              (if (progn
                    (setq syntax (syntax-ppss (funcall match-to-pos-f mdata)))
                    (or (pel--inside-string-p syntax)
                        (pel--inside-comment-p syntax)))
                  ;; skip over token found in string or comment
                  (setq found-pos (pel-syntax-skip-string-and-comment-backward
                                   found-pos syntax))
                ;; not in string or comment: process token
                (setq token (funcall match-to-token-f mdata))
                ;; (message "%s found at %s" token found-pos)
                (cond
                 ((eq token 'if)
                  (pel-= nesting-level 1)
                  (unless to-else
                    (when (eq nesting-level 0)
                      (setq searching nil))))
                 ((eq token 'else)
                  (when (and to-else
                             (<= nesting-level 1))
                    (setq nesting-level 0)
                    (setq searching nil)))
                 ((eq token 'end)
                  (pel+= nesting-level 1)))
                (left-char 1))
            (setq searching nil))
          (and searching found-pos (> nesting-level 0))))
    (if (> nesting-level 0)
        (progn
          (goto-char original-pos)
          (user-error "Can't find matching conditional %s: \
missing %d nested levels" conditional nesting-level))
      (when (and found-pos (/= found-pos original-pos))
        (right-char 1)
        (push-mark original-pos)))
    found-pos))

;;; --------------------------------------------------------------------------
(provide 'pel-syntax)

;;; pel-syntax.el ends here
