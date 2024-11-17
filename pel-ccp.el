;;; pel-ccp.el --- PEL cut & paste, etc... -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2021, 2023, 2024  Pierre Rouleau

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
;; A collection of copy, kill and delete functions targeting specific
;; syntax entities, with the implementation of a flexible function
;; that can kill or delete the current line, multiple lines or the
;; marked region: `pel-kill-or-delete-marked-or-whole-line'.  The file
;; also provides other interactive functions to copy, kill or delete
;; specific entities.
;;
;; Also includes specialized yank function.

;;; Dependencies:
(require 'pel--base)
(require 'pel--options)

;;; Code:
;; ---------------------------------------------------------------------------
;; Utility
;; -------
(defun pel--ccp-require-thingatpt ()
  "Load thingatpt/."
  (unless (and (require 'thingatpt nil :noerror)
               (fboundp 'bounds-of-thing-at-point))
    (user-error "Failed loading thingatpt!")))

;; Display Control
;; ---------------

;;-pel-autoload
(defun pel-toggle-show-copy-cut-text (&optional globally)
  "Toggle display of copied/cut text and display current state.
By default change behaviour in local buffer only.
When GLOBALLY argument is non-nil, change it for all buffers for the current
Emacs editing session (the change does not persist across Emacs sessions).
To modify the global state permanently modify the customized value of the
`pel-show-copy-cut-text' user option via the `pel-pkg-for-cut-and-paste'
group customize buffer."
  (interactive "P")
  (pel-toggle-and-show-user-option 'pel-show-copy-cut-text globally))

(defun pel--show-kill-ring-top (op-name)
  "Display top of kill ring on echo area.
Show the OP-NAME and the content of the kill ring at top."
  (when pel-show-copy-cut-text
    (message "%s:「%s」"
             op-name
             (substring-no-properties (or (car kill-ring)
                                          "")))))
(defun pel--show-copied ()
  "Display what was copied."
  (pel--show-kill-ring-top "Copy"))

(defun pel-show-copied (&rest _ignored)
  "Display what was copied.  Ignore arguments.

Use this function as an :after advice to show what was just copied."
  (pel--show-copied))

(defvar-local pel--isa-delete-operation nil
  "Used to identify a delete operation. Don't change.")

(defun pel--show-killed ()
  "Display what was killed.

Use this function as an :after advice to show what was just killed."
  (pel--show-kill-ring-top (if pel--isa-delete-operation
                               "Delete"
                             "Kill")))

(defun pel-show-killed (&rest _ignored)
  "Display what was killed.  Ignore arguments."
  (pel--show-killed))

;; Copy Commands.
;; --------------

(defun pel--copy-thing-at-point (thing)
  "Copy the `thing-at-point' for the specified kind of THING.
See `bounds-of-thing-at-point' for a list of possible THING symbols."
  (pel--ccp-require-thingatpt)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (progn
          (copy-region-as-kill (car bounds) (cdr bounds))
          (pel--show-copied))
      (error "No %s at point" thing))))

;;-pel-autoload
(defun pel-copy-word-at-point ()
  "Copy and show word at point."
  (interactive)
  (pel--copy-thing-at-point 'word))

;;-pel-autoload
(defun pel-copy-symbol-at-point ()
  "Copy and show symbol at point."
  (interactive)
  (pel--copy-thing-at-point 'symbol))

;;-pel-autoload
(defun pel-copy-sentence-at-point ()
  "Copy and show sentence at point."
  (interactive)
  (pel--copy-thing-at-point 'sentence))

;;-pel-autoload
(defun pel-copy-function-at-point ()
  "Copy and show function at point."
  (interactive)
  (pel--copy-thing-at-point 'defun))

;;-pel-autoload
(defun pel-copy-sexp-at-point ()
  "Copy and show sexp at point.
Only copy the list that start or end at point.
Does not copy when point is *inside* the list.
To do that use \\[pel-copy-list-at-point]."
  (interactive)
  (pel--copy-thing-at-point 'sexp))

;;-pel-autoload
(defun pel-copy-whitespace-at-point ()
  "Copy and show whitespace at/around point on current line."
  (interactive)
  (pel--copy-thing-at-point 'whitespace))

;;-pel-autoload
(defun pel-copy-filename-at-point ()
  "Copy and show filename at point."
  (interactive)
  (pel--copy-thing-at-point 'filename))

;;-pel-autoload
(defun pel-copy-url-at-point ()
  "Copy and show URL at point."
  (interactive)
  (pel--copy-thing-at-point 'url))

;;-pel-autoload
(defun pel-copy-list-at-point ()
  "Copy and show list at point.
Includes the outermost list as opposed to \\[pel-copy-sexp-at-point]
which only includes the list at point."
  (interactive)
  (pel--copy-thing-at-point 'list))

;; ---------------------------------------------------------------------------

;;-pel-autoload
(defun pel-copy-paragraph-at-point (&optional n)
  "Copy and show complete paragraph at point.
With argument N, copy N consecutive paragraphs;
a negative N copies the current one and N-1 previous paragraphs."
  (interactive "P")
  (let ((n (prefix-numeric-value n)))
    (save-excursion
      (if (> n 0)
          (backward-paragraph)
        (forward-paragraph))
      (copy-region-as-kill (point)
                           (progn
                             (forward-paragraph n)
                             (point))))
    (pel--show-copied)))


;;-pel-autoload
(defun pel-copy-paragraph-start ()
  "Copy and show text between beginning of paragraph and point."
  (interactive)
  (save-excursion
    (copy-region-as-kill (point)
                         (progn (backward-paragraph) (point)))
    (pel--show-copied)))

;;-pel-autoload
(defun pel-copy-paragraph-end ()
  "Copy and show text between point and en of paragraph."
  (interactive)
  (save-excursion
    (copy-region-as-kill (point)
                         (progn
                           (forward-paragraph)
                           (point)))
    (pel--show-copied)))


;;-pel-autoload
(defun pel-copy-line-start ()
  "Copy and show text between beginning of line and point."
  (interactive)
  (save-excursion
    (copy-region-as-kill (point)
                         (progn
                           (move-beginning-of-line nil)
                           (point)))
    (pel--show-copied)))

;;-pel-autoload
(defun pel-copy-line-end ()
  "Copy and show text between point and end of line."
  (interactive)
  (save-excursion
    (copy-region-as-kill (point)
                         (progn
                           (move-end-of-line nil)
                           (point)))
    (pel--show-copied)))


;;-pel-autoload
(defun pel-copy-char-at-point (&optional n)
  "Copy and show single character at point.
With argument N, copy N consecutive characters;
a negative N copies the character backwards (before point)."
  (interactive "P")
  (save-excursion
    (copy-region-as-kill (point)
                         (progn
                           (forward-char (prefix-numeric-value n))
                           (point)))
    (pel--show-copied)))


;; ---------------------------------------------------------------------------
;; Kill Commands
;; -------------

(defun pel--kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING.
See `bounds-of-thing-at-point' for a list of possible THING symbols."
  (pel--ccp-require-thingatpt)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (progn
          (kill-region (car bounds) (cdr bounds))
          (pel--show-killed))
      (error "No %s at point" thing))))

;;-pel-autoload
(defun pel-kill-word-at-point ()
  "Kill the word at point."
  (interactive "*")
  (pel--kill-thing-at-point 'word))

;;-pel-autoload
(defun pel-kill-word-part (&optional beginning)
  "Kill the end of word at point: from point to end of current word.
With any prefix argument kill the BEGINNING of word up to current point."
  (interactive "*P")
  (insert " ")
  (if beginning
      (progn
        (left-char 2)
        (pel--kill-thing-at-point 'word)
        (delete-char 1))
    (pel--kill-thing-at-point 'word)
    (delete-char -1)))

;;-pel-autoload
(defun pel-kill-symbol-at-point ()
  "Kill the symbol at point."
  (interactive "*")
  (pel--kill-thing-at-point 'symbol))

;;-pel-autoload
(defun pel-kill-symbol-part (&optional beginning)
  "Kill the end of symbol at point: from point to end of current symbol.
With any prefix argument kill the BEGINNING of symbol up to current point."
  (interactive "*P")
  (insert " ")
  (if beginning
      (progn
        (left-char 2)
        (pel--kill-thing-at-point 'symbol)
        (delete-char 1))
    (pel--kill-thing-at-point 'symbol)
    (delete-char -1)))

;;-pel-autoload
(defun pel-kill-sentence-at-point ()
  "Kill sentence at point."
  (interactive "*")
  (pel--kill-thing-at-point 'sentence))

;;-pel-autoload
(defun pel-kill-function-at-point ()
  "Kill function at point."
  (interactive "*")
  (pel--kill-thing-at-point 'defun))

;;-pel-autoload
(defun pel-kill-sexp-at-point ()
  "Kill sexp at point."
  (interactive "*")
  (pel--kill-thing-at-point 'sexp))

;;-pel-autoload
(defun pel-kill-whitespace-at-point ()
  "Kill all whitespace at and around point.
Copy removed characters to kill ring."
  (interactive "*")
  (pel--kill-thing-at-point 'whitespace))

;;-pel-autoload
(defun pel-kill-filename-at-point ()
  "Kill filename at point."
  (interactive "*")
  (pel--kill-thing-at-point 'filename))

;;-pel-autoload
(defun pel-kill-url-at-point ()
  "Kill URL at point."
  (interactive "*")
  (pel--kill-thing-at-point 'url))

;;-pel-autoload
(defun pel-kill-list-at-point ()
  "Kill list at point."
  (interactive "*")
  (pel--kill-thing-at-point 'list))

;; (defun pel-kill-marked-or-whole-line ()
;;   "Kill the current marked area if any, otherwise kill whole current line.
;; Uses `kill-region' to perform the kill operation. "
;;   (interactive)
;;   (if (use-region-p)
;;       (kill-region (region-beginning) (region-end))
;;     (kill-region (line-beginning-position) (1+ (line-end-position)))))

;;-pel-autoload
(defun pel-kill-paragraph-at-point (&optional n)
  "Kill complete paragraph at point.
With argument N, kill N consecutive paragraphs;
a negative N kills the current one and N-1 previous paragraphs."
  (interactive "*P")
  (let ((n (prefix-numeric-value n)))
    (progn
      (if (> n 0)
          (backward-paragraph)
        (forward-paragraph))
      (kill-region  (point)
                    (progn
                      (forward-paragraph n)
                      (point)))
      (pel--show-killed))))

;;-pel-autoload
(defun pel-kill-char-at-point (&optional n)
  "Kill single character at point.
With argument N, kill N consecutive characters;
a negative N kills characters backwards."
  (interactive "*P")
  (save-excursion
    (kill-region (point)
                 (progn
                   (forward-char (prefix-numeric-value n))
                   (point)))
    (pel--show-killed)))

;; ---------------------------------------------------------------------------
;; Delete Commands
;; ---------------

(defun pel-delete-all-empty-lines (&optional begin end)
  "Remove all empty lines from marked area or the entire buffer."
  (interactive "*")
  (let ((start-point (if (use-region-p)
                         (region-beginning)
                       (or begin (point-min))))
        (end-point   (if (use-region-p)
                         (region-end)
                       (or end (point-max)))))
    (flush-lines "^\\s-*$" start-point end-point)))

;;-pel-autoload
(defun pel-delete-word-at-point ()
  "Delete the word at point."
  (interactive "*")
  (let (kill-ring
        (pel--isa-delete-operation t))
    (pel-kill-word-at-point)))


;;-pel-autoload
(defun pel-delete-word-part (&optional beginning)
  "Delete the end of word at point: from point to end of current word.
With any prefix argument delete the BEGINNING of word up to current point."
  (interactive "*P")
    (let (kill-ring
          (pel--isa-delete-operation t))
      (pel-kill-word-part beginning)))

;;-pel-autoload
(defun pel-delete-symbol-at-point ()
  "Delete the symbol at point."
  (interactive "*")
  (let (kill-ring
        (pel--isa-delete-operation t))
    (pel-kill-symbol-at-point)))

;;-pel-autoload
(defun pel-delete-symbol-part (&optional beginning)
  "Delete the end of symbol at point: from point to end of current symbol.
With any prefix argument delete the BEGINNING of symbol up to current point."
  (interactive "*P")
  (let (kill-ring
        (pel--isa-delete-operation t))
    (pel-kill-symbol-part beginning)))

;;-pel-autoload
(defun pel-delete-sentence-at-point ()
  "Delete sentence at point."
  (interactive "*")
  (let (kill-ring
        (pel--isa-delete-operation t))
    (pel-kill-sentence-at-point)))

;;-pel-autoload
(defun pel-delete-function-at-point ()
  "Delete function at point."
  (interactive "*")
  (let (kill-ring
        (pel--isa-delete-operation t))
    (pel-kill-function-at-point)))

;;-pel-autoload
(defun pel-delete-sexp-at-point ()
  "Delete sexp at point."
  (interactive "*")
  (let (kill-ring
        (pel--isa-delete-operation t))
    (pel-kill-sexp-at-point)))

;;-pel-autoload
(defun pel-delete-whitespace-at-point ()
  "Delete all whitespace at and around point on a single line."
  (interactive "*")
  (let (kill-ring
        (pel--isa-delete-operation t))
    (pel-kill-whitespace-at-point)))

;;-pel-autoload
(defun pel-delete-filename-at-point ()
  "Delete filename at point."
  (interactive "*")
  (let (kill-ring
        (pel--isa-delete-operation t))
    (pel-kill-filename-at-point)))

;;-pel-autoload
(defun pel-delete-url-at-point ()
  "Delete URL at point."
  (interactive "*")
  (let (kill-ring
        (pel--isa-delete-operation t))
    (pel-kill-url-at-point)))

;;-pel-autoload
(defun pel-delete-list-at-point ()
  "Delete list at point."
  (interactive "*")
  (let (kill-ring
        (pel--isa-delete-operation t))
    (pel-kill-list-at-point)))

;;-pel-autoload
(defun pel-delete-line ()
  "Delete text from cursor to end of line."
  (interactive "*")
  (delete-region
   (point)
   (progn
     (move-end-of-line 1)
     (point))))

;;-pel-autoload
(defun pel-delete-paragraph-at-point (&optional n)
  "Delete complete paragraph at point.
With argument N, kill N consecutive paragraphs;
a negative N deletes the current one and N-1 previous paragraphs."
  (interactive "*P")
  (let (kill-ring
        (pel--isa-delete-operation t))
    (pel-kill-paragraph-at-point n)))

;;-pel-autoload
(defun pel-backward-delete-paragraph (&optional n)
  "Delete back to start of paragraph.
With arg N, delete back to Nth start of paragraph;
negative arg -N means delete forward to Nth end of paragraph."
  (interactive "*P")
  (let (kill-ring
        (pel--isa-delete-operation t))
    (pel-require 'paragraphs)
    (when (fboundp 'backward-kill-paragraph)
      (backward-kill-paragraph n))))

;;-pel-autoload
(defun pel-delete-paragraph (&optional n)
  "Delete forward to end of paragraph.
With arg N, delete forward to Nth end of paragraph;
negative arg -N means delete backward to Nth start of paragraph."
  (interactive "*P")
  (let (kill-ring
        (pel--isa-delete-operation t))
    (pel-require 'paragraphs)
    (when (fboundp 'kill-paragraph)
      (kill-paragraph n))))

;;-pel-autoload
(defun pel-delete-sexp (&optional n)
  "Delete the next N sexp or the current one from the point forward.
With numeric argument: delete that many sexp in the direction identified
by the sign of the argument.
With negative sign: delete the previous sexp (the sexp backward)."
  (interactive "*P")
  (let (kill-ring
        (pel--isa-delete-operation t))
    (pel-require 'lisp)
    (when (fboundp 'kill-sexp)
      (kill-sexp n))))

;;-pel-autoload
(defun pel-backward-delete-sexp (&optional n)
  "Delete the sexp (balanced expression) preceding point.
With ARG, delete that many sexps before point.
Negative arg -N means delete N sexps after point.
This command assumes point is not in a string or comment."
  (interactive "*P")
  (let (kill-ring
        (pel--isa-delete-operation t))
    (pel-require 'lisp)
    (when (fboundp 'backward-kill-sexp)
      (backward-kill-sexp n))))

;;-pel-autoload
(defun pel-delete-rectangle (start end)
  "Delete the rectangle region identified by START and END."
  (interactive "*r")
  (let (kill-ring
        (pel--isa-delete-operation t))
    (pel-require 'rect)
    (when (fboundp 'kill-rectangle)
      (kill-rectangle start end nil))))

;; ---------------------------------------------------------------------------
;; Flexible whole-line/marked area delete/kill
;; -------------------------------------------

;;-pel-autoload
(defun pel-delete-whole-line ()
  "Delete current line (including line termination); don't store in kill ring."
  (interactive "*")
  (delete-region (line-beginning-position)
                 (min (point-max) (1+ (line-end-position)))))

(defun pel--current-line-empty-p ()
  "Return t if current line is empty, nil otherwise."
  (string-match-p "\\`\\s-*$" (thing-at-point 'line)))

(defun pel--kill-line-but-delete-if-empty ()
  "Kill current line with text, otherwise delete it."
  (if (pel--current-line-empty-p)
      (pel-delete-whole-line)
    (kill-whole-line 1)))

(defun pel--delete-whole-lines (n)
  "Delete N forward lines, don't remember in kill ring."
  (dotimes (_i n)
    (pel-delete-whole-line)))

;;-pel-autoload
(defun pel-kill-or-delete-marked-or-whole-line (&optional n)
  "Flexible region/whole-line kill/delete.

- N Identifies operation:
  - N = 0           := kill region (active/visible or not)
  - Sign of N selects operation:
    - positive := kill  (default)
    - negative := delete
- Select text to delete/kill based on region presence and state:
  - if a region is marked and active: kill/delete region's text,
  - otherwise (no active region)    : kill/delete abs(N) lines, start at point.
- If operation is to kill one line and the line is empty,
  then *delete* the line instead of killing it.
- Scenarios:
  - With no active (visible) region, no argument: kill current line,
    but if line is empty delete it instead.
  - With arg=0: kill region's text, whether region is active/visible or not.
  - With no active region, arg 4: kill 4 lines including current one.
  - With no active region, arg -: delete current line.
  - With no active region: arg -3: delete 3 lines including current one.
  - With an active (visible) region: no argument, kill the region.
  - With an active (visible) region: argument negative: delete the region.
When text is killed it is killed by `kill-region', so it retains
the filtering and `kill-ring' appending capabilities."
  (interactive "*P")
  ;; if n is nil or 0, interpret as if it was 1,
  ;; otherwise retain the positive or negative value.
  (let ((n (prefix-numeric-value n)))
    (if (= n 0)
        (kill-region (region-beginning) (region-end))
      (if (use-region-p)
          (progn
            ;; lazy load delsel because it's the only used here.
            ;; delsel is part of standard Emacs distribution.
            (require 'delsel nil :no-error)
            (if (fboundp 'delete-active-region)
                ;; when n>0, kill region otherwise delete it
                (delete-active-region (> n 0))))
        (if (> n 0)
            ;; when nothing is marked
            (if (= n 1)
                (pel--kill-line-but-delete-if-empty)
              ;; kill n lines when n is 2 or more
              (kill-whole-line n))
          ;; otherwise delete the (abs n) lines
          (pel--delete-whole-lines (abs n)))))
    (pel--show-killed)))

;; ;;-pel-autoload
;; (defun pel-replace-with-kill ()
;;   ""
;;   (interactive)
;;   (yank)
;;   (pel-kill-or-delete-marked-or-whole-line -1))

;; Copy current marked region or whole current line
;; ------------------------------------------------

;;-pel-autoload
(defun pel-mark-whole-line ()
  "Mark the complete current line."
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil))

;;-pel-autoload
(defun pel-copy-marked-or-whole-line (&optional n)
  "Copy visible region if any, otherwise copy current line to kill ring.
The copy operation is controlled by the (optional) argument:
- If N = 0:  copy region (regardless of whether it is visible or not.
- If a region is active/visible: copy the region's text.
- if no region is active/visible copy N lines:
  - If no argument, (N=1) copy current line.
  - If N > 0: copy current line and N-1 following lines.
  - If I < 0: copy current line and N-1 previous lines.
All copied lines are complete.
The copied text is saved in the `kill-ring', and in graphics
mode, to the OS clipboard.
All copy operations are performed by `kill-ring-save'."
  (interactive "P")
  (let ((n (prefix-numeric-value n)))
    (if (= n 0)
        (kill-ring-save (region-beginning) (region-end))
      (if (use-region-p)
          (copy-region-as-kill (region-beginning) (region-end))
        (save-excursion
          (if (> n 0)
              (kill-ring-save (line-beginning-position)
                              (progn
                                (forward-line n)
                                (point)))
            (kill-ring-save (1+ (line-end-position))
                            (progn
                              (forward-line n)
                              (beginning-of-line)
                              (point)))))))
    (pel--show-copied)))

;; Kill beginning of line
;; ----------------------
;;-pel-autoload
(defun pel-kill-from-beginning-of-line ()
  "Kill from the beginning of the line to point."
  (interactive "*")
  ;; lazy load simple because it's one of the 2 functions here that uses it.
  ;; simple is part of Emacs standard distribution.
  (require 'simple)
  (kill-line 0)
  (pel--show-killed))

;;-pel-autoload
(defun pel-delete-from-beginning-of-line ()
  "Delete from the beginning of the line to point."
  (interactive "*")
  (let (kill-ring
        (pel--isa-delete-operation t))
    (pel-kill-from-beginning-of-line)))

;; ---------------------------------------------------------------------------
;; Delete whitespace between point and next non-whitespace
;; -------------------------------------------------------
;;-pel-autoload
(defun pel-delete-to-next-visible ()
  "Delete all whitespace between point and next non-whitespace character."
  (interactive "*")
  (if (and (require 'pel-navigate nil :no-error)
           (fboundp 'pel-next-visible))
      (progn
        (set-mark-command nil)
        (pel-next-visible 1)
        (backward-delete-char-untabify 1))
    (error "Function pel-next-visible not loaded")))

;;-pel-autoload
(defun pel-kill-word-and-whitespace (arg)
  "Kill forward word, delete all whitespace following it.
With argument ARG, kill that many words then the whitespace following them."
  (interactive "*p")
  ;; lazy load simple because it's one of the 2 functions here that uses it.
  ;; simple is part of Emacs standard distribution.
  (require 'simple)
  (kill-word arg)                   ; kill next word(s)
  (kill-append " " nil)             ; separate works in kill ring
  (pel-delete-to-next-visible))     ; delete whitespace to next word

;; ---------------------------------------------------------------------------
;; Duplicate current line
;; ----------------------

;;-pel-autoload
(defun pel-duplicate-line (&optional n)  ; line a line another line
  "Duplicate the current line N times.  N defaults to 1.

Insert new line(s) below and move point to the last one entered,
at the same relative position inside the line.

When N is 0, one copy is made but the cursor stays on the
original line.

If some text on the original line is marked, the function prompts
for a replacement, and replace each instance of that text in the
duplicated line. If N is negative the replacement is only done
for the marked area.

When (abs N) is larger than 1 the function inserts that many
duplicated lines, and prompts for a new replacement for each new
line.  The command maintains a prompt history, accessible with
'M-n' and 'M-p'.

Nothing is copied to the kill ring."
  (interactive "*p")
  (let* ((keep-original-position nil)
         (replace-once (< n 0))
         (n (abs n))
         (original-pos (point))
         (original-bol (line-beginning-position))
         (original-line (buffer-substring-no-properties original-bol
                                                        (line-end-position)))
         (text-to-replace (when (use-region-p)
                            (buffer-substring-no-properties (region-beginning)
                                                            (region-end))))
         text-before
         text-after)
    (when (eq n 0)
      (setq n 1)
      (setq keep-original-position t))
    (when (use-region-p)
      (setq text-before (buffer-substring-no-properties
                         original-bol
                         (region-beginning)))
      (setq text-after (buffer-substring-no-properties
                        (region-end)
                        (line-end-position))))
    (while (progn
             (forward-line 1)
             (insert (if text-to-replace
                         (let ((new-text (read-from-minibuffer
                                          (format "%s  -> " text-to-replace)
                                          nil nil nil
                                          'pel-duplicate-line-history)))
                           (if replace-once
                               (concat text-before new-text text-after)
                             (replace-regexp-in-string
                              text-to-replace
                              new-text
                              original-line nil t nil)))
                       original-line)
                     "\n")
             (forward-line -1)
             (right-char (- original-pos original-bol))
             (> (setq n (1- n)) 0)))
    (when keep-original-position
      (goto-char original-pos))))

;; ---------------------------------------------------------------------------
;; Join with next line
;; -------------------

;;-pel-autoload
(defun pel-join-next-line ()
  "Join this line to the following line."
  (interactive)
  (delete-indentation 1))


;; ---------------------------------------------------------------------------
;; Specialized yank
;; ----------------
;;
;; PEL provides a basic yank command, `pel-overwrite-yank' that behaves as
;; Emacs standard yank or as text replacement when the buffer is in overwrite
;; mode.  The insertion/overwrite behaviour is controlled by the
;; `pel-activate-overwrite-yank' user option, which initializes the buffer
;; local `pel--activate-overwrite-yank'.  That can be further modified
;; dynamically in each buffer by execution of the `pel-toggle-overwrite-yank'
;; command.

(defvar-local pel--activate-overwrite-yank pel-activate-overwrite-yank
  "Activate overwrite yank when overwrite-mode is on.")

(defun pel-toggle-overwrite-yank ()
  "Toggle overwrite yank."
  (interactive)
  (if (eq (key-binding (kbd "C-y")) 'pel-overwrite-yank)
      (pel-toggle-and-show 'pel--activate-overwrite-yank
                           "overwriting" "inserting"
                           :locally "When in overwrite-mode, yank")
    (user-error "C-y is not bound to pel-overwrite-yank in this buffer")))

(defun pel-overwrite-yank ()
  "Yank over existing text when overwrite-mode is active.
Otherwise yank/insert as usual."
  (interactive)
  (when (and pel--activate-overwrite-yank
             (bound-and-true-p overwrite-mode))
    (delete-char (length (current-kill 0))))
  (yank))

;;; --------------------------------------------------------------------------

(provide 'pel-ccp)

;;; pel-ccp.el ends here
