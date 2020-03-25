;;; pel-file.el --- File Management utility: open file/URL at/around point.

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
;; This file is part of the PEL package.  It holds the logic to
;; extract a file name or URL from the point location, and then open
;; the file inside a specified Emacs window or the URL inside an
;; external process frame.
;;
;; The point can be located anywhere inside the file or URL in all
;; cases except when the file name includes embedded spaces.  To
;; extract a file name with embedded spaces, the file name must be
;; enclosed with double quotes and the point must be located on the
;; opening quote.  Line and column numbers are also supported.
;;
;; The file is opened inside a specified window.  The window must be
;; an existing window, and must not be the minibuffer window.  It can
;; be the current window or one of the adjacent windows at one of its
;; four borders (up, down, right and left).  The top-level interactive
;; function (`pel-find-file-at-point-in-window') accepts an numerical
;; argument that identifies those, but the other internal functions
;; use the following tokens instead: 'up, 'down, 'right, 'left and
;; 'current.
;;
;; Credits: The pathstop string with Unicode block characters
;;          originally borrowed from Xah Lee's xah-open-file-at-cursor
;;          function at his web site:
;;          `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'.


(require 'pel--base)                    ; use: pel-val-or-default,
                                        ;      pel-goto-position

(eval-when-compile
  (require 'subr-x))           ; use: inlined: string-trim

;; -----------------------------------------------------------------------------
;;; Code:


;; -----------------------------------------------------------------------------
;; Implementation call hierarchy
;; -----------------------------
;;
;; * pel-show-filename-parts-at-point
;; * pel-find-file-at-point-in-window
;;   - pel-find-file-at-point
;;     - pel-find-file-in-window
;;       - pel-prompt-for-filename
;;     - pel-filename-parts-at-point
;;
;; * pel-show-filename-at-point
;;     - pel-filename-at-point
;;       - pel-string-at-point
;;

(defun pel-filename-parts-at-point ()
  "Extract and return (filename line column) from string at point.
.
Return:
- nil if no valid file name found at point.
- a list of (name-type filename line column) if a file name  is present.
  where:
  - name-type := symbol : fname | fname-w-ddrv
                 : 'fname := normal file name, with/without Unix-style path
                 : 'fname-w-ddrv := filename with disk drive letter
  - filename : filename string
  - line     : integer.  1 for missing number.
  - column   : integer.  0 for missing number.

The function accepts Unix and Windows style file names and path.
It accepts ':' and '@' as separators between the elements.
Spaces are accepted within the file name and between the separators
but *only* when the complete string is enclosed in double quotes
*and* when point is located at the first quote."
  (let ((str (replace-regexp-in-string "^file:////?/?" "/" (pel-filename-at-point))))
    ;; first check for web URIs and return them.
    (if (string-match-p "\\`https?://" str)
        (list 'http str)
      ;; - Regexp provides ability to match with and without line and columns
      ;; - If line is present it's a number between 2 separators, with each
      ;;   each separator being either ':' or '@' with optional spaces.
      ;; - If column is present it follows the second separator. The column
      ;;   group can hold any alphanumeric and some punctuation characters.
      ;;
      ;; Prior to processing the string, the potential file URI prefix
      ;; is removed:
      ;;  "file:////?/?" optional prefix is removed. The standard allows 3 slashes
      ;;                 but some people use 4 or 5, so they are removed too.
      ;;
      ;; The overall structure of the regexp identifies the following groups:
      ;; G1?G2((G5 G6)(G8 G9)?)?
      ;; where:
      ;; - G1 := MS-DOS/Windows disk-drive letter and colon. nil if absent.
      ;; - G2 := filename
      ;; - G6 := line number string (only digits), if it exists (it can be nil)
      ;; - G9 := column field.  It may be any text, may start with number, me be nil.
      ;; The numbers are extracted with string-to-number which return 0 if it's text.
      (if (string-match
           ;;     G1              G2          G3 G4 G5            G6              G7 G8            G9
           ;;     (-----------)   (--------)   (  (  (----------)  (---------)  )  (  (----------)  (---------------------)  )  )
           "^\\`\\([a-zA-Z]:\\)?\\([^:@]+?\\)\\(\\(\\( *[:@] *\\)\\([0-9]+?\\)\\)\\(\\( *[:@] *\\)\\([[:alnum:] ,:;\\.]+\\)\\)\\)?\\'"
           str)
          (let* (
                 (ddrv_str  (match-string 1 str))
                 (fpath_str (concat ddrv_str (match-string 2 str)))
                 (line_num  (string-to-number (pel-val-or-default (match-string 6 str) "")))  ; line to 0 if no line in str.
                 (line_num  (if (equal line_num 0) 1 line_num))                               ; but change line 0 to line 1
                 (col_num   (string-to-number (pel-val-or-default (match-string 9 str) "")))) ; column to 0 if no column in str.
            (list (if ddrv_str 'fname-w-ddrv 'fname) fpath_str line_num col_num))
        ;; For reasons I don't yet understand, the above regexp does not work if only one separator
        ;; with line number follows the file name.  So, I try again, with a different regexp, not looking
        ;; for a column.
        (if (string-match
             ;;     G1              G2           G3     G4
             ;;     (-----------)   (---------)  (   )  (---------)
             "^\\`\\([a-zA-Z]:\\)?\\([^:@]+?\\)\\(:\\)\\([0-9]+?\\)\\'"
             str)
            (let* ((ddrv_str  (match-string 1 str))
                   (fpath_str (concat ddrv_str (match-string 2 str)))
                   (line_num  (string-to-number (pel-val-or-default (match-string 4 str) "")))  ; line to 0 if no line in str.
                   (line_num  (if (equal line_num 0) 1 line_num)))                              ; but change line 0 to line 1
              (list (if ddrv_str 'fname-w-ddrv 'fname) fpath_str line_num 0)))))))

(defun pel-prompt-for-filename (default-filename)
  "Prompt for a file name, with DEFAULT-FILENAME shown and editable.
User can either accept the filename or modify it.
If the file does not already exist, a confirmation is requested.
Returns the filename string."
  (read-file-name "Open? (C-g to quit): "
                  (if (file-name-absolute-p default-filename)
                      default-filename
                    nil)
                  nil
                  'confirm
                  (if (file-name-absolute-p default-filename)
                      nil
                    default-filename)
                  'file-exists-p))


(defun pel-find-file-in-window
    (filename direction &optional line column force)
  "Open FILENAME in a window identified by its relative position.
.
The window to used identified by DIRECTION, which must be one
of: 'up, 'down, 'right, 'left, 'current, 'other or 'new.
See `pel-find-file-at-point' for a description of these values.
.
If the LINE is specified, point is moved to specified line
number.
.
If the COLUMN is specified, point is moved to the specified
column number otherwise point goes to the beginning of the line.
If no window is present at specified DIRECTION, signal an
error and abort operation.
.
If a buffer for non-committed file with same name as FILENAME
exists, open that buffer in the window.  Otherwise look for a
file with FILENAME.  If FORCE is non-nil and the file does not
exist, force creation of that FILENAME.  When FORCE is nil
and the FILENAME does not correspond to an exiting file,
prompt the user to create that file.  Allow selection of
Yes, No or Edit action.  If Edit is selected, ask the user to
modify the FILENAME and then use that file to open or create.
If No is selected, prompt to open the Emacs Lisp library file
with the specified name.  That name does not need an file name
extension; the function attempts to find any Emacs Lisp or C
file (even compressed ones).
.
Return the selected window where the file is edited, or nil
if an invalid window selection would have been made
ie. if there's no window in the specified direction, or it's
the minibuffer window.  When nil is returned the point was
not moved."
  (if (and (require 'pel-window nil :no-error)
           (fboundp 'pel-window-valid-for-editing-p)
           (fboundp 'pel-window-select))
      (if (pel-window-valid-for-editing-p direction)
          (let ((buffer_for_file (find-buffer-visiting filename)))
            (if buffer_for_file
                (if (pel-window-select direction)
                    (progn
                      (pop-to-buffer-same-window buffer_for_file)
                      (pel-goto-position line column)
                      (message "Editing Buffer %S @ %S %S" buffer_for_file line column)
                      (selected-window)))
              (if (or (file-exists-p filename)
                      force
                      (if (and (require 'pel-prompt nil :no-error)
                               (fboundp 'pel-y-n-e-or-l-p))
                          (let* ((action (pel-y-n-e-or-l-p
                                          (format "File「%s」not found.\
  Create it, edit name or find Library file? "
                                                  filename)))
                                 (act (cond
                                       ((equal action 'yes)  t)
                                       ((equal action 'no)  nil)
                                       ((equal action 'edit)
                                        (progn
                                          (setq filename
                                                (pel-prompt-for-filename filename))
                                          t))
                                       ((equal action 'findlib)
                                        (if (fboundp 'find-library-name)
                                            (progn
                                              (setq filename (find-library-name (file-name-base filename)))
                                              t))))))
                            act)
                        (error "Function pel-prompt not loaded")))
                  (if (pel-window-select direction)
                      (progn
                        (find-file filename)
                        (pel-goto-position line column)
                        (message "Editing File %S @ %S %S" filename line column)
                        (selected-window)))))))
    (error "File pel-window no loaded")))

(defun pel-find-file-at-point (direction &optional force)
  "Open file of name located at/around point in window identified by DIRECTION.
.
DIRECTION identifies the direction from the current window.
It must be one of:  'up, 'down, 'right, 'left, 'current,
'other or 'new.  The value 'other selects the windows
identified by `next-window' while 'new selects the window
identified by `pel-split-window-sensibly'.
.
If file or buffer for extracted file name does not exist, the
function prompts to create new buffer for the name unless the
optional argument FORCE is non-nil.
.
Don't call `pel-find-file-at-point' interactively.
Use `pel-find-file-at-point-in-window' for interactive calls.
.
Return one of:
- nil    : if nothing opened,
- window : the window the file in which a file was opened/edited
- process: the process of the browser launched to browse the URL."
  (let ((fileparts (pel-filename-parts-at-point)))
    (if fileparts
        (let* ((kind (car fileparts))
               (fileparts (cdr fileparts)))
          (if (eq kind 'http)
              (browse-url (car fileparts))
            (if (and (eq kind 'fname-w-ddrv)
                     (not (pel-running-under-windows-p)))
                (user-error "Invalid Windows-type filename 「%s」; Aborted"
                            (car fileparts))
              (let* ((filename (car fileparts))
                     (line    (cadr fileparts))
                     (column (caddr fileparts)))
                (if (not (string= filename ""))
                    (pel-find-file-in-window
                     ;; expand relative paths: use current-directory of buffer
                     (expand-file-name filename)
                     direction
                     line
                     column
                     force))))))
      (user-error "Nothing valid found at point"))))

;;-pel-autoload
(defun pel-find-file-at-point-in-window (&optional n)
  "Open file/URL of name located at/around point in specified window.
.
*Window selection:*
- Only effective for opening file.  Ignored when opening a URL.
- If no argument is provided, open the file in the the window selected
   according to the number of windows in the current frame:
   - 2 windows in frame: open in *other* window
   - 1 window  in frame: split window sensibly and open in that window.
   - otherwise, open in window below (if there is one, otherwise: error)
- If a prefix numeric argument N is supplied, it identifies the location
  of the target window, mimicking the layout of a numeric keypad:
  -             8 := 'up
  - 4 := 'left  5 := 'current  6 := 'right
  -             2 := 'down
  - 0 := 'other
  - negative:= 'new
- Selecting the minibuffer window or a non-existing window is
  considered an error, identified by a user error.
.
*File/URL selection at point:*
If the string starts with `http:/' or `https:/' it is
identified as a URL.  In that case a browser process is
launched to open the URL.
Otherwise the string is used as a file name.
- The file string can have line and column integer numbers
  using the following sections: {filename}{sep}{line}{sep}{column}
  where:
  - {filename} is the filename with or without path and
    extension.  The filename can, but does not need to,
    use a 'file:///' RFC-3986 file URI prefix.
  - {sep} is a separator, one of the ':' or '@' character
  - {line} is an integer identifying a line inside the file.
    If none is specified, point is moved at the first line.
    If the line number is too large, point is moved at the last
    line of the file.
  - {column} is an integer identifying a column inside that line.
    Its possible to specify a column number without a line number,
    the line number is interpreted as being the first line.
  - {filename} is the only mandatory component.  The other sections
    are optional, but they must be used in order.
- Space characters are allowed in the {filename} and {sep}
  sections *only* when:
  - the entire string is enclosed in double quotes
  - point is located just before (at) the opening quote.
- If there are no space in the entire string, point can be
  located anywhere.
.
*Opening a file:*
- If the identified filename corresponds to an existing
  file-system file or a currently un-committed buffer, the
  function opens a, or visit the already existing, buffer for
  that file, in the selected window.
- If the file does not exist, the user is prompted to:
  - edit (modify) the file name and then open that, or
  - use the filename as is, create a new buffer for the
    unmodified file name,
  - search a library file corresponding to the identified
    file name, or
  - decline and quit.
If the user responds negatively the function displays a message
stating that nothing was opened.  Otherwise it proceeds.
If the selection was to find a library file and nothing is found
the function prints an error message and quits.
.
*Design Details*:
- The file name, line and column numbers are extracted by
  `pel-filename-parts-at-point'.
- `pel-find-file-at-point-in-window' is meant to be called
  interactively only.  From Lisp code, call `pel-find-file-at-point'
  instead."
  (interactive "P")
  ;; select direction from numerical argument.
  ;; If there are no argument, select the direction like this:
  ;; - if there is only 1 window:  use 'current
  ;; - if there is only 2 windows: use 'other
  ;; - otherwise: use 'down
  (let* ((n (prefix-numeric-value n))
         (nwindows (count-windows))
         (default-direction (cond ((eq nwindows 2) 'other)
                                  ((eq nwindows 1) 'new)
                                  (t 'down)))
         (direction (cond;((eq 2 n) 'down)
                     ((eq 8 n) 'up)
                     ((eq 4 n) 'left)
                     ((eq 6 n) 'right)
                     ((eq 5 n) 'current)
                     ((eq 0 n) 'other)
                     ((< n 0)  'new)
                     (t default-direction))))
    (unless (pel-find-file-at-point direction nil)
      (if (and (require 'pel-window nil :no-error)
               (fboundp 'pel-window-valid-for-editing-p))
          (if (pel-window-valid-for-editing-p direction)
              (message "User cancelled: nothing opened")
            (user-error
             "No valid window %s of current one: nothing opened" direction))
        (error "File pel-window is not loaded")))))

;; --

(defun pel-show-filename-parts-at-point ()
  "Display file parts extracted from point.  Testing utility."
  (interactive)
  (message "%S" (pel-filename-parts-at-point)))

;; --

(defun pel-string-at-point (delimiter forward_only)
  "Return the string at point delimited by DELIMITER string.
When FORWARD_ONLY is non-nil search is done at and after the
current point, otherwise it also checks before point.  This is
used to handle space characters inside a file name.  To extract
such a file name, it must be enclosed in double quotes and the
extraction must be done with point at the first double quote with
FORWARD_ONLY set to non-nil.  To extract file names with no
embedded spaces, with ability to position the point anywhere in
the file name, set FORWARD-ONLY to nil."
  (save-excursion
    (let (p1 p2)
      (if (not forward_only)
          (skip-chars-backward delimiter))
      (setq p1 (point))
      (skip-chars-forward delimiter)
      (setq p2 (point))
      (buffer-substring-no-properties p1 p2))))

(defun pel-filename-at-point ()
  "Return the file name at point (or marked region).
.
Spaces inside the filename are accepted *only* when point
is located before a double quote to the left of the filename.
Spaces between the quote and the first character and
last character or the filename are accepted but removed."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (save-excursion
      (let* ((point_isat_quote (eq (char-after) 34))
             (pathstop "\t\n\"`'‘’“”|()[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。")
                                        ; if char at cursor is a double quote, allow spaces in file name.
             (pathstop (concat "^" (if point_isat_quote
                                       (prog1 pathstop
                                         (forward-char))
                                     (concat " \"" pathstop)))))
        (string-trim (pel-string-at-point pathstop point_isat_quote))))))

;;-pel-autoload
(defun pel-show-filename-at-point ()
  "Display file name at point in the mini-buffer."
  (interactive)
  (message "File name :=「%s」" (pel-filename-at-point)))

;; -----------------------------------------------------------------------------

(provide 'pel-file)

;;; pel-file.el ends here
