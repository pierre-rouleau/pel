;;; pel-file.el --- File Management utilities -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2021  Pierre Rouleau

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
;; This file holds the logic to extract a file name or URL from the point
;; location, and then open the file inside a specified Emacs window or the URL
;; inside an external process frame.
;;
;; The point can be located anywhere inside the file or URL in all
;; cases except when the file name includes embedded spaces.  To
;; extract a file name with embedded spaces, the file name must be
;; enclosed with double quotes and the point must be located on the
;; opening quote.  Line and column numbers are also supported.
;;
;; The file is opened inside a specific window if it is specified by numeric
;; argument.  If a window already contains the file and no argument specify
;; where to open the file, just move point to that window.  The numeric argument
;; identify a cardinal direction to the target window.  If the pointed window is
;; the minibuffer or a dedicated window the command fails.
;;
;; Credits: The pathstop string with Unicode block characters
;;          originally borrowed from Xah Lee's xah-open-file-at-cursor
;;          function at his web site:
;;          `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'.


(require 'pel--base)     ; use: pel-val-or-default,
                         ;      pel-goto-position,
                         ;      pel-system-is-windows-p
(require 'pel-read)      ; use: pel-string-at-point
(require 'pel-window)    ; use pel-window-direction-for
;;                       ;     pel-window-valid-for-editing-p

(eval-when-compile (require 'subr-x))  ; use: inlined: string-trim

;;; Code:

;; Implementation call hierarchy
;; -----------------------------
;;
;; * pel-show-filename-at-point  (A)
;;     - pel-filename-at-point
;;       - pel-string-at-point
;;
;;
;; * pel-show-filename-parts-at-point    (A)
;; * pel-find-file-at-point-in-window    (A)
;;   - pel-filename-parts-at-point
;;   - pel--complete-filename-for
;;     - pel--lib-filename
;;       - pel-prompt-for-filename
;;  - pel--show-edit-action
;;

;; ---------------------------------------------------------------------------
;; pel-find-file-at-point-in-window
;; --------------------------------

(defun pel-filename-parts-at-point (&optional keep-file-url)
  "Extract and return (filename line column) from string at point.
.
Return:
- nil if no valid file name found at point.
- a ('http . url-string) cons where url-string is the URL.
- a list of (name-type filename line column) if a file name  is present.
  where:
  - name-type := symbol : fname | fname-w-ddrv | http
                 : 'fname := normal file name, with/without Unix-style path
                 : 'fname-w-ddrv := filename with disk drive letter
  - filename : filename string.
  - line     : integer, or nil for missing number.  Starts at 1.
  - column   : integer, or nil for missing number.  Starts at 0.

The function accepts Unix and Windows style file names and path.
It accepts ':' and '@' as separators between the elements.
Spaces are accepted within the file name and between the separators
but *only* when the complete string is enclosed in double quotes
*and* when point is located at the first quote.

A file URL is stripped from the string unless KEEP-FILE-URL is non-nil."
  (let ((str (pel-filename-at-point)))
    (unless keep-file-url
      (setq str (replace-regexp-in-string "^file:////?/?" "/" str)))
    ;; first check for web URIs and return them.
    (if (string-match-p "\\`https?://" str)
        (cons 'http str)
      (if (string-match-p "\\`file://" str)
          (cons 'http str)

        ;; - Regexp provides ability to match with and without line and columns
        ;; - If line is present it's a number between 2 separators, with each
        ;;   each separator being either ':' or '@' with optional spaces.
        ;; - If column is present it follows the second separator. The column
        ;;   group can hold any alphanumeric and some punctuation characters.
        ;;
        ;; Prior to processing the string, the potential file URI prefix
        ;; is removed:
        ;;  "file:////?/?" optional prefix is removed.
        ;;                 The standard allows 3 slashes but some people use
        ;;                 4 or 5, so they are removed too.
        ;;
        ;; The overall structure of the regexp identifies the following groups:
        ;; G1?G2((G5 G6)(G8 G9)?)?
        ;; where:
        ;; - G1 := MS-DOS/Windows disk-drive letter and colon. nil if absent.
        ;; - G2 := filename
        ;; - G6 := line number string (only digits), if it exists (it can be nil)
        ;; - G9 := column field.  It may be any text, may start with number,
        ;;                        maybe nil.
        ;; The numbers are extracted with string-to-number which return 0
        ;; if it's text.
        ;; Note: The regexp is split to fit in 80 columns: the G-g identify
        ;;       the beginning and end of a group in the expression that spans
        ;;       3 lines.
        (if (string-match
             ;;
             ;;
             (concat
              ;;     G1         g1    G2     g2   G3 G4 G5        g5
              ;;     (-----------)   (--------)   (  (  (----------)
              "^\\`\\([a-zA-Z]:\\)?\\([^:@]+?\\)\\(\\(\\( *[:@] *\\)"
              ;; G6       g6 g4  G7 G8
              ;; (---------)  )  (  (----------)
              "\\([0-9]+?\\)\\)\\(\\( *[:@] *\\)"
              ;; G9                      g7 g3
              ;; (---------------------)  )  )
              "\\([[:alnum:] ,:;\\.]+\\)\\)\\)?\\'")
             str)
            (let* ((ddrv_str  (match-string 1 str))
                   (fpath_str (concat ddrv_str (match-string 2 str)))
                   ;; line to 0 if no line in str.
                   (match6    (match-string 6 str))
                   (line_num  (if match6 (string-to-number match6)))
                   ;; change line 0 to line 1
                   (line_num  (if (equal line_num 0) 1 line_num))
                   ;; column to nil if no line or column in str.
                   (match9   (match-string 9 str))
                   (col_num   (when (and match6 match9)
                                (string-to-number match9))))
              (list (if ddrv_str 'fname-w-ddrv 'fname)
                    fpath_str line_num col_num))
          ;; For reasons I don't yet understand, the above regexp does not work
          ;; if only one separator; with line number follows the file name.
          ;; So, I try again, with a different regexp, not looking
          ;; for a column.
          (if (string-match
               ;;     G1              G2           G3     G4
               ;;     (-----------)   (---------)  (   )  (---------)
               "^\\`\\([a-zA-Z]:\\)?\\([^:@]+?\\)\\(:\\)\\([0-9]+?\\)\\'"
               str)
              (let* ((ddrv_str  (match-string 1 str))
                     (fpath_str (concat ddrv_str (match-string 2 str)))
                     ;; line to 0 if no line in str.
                     (line_num  (string-to-number (pel-val-or-default
                                                   (match-string 4 str) "")))
                     ;; but change line 0 to line 1
                     (line_num  (if (equal line_num 0) 1 line_num)))
                (list
                 (if ddrv_str 'fname-w-ddrv 'fname) fpath_str line_num nil))))))))

(defun pel-prompt-for-filename (default-filename)
  "Prompt for a file name, with DEFAULT-FILENAME shown.
The DEFAULT-FILENAME must be a string or nil.
User can either accept the filename or modify it.
If the file does not already exist, a confirmation is requested.
Returns the filename string."
  (unless default-filename
    (setq default-filename ""))
  ;; read-file-name is flexible but I find it non-obvious. To get it to show
  ;; the filename, it seems to have to be placed in the DIR argument.
  ;; With it a single word (no path) will be interpreted as a file in the local
  ;; directory (which is what I want).
  ;; MUSTMATCH and PREDICATE are set to ensure the file exists.  If it does not
  ;; the user must confirm and then the caller can create it.
  (expand-file-name (read-file-name
                     ;; PROMPT
                     "Open? (C-g to quit): "
                     ;; DIR
                     default-filename
                     ;; DEFAULT_FILENAME
                     nil
                     ;; MUSTMATCH
                     'confirm
                     ;; INITIAL
                     nil
                     ;; PREDICATE
                     'file-exists-p)))

(defun pel--lib-filename (filename)
  "Infer or prompt for library filename using incomplete FILENAME.
Return (filename . action), where:
- filename is the file to create or open, or nil if no file to handle.
- action is: 'create | 'edit | reason-for-nil-filename

nil if user gave up, otherwise return the file name to open."
  (if (and (require 'pel-prompt nil :no-error)
           (fboundp 'pel-y-n-e-or-l-p))
      (let ((action
             (pel-y-n-e-or-l-p
              (format "\
File「%s」not found.   Create it, edit name or find Library file? "
                      filename))))
        (cond
         ((equal action 'yes)
          (cons filename 'create))
         ;;
         ((equal action 'no)
          (cons nil   "Cancelled."))
         ;;
         ((equal action 'edit)
          (let ((filename (pel-prompt-for-filename filename)))
            (cons filename (if (file-exists-p filename)
                               'edit
                             'create))))
         ;;
         ((equal action 'findlib)
          (when (and (require 'find-func)
                     (fboundp 'find-library-name))
            (let ((filename (find-library-name (file-name-base filename))))
              (cons filename (if (file-exists-p filename)
                                 'edit
                               'create)))))))
    (error "Function pel-prompt not loaded")))



(defun pel--complete-filename-for (filename)
  "Identify the complete file name for a potentially incomplete FILENAME.
Prompt the user if necessary.  In some case the user may want to create a
new file with a specified filename.
Return: (filename . action)
where: - filename:= string or nil: the  filename to act upon if not nil.
       - action  := 'edit | 'create | message-string
         where the message string is returned with nil to describe why
         we do not edit or create the file."
  (let (action)
    (if (file-exists-p filename)
        (setq action 'edit)
      (let* ((filename-action    (pel--lib-filename filename))
             (selected-filename  (car filename-action))
             (selected-action    (cdr filename-action)))
        (when selected-filename
          (setq filename selected-filename))
        (setq action selected-action)))
    (cons filename action)))


(defun pel--show-edit-action (action filename &optional line column)
    "Display message showing ACTION done on FILENAME at LINE and COLUMN.
ACTION   := symbol | string
FILENAME := string
LINE     := integer | nil
COLUMN   := integer | nil"
    (message "%s %s%s%s%s" action filename
             (if (or line column) " at " "")
             (if line   (format "line:%d" line) "")
             (if column (format "col:%d" column) "")))

;;-pel-autoload
(defun pel-find-file-at-point-in-window (&optional n)
  "Open file/URL of name located at/around point in specified window.
.
*Window selection:*
- Only effective for opening file.  Ignored when opening a URL.
- If no argument is provided,
  - If file is already open in an existing window, select that window
  - If file is not already opened in a window, select the window
    according to the number number of windows in the current frame:
   - 2 windows in frame: open in *other* window
   - 1 window  in frame: split window sensibly and open in new  window.
   - otherwise, open in current window
- If a prefix numeric argument N is supplied, it identifies the location
  of the target window:
  - N < 0 := 'new
  - N = 0 := 'other
  - N = 1,3 or 7:= select the window according to the number number of windows
             in the current frame:
             - 2 windows in frame: open in *other* window
             - 1 window  in frame: split window sensibly and open in new
                                   window.
             - otherwise, open in current window.
  - For N= 2, 4, 5, 6 or 8, select window pointed by what is pointed
    by cursor positionned at the layout of numeric keypad:
    -             8 := 'up
    - 4 := 'left  5 := 'current  6 := 'right
    -             2 := 'down
  - For N=9  := open the file in the system's browser.
- Explicitly selecting the minibuffer window, a dedicated window
  or a non-existing window is not allowed.  Instead the command creates
  a new window for the file.
.
*File/URL selection at point:*
If the string starts with `http:/' or `https:/' it is
identified as a URL.  In that case a browser process is
launched to open the URL.  If N is 9, open the file in a browser.
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
When an action is taken the action and the URL or (potentially expanded)
filename is displayed in the echo area, showing line and column number if they
were specified."
  (interactive "P")
  ;; - grab file name/URL at point
  ;; - if point is a URL, launch the system browser for it.
  ;; - otherwise, check if this filename is already in a buffer in a window
  ;; - select window:
  ;;   - if N is nil and a buffer holds the file, check if a window is currently
  ;;     displaying the buffer that holds the file.  If so, use that window.
  ;;     Otherwise, search for a window the normal way.
  (let* ((use-browser (eq 9 (prefix-numeric-value n)))
         (fileparts (pel-filename-parts-at-point use-browser))
         (file-kind (car fileparts)))
    (cond ((eq file-kind 'http)
           (browse-url (cdr fileparts))
           (pel--show-edit-action "browse" (cdr fileparts)))

          ;; It's a file, not a URL but forced to use a browser.
          (use-browser
           (browse-url (format "file:///%s" (expand-file-name (cadr fileparts))))
           (pel--show-edit-action "browse" (cdr fileparts)))

          ;; nothing found
          ((not file-kind)
           (user-error "No valid filename/URL at point!"))

          ;; A filename string found at point. It might be incomplete.
          ;; If incomplete: complete it, prompt user if necessary.
          ;; Then check if filename is currently opened in a buffer
          ;; and if that buffer is in a window already.
          ;; At this point:  fileparts := (kind filename line column)
          (t
           (let* ((filename (cadr fileparts))
                  (fn-action (pel--complete-filename-for filename))
                  (filename  (expand-file-name (car fn-action)))
                  (action    (cdr fn-action))
                  (buffer   (find-buffer-visiting filename))
                  (window   (when buffer (get-buffer-window buffer)))
                  (line     (caddr fileparts))
                  (column   (cadddr fileparts)))
             (if (and window (null n))
                 ;; file is already in a buffer and window and position
                 ;; is not imposed by argument n: use that existing
                 ;; window and move point to where specified if any.
                 (progn
                   (select-window window)
                   (pel-goto-position line column)
                   (pel--show-edit-action "show" filename line column))
               ;; the file is not inside a existing window,
               ;; but a buffer may hold the file.
               ;; Since find-file will open that buffer then
               ;; what is needed now is to determine what window to use
               ;; and open the file inside that window.
               ;; The filename might be absolute, relative, incomplete.
               (let ((direction (pel-window-direction-for
                             (prefix-numeric-value n) nil :for-editing)))
                 (cond ((eq action 'edit) (progn  ; progn just to indent
                                            (pel-window-select direction)
                                            (find-file filename)
                                            (pel-goto-position line column)
                                            (pel--show-edit-action action
                                                                   filename line column)))
                       ((eq action 'create) (progn
                                              (pel-window-select direction)
                                              (find-file filename)
                                              (pel--show-edit-action action
                                                                   filename line column)))
                       ((stringp action)   (message "%s" action))
                       (t (error "Internal error condition detected!"))))))))))

;; --

;;-pel-autoload
(defun pel-show-filename-parts-at-point (&optional keep-file-url)
  "Display file parts extracted from point.  Testing utility."
  (interactive "P")
  (message "%S" (pel-filename-parts-at-point keep-file-url)))

;; -----------------------------------------------------------------------------
;; Show filename at point
;; ----------------------

(defun pel-filename-at-point ()
  "Return the file name at point (or marked region).
.
Spaces inside the filename are accepted *only* when point
is located before a double quote to the left of the filename.
Spaces between the quote and the first character and
last character or the filename are accepted but removed.
When executed from with a buffer in sh-mode, the shell variables
found in the string are expanded and the delimiters include the
'=' and ':' characters.  This helps extracting file names in shell scripts."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (save-excursion
      (let ((delimiters
             "\t\n\"`'‘’“”|()[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。"))
        ;; In shell modes, allow delimiting the filenames by path separators
        ;; and equal sign used in various statements.
        (when (eq major-mode 'sh-mode)
          (setq delimiters (concat "=:" delimiters)))
        (let ((fname (string-trim (pel-string-at-point delimiters))))
          (when (eq major-mode 'sh-mode)
            (require 'env nil :noerror)
            (setq fname (substitute-env-vars fname)))
          (if (string= (substring fname -1) ":")
            (substring fname 0 -1)
          fname))))))

;;-pel-autoload
(defun pel-show-filename-at-point ()
  "Display file name at point in the mini-buffer."
  (interactive)
  (message "File name :=「%s」" (pel-filename-at-point)))

;;-pel-autoload
(defun pel-load-visited-file (&optional use-elc)
  "Load the elisp file visited by current buffer.
Load the file source, not the byte-compiled version unless
the optional USE-ELC argument is specified.
Interactively use any prefix argument."
  (interactive "P")
  (let ((fn (pel-current-buffer-filename)))
    (if (string= (file-name-extension fn) "el")
        (let ((fn (file-name-sans-extension fn)))
          (if use-elc
              (load-file (concat fn ".elc"))
            (load-file (concat fn ".el"))))
      (user-error "Cannot load %s.  It is not an Emacs Lisp file!" fn))))

;; -----------------------------------------------------------------------------

(provide 'pel-file)

;;; pel-file.el ends here
