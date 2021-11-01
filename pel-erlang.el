;;; pel-erlang.el --- Erlang programming Language support  -*- lexical-binding: t; -*-

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
;;  This file file provides code that help using and programming with Erlang.
;;  It enhances what is already available in the erlang.el file which provides
;;  the `erlang-mode'.  The features provided by the file are listed below.
;;  The functions identified with a '*' are commands.

;; Query Erlang information using Erlang
;; - `pel-erlang-detected-root-dir'

;; Toggle electric behaviour of keys:
;; * `pel-erlang-period'
;; * `pel-erlang-semicolon'
;; * `pel-erlang-newline'
;; * `pel-erlang-gt'
;; * `pel-erlang-comma'
;;   - `pel--erlang-toggle-electric-of'

;; Space after Comma electric behaviour:
;; - `pel-erlang-enhance-electric-keys'
;;   - `pel--enhanced-electric-comma'
;; * `pel-erlang-toggle-space-after-comma'

;; Period electric behaviour:
;; * `pel-erlang-electric-period'
;;   - `pel--after-dash'

;; Erlang Shell Control:
;; - `pel-erlang-shell-mode-init'

;; Navigation in Erlang source code:
;; * `pel-end-of-previous-clause'
;; * `pel-beginning-of-next-clause'
;;
;; * `pel-next-erl-function'
;; * `pel-previous-erl-function'
;;   - `pel--moveto-function'

;; Erlang specialized forward-sexp/backward-sexp:
;; * `pel-erlang-backward-sexp'
;;   - `pel-erlang-backward-binary'
;;     - `pel-erlang-after-binary'
;; * `pel-erlang-forward-sexp'
;;   - `pel-erlang-forward-binary'
;;     - `pel-erlang-before-binary'

;; Erlang Version and Man Page access management:
;; - `pel-read-available-erlang-man-versions'
;;
;; - `pel-erlang-set-dirpath'
;;
;; - `pel-erlang-exec-path'
;; - `pel-erlang-man-parent-rootdir'
;;   - `pel--erlang-dirpath'

;; Detect Erlang Root Directory:
;; - `pel-erlang-root-path'
;;   - `pel--read-erlang-root-dir'

;; Read/Show Erlang Version:
;; * `pel-show-erlang-version'
;;   - `pel-erlang-ls-version'
;;   - `pel-erlang-version'

;; Erlang Syntax Checking Control:
;; - `pel-erlang-toggle-syntax-checker'

;; Insertion of Erlang Comments:
;; * `pel-erlang-comment-dwim'
;;   - `pel--erlang-line-3%-comment-p'

;; Modify erlang.el electric key behaviour and key bindings:
;; - `pel-erlang-setup-electric-key-behaviour'
;;   - `pel-erlang-stop-when-arg-used-p'

;; Erlang Cross Reference Command Control:


;;; --------------------------------------------------------------------------
;;; Dependencies:
(require 'comint)
(require 'pel--base)            ; use: `pel-toggle-syntax-check-mode'
(require 'pel--options)         ; use: `pel-erlang-version-detection-method'
;;                              ;      `pel-erlang-path-detection-method'
;;                              ;      `pel-erlang-electric-keys'
(require 'pel-ffind)            ; use: `pel-ffind'
(require 'pel-fs)               ; use: `pel-exec-pel-bin'
(require 'pel-syntax)           ; use: `pel-insert-space-in-enclosing-block'
(require 'pel-xref)             ; use: `pel-xref-find-definitions'

;; newcomment is always available
(require 'newcomment)           ; use: `comment-dwim'
(require 'xref)

;;; --------------------------------------------------------------------------
;;; Code:

;; -------------------------------------
;; Query Erlang information using Erlang
;; -------------------------------------

(defun pel-erlang-detected-root-dir ()
  "Read Erlang's code:root_dir(): root of Erlang system.

Use the pel/bin/erlang-root-dir script to Extract the information directly
from Erlang."
  (let ((exit-code.path (pel-exec-pel-bin "erlang-root-dir")))
    (unless (eq 0 (car exit-code.path))
      (error "Failed extracting Erlang root path: %S" exit-code.path))
    (cdr exit-code.path)))

;; ---------------------------------
;; Electric Key Behaviour Management
;; ---------------------------------

;; Toggle electric behaviour of keys.
;;
;; The code below dynamically add or remove a symbol corresponding to a key
;; into the `erlang-electric-command' variable to enable or disable the
;; electricity of the corresponding key.

(defvar erlang-electric-commands)       ; erlang.el variable identifying
                                        ; electric keys.  It's a list that can
                                        ; hold one or several of the following
                                        ; symbols: erlang-electric-comma,
                                        ; erlang-electric-gt,
                                        ; erlang-electric-newline and
                                        ; erlang-electric-semicolon.

(defconst pel--electric-key-name
  (list
   (cons 'erlang-electric-comma      "comma")
   (cons 'erlang-electric-gt         "greater-than")
   (cons 'erlang-electric-newline    "newline")
   (cons 'erlang-electric-semicolon  "semicolon"))
  "Maps erlang.el symbol to name of key.")

(defun pel--erlang-toggle-electric-of (key globally)
  "Toggle Erlang electric behaviour of KEY."
  (unless (memq key '(erlang-electric-comma
                      erlang-electric-gt
                      erlang-electric-newline
                      erlang-electric-semicolon
                      pel-erlang-electric-period))
    (error "Erlang %S is not supported for electric behaviour" key))
  (unless globally
    (with-current-buffer (current-buffer)
      (unless (local-variable-p 'erlang-electric-commands)
        (make-local-variable 'erlang-electric-commands))))
  (if (memq key erlang-electric-commands)
      (setq erlang-electric-commands (delete key erlang-electric-commands))
    (setq erlang-electric-commands (cons key erlang-electric-commands)))
  (message "%s key electric behaviour is now %s for the current session %s"
           (cdr (assoc key pel--electric-key-name))
           (pel-on-off-string (memq key erlang-electric-commands))
           (if globally
               "in all Erlang buffers"
             "for the current buffer")))

;;-pel-autoload
(defun pel-erlang-comma (&optional globally)
  "Toggle Erlang electric behaviour of the comma key.
Affects current erlang buffer unless GLOBALLY is set."
  (interactive "*P")
  (pel--erlang-toggle-electric-of 'erlang-electric-comma globally))

;;-pel-autoload
(defun pel-erlang-gt (&optional globally)
  "Toggle Erlang electric behaviour of the gt key.
Affects current erlang buffer unless GLOBALLY is set."
  (interactive "*P")
  (pel--erlang-toggle-electric-of 'erlang-electric-gt globally))

;;-pel-autoload
(defun pel-erlang-newline (&optional globally)
  "Toggle Erlang electric behaviour of the newline key.
Affects current erlang buffer unless GLOBALLY is set."
  (interactive "*P")
  (pel--erlang-toggle-electric-of 'erlang-electric-newline globally))

;;-pel-autoload
(defun pel-erlang-semicolon (&optional globally)
  "Toggle Erlang electric behaviour of the semicolon key.
Affects current erlang buffer unless GLOBALLY is set."
  (interactive "*P")
  (pel--erlang-toggle-electric-of 'erlang-electric-semicolon globally))

;;-pel-autoload
(defun pel-erlang-period (&optional globally)
  "Toggle Erlang electric behaviour of the semicolon key.
Affects current erlang buffer unless GLOBALLY is set."
  (interactive "*P")
  (pel--erlang-toggle-electric-of 'pel-erlang-electric-period globally))

;; ------------------------------
;; Enhance Electric Key Behaviour
;; ------------------------------

(defvar pel--erlang-space-after-comma-in-blocks
  pel-erlang-space-after-comma-in-blocks
  "Activates automatic insertion of space after comma.
Can be toggled by the \\[pel-erlang-toggle-space-after-comma] command.")

;;-pel-autoload
(defun pel-erlang-toggle-space-after-comma (&optional globally)
  "Toggle automatic insertion of space after comma.
Affects current erlang buffer unless GLOBALLY is set."
  (interactive "*P")
  (pel-toggle-and-show-user-option 'pel--erlang-space-after-comma-in-blocks
                                   globally nil nil "Spaces after comma in blocks"))

(defun pel--enhanced-electric-comma (&rest _args)
  "Post handler for electric-comma: insert space inside parens blocks."
  (when (and (memq 'erlang-electric-comma erlang-electric-commands)
             pel--erlang-space-after-comma-in-blocks
             (not current-prefix-arg))
    (when (pel-inside-block-p)
      (pel-insert-space-in-enclosing-block))))

(defun pel-erlang-enhance-electric-keys ()
  "Enhance electric behaviour of Erlang electric keys as per PEL options.

The following options are observed:
- `pel-erlang-space-after-comma-in-blocks': when set the
  `erlang-electric-comma' inserts a space after the comma inside blocks."
  (when pel-erlang-space-after-comma-in-blocks
    (advice-add 'erlang-electric-comma
                :after (function pel--enhanced-electric-comma))))

;; -----------------------------------
;; Period Character Electric behaviour
;; -----------------------------------
;;
;; The erlang.el does not provide any electric behaviour for the period
;; character.  This code uses it to act as a > character when typed just after
;; a - but not after $- and not inside a comment or string.  Allowing to type
;; -> as -. which is easier to type on many keyboards and allows selecting the
;; non-electric behaviour of the > key indirectly but easily.

(defun pel--after-dash ()
  "Return the true if point is after a dash and not after $- characters."
  (and (>= (point) 3)
       (save-excursion
         ;; (backward-char 2)
         (and (eq (char-before (point)) ?-)
              (not (progn
                     (backward-char)
                     (eq (char-before (point)) ?$)))))))

(defun pel-erlang-electric-period (&optional arg)
  "Insert > after - but not after $- nor in string or comment or with ARG."
  (interactive "*p")
  (if (and (memq 'pel-erlang-electric-period erlang-electric-commands)
           (pel--after-dash)
           (not (or current-prefix-arg
                    (pel-inside-comment-p)
                    (pel-inside-string-p))))
      (insert ">")
    (self-insert-command arg)))

;; ---------------------------------------------------------------------------
;; Erlang Shell Control
;; --------------------

;;-pel-autoload
(defun pel-erlang-shell-mode-init ()
  "Initialize the Erlang shell mode."
  ;; Prevent Erlang shell mode echo
  (setq comint-process-echoes t))

;; ---------------------------------------------------------------------------
;; Navigation in Erlang source code
;; --------------------------------

;;-pel-autoload
(defun pel-end-of-previous-clause ()
  "Move point backward to the end of the previous clause.
Push current position on the mark ring."
  (interactive "^")
  (if (and (require 'erlang nil :noerror)
           (fboundp 'erlang-beginning-of-clause)
           (fboundp 'erlang-end-of-clause))
      (progn
        (push-mark)
        (erlang-beginning-of-clause 2)
        (erlang-end-of-clause 1))
    (user-error "Erlang support not loaded!")))

;;-pel-autoload
(defun pel-beginning-of-next-clause ()
  "Move point forward to the beginning of next clause.
Push current position on the mark ring."
  (interactive "^")
  (if (and (require 'erlang nil :noerror)
           (fboundp 'erlang-beginning-of-clause)
           (fboundp 'erlang-end-of-clause))
      (progn
        (push-mark)
        (erlang-end-of-clause 2)
        (erlang-beginning-of-clause 1))
    (user-error "Erlang support not loaded!")))

;; --

(defun pel--moveto-function (forward n)
  "Move to the beginning of function identified by FORWARD and N."
  (if (and (require 'erlang nil :noerror)
           (fboundp 'erlang-beginning-of-function)
           (fboundp 'erlang-get-function-name))
      (let ((direction (if forward -1 nil)))
        (push-mark)
        (while (> n 0)
          (setq n (1- n))
          (erlang-beginning-of-function direction)
          ;; erlang-beginning-of-function stops on directive but
          ;; erlang-get-function-name returns nil for them.
          (while (and (not (erlang-get-function-name))
                      (not (= (point)
                              (if forward (point-max) (point-min)))))
            (erlang-beginning-of-function direction))))
    (user-error "Erlang support not loaded!")))

;;-pel-autoload
(defun pel-previous-erl-function (&optional n)
  "Move point to beginning of previous Erlang function.  Repeat N times.
Push current position on the mark ring.
Skip over all compiler directives.
Stop at top of buffer."
  (interactive "^p")
  (pel--moveto-function nil (or n 1)))

;;-pel-autoload
(defun pel-next-erl-function (&optional n)
  "Move point to beginning of next Erlang function.  Repeat N times.
Push current position on the mark ring.
Skip over all compiler directives.
Stop at end of buffer."
  (interactive "^p")
  (pel--moveto-function t (or n 1)))


;; Erlang specialized forward-sexp/backward-sexp
;; ---------------------------------------------

(defun pel-erlang-before-binary (&optional pos)
  "Return non-nil if POS or point is just before \"<<\", nil otherwise."
  (or pos (setq pos (point)))
  (and (eq (char-after (point)) ?<)
       (save-excursion
         (forward-char)
         (eq (char-after (point)) ?<))))

(defun pel-erlang-after-binary (&optional pos)
  "Return non-nil if POS or point is just before \"<<\", nil otherwise."
  (or pos (setq pos (point)))
  (save-excursion
    (backward-char)
    (and (eq (char-after (point)) ?>)
         (progn
          (backward-char)
          (eq (char-after (point)) ?>)))))

(defun pel-erlang-forward-binary (&optional pos)
  "Move forward to match closing >> binary block."
  (or pos (setq pos (point)))
  (if (pel-erlang-before-binary pos)
      (let ((nesting-level 0)
            (found-pos nil)
            (syntax nil)
            (saved-match-data nil))
        (while
            (progn
              (setq found-pos
                    (re-search-forward "\\(<<\\)\\|\\(>>\\)" nil :noerror))
              (setq saved-match-data (match-data))
              ;; if found something in a string or comment, skip past it
              (when (and found-pos
                         (progn
                           (setq syntax (syntax-ppss found-pos))
                           (or (pel--inside-string-p syntax)
                               (pel--inside-comment-p syntax))))
                (pel-syntax-skip-string-and-comment-forward found-pos syntax)
                (setq found-pos
                      (re-search-forward "\\(<<\\)\\|\\(>>\\)" nil :noerror))
                (setq saved-match-data (match-data)))
              ;; now over comment or string. Search match data is in saved-match-data
              (when found-pos
                (cond
                 ((nth 3 saved-match-data) ; found <<
                  (setq nesting-level (1+ nesting-level)))
                 ((nth 5 saved-match-data) ; found >>
                  (setq nesting-level (1- nesting-level)))
                 (t
                  (error "logic error"))))
              (and found-pos
                   (/= nesting-level 0)))))
    (user-error "Point not located before <<")))

(defun pel-erlang-backward-binary (&optional pos)
  "Move to match backward to opening << binary block."
  (or pos (setq pos (point)))
  (if (pel-erlang-after-binary pos)
      (let ((nesting-level 0)
            (found-pos nil)
            (syntax nil)
            (saved-match-data nil))
        (while
            (progn
              (setq found-pos
                    (re-search-backward "\\(<<\\)\\|\\(>>\\)" nil :noerror))
              (setq saved-match-data (match-data))
              ;; if found something in a string or comment, skip past it
              (when (and found-pos
                         (progn
                           (setq syntax (syntax-ppss found-pos))
                           (or (pel--inside-string-p syntax)
                               (pel--inside-comment-p syntax))))
                (pel-syntax-skip-string-and-comment-backward found-pos syntax)
                (setq found-pos
                      (re-search-backward "\\(<<\\)\\|\\(>>\\)" nil :noerror))
                (setq saved-match-data (match-data)))
              ;; now over comment or string. Search match data is in saved-match-data
              (when found-pos
                (cond
                 ((nth 3 saved-match-data) ; found <<
                  (setq nesting-level (1- nesting-level)))
                 ((nth 5 saved-match-data) ; found >>
                  (setq nesting-level (1+ nesting-level)))
                 (t
                  (error "logic error"))))
              (and found-pos
                   (/= nesting-level 0)))))
    (user-error "Point not located after >>")))

;;-pel-autoload
(defun pel-erlang-forward-sexp (&optional arg)
  "Erlang compatible `forward-sexp' replacement.

Supports bit syntax expressions."
  (interactive "^p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (pel-erlang-backward-sexp (abs arg))
    (if (pel-erlang-before-binary)
        (dotimes (_i arg)
          (pel-erlang-forward-binary))
      (forward-sexp arg))))

;;-pel-autoload
(defun pel-erlang-backward-sexp (&optional arg)
  "Erlang compatible `backward-sexp' replacement.

Supports bit syntax expressions."
  (interactive "^p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (pel-erlang-forward-sexp (abs arg))
    (if (pel-erlang-after-binary)
        (dotimes (_i arg)
          (pel-erlang-backward-binary))
      (backward-sexp arg))))

;; -----------------------------------------------------------------------------
;; Detecting Erlang Versions and Controlling Erlang Man Pages to Use
;; -----------------------------------------------------------------

(defconst pel--erlang-md5-url "https://erlang.org/download/MD5"
  "URL of the official Erlang version MD5 file, using HTTPS.")

(defconst pel--hard-coded-erlang-man-versions
  '("R7B"
 "R16B03" "R16B03-1" "R16B02" "R16B01" "R16B"
 "R16A_RELEASE_CANDIDATE"
 "R15B03" "R15B03-1" "R15B02" "R15B01" "R15B"
 "R14B04" "R14B03" "R14B02" "R14B01" "R14B"
 "R14A"
 "R13B04" "R13B03" "R13B02" "R13B02-1" "R13B01"
 "R13B"
 "R13A"
 "R12B-5" "R12B-4" "R12B-3" "R12B-2" "R12B-1" "R12B-0"
 "R11B-5" "R11B-4" "R11B-3" "R11B-2" "R11B-1" "R11B-0"
 "R10B-9" "R10B-8" "R10B-7" "R10B-6" "R10B-5" "R10B-4"
 "R10B-3" "R10B-2" "R10B-1a" "R10B-10" "R10B-0"
 "24.0" "24.0-rc3" "24.0-rc2" "24.0-rc1"
 "23.3"
 "23.2"
 "23.1"
 "23.0" "23.0-rc3" "23.0-rc2"
 "23.0-rc1"
 "22.3"
 "22.2"
 "22.1"
 "22.0" "22.0-rc3" "22.0-rc2" "22.0-rc1"
 "21.3"
 "21.2"
 "21.1"
 "21.0"
 "20.3"
 "20.2"
 "20.1"
 "20.0"
 "19.3"
 "19.2"
 "19.1"
 "19.0"
 "18.3"
 "18.2" "18.2.1"
 "18.1"
 "18.0"
 "17.5"
 "17.4"
 "17.3"
 "17.1"
 "17.0" "17.0-rc2" "17.0-rc1")
  "List of available version of Erlang with specific man files.
As per https://erlang.org/download/MD5
List updated on Wednesday, June 02, 2021.
Used when access to `pel--erlang-md5-url' fails.")

(defvar pel--downloaded-erlang-man-versions nil
  "Cache of the Erlang Man versions.
Stored by `pel-read-available-erlang-man-versions'.")

(defun pel-read-available-erlang-man-versions (&optional force-download)
  "Return a list of strings: the official versions of Erlang Man pages.

Extract the list from the official Erlang.org web page listing
the MD5 of all official Erlang tarball files for download.  Store
the list in the variable `pel--downloaded-erlang-man-versions'
and return that in following calls unless the FORCE-DOWNLOAD
argument is set to non-nil.

If the function fails to read or extract the version numbers it
displays a warning and returns the list from the variable
`pel--hard-coded-erlang-man-versions'."
  (if (or force-download
          (null pel--downloaded-erlang-man-versions))
      (let ((temp-buffer (url-retrieve-synchronously pel--erlang-md5-url))
            (versions '()))
        (unwind-protect
            (with-current-buffer temp-buffer
              (progn
                (goto-char (point-min))
                (while (re-search-forward "otp_doc_man_\\(.+\\).tar.gz" nil t)
                  (setq versions (cons (match-string 1) versions)))))
          (and (buffer-name temp-buffer)
               (kill-buffer temp-buffer)))
        (unless versions
          (display-warning 'pel-read-availableerlang-man-versions
                           "Failed reading Erlang man versions from OTP site!
Returning the values stored locally which may be out-of date!"
                           :warning)
          (setq versions pel--hard-coded-erlang-man-versions))
        versions)
    pel--downloaded-erlang-man-versions))


;; ---------------------------------------------------------------------------
;; Extract value of pel-erlang-man-parent-rootdir
;; ----------------------------------------------

(defun pel--erlang-dirpath (dirpath-user-option)
  "Extract and return value of DIRPATH-USER-OPTION.
Return a (value . error) cons cell where value is a string and error is
nil when all is OK or a message describing the detected error if any.
Return nil if it is not defined."
  (when dirpath-user-option
    (let (dirpath source envvar)
      (cond
       ((stringp dirpath-user-option)
        (setq dirpath pel-erlang-man-parent-rootdir)
        (setq source 'user-option))
       ;;
       ((consp dirpath-user-option)
        (setq envvar (cdr pel-erlang-man-parent-rootdir))
        (setq dirpath (getenv envvar))
        (setq source 'envvar)))
      ;; return result
      (cond
       ((null dirpath)
        (cons nil (if (eq source 'user-option)
                      "Nil user option"
                    (format "Defined by absent environment variable %s"
                            envvar))))
       ;; all is OK: return a cell with only the dirpath
       ((file-exists-p dirpath)
        (list dirpath))
       ;; directory does not exists
       (t (cons dirpath
                (format "Directory %s, specified by %s, does not exist"
                        dirpath
                        (if (eq source 'user-option)
                            "user-option"
                          (format "environment variable %s"
                                  envvar)))))))))

;;-pel-autoload
(defun pel-erlang-man-parent-rootdir ()
  "Extract and return value of `pel-erlang-man-parent-rootdir' user-option.
Return a (value . error) cons cell where value is a string and error is
nil when all is OK or a message describing the detected error if any.
Return nil if it is not defined."
  (pel--erlang-dirpath pel-erlang-man-parent-rootdir))

;;-pel-autoload
(defun pel-erlang-exec-path ()
  "Extract and return value of `pel-erlang-exec-path' user-option.
Return a (value . error) cons cell where value is a string and error is
nil when all is OK or a message describing the detected error if any.
Return nil if it is not defined."
  (pel--erlang-dirpath pel-erlang-exec-path))

;;-pel-autoload
(defun pel-erlang-set-dirpath (user-option action)
  "Extract the USER-OPTION value and perform specified ACTION when non-nil.
The USER-OPTION must be a function that has the same name as the user-option
it extracts.
The ACTION is a function that takes one argument: a dirpath extracted from the
user option."
  (let* ((erl-dirpath.error-msg (funcall user-option))
         (erl-dirpath (car erl-dirpath.error-msg))
         (error-msg (cdr erl-dirpath.error-msg)))
    (when erl-dirpath
      (if error-msg
          (display-warning
           'pel-erlang-set
           (format "Invalid %s: %s" (symbol-name user-option) error-msg)
           :error)
        (funcall action erl-dirpath)))))


;; ---------------------------------------------------------------------------
;; Replacement of erlang-man-dir that uses pel-erlang-man-parent-rootdir for
;; erlang-root-dir

(defvar pel---detected-erlang-root-dir nil
  "Copied value for debugging purpose.")

(defun pel--erlang-man-dir (erlang-man-dir subdir &optional no-download)
  "Replace erlang-man-dir with Erlang root directory specified by `pel-erlang-man-parent-rootdir'."
  ;; erlang-man-dir arguments: (subdir &optional no-download)
  (let ((erlang-root-dir (pel-erlang-root-path)))
    (setq pel---detected-erlang-root-dir erlang-root-dir)
    (funcall erlang-man-dir subdir no-download)))

(defvar pel--erlang-man-dir--setup nil
  "Set t if pel--erlang-man-dir is setup.")

;;-pel-autoload
(defun pel-erlang-setup-erlang-man-dir-root ()
  "Setup a replacement for erlang-man-dir."
  (unless pel--erlang-man-dir--setup
    (advice-add 'erlang-man-dir
                :around (function pel--erlang-man-dir))
    (setq pel--erlang-man-dir--setup t)))

;; ---------------------------------------------------------------------------
;; Detect Erlang Root Directory
;; ----------------------------

(defvar pel---extracted-erlang-root-dir nil
  "Cached value of the Erlang root dir  extracted by pel/bin/erlang-root-dir.")

(defvar pel--read-erlang-root-dir-reported-error nil
  "Remember error detected by pel--read-erlang-root-dir.")

(defun pel--read-erlang-root-dir ()
  "Return Erlang root directory extracted from pel/bin/erlang-root-dir."
  (or pel---extracted-erlang-root-dir
      (let* ((exit-code.version (pel-exec-pel-bin "erlang-root-dir"))
             (exit-code (car exit-code.version)))
        (if (eq exit-code 0)
            (setq pel---extracted-erlang-root-dir (cdr exit-code.version))
          (progn
            (unless pel--read-erlang-root-dir-reported-error
              (display-warning
               'pel-erlang-root-path
               (format "pel/bin/erlang-root-dir failed with exit code: %S.
Cannot detect Erlang root path!" (car exit-code.version))
               :error)
              (setq pel--read-erlang-root-dir-reported-error t))
            nil)))))


;;-pel-autoload
(defun pel-erlang-root-path ()
  "Return the Erlang root path to use, according to PEL's customization.

The method used by PEL to detect Erlang root path is the
`pel-erlang-path-detection-method' user-option.

Return the path string on success.
On error issue a warning describing the error and return nil."
  (cond
   ((stringp pel-erlang-path-detection-method)
    pel-erlang-path-detection-method)
   ;;
   ((eq pel-erlang-path-detection-method 'auto-detect)
    (pel--read-erlang-root-dir))
   ;;
   ((and (consp pel-erlang-path-detection-method)
         (eq (car pel-erlang-path-detection-method) 'by-envvar))
    (let* ((envvar-name (cadr pel-erlang-path-detection-method))
           (version-string (getenv envvar-name)))
      (unless version-string
        (display-warning
         'pel-erlang-root-path
         (format "Can't extract Erlang root path from envvar '%S'"
                 envvar-name)
         :error))
      version-string))
   (t (display-warning
       'pel-erlang-version
       (format "Non-compliant data type: %S
Can't detect Erlang root path." pel-erlang-path-detection-method)
       :error)
      nil)))

;; ---------------------------------------------------------------------------
;; Read/Show Erlang Version
;; ------------------------

;;-pel-autoload
(defun pel-erlang-version ()
  "Return the version of Erlang to use, according to PEL's customization.
Return the version string on success.
On error issue a warning describing the error and return nil."
  (cond
   ((stringp pel-erlang-version-detection-method)
    pel-erlang-version-detection-method)
   ;;
   ((eq pel-erlang-version-detection-method 'auto-detect)
    (let* ((exit-code.version (pel-exec-pel-bin "version-erl"))
           (exit-code (car exit-code.version)))
      (if (eq exit-code 0)
          (cdr exit-code.version)
        (display-warning
         'pel-erlang-version
         (format "pel/bin/version-erl failed with exit code: %S.
Cannot detect Erlang version!" (car exit-code.version))
         :error))))
   ;;
   ((and (consp pel-erlang-version-detection-method)
         (eq (car pel-erlang-version-detection-method) 'by-envvar))
    (let* ((envvar-name (cadr pel-erlang-version-detection-method))
           (version-string (getenv envvar-name)))
      (unless version-string
        (display-warning
         'pel-erlang-version
         (format "Can't extract Erlang version from envvar '%S'"
                 envvar-name)
         :error))
      version-string))
   (t (display-warning
       'pel-erlang-version
       (format "Non-compliant data type: %S
Can't detect Erlang version." pel-erlang-version-detection-method)
       :error)
      nil)))

(defun pel-erlang-ls-version ()
  "Return a string describing erlang_ls version or nil if not available."
  (let ((exit-code.stdout (pel-exec-cmd "erlang_ls" "--version")))
    (when (car exit-code.stdout)
      (cadr (split-string (cdr exit-code.stdout))))))

;;-pel-autoload
(defun pel-show-erlang-version ()
  "Display version of Erlang, erlang.el and erlang_ls if available.
Also displays `erlang-root-dir' and `pel-erlang-man-parent-rootdir'"
  (interactive)
  (require 'erlang nil :noerror)
  (let* ((erlang-ls-version (pel-erlang-ls-version))
         (detected-erlang-root-dir (pel-erlang-detected-root-dir))
         (path-mismatch (not (string=  detected-erlang-root-dir
                                       (bound-and-true-p erlang-root-dir)))))
    (message "Erlang version: %s, erlang.el version: %s%s
pel-erlang-version-detection-method : %s
pel-erlang-path-detection-method    : %s
Detected erlang-root-dir            : %s
pel-erlang-man-parent-rootdir       : %s%s%s%s%s"
             (pel-erlang-version)
             (if (fboundp 'erlang-version)
                 (erlang-version)
               "Unknown - not loaded!")
             (if erlang-ls-version
                 (format ", erlang_ls: %s" erlang-ls-version)
               "")
             pel-erlang-version-detection-method
             pel-erlang-path-detection-method
             (if path-mismatch
                 (format "[%s] ⚠️
         erlang-root-dir user-option: [%s] ⚠️ "
                         detected-erlang-root-dir
                         (bound-and-true-p erlang-root-dir))
               detected-erlang-root-dir)
             (let* ((value.error (pel-erlang-man-parent-rootdir))
                    (man-rootdir (or (cdr value.error)
                                     (car value.error))))
               (if man-rootdir
                   man-rootdir
                 (format "nil, but Man operations use %s"
                         (bound-and-true-p erlang-root-dir))))
             (pel-string-when path-mismatch "
⚠️  Automatic detection of Erlang Root Directory done by the script
   pel/bin/erlang-root-dir returns a path that differs from what is stored
   in the erlang-root-dir user-option. Some Erlang features may fail.
   You may want to set pel-erlang-path-detection-method user-option to
auto-detect.")
             (if (and pel-use-ivy-erlang-complete
                      (boundp 'ivy-erlang-complete-erlang-root))
                 (format "
ivy-erlang-complete-erlang-root     : %s" ivy-erlang-complete-erlang-root)
               "")
             (if (boundp 'ivy-erlang-complete-project-root)
                 (format "
ivy-erlang-complete-project-root    : %s" ivy-erlang-complete-project-root)
               "")
             (if (and pel-use-erlang-ls
                      (boundp 'lsp-keymap-prefix))
                 (format "
lsp-keymap-prefix                   : %s" lsp-keymap-prefix)
               ""))))

;; ---------------------------------------------------------------------------

;; (defun pel-erlang-organize-fs ()
;;   "Organize the file system for Erlang.
;; Set it up to support both erlang.el and EDTS.
;; Ensure they both get the man pages for the Erlang version accessible
;; by default.  EDTS configuration can override this, but not erlang.el."
;;   ;;
;;   ;; Find what erlang version is available from Emacs parent shell
;;   (let ((erlang-version (pel-erlang-version)))
;;     ;;
;;     (setq edts-man-download "https://erlang.org/download"))) ; use secure HTTP

;; ---------------------------------------------------------------------------
;; Erlang Syntax Checking Control
;; ------------------------------

;;-pel-autoload
(defun pel-erlang-toggle-syntax-checker ()
  "Toggle the syntax checker mode on/off.
The syntax checker activated or deactivated is either flycheck
or flymake, as selected by the user-option variable
`pel-use-erlang-syntax-check'."
  (interactive)
  (pel-toggle-syntax-check-mode 'pel-use-erlang-syntax-check))

;; ---------------------------------------------------------------------------
;; Insertion of Erlang Comments
;; ----------------------------

(defun pel--erlang-line-3%-comment-p ()
  "Return t if the %%% style comment should be used at point, nil otherwise."
  (save-excursion
    (when (region-active-p)
      (goto-char (region-beginning)))
    (if (bobp)
        t
      (when (eq (current-column) 0)
        (ignore-errors
          (forward-line -1)
          (string= "%%%"
                   (buffer-substring-no-properties (point)
                                                   (+ 3 (point)))))))))

;;-pel-autoload
(defun pel-erlang-comment-dwim (&optional arg)
  "Insert comment like `comment-dwim' with ability to extend \"%%%\" comments.

The \"%%%\" comment style is only placed at the beginning of a
line, when the line is the first line of a buffer or a line that
follows a line that starts with a \"%%%\" style comment.  When
commenting a region, if the region starts just below a line with
\"%%%\" comment the new comment uses \"%%%\" comment as well.

In all other cases the %% style comment is used at the beginning
of a line and a single % is used after the beginning of a line."
  ;; Erlang comments at beginning of line might use 2 or 3 percent characters.
  ;; Check the style used on the preceding line and if it is using 3 percent
  ;; characters, force the comment-add to 2 to ensure that we use "%%% " for
  ;; comment.
  (interactive "*P")
  (if (pel--erlang-line-3%-comment-p)
      (let ((comment-add 2))
        (comment-dwim arg))
    (comment-dwim arg)))

;; ---------------------------------------------------------------------------
;; Modify erlang.el electric key behaviour and key bindings
;; --------------------------------------------------------

;; The following variables are defined inside erlang.el.
;; They are re-declared here to prevent byte-compiler warnings.
(defvar erlang-mode-map)
(defvar erlang-mode-syntax-table)
(defvar erlang-electric-arrow-criteria)

(defun pel-erlang-stop-when-arg-used-p ()
  "Return `stop' when invoking command invoked with arguments, nil otherwise."
  (if current-prefix-arg
      'stop
    t))

;;-pel-autoload
(defun pel-erlang-setup-electric-key-behaviour ()
  "Setup Erlang behaviour of electric keys."

  ;; Activate Electric key behaviour selected by PEL user-option
  (setq erlang-electric-commands pel-erlang-electric-keys)
  (pel-erlang-enhance-electric-keys)

  ;; Change behaviour of the > electric behaviour:
  ;; The erlang.el sets erlang-electric-arrow-criteria to:
  ;;    '(erlang-stop-when-in-type-spec
  ;;      erlang-next-lines-empty-p
  ;;      erlang-at-end-of-function-p)
  ;;
  ;; But this means an empty line activates the  electric behaviour.  This
  ;; prevents the electric behaviour when the next line has code.
  ;; It would be better to be able to allow electric behaviour with and
  ;; without empty following lines while allowing the user to prevent electric
  ;; behaviour when typing the character.  This is now done setting the
  ;; erlang-electric-arrow-criteria to this instead:
  ;;
  (setq erlang-electric-arrow-criteria '(erlang-stop-when-in-type-spec
                                         pel-erlang-stop-when-arg-used-p))

  ;; This way just type ``M-1 >`` to insert just one > and prevent electric
  ;; behaviour.  Using a larger number to insert several > one after another,
  ;; with another prefix also disable electric behaviour as should be
  ;; expected.
  ;;
  ;; Note that it is also possible to disable electric behaviour of the > key
  ;; for a longer stretch of time by using ``<f12> M-` >``.

  ;; To ease the typing of '->' with no electric behaviour, give the '.' key
  ;; electric behaviour to transform '.-' into '->' when appropriate, ie: as
  ;; long as it's not in string or comment or not following a '$-'.
  (define-key erlang-mode-map "." 'pel-erlang-electric-period)

  ;; TODO: fix erlang.el code that prevents the following to work.
  ;;       I have tried to get the syntax-propertize-function to help
  ;;       but I can't get it to work.  I don't understand this mechanism yet.

  ;;
  ;; Add << >> pairing navigation and marking, without pairing < > because
  ;; that would cause problems in comparison operators < and > and with the ->
  ;; the <- the => and the <= operators.
  ;;
  ;; So instead of using the following:
  ;;     (modify-syntax-entry ?< "(>" erlang-mode-syntax-table)
  ;;     (modify-syntax-entry ?> ")<" erlang-mode-syntax-table)
  ;;
  ;; The code could perhaps use the syntax-propertize-function to activate the
  ;; pairing:

  ;; (defconst erlang-mode-syntax-propertize-function
  ;;   (syntax-propertize-rules
  ;;    ("\\(<\\)<" (1 "(>"))
  ;;    (">\\(>\\)" (1 ")<")))
  ;;   "Syntax properties to activate << >> pairing.")
  ;; (setq-local parse-sexp-lookup-properties t)
  ;; (setq-local syntax-propertize-function
  ;;             erlang-mode-syntax-propertize-function)

  ;; Until I'm able to fix the behaviour of forward-sexp and backward-sexp  to
  ;; support Erlang bit syntax I'll remap the keys used for those to the
  ;; specialized command I wrote that support Erlang bit syntax.
  (define-key erlang-mode-map [remap forward-sexp]  'pel-erlang-forward-sexp)
  (define-key erlang-mode-map [remap backward-sexp] 'pel-erlang-backward-sexp))


;; ---------------------------------------------------------------------------
;; Erlang Cross Reference Command Control
;; --------------------------------------


(defvar pel--erlang-xref-engine pel-erlang-xref-engine
  "Currently active cross reference engine for Erlang code.

The `pel-erlang-xref-engine' user-option identifies the persistent selection.
During an editing session use the `pel-erlang-select-xref' command to
dynamically select another engine for the current editing session.")

;;-pel-autoload
(defun pel-erlang-show-xref ()
  "Show Erlang cross reference engine selection."
  (interactive)
  (message "Erlang XRef Engine:
- Selected by pel-erlang-xref-engine user-option: %s
- Currently active                              : %s"
           pel-erlang-xref-engine
           pel--erlang-xref-engine))


(defun pel--erlang-xref-choices ()
  "Return available Erlang xref engine choice list."
  (let ((choices (list (list ?T "etag" 'etag))))
    (when pel-use-ivy-erlang-complete
      (push (list ?i "ivy-erlang-complete" 'ivy-erlang-complete) choices))
    (when pel-use-dumb-jump
      (push (list ?D "dumb-jump" 'dumb-jump) choices))
    (when pel-use-ggtags
      (push (list ?G "ggtags" 'ggtags) choices))
    (when pel-use-edts
      (push (list ?e "edts" 'edts) choices))
    (when pel-use-erlang-ls
      (push (list ?l "erlang-ls" 'erlang-ls) choices))
    (reverse choices)))

;;-pel-autoload
(defun pel-erlang-select-xref ()
  "Select another Erlang cross reference engine.

The selection remains active for the current editing session.
The `pel-erlang-xref-engine' user-option identifies the persistent selection."
  (interactive)
  (setq pel--erlang-xref-engine
        (pel-select-from "Erlang XRef Engine"
                         (pel--erlang-xref-choices)
                         (or pel--erlang-xref-engine pel-erlang-xref-engine)))
  (pel-erlang-show-xref))

(defun pel--erlang-disable-xref-engines ()
  "Disable all Xref Engines."
  (when xref-etags-mode
    (xref-etags-mode -1))
  (when (pel-xref-dumb-jump-active-p)
    (pel-xref-dumb-jump-deactivate nil))
  (when (pel-xref-gxref-active-p)
    (pel-xref-toggle-gxref :quiet))
  (when (and (bound-and-true-p ggtags-mode)
             (fboundp 'ggtags-mode))
    (ggtags-mode -1)))


;;-pel-autoload
(defun pel-erlang-find-definitions ()
  "Find definition of identifier at point using the active back-end."
  (interactive)
  (cl-case pel--erlang-xref-engine
    ((nil)
     (pel-erlang-select-xref))
    ;;
    (etag
     (progn
       (when (pel-xref-dumb-jump-active-p)
         (pel-xref-dumb-jump-deactivate nil))
       (when (pel-xref-gxref-active-p)
         (pel-xref-toggle-gxref :quiet))
       (when (and (bound-and-true-p ggtags-mode)
                  (fboundp 'ggtags-mode))
         (ggtags-mode -1))
       (unless xref-etags-mode
         (xref-etags-mode 11))
       (pel-xref-find-definitions)))
    ;;
    (ivy-erlang-complete
     (progn
       ;; (pel--erlang-disable-xref-engines)
       (call-interactively 'ivy-erlang-complete-find-definition)))
    ;;
    (dumb-jump
     (progn
       (when xref-etags-mode
         (xref-etags-mode -1))
       (when (pel-xref-gxref-active-p)
         (pel-xref-toggle-gxref :quiet))
       (when (and (bound-and-true-p ggtags-mode)
                  (fboundp 'ggtags-mode))
         (ggtags-mode -1))
       (unless (pel-xref-dumb-jump-active-p)
         (pel-xref-dumb-jump-activate nil))
       (pel-xref-find-definitions)))
    ;;
    (ggtags
     (progn
       (when xref-etags-mode
         (xref-etags-mode -1))
       (when (pel-xref-dumb-jump-active-p)
         (pel-xref-dumb-jump-deactivate nil))
       (unless (pel-xref-gxref-active-p)
         (pel-xref-gxref-activate))
       (unless (bound-and-true-p ggtags-mode)
         (if (and (require 'ggtags nil :noerror)
                  (fboundp 'ggtags-mode))
             (ggtags-mode 1)
           (user-error "ggtags is not available!")))
       ;;
       (pel-xref-find-definitions)))
    ;;
    (edts
     (progn
       (unless (bound-and-true-p edts-mode)
         (if (and (require 'edts-start nil :no-error)
                  (fboundp 'edts-mode))
             (edts-mode 1)))
       (call-interactively 'edts-find-source-under-point)))
    ;;
    (erlang-ls
     (call-interactively 'lsp-ui-peek-find-implementation))))


;;-pel-autoload
(defun pel-erlang-unwind ()
  "Unwind back to point position."
  (interactive)
  (cond
   ((memq pel--erlang-xref-engine '(etag
                                    ivy-erlang-complete
                                    dumb-jump
                                    ggtags
                                    erlang-ls))
    (if (fboundp 'xref-pop-marker-stack)
        (xref-pop-marker-stack)
      (user-error "xref-pop-marker-stack is void.")))
   ;;
   ((eq pel--erlang-xref-engine 'edts)
    (if (fboundp 'edts-find-source-unwind)
        (edts-find-source-unwind)
      (user-error "edts-find-source-unwind is void.")))))

;; ---------------------------------------------------------------------------

(defvar pel-erlang-extra-directories nil
  "List of directories where to search Erlang files.
This is used by `pel-erlang-source-directories'.")

(defun pel-erlang-source-directories (&optional directories)
  "Return a list of directories that contain Erlang source code files.

The list includes the Erlang root directory and the current project.
The project directory tree is the root of the directory that contains
the current directory and holds one of the files identified by the
`pel-erlang-project-root-identifiers' user-option.

If DIRECTORIES is specified , then the list also includes these directories.
You can also specify extra directories in the `pel-erlang-extra-directories'
variable by dynamically binding it in the caller of the function.

The returned list contains a list of expanded directory names that do not end
with a slash.  There are no duplicates and the list is sorted."
  (let ((dir-list (list (directory-file-name pel---extracted-erlang-root-dir)))
        (directory nil))
    ;; Add project directory looking for each possible project root identifier
    (setq directory
          (pel-ffind-project-directory pel-erlang-project-root-identifiers))
    (when (and directory
               (not (member directory  dir-list)))
      (push directory dir-list))
    ;; Add extra directories potentially identified by argument and let
    ;; binding
    (dolist (extra-dirlist (list directories pel-erlang-extra-directories))
      (when extra-dirlist
        (dolist (directory extra-dirlist)
          (when (and directory
                     (setq directory (expand-file-name
                                      (directory-file-name directory)))
                     (not (member directory dir-list)))
            (push (directory-file-name directory ) dir-list)))))
    ;; Return the list of directories sorted.
    (sort dir-list 'string<)))


(defun pel-erlang-find-file (filename &optional directories)
  "Find a file FILENAME from and Erlang library.

Search in the Erlang root and the project directories by default.
If DIRECTORIES is specified also search in these extra directories."
  (pel-ffind filename (pel-erlang-source-directories directories)))

;;; --------------------------------------------------------------------------
(provide 'pel-erlang)

;;; pel-erlang.el ends here
