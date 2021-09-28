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
;;  The content of this file provides code that help using and programming with
;;  Erlang.

;;; --------------------------------------------------------------------------
;;; Dependencies:
(require 'comint)
(require 'pel--base)            ; use: pel-toggle-syntax-check-mode
(require 'pel--options)         ; use: pel-erlang-version-detection-method
(require 'pel-fs)               ; use: pel-exec-pel-bin

;; newcomment is always available
(require 'newcomment)           ; use: comment-dwim


;;; --------------------------------------------------------------------------
;;; Code:

;; Toggle electric behaviour of keys

(defvar erlang-electric-commands)

(defconst pel--electric-key-name
  (list
   (cons 'erlang-electric-comma      "comma")
   (cons 'erlang-electric-gt         "greater-than")
   (cons 'erlang-electric-newline    "newline")
   (cons 'erlang-electric-semicolon  "semicolon"))
  "Maps erlang.el symbol to name of key.")

(defun pel--erlang-toggle-electric-of (key)
  "Toggle Erlang electric behaviour of KEY."
  (unless (memq key '(erlang-electric-comma
                      erlang-electric-gt
                      erlang-electric-newline
                      erlang-electric-semicolon))
    (error "Erlang %S is not supported for electric behaviour" key))
  (if (memq key erlang-electric-commands)
      (setq erlang-electric-commands (delete key erlang-electric-commands))
    (setq erlang-electric-commands (cons key erlang-electric-commands)))
  (message "%s key electric behaviour is now %s"
           (cdr (assoc key pel--electric-key-name))
           (pel-on-off-string (memq key erlang-electric-commands))))

;;-pel-autoload
(defun pel-erlang-comma ()
  "Toggle Erlang electric behaviour of the comma key."
  (interactive)
  (pel--erlang-toggle-electric-of 'erlang-electric-comma))

;;-pel-autoload
(defun pel-erlang-gt ()
  "Toggle Erlang electric behaviour of the gt key."
  (interactive)
  (pel--erlang-toggle-electric-of 'erlang-electric-gt))

;;-pel-autoload
(defun pel-erlang-newline ()
  "Toggle Erlang electric behaviour of the newline key."
  (interactive)
  (pel--erlang-toggle-electric-of 'erlang-electric-newline))

;;-pel-autoload
(defun pel-erlang-semicolon ()
  "Toggle Erlang electric behaviour of the semicolon key."
  (interactive)
  (pel--erlang-toggle-electric-of 'erlang-electric-semicolon))


;; ---------------------------------------------------------------------------
;; Erlang Shell Control

;;-pel-autoload
(defun pel-erlang-shell-mode-init ()
  "Initialize the Erlang shell mode."
  ;; Prevent Erlang shell mode echo
  (setq comint-process-echoes t))

;; ---------------------------------------------------------------------------
;; Navigation in Erlang source code

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
;; Read Erlang Version
;; -------------------

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
         (format "version-erl failed with exit code: %S.
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
  (let ((erlang-ls-version (pel-erlang-ls-version)))
    (message "Erlang version: %s, erlang.el version: %s%s
erlang-root-dir              : %s
pel-erlang-man-parent-rootdir: %s"
             (pel-erlang-version)
             (if (and (require 'erlang nil :noerror)
                      (fboundp 'erlang-version))
                 (erlang-version)
               "Unknown - not loaded!")
             (if erlang-ls-version
                 (format ", erlang_ls: %s" erlang-ls-version)
               "")
             (bound-and-true-p erlang-root-dir)
             (let ((value.error (pel-erlang-man-parent-rootdir)))
               (or (cdr value.error)
                   (car value.error))))))

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

;;-pel-autoload
(defun pel-erlang-toggle-syntax-checker ()
  "Toggle the syntax checker mode on/off.
The syntax checker activated or deactivated is either flycheck
or flymake, as selected by the user-option variable
`pel-use-erlang-syntax-check'."
  (interactive)
  (pel-toggle-syntax-check-mode 'pel-use-erlang-syntax-check))


;; ---------------------------------------------------------------------------
;; Erlang Comments
;; ---------------

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
The \"%%%\" comment style is only placed at the beginning of a line,
when the line is the first line of a buffer or a line that follows a line that
starts with a \"%%%\" style comment.
When commenting a region, if the region starts just below a line with \"%%%\"
comment the new comment uses \"%%%\" comment as well.

In all other cases the %% style comment is used at the beginning of a line
and a single % is used after the beginning of a line."
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
;; Erlang-compatible forward-sexp-function
;; ---------------------------------------
;;
;; Non-complete code.  Checked in to remember it.  It will be removed
;; because I was able to fix the problem with a simplemmodification of the
;; syntax table.

(defconst pel-erlang-forward-matching-chars '( ?\(
                                               ?\[
                                               ?\{
                                               ?\"
                                               ?')
  "List of characters that begin a matching pair in erlang-mode.")

(defconst pel-erlang-backward-matching-chars '( ?\)
                                                ?\]
                                                ?\}
                                                ?\"
                                                ?')
  "List of characters that terminate a matching pair in erlang-mode.")


(defun pel--previous-char ()
  ""
  (char-after (- (point) 1)))


(defun pel--erlang-forward-sexp (&optional arg)
  ""
  (let ((nesting 1)
        (previous-char nil)
        (found-pos nil)
        (continue t))
    (save-excursion
      (right-char 1)
      (while continue
        (if (re-search-forward "[<>]" nil :noerror)
            (progn
              (setq previous-char (char-after (- (point) 1)))
              (cond
               ((eq previous-char ?<)
                (setq nesting (1+ nesting)))
               ((eq previous-char ?>)
                (setq nesting (1- nesting))
                (when (eq nesting 0)
                  (setq found-pos (point))
                  (setq continue nil))))))))
    (when found-pos
      (goto-char found-pos))))

(defun pel--erlang-backward-sexp (&optional arg)
  ""
  (let ((nesting 1)
        (current-char nil)
        (found-pos nil)
        (continue t))
    (save-excursion
      (left-char 1)
      (while continue
        (if (re-search-backward "[<>]" nil :noerror)
            (progn
              (setq current-char (char-after (point)))
              (cond
               ((eq current-char ?>)
                (setq nesting (1+ nesting)))
               ((eq current-char ?<)
                (setq nesting (1- nesting))
                (when (eq nesting 0)
                  (setq found-pos (point))
                  (setq continue nil))))))))
    (when found-pos
      (goto-char found-pos))))

(defun pel--erlang-sexp (&optional arg)
  ""
  (if (eq (char-after (point)) ?<)
      (if (>= arg 0)
          (pel--erlang-forward-sexp arg)
        (pel--erlang-backward-sexp (- arg)))
    (if (>= arg 0)
        (pel--erlang-backward-sexp arg)
      (pel--erlang-forward-sexp (- arg)))))

;;-pel-autoload
(defun pel-erlang-forward-sexp (&optional arg)
  ""
  (interactive "^p")
  (or arg (setq arg 1))
  (if (and (eq major-mode 'erlang-mode)
           (memq (char-after (point)) '(?< ?>)))
      (pel--erlang-sexp arg)
    ;; normally use default code
    (goto-char (or (scan-sexps (point) arg) (buffer-end arg)))
    (if (< arg 0) (backward-prefix-chars))))

;;; --------------------------------------------------------------------------
(provide 'pel-erlang)

;;; pel-erlang.el ends here
