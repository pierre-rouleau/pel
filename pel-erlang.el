;;; pel-erlang.el --- Erlang programming Language support  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Pierre Rouleau

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


;;; Code:
(require 'comint)
(require 'pel--options)         ; use: pel-erlang-version-detection-method
(require 'pel-fs)               ; use: pel-exec-pel-bin
(require 'pel-list)             ; use: pel-insert-list-in-list
(require 'pel-tempo)            ; use: pel-tempo-mode

;;-pel-autoload
(defun pel-erlang-shell-mode-init ()
  "Initialize the Erlang shell mode."
  ;; Prevent Erlang shell mode echo
  (setq comint-process-echoes t))


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
          (while (not (erlang-get-function-name))
            (erlang-beginning-of-function direction))))
    (user-error "Erlang support not loaded!")))

;;-pel-autoload
(defun pel-previous-erl-function (&optional n)
  "Move point to beginning of previous Erlang function.  Repeat N times.
Push current position on the mark ring.
Skip over all compiler directives."
  (interactive "^p")
  (pel--moveto-function nil (or n 1)))

;;-pel-autoload
(defun pel-next-erl-function (&optional n)
  "Move point to beginning of next Erlang function.  Repeat N times.
Push current position on the mark ring.
Skip over all compiler directives."
  (interactive "^p")
  (pel--moveto-function t (or n 1)))

;; -----------------------------------------------------------------------------
;; Install Erlang Skeletons as key-bound commands
;; ----------------------------------------------


(defvar pel--erl-skel-key '(("if"                      . "i")
                            ("case"                    . "c")
                            ("export"                  . "x")
                            ("import"                  . "I")
                            ("try"                     . "t")
                            ("try-of"                  . "T")
                            ("receive"                 . "r")
                            ("after"                   . "a")
                            ("loop"                    . "l")
                            ("module"                  . "m")
                            ("function"                . "f")
                            ("author"                  . "`")
                            ("spec"                    . "s")
                            ("small-header"            . "M-h")
                            ("normal-header"           . "M-H")
                            ("large-header"            . "C-h")
                            ("small-server"            . "M-s")
                            ("application"             . "M-a")
                            ("supervisor"              . "M-u")
                            ("supervisor-bridge"       . "M-b")
                            ("generic-server"          . "M-g")
                            ("gen-event"               . "M-e")
                            ("gen-fsm"                 . "M-f")
                            ("gen-statem-StateName"    . "M-S")
                            ("gen-statem-handle-event" . "M-E")
                            ("wx-object"               . "M-w")
                            ("gen-lib"                 . "M-l")
                            ("gen-corba-cb"            . "M-c")
                            ("ct-test-suite-s"         . "M-1")
                            ("ct-test-suite-l"         . "M-2")
                            ("ts-test-suite"           . "M-3"))
  "Key mapping for skeletons defined in erlang-skel.el")

;; The standard Erlang mode support does not define skeleton for all statements.
;; Add more skeletons using the tempo package here to complement the official ones.

;; Functions used inside the skeleton descriptions below.
(defun pel--erlang-skel-skip-blank ()
  (skip-chars-backward " \t")
  nil)

(defvar pel-erlang-skel-export
  '((pel--erlang-skel-skip-blank) o >
    "-export([" n>
    p n>
    "])." p )
  "*The skeleton of a `export' declaration.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-import
  '((pel--erlang-skel-skip-blank) o >
    "-import(" p ", [" n>
    p n>
    "])." p )
  "*The skeleton of a `import' declaration.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-try
  '((pel--erlang-skel-skip-blank) o >
    "try "  n>
    p  n>
    p "catch" n>
    p "oops         -> got_throw_oops;"      n>
    p "throw:Other  -> {got_throw, Other};"  n>
    p "exit:Reason  -> {got_exit, Reason};"  n>
    p "error:Reason -> {got_error, Reason}"  n>
    "end." > p)
  "*The skeleton of a `try' expression.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-try-of
  '((pel--erlang-skel-skip-blank) o >
    "try " p " of" n>
    " when " p " ->"  n>
    p " ;" n>
    " when " p " ->"  n>
    p  n>
    "catch" n>
    p "oops         when " p "  -> got_throw_oops;"      n>
    p "throw:Other  when " p "  -> {got_throw, Other};"  n>
    p "exit:Reason  when " p "  -> {got_exit, Reason};"  n>
    p "error:Reason when " p "  -> {got_error, Reason}"  n>
    "end." > p)
  "*The skeleton of a `try' expression.
Please see the function `tempo-define-template'.")


(defvar pel--more-erlang-skel
  '(("Export"    "export"    pel-erlang-skel-export)
    ("Import"    "import"    pel-erlang-skel-import)
    ("Try"       "try"       pel-erlang-skel-try)
    ("Try-of"    "try-of"    pel-erlang-skel-try-of))
  "Extra template entries to inserted by PEL inside `erlang-skel'.")

;;-pel-autoload
(defun pel--update-erlang-skel ()
  "Update the list of Erlang skeletons."
    (if (and (require 'erlang nil :noerror)
           (boundp 'erlang-skel-file)
           (load erlang-skel-file :noerror)
           (boundp 'erlang-skel))
      ;; Install the extra skeletons inside the erlang.el list of skeletons:
      ;; the list erlang-skel
      (setq erlang-skel (pel-insert-list-in-list
                         pel--more-erlang-skel 2 erlang-skel))))
(defun pel--install-erlang-skel (prefix)
  "Create PEL functions and key bindings in PREFIX for Erlang skeletons.
This function is meant to be called by pel-init() only."
  (if (and (require 'erlang nil :noerror)
           (boundp 'erlang-skel))
      (dolist (skel erlang-skel)
        (when skel
          (let* ((s-name (cadr skel))
                 (s-tempo-name (format "tempo-template-erlang-%s" s-name))
                 (s-pel-name   (format "pel-erl-%s" s-name))
                 (docstring    (format "\
Insert '%s' Erlang skeleton (also available through Erlang/Skeleton menu)."
                                       s-name))
                 (key          (cdr (assoc s-name pel--erl-skel-key))))
            (defalias (intern s-pel-name)
              (lambda ()
                (interactive)
                (funcall (intern s-tempo-name))
                (pel-tempo-mode 1))
              docstring)
            (when key
              (if (> (length key) 1)
                  (define-key prefix (kbd key) (intern s-pel-name))
                (define-key prefix key (intern s-pel-name)))))))
    (user-error "The erlang.el package is not loaded!")))


;;-pel-autoload
(defun pel--erlang-mode-setup ()
  "A replacement function for the erlang-mode.
It adds extra tempo templates to the Erlang mode.
This function is meant to be used as an `advice-add'
to execute *before* `erlang-mode'."
  (pel--update-erlang-skel))

;; -----------------------------------------------------------------------------
;; Detecting Erlang Versions and Controlling Erlang Man Pages to Use
;; -----------------------------------------------------------------


(defconst pel-erlang-md5-url "https://erlang.org/download/MD5"
  "URL of the official Erlang version MD5 file, using HTTPS.")

(defconst pel-erlang-man-versions
  '("R7B"
    "R16B03" "R16B03-1" "R16B02" "R16B01" "R16B"
    "R16A_RELEASE_CANDIDATE"
    "R15B03" "R15B03-1" "R15B02" "R15B01" "R15B"
    "R14B04" "R14B03" "R14B02" "R14B01" "R14B"
    "R14A"
    "R13B04" "R13B03" "R13B02" "R13B02-1" "R13B01" "R13B"
    "R13A"
    "R12B-5" "R12B-4" "R12B-3" "R12B-2" "R12B-1" "R12B-0"
    "R11B-5" "R11B-4" "R11B-3" "R11B-2" "R11B-1" "R11B-0"
    "R10B-9" "R10B-8" "R10B-7" "R10B-6" "R10B-5" "R10B-4"
    "R10B-3" "R10B-2" "R10B-1a" "R10B-10" "R10B-0"
    "23.0" "23.0-rc3" "23.0-rc2" "23.0-rc1"
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
    "18.2"
    "18.2.1"
    "18.1"
    "18.0"
    "17.5"
    "17.4"
    "17.3"
    "17.1"
    "17.0" "17.0-rc2" "17.0-rc1")
  "List of available version of Erlang Man files.
Snapshot read on 2020-07-24 16:12:52 (UTC).
Used when access to `pel-erlang-md5-url' fails.")



(defun pel-erlang-version ()
  "Return the version of Erlang to use, according to PEL's customization."
  (cond ((eq pel-erlang-version-detection-method 'auto-detect)
         (let* ((exit-code.version (pel-exec-pel-bin "version-erl"))
                (exit-code (car exit-code.version)))
           (if (eq exit-code 0)
               (cdr exit-code.version)
             (user-error "Cannot detect Erlang version automatically!"))))
        ;;
        ((eq pel-erlang-version-detection-method 'specified)
         pel-erlang-version)
        ;;
        ((eq pel-erlang-version-detection-method 'by-envvar)
         (getenv pel-erlang-version-envvar))))


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


(defun pel-read-available-erlang-man-versions ()
  "Return a list of strings: the official versions of Erlang Man pages.
The list is extracted from the official Erlang.org web page listing the MD5
of all official Erlang tarball files for download.
On error, return nil."
  (let ((temp-buffer (url-retrieve-synchronously pel-erlang-md5-url))
        (versions '()))
    (unwind-protect
        (with-current-buffer temp-buffer
          (progn
            (goto-char (point-min))
            (while (re-search-forward "otp_doc_man_\\(.+\\).tar.gz" nil t)
              (setq versions (cons (match-string 1) versions)))))
      (and (buffer-name temp-buffer)
           (kill-buffer temp-buffer)))
    versions))

;; -----------------------------------------------------------------------------
(provide 'pel-erlang)

;;; pel-erlang.el ends here
