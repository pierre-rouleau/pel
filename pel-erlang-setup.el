;;; pel-erlang-setup.el --- Erlang early setup  -*- lexical-binding: t; -*-

;; Created   : Saturday, October 10 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-10-14 09:26:01 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2026  Pierre Rouleau
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
;; The code of this file is used in pel_keys.el before it completes loading.
;; It limits what it loads to prevent loading too much at startup.
;;
;; Detect Erlang Root Directory:
;; - `pel-erlang-root-path'
;;   - `pel--read-erlang-root-dir'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--options)   ; use  `pel-erlang-path-detection-method'

;; Declare the following auto-loaded functions; they are only needed via
;; execution of interactive commands, therefore we prevent early loading of a
;; tree of files during initialization by just declaring the function that
;; resides in them.
(declare-function pel-exec-pel-bin "pel-fs")

;;; --------------------------------------------------------------------------
;;; Code:
;;

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
      (let* ((exit-code.version.stderr (pel-exec-pel-bin "erlang-root-dir"))
             (exit-code (car exit-code.version.stderr)))
        (if (eq exit-code 0)
            (setq pel---extracted-erlang-root-dir (cadr exit-code.version.stderr))
          (progn
            (unless pel--read-erlang-root-dir-reported-error
              (display-warning
               'pel-erlang-root-path
               (format "pel/bin/erlang-root-dir failed with exit code: %S.
Cannot detect Erlang root path: %s"
                       (car exit-code.version.stderr)
                       (nth 2 exit-code.version.stderr))
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
   ;;
   ;; When a path is specified, use it.
   ((stringp pel-erlang-path-detection-method)
    pel-erlang-path-detection-method)
   ;;
   ;; When automatic detection is requested, use the utility.
   ((eq pel-erlang-path-detection-method 'auto-detect)
    (pel--read-erlang-root-dir))
   ;;
   ;; When environment variable is used, read its value.
   ((and (consp pel-erlang-path-detection-method)
         (eq (car pel-erlang-path-detection-method) 'by-envvar))
    (let* ((envvar-name (cdr pel-erlang-path-detection-method))
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
;; Replacement of erlang-man-dir that uses pel-erlang-man-parent-rootdir for
;; erlang-root-dir

(defvar pel---detected-erlang-root-dir nil
  "Copied value for debugging purpose.")
(defvar erlang-root-dir)  ; defined as defcustom in erlang.el

(defun pel--erlang-man-dir (erlang-man-dir subdir &optional no-download)
  "Replace `erlang-man-dir' with PEL controlled value.

specified Erlang root directory.
The directory used is controlled by PEL Erlang customization, as
selected dynamically by the `pel-erlang-root-path' function."
  ;; erlang-man-dir arguments: (subdir &optional no-download)
  (let ((erlang-root-dir (pel-erlang-root-path)))
    (setq pel---detected-erlang-root-dir erlang-root-dir)
    (funcall erlang-man-dir subdir no-download)))

(defvar pel--erlang-man-dir--setup nil
  "Set t if pel--erlang-man-dir is setup.")

;;-pel-autoload
(defun pel-erlang-setup-erlang-man-dir-root ()
  "Setup a replacement for the erlang.el `erlang-man-dir'."
  (unless pel--erlang-man-dir--setup
    (advice-add 'erlang-man-dir
                :around (function pel--erlang-man-dir))
    (setq pel--erlang-man-dir--setup t)))

;;; --------------------------------------------------------------------------
(provide 'pel-erlang-setup)

;;; pel-erlang-man.el ends here
