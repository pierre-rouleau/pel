;;; pel-elisp-eval.el --- Evaluate Emacs Lisp Code.  -*- lexical-binding: t; -*-

;; Created   : Saturday, June  7 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-04-30 10:11:05 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2025, 2026  Pierre Rouleau
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
;; This file holds Emacs Lisp expansion utility.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'macroexp)                     ; use: `macroexpand-all'
(require 'elisp-mode)                   ; use: `eval-sexp-add-defvars'
;;                                      ;      `elisp--eval-defun-1'

;;; --------------------------------------------------------------------------
;;; Code:
;;
;; Credit: Stephen Berman provided the core of the following function to
;; extract the result to copy in the kill-ring.

;;-pel-autoload
(defun pel-eval-last-sexp-and-copy ()
  "Evaluate sexp before point; print value in echo area and copy to kill-ring."
  (interactive)
  (let ((result (eval (macroexpand-all
                           (eval-sexp-add-defvars
                            (elisp--eval-defun-1
                             (macroexpand (elisp--preceding-sexp)))))
                      lexical-binding)))
    (message "%s" result)
    (kill-new (format "%s" result))))


;; ---------------------------------------------------------------------------
;;* Elisp Code Stepper - With Target Buffer
;;  =======================================
;;
;; The purpose of the following code is to single step inside some Emacs Lisp
;; code as if it ran inside another buffer.  This is quite useful when writing
;; unit test code for Emacs Lisp files.
;;
;; To use it you need at least 2 buffers: one buffer where the operation will
;; be performed and a buffer with the Emacs Lisp code to execute.
;; Identify the target buffer by executing `pel-eval-set-target-buffer'.
;;
;; Once that done, select the buffer that holds the code you want to execute
;; manually.  Place point at or before the start of the first sexp (not inside
;; it) to execute with `pel-eval-to-target' which then moves to the next one.


(defvar-local pel--eval-target-buffer nil
  "The context buffer where extracted code will be executed.")


(defun pel--eval-buffer-binding-type ()
  "Return a string describing the binding type of current buffer code."
  (if (buffer-local-value 'lexical-binding (current-buffer))
      "lexical"
    "dynamic"))

;;-pel-autoload
(defun pel-eval-info ()
  "Print state of single step executer."
  (interactive)
  (if pel--eval-target-buffer
      (if (buffer-live-p pel--eval-target-buffer)
          (message
           "Elisp %s binding code stepping done in context of buffer: %s"
           (pel--eval-buffer-binding-type)
           (buffer-name pel--eval-target-buffer))
        ;; The buffer previously used is no longer valid.
        (setq pel--eval-target-buffer nil)
        (message
         "Previously used buffer was killed; Elisp code stepping now stopped"))
    (message "Elisp Code Stepping is inactive")))

;;-pel-autoload
(defun pel-eval-set-target-buffer ()
  "Prompt for and set the target buffer for remote evaluation."
  (interactive)
  (let* ((buf-name (read-buffer "Select buffer: "
                                (other-buffer (current-buffer))
                                t))
         (buf-obj (get-buffer buf-name)))
    (setq pel--eval-target-buffer buf-obj)
    (message "Evaluation target set to: %s" (buffer-name buf-obj))))

;;-pel-autoload
(defun pel-eval-stop ()
  "Stop using the previously selected buffer for code stepping."
  (interactive)
  (unless pel--eval-target-buffer
    (user-error "No target buffer selected for single stepping.
 Execute pel-eval-set-target-buffer to select a target buffer first!"))
  (setq pel--eval-target-buffer nil))

(defun pel--safe-forward-sexp ()
  "Move to the end of the next sexp.
Signal user-error if there is no sexp forward."
  (let ((original-pos (point)))
    (condition-case _err
        ;; move to the end of sexp.
        ;; May throw a scan-error in some versions of Emacs but not all.
        (forward-sexp)
      (scan-error
       (user-error "No sexp found at point (%d)!" original-pos)))
    (when (or (= (point)  original-pos)
              (and (eobp)
                   (not (eq (char-before) ?\)))))
      (user-error "No sexp found at point (%d)!" original-pos))))

;;-pel-autoload
(defun pel-eval-to-target ()
  "Evaluate the expression at point in the target buffer and move to the next.
The evaluation of the sexp in the current buffer is done in the context buffer
that was identified by the last execution of `pel-eval-set-target-buffer'.
That evaluation is using the same kind of binding that is used by the current
buffer holding the Elisp code that is executed.
Signals a user  error if there is no sexp forward."
  (interactive)

  (unless (and pel--eval-target-buffer (buffer-live-p pel--eval-target-buffer))
    (call-interactively 'pel-eval-set-target-buffer))

  (let ((sexp (save-excursion
                ;; Move to end of current sexp to capture it
                (pel--safe-forward-sexp)
                (elisp--preceding-sexp))))
    (with-current-buffer pel--eval-target-buffer
      (eval sexp (buffer-local-value 'lexical-binding (current-buffer))))
    ;; Move point forward for the next "step".
    (condition-case _err
        (forward-sexp)
      (scan-error
       (user-error "No more sexp found!")))
    ;; Skip as many comments as possible.
    (forward-comment most-positive-fixnum)))

;;; --------------------------------------------------------------------------
(provide 'pel-elisp-eval)

;;; pel-elisp-eval.el ends here
