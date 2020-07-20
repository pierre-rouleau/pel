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


;;-pel-autoload
(defun pel-erlang-shell-mode-init ()
  "Initialize the Erlang shell mode."
  ;; Prevent Erlang shell mode echo
  (setq comint-process-echoes t))


;;-pel-autoload
(defun pel-end-of-previous-clause ()
  "Move point backward to the end of the previous clause."
  (interactive "^")
  (if (and (require 'erlang nil :noerror)
           (fboundp 'erlang-beginning-of-clause)
           (fboundp 'erlang-end-of-clause))
      (progn
        (erlang-beginning-of-clause 2)
        (erlang-end-of-clause 1))
    (user-error "Erlang support not loaded!")))

;;-pel-autoload
(defun pel-beginning-of-next-clause ()
  "Move point forward to the beginning of next clause."
  (interactive "^")
  (if (and (require 'erlang nil :noerror)
           (fboundp 'erlang-beginning-of-clause)
           (fboundp 'erlang-end-of-clause))
      (progn
        (erlang-end-of-clause 2)
        (erlang-beginning-of-clause 1))
    (user-error "Erlang support not loaded!")))

;; -----------------------------------------------------------------------------
;; Install Erlang Skeletons as key-bound commands
;; ----------------------------------------------

(defvar pel--erl-skel-key '(("if"                      . "i")
                            ("case"                    . "c")
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
                            ("ts-test-suite"           . "M-3")))

;; Added more?:
;;   erlang-skel-export
;;   erlang-skel-import

;;-pel-autoload
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
                 (docstring    (format "Insert '%s' Erlang skeleton (also available through Erlang/Skeleton menu)." s-name))
                 (key          (cdr (assoc s-name pel--erl-skel-key))))
            (defalias (intern s-pel-name)
              (lambda ()
                (interactive)
                (funcall (intern s-tempo-name)))
              docstring)
            (when key
              (if (> (length key) 1)
                  (define-key prefix (kbd key) (intern s-pel-name))
                (define-key prefix key (intern s-pel-name)))))))
    (user-error "The erlang.el package is not loaded!")))

;; -----------------------------------------------------------------------------
(provide 'pel-erlang)

;;; pel-erlang.el ends here
