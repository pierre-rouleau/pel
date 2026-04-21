;;; erlang.el --- Test stub for erlang.el used during test of PEL erlang code  -*- lexical-binding: t; -*-

;; Created   : Tuesday, April 21 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-04-21 13:42:45 EDT, updated by Pierre Rouleau>

;; This file is part of the ERLANG package.
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
;; A minimal replacement for erlang.el, used for testing PEL Erlang support.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defvar erlang-electric-commands nil
  "Stubbed var used for testing pel-erlang.el.")

(defvar erlang-man-dir nil
  "Stubbed var used for testing pel-erlang.el.")

(defun erlang-mode ()
  "Stub erlang-mode."
  (interactive))

(message "************** USING STUB: test/stub/erlang.el *******************")

;;; --------------------------------------------------------------------------
(provide 'erlang)

;;; erlang.el ends here
