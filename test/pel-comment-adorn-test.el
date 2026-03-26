;;; pel-comment-adorn-test.el --- ERT tests for pel-comment-adorn.el  -*- lexical-binding: t; -*-

;; Created   : Wednesday, March 25 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-26 08:36:30 EDT, updated by Pierre Rouleau>

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

;; ERT tests for pel-comment-adorn.el.
;; Tests verify that:
;; - The 10 `pel-commented-adorn-N' commands are defined and callable.
;; - The `pel--mk-adorn-cmd' macro generates commands with correct names and
;;   docstrings.
;; - `pel-commented-adorn' modifies the buffer as expected (with mocked
;;   dependencies).

;;; --------------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'pel-comment-adorn)

;;; --------------------------------------------------------------------------
;;; Tests for macro-generated commands existence
;;; --------------------------------------------------------------------------

(ert-deftest pel-comment-adorn-test/commands-exist/level-1 ()
  "pel-commented-adorn-1 command is defined."
  (should (fboundp 'pel-commented-adorn-1)))

(ert-deftest pel-comment-adorn-test/commands-exist/level-2 ()
  "pel-commented-adorn-2 command is defined."
  (should (fboundp 'pel-commented-adorn-2)))

(ert-deftest pel-comment-adorn-test/commands-exist/level-3 ()
  "pel-commented-adorn-3 command is defined."
  (should (fboundp 'pel-commented-adorn-3)))

(ert-deftest pel-comment-adorn-test/commands-exist/level-4 ()
  "pel-commented-adorn-4 command is defined."
  (should (fboundp 'pel-commented-adorn-4)))

(ert-deftest pel-comment-adorn-test/commands-exist/level-5 ()
  "pel-commented-adorn-5 command is defined."
  (should (fboundp 'pel-commented-adorn-5)))

(ert-deftest pel-comment-adorn-test/commands-exist/level-6 ()
  "pel-commented-adorn-6 command is defined."
  (should (fboundp 'pel-commented-adorn-6)))

(ert-deftest pel-comment-adorn-test/commands-exist/level-7 ()
  "pel-commented-adorn-7 command is defined."
  (should (fboundp 'pel-commented-adorn-7)))

(ert-deftest pel-comment-adorn-test/commands-exist/level-8 ()
  "pel-commented-adorn-8 command is defined."
  (should (fboundp 'pel-commented-adorn-8)))

(ert-deftest pel-comment-adorn-test/commands-exist/level-9 ()
  "pel-commented-adorn-9 command is defined."
  (should (fboundp 'pel-commented-adorn-9)))

(ert-deftest pel-comment-adorn-test/commands-exist/level-10 ()
  "pel-commented-adorn-10 command is defined."
  (should (fboundp 'pel-commented-adorn-10)))

;;; --------------------------------------------------------------------------
;;; Tests for command interactivity
;;; --------------------------------------------------------------------------

(ert-deftest pel-comment-adorn-test/commands-are-interactive/level-1 ()
  "pel-commented-adorn-1 is an interactive command."
  (should (commandp 'pel-commented-adorn-1)))

(ert-deftest pel-comment-adorn-test/commands-are-interactive/level-5 ()
  "pel-commented-adorn-5 is an interactive command."
  (should (commandp 'pel-commented-adorn-5)))

(ert-deftest pel-comment-adorn-test/commands-are-interactive/level-10 ()
  "pel-commented-adorn-10 is an interactive command."
  (should (commandp 'pel-commented-adorn-10)))

;;; --------------------------------------------------------------------------
;;; Tests for macro-generated docstrings
;;; --------------------------------------------------------------------------

(ert-deftest pel-comment-adorn-test/docstrings/level-1-contains-level-number ()
  "pel-commented-adorn-1 docstring mentions level 1."
  (let ((doc (documentation 'pel-commented-adorn-1)))
    (should (stringp doc))
    (should (string-match-p "1" doc))))

(ert-deftest pel-comment-adorn-test/docstrings/level-7-contains-level-number ()
  "pel-commented-adorn-7 docstring mentions level 7."
  (let ((doc (documentation 'pel-commented-adorn-7)))
    (should (stringp doc))
    (should (string-match-p "7" doc))))

(ert-deftest pel-comment-adorn-test/docstrings/level-10-contains-level-number ()
  "pel-commented-adorn-10 docstring mentions level 10."
  (let ((doc (documentation 'pel-commented-adorn-10)))
    (should (stringp doc))
    (should (string-match-p "10" doc))))

;;; --------------------------------------------------------------------------
;;; Tests for `pel-commented-adorn' behavior (mocked dependencies)
;;; --------------------------------------------------------------------------

(ert-deftest pel-comment-adorn-test/commented-adorn/calls-rst-adorn-with-level ()
  "pel-commented-adorn calls pel-rst-adorn with the correct level."
  (let ((called-with nil))
    (cl-letf (((symbol-function 'pel-rst-adorn)
               (lambda (level) (setq called-with level)))
              ((symbol-function 'pel-mark-line-down) #'ignore)
              ((symbol-function 'comment-dwim)       #'ignore)
              ((symbol-function 'forward-line)       #'ignore))
      (with-temp-buffer
        (insert "My Section Title\n")
        (goto-char (point-min))
        (pel-commented-adorn 3))
      (should (= called-with 3)))))

(ert-deftest pel-comment-adorn-test/commented-adorn/calls-mark-line-down-with-2 ()
  "pel-commented-adorn calls pel-mark-line-down with 2."
  (let ((mark-arg nil))
    (cl-letf (((symbol-function 'pel-rst-adorn)     #'ignore)
              ((symbol-function 'pel-mark-line-down)
               (lambda (n) (setq mark-arg n)))
              ((symbol-function 'comment-dwim)       #'ignore)
              ((symbol-function 'forward-line)       #'ignore))
      (with-temp-buffer
        (insert "My Section Title\n")
        (goto-char (point-min))
        (pel-commented-adorn 1))
      (should (= mark-arg 2)))))

(ert-deftest pel-comment-adorn-test/commented-adorn/moves-to-bol-first ()
  "pel-commented-adorn starts from the beginning of the current line."
  (let ((bol-called nil))
    (cl-letf (((symbol-function 'move-beginning-of-line)
               (lambda (_) (setq bol-called t)))
              ((symbol-function 'pel-rst-adorn)     #'ignore)
              ((symbol-function 'pel-mark-line-down) #'ignore)
              ((symbol-function 'comment-dwim)       #'ignore)
              ((symbol-function 'forward-line)       #'ignore))
      (with-temp-buffer
        (insert "My Section Title\n")
        (goto-char (point-min))
        (pel-commented-adorn 2))
      (should bol-called))))

;;; --------------------------------------------------------------------------
;;; Tests for all 10 commands delegating to pel-commented-adorn
;;; --------------------------------------------------------------------------

(ert-deftest pel-comment-adorn-test/delegation/level-1-delegates ()
  "pel-commented-adorn-1 delegates to pel-commented-adorn with level 1."
  (let ((called-level nil))
    (cl-letf (((symbol-function 'pel-commented-adorn)
               (lambda (level) (setq called-level level))))
      (pel-commented-adorn-1)
      (should (= called-level 1)))))

(ert-deftest pel-comment-adorn-test/delegation/level-5-delegates ()
  "pel-commented-adorn-5 delegates to pel-commented-adorn with level 5."
  (let ((called-level nil))
    (cl-letf (((symbol-function 'pel-commented-adorn)
               (lambda (level) (setq called-level level))))
      (pel-commented-adorn-5)
      (should (= called-level 5)))))

(ert-deftest pel-comment-adorn-test/delegation/level-10-delegates ()
  "pel-commented-adorn-10 delegates to pel-commented-adorn with level 10."
  (let ((called-level nil))
    (cl-letf (((symbol-function 'pel-commented-adorn)
               (lambda (level) (setq called-level level))))
      (pel-commented-adorn-10)
      (should (= called-level 10)))))

;;; --------------------------------------------------------------------------
(provide 'pel-comment-adorn-test)
;;; pel-comment-adorn-test.el ends here
