;;; pel-as-test.el --- ERT tests for pel-as.el  -*- lexical-binding: t; -*-

;; Created   : Monday, March 23 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-24 16:37:38 EDT, updated by Pierre Rouleau>

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
;; ERT tests for pel-as.el.
;;
;; Covered items:
;;
;;   pel--sh-modes    - list of shell-script modes derived from sh-ancestor-alist
;;   pel--all-modes   - full list of modes with shebang support
;;
;; Items intentionally NOT covered:
;;   - `pel--as-sh'  (activates real Emacs major modes; integration concern)
;;   - `pel-as'      (interactive command; drives mode switching and file I/O)
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-as)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:

;; ===========================================================================
;; pel--sh-modes
;; ===========================================================================

(ert-deftest pel-as-test/sh-modes/is-non-empty-list ()
  "`pel--sh-modes' must be a non-empty list."
  (should (listp pel--sh-modes))
  (should (> (length pel--sh-modes) 0)))

(ert-deftest pel-as-test/sh-modes/contains-sh ()
  "`pel--sh-modes' must contain the generic `sh' shell."
  (should (memq 'sh pel--sh-modes)))

(ert-deftest pel-as-test/sh-modes/contains-bash ()
  "`pel--sh-modes' must contain `bash'."
  (should (memq 'bash pel--sh-modes)))

(ert-deftest pel-as-test/sh-modes/elements-are-symbols ()
  "Every element of `pel--sh-modes' must be a symbol."
  (dolist (mode pel--sh-modes)
    (should (symbolp mode))))

;; ===========================================================================
;; pel--all-modes
;; ===========================================================================

(ert-deftest pel-as-test/all-modes/is-non-empty-list ()
  "`pel--all-modes' must be a non-empty list."
  (should (listp pel--all-modes))
  (should (> (length pel--all-modes) 0)))

(ert-deftest pel-as-test/all-modes/elements-are-symbols ()
  "Every element of `pel--all-modes' must be a symbol."
  (dolist (mode pel--all-modes)
    (should (symbolp mode))))

(ert-deftest pel-as-test/all-modes/contains-config ()
  "`pel--all-modes' must contain the `config' mode."
  (should (memq 'config pel--all-modes)))

(ert-deftest pel-as-test/all-modes/contains-d ()
  "`pel--all-modes' must contain the `d' mode."
  (should (memq 'd pel--all-modes)))

(ert-deftest pel-as-test/all-modes/contains-lua ()
  "`pel--all-modes' must contain `lua'."
  (should (memq 'lua pel--all-modes)))

(ert-deftest pel-as-test/all-modes/contains-nim ()
  "`pel--all-modes' must contain `nim'."
  (should (memq 'nim pel--all-modes)))

(ert-deftest pel-as-test/all-modes/contains-perl ()
  "`pel--all-modes' must contain `perl'."
  (should (memq 'perl pel--all-modes)))

(ert-deftest pel-as-test/all-modes/contains-pike ()
  "`pel--all-modes' must contain `pike'."
  (should (memq 'pike pel--all-modes)))

(ert-deftest pel-as-test/all-modes/contains-python ()
  "`pel--all-modes' must contain `python'."
  (should (memq 'python pel--all-modes)))

(ert-deftest pel-as-test/all-modes/contains-ruby ()
  "`pel--all-modes' must contain `ruby'."
  (should (memq 'ruby pel--all-modes)))

(ert-deftest pel-as-test/all-modes/contains-seed7 ()
  "`pel--all-modes' must contain `seed7'."
  (should (memq 'seed7 pel--all-modes)))

(ert-deftest pel-as-test/all-modes/contains-expect ()
  "`pel--all-modes' must contain `expect'."
  (should (memq 'expect pel--all-modes)))

(ert-deftest pel-as-test/all-modes/contains-tcl ()
  "`pel--all-modes' must contain `tcl'."
  (should (memq 'tcl pel--all-modes)))

(ert-deftest pel-as-test/all-modes/is-superset-of-sh-modes ()
  "Every mode in `pel--sh-modes' must also appear in `pel--all-modes'."
  (dolist (mode pel--sh-modes)
    (should (memq mode pel--all-modes))))

(ert-deftest pel-as-test/all-modes/no-duplicates ()
  "`pel--all-modes' must contain no duplicate entries."
  (let ((seen '()))
    (dolist (mode pel--all-modes)
      (should-not (memq mode seen))
      (push mode seen))))

;;; --------------------------------------------------------------------------
(provide 'pel-as-test)

;;; pel-as-test.el ends here
