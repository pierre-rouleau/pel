;;; pel--install-test.el --- Test installatoin helper logic  -*- lexical-binding: t; -*-

;; Created   : Wednesday, October 14 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-10-14 14:35:21 EDT, updated by Pierre Rouleau>

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
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--install)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; Test the distinction between top-level and nested alternation
;; =============================================================

(ert-deftest pel-regexp-top-level-alternation ()
  "A top-level alternation needs protection before adding an anchor."
  (should
   (pel--regexp-top-level-alternation-p "icc\\|inl"))

  ;; A nested alternation does not require another enclosing group.
  (should-not
   (pel--regexp-top-level-alternation-p
    "\\.\\(?:icc\\|inl\\)"))

  ;; An escaped literal backslash followed by | is not alternation.
  (should-not
   (pel--regexp-top-level-alternation-p "\\\\|"))

  ;; Backslash and | inside a character class are literal characters.
  (should-not
   (pel--regexp-top-level-alternation-p "[\\|]"))

  ;; This class also contains a literal closing bracket.
  (should-not
   (pel--regexp-top-level-alternation-p "[]\\|]")))


;; Test a known group reduction
;; ============================

(ert-deftest pel-regexp-removes-redundant-outer-group ()
  "Remove the redundant outer group without changing literal matches."
  (let* ((patterns '(".inl" ".icc"))
         (original (regexp-opt patterns))
         (optimized
          (pel--literal-alternatives-regexp patterns))
         (anchored (concat "\\`" optimized "\\'")))
    ;; The optimized regexp must not be longer than the original.
    (should (<= (length optimized) (length original)))
    (should (string-prefix-p "\\(?:" original))
    (should (string-suffix-p "\\)" original))
    (should (equal optimized (substring original 4 -2)))

    (dolist (name '(".icc" ".inl"))
      (should (string-match-p anchored name)))
    (dolist (name '(".iccx" ".inlx" "x.icc" ".cc" ".nl"))
      (should-not (string-match-p anchored name)))))


;; Test matching behaviour against an independent rule
;; ===================================================

(ert-deftest pel-regexp-filenames-and-suffixes ()
  "Test matching behaviour against an independent rule"
  (let ((regexp
         (pel--patterns-to-regexp
          '("Emakefile" "/.erlang" ".inl" ".icc"))))
    (dolist (filename '("/project/Emakefile"
                        "/project/.erlang"
                        "/project/header.inl"
                        "/project/header.icc"
                        "C:/project/Emakefile"))
      (should (string-match-p regexp filename)))

    (dolist (filename '("/project/MyEmakefile"
                        "/project/Emakefile.old"
                        "/project/other.erlang"
                        "/project/header.inlx"
                        "/project/headerXicc"
                        "/project/Emakefile/source.txt"))
      (should-not (string-match-p regexp filename)))))

;;; --------------------------------------------------------------------------
(provide 'pel--install-test)

;;; pel--install-test.el ends here
