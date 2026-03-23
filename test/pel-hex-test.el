;;; pel-hex-test.el --- ERT tests for pel-hex.el  -*- lexical-binding: t; -*-

;; Created   : Monday, March 23 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-23 12:42:35 EDT, updated by Pierre Rouleau>

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
;; ERT tests for pel-hex.el.
;;
;; Covered items:
;;
;;   pel-bibyte   - converts a byte count to (gibibyte mebibyte kibibyte byte)
;;
;; Items intentionally NOT covered:
;;   (none — pel-bibyte is the only public function in pel-hex.el)
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-hex)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:

;; ===========================================================================
;; pel-bibyte
;;
;; Conversion constants (exact integer arithmetic):
;;   1 KiB = 1024
;;   1 MiB = 1024 * 1024         = 1 048 576
;;   1 GiB = 1024 * 1024 * 1024  = 1 073 741 824
;;
;; The function decomposes a number N into (G M K B) where:
;;   G = N / GiB (integer)
;;   remainder after G
;;   M = remainder / MiB (integer)
;;   remainder after M
;;   K = remainder / KiB (integer)
;;   B = final remainder
;; ===========================================================================

(ert-deftest pel-hex-test/bibyte/zero ()
  "Zero maps to all-zero components."
  (should (equal (pel-bibyte 0) '(0 0 0 0))))

;; ---------------------------------------------------------------------------
;; Pure-byte range: 1 – 1023
;; ---------------------------------------------------------------------------

(ert-deftest pel-hex-test/bibyte/one-byte ()
  "A single byte."
  (should (equal (pel-bibyte 1) '(0 0 0 1))))

(ert-deftest pel-hex-test/bibyte/arbitrary-byte ()
  "A mid-range byte value."
  (should (equal (pel-bibyte 512) '(0 0 0 512))))

(ert-deftest pel-hex-test/bibyte/max-byte ()
  "1023 bytes — one less than a full kibibyte."
  (should (equal (pel-bibyte 1023) '(0 0 0 1023))))

;; ---------------------------------------------------------------------------
;; Kibibyte boundaries
;; ---------------------------------------------------------------------------

(ert-deftest pel-hex-test/bibyte/one-kibibyte-exact ()
  "Exactly one kibibyte produces zero in the byte slot."
  (should (equal (pel-bibyte 1024) '(0 0 1 0))))

(ert-deftest pel-hex-test/bibyte/one-kibibyte-plus-one ()
  "One kibibyte plus one byte."
  (should (equal (pel-bibyte 1025) '(0 0 1 1))))

(ert-deftest pel-hex-test/bibyte/one-kibibyte-max-bytes ()
  "One kibibyte plus 1023 bytes — just below two kibibytes."
  (should (equal (pel-bibyte 2047) '(0 0 1 1023))))

(ert-deftest pel-hex-test/bibyte/max-kibibytes-and-bytes ()
  "1023 KiB + 1023 B — one byte below one mebibyte.
1023*1024 + 1023 = 1 047 552 + 1023 = 1 048 575."
  (should (equal (pel-bibyte 1048575) '(0 0 1023 1023))))

;; ---------------------------------------------------------------------------
;; Mebibyte boundaries
;; ---------------------------------------------------------------------------

(ert-deftest pel-hex-test/bibyte/one-mebibyte-exact ()
  "Exactly one mebibyte (1 048 576 bytes)."
  (should (equal (pel-bibyte 1048576) '(0 1 0 0))))

(ert-deftest pel-hex-test/bibyte/one-mebibyte-plus-kibibyte ()
  "One mebibyte plus one kibibyte."
  (should (equal (pel-bibyte (+ 1048576 1024)) '(0 1 1 0))))

(ert-deftest pel-hex-test/bibyte/one-mebibyte-plus-kibibyte-plus-byte ()
  "One mebibyte + one kibibyte + one byte."
  (should (equal (pel-bibyte (+ 1048576 1024 1)) '(0 1 1 1))))

(ert-deftest pel-hex-test/bibyte/max-mebibytes-and-below ()
  "1023 MiB + 1023 KiB + 1023 B — one byte below one gibibyte.
1023*1048576 + 1023*1024 + 1023 = 1 073 741 823."
  (should (equal (pel-bibyte 1073741823) '(0 1023 1023 1023))))

;; ---------------------------------------------------------------------------
;; Gibibyte boundaries
;; ---------------------------------------------------------------------------

(ert-deftest pel-hex-test/bibyte/one-gibibyte-exact ()
  "Exactly one gibibyte (1 073 741 824 bytes)."
  (should (equal (pel-bibyte 1073741824) '(1 0 0 0))))

(ert-deftest pel-hex-test/bibyte/one-gibibyte-plus-mebibyte ()
  "One gibibyte plus one mebibyte."
  (should (equal (pel-bibyte (+ 1073741824 1048576)) '(1 1 0 0))))

(ert-deftest pel-hex-test/bibyte/one-gibibyte-plus-kibibyte ()
  "One gibibyte plus one kibibyte."
  (should (equal (pel-bibyte (+ 1073741824 1024)) '(1 0 1 0))))

(ert-deftest pel-hex-test/bibyte/one-gibibyte-plus-one-byte ()
  "One gibibyte plus one byte."
  (should (equal (pel-bibyte (+ 1073741824 1)) '(1 0 0 1))))

;; ---------------------------------------------------------------------------
;; All-nonzero and multi-GiB values
;; ---------------------------------------------------------------------------

(ert-deftest pel-hex-test/bibyte/all-nonzero-components ()
  "All four components non-zero simultaneously.
1 GiB + 1 MiB + 1 KiB + 1 B = 1 073 741 824 + 1 048 576 + 1024 + 1
                              = 1 074 791 425."
  (should (equal (pel-bibyte 1074791425) '(1 1 1 1))))

(ert-deftest pel-hex-test/bibyte/two-gibibytes ()
  "Two exact gibibytes."
  (should (equal (pel-bibyte (* 2 1073741824)) '(2 0 0 0))))

(ert-deftest pel-hex-test/bibyte/large-mixed ()
  "2 GiB + 512 MiB + 256 KiB + 128 B."
  (let ((n (+ (* 2 1073741824)
              (* 512 1048576)
              (* 256 1024)
              128)))
    (should (equal (pel-bibyte n) '(2 512 256 128)))))

(ert-deftest pel-hex-test/bibyte/return-type-is-list-of-four-integers ()
  "Return value is always a proper list of exactly four integers."
  (let ((result (pel-bibyte 1074791425)))
    (should (listp result))
    (should (= (length result) 4))
    (should (cl-every #'integerp result))))

;;; --------------------------------------------------------------------------
(provide 'pel-hex-test)

;;; pel-hex-test.el ends here
