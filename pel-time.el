;;; pel-time.el --- Time value arithmetic utilities.  -*- lexical-binding: t; -*-

;; Created   : Monday, January 31 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-07-04 12:02:31 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2022, 2025  Pierre Rouleau
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
;; This file holds time related code.
;;
;; First it holds a set of simple time aritmetic utilities, converting
;; hh:mm:ss strings into (hour minute second) triple integer values with the
;; ability to detect invalid values (when the minute or the second count
;; exceeds 59) and the ability to normalize it.
;;
;;     The function `pel-sum-hms' can sum (hour minute second) triple and the
;;     function `pel-sum-hms-strings' can sum "hh:mm:ss" strings.  Both return
;;     a (hour minute second) triple.  The functions `pel-subtract-hms' and
;;     `pel-subtract-hms-strings' perform similar subtraction operations.
;;
;;     The functions operate on the following types identified by the argument
;;     names:
;;
;;     - `seconds'    : integer number of seconds
;;     - `hms'        : a (hours minutes seconds) triple of integers
;;     - `hms-elems'  : a sequence of `hms'
;;     - `hms-string' : a "hh:mm:ss" string representing hours:minutes:seconds
;;     - `hms-strings': a sequence of `hms-string'
;;
;;
;;     The following conversion function are provided:
;;
;;     - `pel-seconds-to-hms'
;;     - `pel-hms-to-sec'
;;
;;     - `pel-sum-hms-strings'
;;     - `pel-sum-hms'
;;
;;     - `pel-hms-string-to-hms'
;;
;;
;;     The next 2 functions are used to normalize and validate the time
;;     elements:
;;
;;     - `pel-normalize-hms'
;;     - `pel-validate-hms'
;;
;; It also holds time measurement utility:
;;
;;  - `pel-time-spent-by'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--base)           ; use: `pel+='
(require 'subr-x)              ; use: `split-string'
(require 'time-date)           ; use: `time-since'

;; ---------------------------------------------------------------------------
;;; Code
;;; ----
;;;
;;;

(defun pel-seconds-to-hms (seconds)
  "Convert SECONDS integer into a (hour minute second) list."
  (list (/ seconds 3600) (/ (% seconds 3600) 60) (% seconds 60)))


(defun pel-validate-hms (hms)
  "Check if the HMS list (hours minutes seconds) is valid.

Return the value untouched if valid, raise an error otherwise."
  (unless (< -1 (nth 2 hms) 60)
    (error "Invalid second count in %s" hms))
  (unless (< -1 (nth 1 hms) 60)
    (error "Invalid minute count in %s" hms))
  hms)

(defun pel-hms-string-to-hms (hms-string)
  "Convert a hh:mm:ss HMS-STRING into a (hour minute second) list."
  (pel-validate-hms (mapcar (function string-to-number)
                            (split-string hms-string ":"))))

(defun pel-hms-to-sec (hms)
  "Return number of seconds in the HMS triple."
  (+ (* 3600 (nth 0 hms))
     (* 60 (nth 1 hms))
     (nth 2 hms)))

(defun pel-normalize-hms (hms)
  "Normalize a HMS set of counts that potentially is not normalized.

Ensure that the number of minutes and seconds do not exceed 59 in
the returned value."
  (pel-seconds-to-hms (pel-hms-to-sec hms)))

(defun pel-sum-hms (&rest hms-elems)
  "Return the (hour minute second) sum of the specified HMS elements."
  (let ((total-seconds 0))
    (dolist (h-m-s hms-elems)
      (pel+= total-seconds (pel-hms-to-sec h-m-s)))
    (pel-seconds-to-hms total-seconds)))

(defun pel-sum-hms-strings (&rest hms-strings)
  "Return the (hour minutes second) sum of the specified H:M:S HMS-STRINGS."
  (apply (function pel-sum-hms)
         (mapcar
          (function pel-hms-string-to-hms)
          hms-strings)))

(defun pel-subtract-hms (hms &rest hms-elems)
  "Subtract HMS or subtract HMS_ELEMS and return (hour minute second) triple."
  (let ((result (pel-hms-to-sec hms)))
    (dolist (h-m-s hms-elems)
      (pel-= result (pel-hms-to-sec h-m-s)))
    (pel-seconds-to-hms result)))

(defun pel-subtract-hms-strings (hms-string &rest hms-strings)
  "Return the (hour minutes second) difference of the specified H:M:S HMS-STRINGS."
  (apply (function pel-subtract-hms)
         (pel-hms-string-to-hms hms-string)
         (mapcar
          (function pel-hms-string-to-hms)
          hms-strings)))


;; ---------------------------------------------------------------------------
;; Time measurement
;; ----------------

(defun pel--time-fmt (number)
  "Return empty string if NUMBER is 0, NUMBER followed by colon otherwise."
  (if (eq number 0)
      ""
    (format "%d:" number)))

(defun pel-time-to-hms-fraction-string (time)
  "Return TIME as a formatted string."
  (let* ((all-seconds (floor time))
         (sec-fraction (- time (float all-seconds)))
         (hms (pel-seconds-to-hms all-seconds))
         (hours (nth 0 hms))
         (minutes (nth 1 hms))
         (seconds (nth 2 hms)))
    (format "%s%s%.06f"
            (pel--time-fmt hours)
            (pel--time-fmt minutes)
            (+ seconds sec-fraction))))

(defmacro pel-time-spent-by (&rest body)
  "Measure time spent by the execution of BODY."
  `(let ((start-time (current-time)))
     ,@body
     (message "%s" (pel-time-to-hms-fraction-string
                    (float-time (time-since start-time))))))

;;; --------------------------------------------------------------------------
(provide 'pel-time)
