;;; pel-time.el --- Time value arithmetic utilities.  -*- lexical-binding: t; -*-

;; Created   : Monday, January 31 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-02-01 08:24:45, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2022  Pierre Rouleau
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
;; This file holds a set of simple time aritmetic utilities, converting
;; hh:mm:ss strings into (hour minute second) triple integer values with the
;; ability to detect invalid values (when the minute or the second count
;; exceeds 59) and the ability to normalize it.
;;
;; The function `pel-sum-hms' can sum (hour minute second) triple and the
;; function `pel-sum-hms-strings' can sum "hh:mm:ss" strings.   Both return a
;; (hour minute second) triple.
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--base)           ; use: `pel+='
(require 'subr-x)              ; use: `split-string'

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

(defun pel-subtract-hms-strings (hms &rest hms-strings)
  "Return the (hour minutes second) sum of the specified H:M:S HMS-STRINGS."
  (apply (function pel-subtract-hms)
         (pel-hms-string-to-hms hms)
         (mapcar
          (function pel-hms-string-to-hms)
          hms-strings)))

;;; --------------------------------------------------------------------------
(provide 'pel-time)
