;;; pel-benchmark-macros.el --- Benchmark macro utilities for development  -*- lexical-binding: t; -*-

;; Created   : Sunday, October 25 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-10-25 12:16:42 EDT, updated by Pierre Rouleau>

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
;; This file provides useful benchmark tools that can be used inside ielm to
;; produce relatively accurate execution timing measurement of code.  To get
;; the best accuracy create code inside a file and byte compile and/or native
;; compile it to run it.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'benchmark)     ; use: `benchmark-run-compiled'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defmacro pel-benchmark (count form)
  "Garbage collect, then benchmark FORM evaluated COUNT times.
The FORM is automatically compiled to eliminate interpreter overhead."
  (declare (indent 1))
  `(progn
     (garbage-collect)
     (benchmark-run ,count ,form)))


;; (defmacro pel-benchmark-compare (count &rest implementations)
;;   (let (body-forms)
;;     (while implementations
;;       (let ((label (pop implementations)) (form (pop implementations)))
;;         (push `(let (res) (garbage-collect) (setq res (benchmark-run-compiled ,count ,form))
;;                     (princ (format "%-35s | Total: %7.4fs | GCs: %3d | GC Time: %7.4fs\n" ,label (nth 0 res) (nth 1 res) (nth 2 res)))) body-forms)))
;;     `(progn (princ "---------------A--------------------------------------------------\n")
;;             (princ (format "%-35s | %-15s | %-8s | %-12s\n" "Implementation" "Total Time" "GCs" "GC Time"))
;;             (princ "---------------B--------------------------------------------------\n")
;;             ,@(nreverse body-forms)
;;             (princ "---------------CC--------------------------------------------------\n")
;;             nil)))
(defmacro pel-benchmark-compare (count &rest implementations)
  "Compare implementations COUNT times. Print table and return the fastest label."
  (declare (indent 1))
  (let (body-forms)
    (while implementations
      (let ((label (pop implementations))
            (form (pop implementations)))
        (push
         `(let (res)
            (garbage-collect)
            (setq res (benchmark-run-compiled ,count ,form))
            (princ (format "%-35s | Total: %7.4fs | GCs: %3d | GC Time: %7.4fs\n"
                           ,label (nth 0 res) (nth 1 res) (nth 2 res)))
            ;; Store the label and the total execution time (nth 0 res)
            (push (cons ,label (nth 0 res)) results-alist))
         body-forms)))
    `(let (results-alist)
       (princ "-----------------------------------------------------------------\n")
       (princ (format "%-35s | %-13s | %-4s | %-12s\n" "Implementation" "Total Time" "GCs" "GC Time"))
       (princ "-----------------------------------------------------------------\n")
       ,@(nreverse body-forms)
       (princ "-----------------------------------------------------------------\n")
       ;; Sort by execution time ascending (fastest first) and return the car of the first element
       (format "Fastest implementation: %s\n"
               (car (car (sort results-alist (lambda (a b) (< (cdr a) (cdr b))))))))))

(defmacro pel-benchmark-compare-auto (count &rest implementations)
  "Compare multiple code implementations side-by-side COUNT times.

Labels are automatically generated from the string representation of the
code forms.
Returns the string name of the fastest implementation."
  (declare (indent 1))
  (let (expanded-implementations)
    ;; Loop through each code block provided in the arguments
    (dolist (form implementations)
      ;; Convert the raw code structure into a clean string label
      (let ((label (prin1-to-string form)))
        (push label expanded-implementations)
        (push form expanded-implementations)))
    ;; Forward the generated pairs to our existing comparison engine
    `(pel-benchmark-compare ,count ,@(nreverse expanded-implementations))))


;;; --------------------------------------------------------------------------
(provide 'pel-benchmark-macros)

;;; pel-benchmark-macros.el ends here
