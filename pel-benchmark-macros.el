;;; pel-benchmark-macros.el --- Benchmark macro utilities for development  -*- lexical-binding: t; -*-

;; Created   : Sunday, October 25 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-10-25 15:49:08 EDT, updated by Pierre Rouleau>

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
(require 'benchmark)     ; use: `benchmark-run'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defmacro pel-benchmark (count form)
  "Garbage collect, then evaluate FORM COUNT times and return timing data.

Compile the enclosing function before calling it to measure compiled code.
This macro does not compile FORM at run time."
  (declare (indent 1))
  `(progn
     (garbage-collect)
     (benchmark-run ,count ,form)))

(defmacro pel-benchmark-compare (count &rest implementations)
  "Evaluate each implementation COUNT times and print a timing table.

IMPLEMENTATIONS must contain one or more alternating literal string
labels and forms.  For example:

  (pel-benchmark-compare 1000
    \"concat\" (concat \"c\" \"-mode\")
    \"format\" (format \"%s-mode\" \"c\"))

Return a formatted report string identifying the implementation with
the shortest measured total time.  The returned report is not printed
by this macro."
  (declare (indent 1))
  ;; Validate list of implementations
  (unless (and implementations
               (zerop (% (length implementations) 2)))
    (error "Expected one or more benchmark label/form pairs"))
  (let ((results-alist (make-symbol "results-alist"))
        (res (make-symbol "res"))
        body-forms)
    (while implementations
      (let ((label (pop implementations))
            (form (pop implementations)))
        (unless (stringp label)
          (error "Benchmark label must be a literal string: %S" label))
        (push
         `(let (,res)
            (garbage-collect)
            (setq ,res (benchmark-run ,count ,form))
            (princ (format "%-35s | Total: %7.4fs | GCs: %3d | GC Time: %7.4fs\n"
                           ,label (nth 0 ,res) (nth 1 ,res) (nth 2 ,res)))
            ;; Store the label and the total execution time (nth 0 res)
            (push (cons ,label (nth 0 ,res)) ,results-alist))
         body-forms)))
    `(let (,results-alist)
       (princ "-----------------------------------------------------------------\n")
       (princ (format "%-35s | %-13s | %-4s | %-12s\n" "Implementation" "Total Time" "GCs" "GC Time"))
       (princ "-----------------------------------------------------------------\n")
       ,@(nreverse body-forms)
       (princ "-----------------------------------------------------------------\n")
       ;; Sort otal time, then return a report for the first (fastest) entry.
       (format "Fastest implementation: %s\n"
               (car (car (sort ,results-alist (lambda (a b) (< (cdr a) (cdr b))))))))))

(defmacro pel-benchmark-compare-auto (count &rest implementations)
  "Compare multiple code implementations side-by-side COUNT times.

IMPLEMENTATIONS must contain one or more forms.
Generate each label from the printed representation of its form.

Return a formatted report string identifying the implementation with
the shortest measured total time.  The returned report is not printed
by this macro."
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
