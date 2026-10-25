;;; pel-b-basic.el --- Benchmark basic low-level function candidate implementations  -*- lexical-binding: t; -*-

;; Created   : Sunday, October 25 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-10-25 18:37:08 EDT, updated by Pierre Rouleau>

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
(require 'pel--base)
(require 'pel-benchmark-macros)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-b-print-command (program &rest arguments)
  "Print output from PROGRAM with ARGUMENTS.
Report missing programs and failures; don't stop execution."
  (let ((executable (executable-find program)))
    (if (not executable)
        (princ (format "\n%s: unavailable\n" program))
      (princ (format "\n--- %s %s ---\n"
                     program (mapconcat #'identity arguments " ")))
      (condition-case err
          (with-temp-buffer
            (let ((status
                   (apply #'call-process
                          executable nil t nil arguments)))
              (princ (buffer-string))
              (unless (equal status 0)
                (princ (format "\nCommand status: %S\n" status)))))
        (error
         (princ (format "Could not run command: %s\n"
                        (error-message-string err))))))))

(defun pel-b-report-system ()
  "Print information about the system running the benchmark."
  (cond
   ((eq system-type 'gnu/linux)
    (pel-b-print-command "uname" "-sr")
    (pel-b-print-command "lscpu")
    (pel-b-print-command "free" "-h"))
   ((eq system-type 'darwin)
    (pel-b-print-command "sw_vers")
    (pel-b-print-command "sysctl"
                         "hw.model" "hw.machine"
                         "hw.physicalcpu" "hw.logicalcpu"
                         "hw.memsize")
    (pel-b-print-command "sysctl" "machdep.cpu.brand_string"))))

;; ---------------------------------------------------------------------------

(eval-and-compile
  (defconst pel-b-basic-count 100000
    "Number of repetitions for each basic benchmark."))

;; ---------------------------------------------------------------------------

(defun pel-major-modes-for-2 (modename)
  "Return a list of major mode symbols associated with a MODENAME string.

The major mode symbols are created by appending \"-mode\" and \"-ts-mode\" to
the MODENAME string.

Examples:
 (pel-major-modes-for \"c\")  ; => \\='(c-mode c-ts-mode)"
  (list (intern (format "%s-mode"    modename))
        (intern (format "%s-ts-mode" modename))))

(defun pel-b-major-modes-for ()
  "Benchmark different implementations of the `pel-major-modes-for'."
  (princ (pel-benchmark-compare-auto pel-b-basic-count
           (pel-major-modes-for "c")
           (pel-major-modes-for-2 "c")))
  nil)

;; ---------------------------------------------------------------------------

(defun pel-mode-name-for-2 (mode-symbol)
  "Return the base mode name of MODE-SYMBOL as a string.

Remove the trailing \"-mode\" or \"-ts-mode\" suffix.
Signal an error if MODE-SYMBOL has neither suffix."
  (let* ((name-str (symbol-name mode-symbol))
         (case-fold-search nil)
         (suffix-start
          (string-match-p "\\(?:-ts\\)?-mode\\'" name-str)))
    (unless suffix-start
      (error "Mode symbol lacks a -mode suffix: %S" mode-symbol))
    (substring name-str 0 suffix-start)))

(defun pel-b-mode-name-for ()
  "Benchmark different implementations of the `pel-mode-name-for'."
  (princ (pel-benchmark-compare-auto pel-b-basic-count
           (pel-mode-name-for 'c-mode)
           (pel-mode-name-for-2 'c-mode)))
  (princ "\n")
  (princ (pel-benchmark-compare-auto pel-b-basic-count
           (pel-mode-name-for 'c-ts-mode)
           (pel-mode-name-for-2 'c-ts-mode)))
  (princ "\n")
  (princ (pel-benchmark-compare-auto pel-b-basic-count
           (pel-mode-name-for 'c++-mode)
           (pel-mode-name-for-2 'c++-mode)))
  (princ "\n")
  (princ (pel-benchmark-compare-auto pel-b-basic-count
           (pel-mode-name-for 'c++-ts-mode)
           (pel-mode-name-for-2 'c++-ts-mode)))
  nil)

;; ---------------------------------------------------------------------------

(defun pel-b-basic-run ()
  "Run the basic benchmarks and print their reports.

Use this function as the entry point for batch execution.
Compile and load the benchmark functions and candidate implementations
before calling this function to measure compiled code."
  (pel-b-report-system)
  (princ (format "Emacs %s | %s\n"
                 emacs-version system-configuration))
  (princ "\nMode-name construction\n")
  (princ (pel-b-major-modes-for))
  (princ "\nMode-name extraction\n")
  (pel-b-mode-name-for))

;;; --------------------------------------------------------------------------
(provide 'pel-b-basic)

;;; pel-b-basic.el ends here
