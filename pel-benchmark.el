;;; pel-benchmark.el --- Emacs benchmark reports.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, September  1 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2020-09-25 13:38:31, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020  Pierre Rouleau
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

;;; ----------------------------------------------------------------------------
;;; Commentary:
;;
;; This file contains Emacs benchmark utilities.

;;; ----------------------------------------------------------------------------
;;; Dependencies:
;;
;; This depends on some code being stored inside your init.el file.
;; See `pel-show-init-time' docstring.

;;; ----------------------------------------------------------------------------
;;; Code:
;;

(defun pel-show-init-time ()
    "Display benchmark startup time.
Display the benchmark initialization and duration tree in 2 buffers.
Also display the Emacs startup time.

Requires installation of the benchmark-init.
Use M-x list-package, select benchmark-init and install it.

Then update your init.el file and place the following lines as close as possible
to the top of the file:

  ;; -----------------------------------------------------------------------------
  ;; Setup Benchmark Measurement
  ;; ---------------------------
  ;; Load benchmark right away using the file name explicitly so we can use it
  ;; to benchmark the complete package loading mechanism.
  ;; CAUTION: Modify the path when a new version is available.
  (require 'benchmark-init
           (expand-file-name
            \"~/.emacs.d/elpa/benchmark-init-20150905.938/benchmark-init\"))
  (add-hook 'after-init-hook 'benchmark-init/deactivate)

Update the path if necessary.

If the above lines are not written in your init.el this function only
prints the Emacs init time on the echo area."
    (interactive)
    (when (and (fboundp 'benchmark-init/show-durations-tree)
               (fboundp 'benchmark-init/show-durations-tabulated))
      (delete-other-windows)
      (split-window-below)
      ;; max-specpdl-size default is 1000.
      ;; When there's a lot of packages installed benchmark hits a limit
      ;; with 1000 and stops with a warning
      ;; "Variable binding depth exceeds max-specpdl-size".
      ;; Change the value for the duration of the benchmark dump.
      (let ((max-specpdl-size 2000))
        (ignore-errors
            (benchmark-init/show-durations-tree)
            (other-window 1)
            (benchmark-init/show-durations-tabulated))))
    (message "Emacs startup time: %s" (emacs-init-time)))

;;; ----------------------------------------------------------------------------
(provide 'pel-benchmark)

;;; pel-benchmark.el ends here
