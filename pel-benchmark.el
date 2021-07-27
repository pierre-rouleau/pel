;;; pel-benchmark.el --- Emacs benchmark reports.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, September  1 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-07-27 00:17:19, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2021  Pierre Rouleau
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

(require 'pel-window)
(require 'pel-setup)

;;; ----------------------------------------------------------------------------
;;; Code:
;;

(defun pel-show-init-time (&optional log-it)
  "Display benchmark startup time.
Display the benchmark initialization and duration tree in 2 buffers
if the benchmark-init external package is installed and available.

If the optional LOG-IT argument is non-nil then instead logs
the Emacs startup time, process mode, operation mode and Emacs version
into the pel-startup-time.log.txt file stored in the user-emacs-directory.

In both cases, also display the Emacs startup time.

Requires installation of the benchmark-init.
Use M-x list-package, select benchmark-init and install it.

Then update your init.el file and place the following lines as close as possible
to the top of the file:

  ;; -----------------------------------------------------------------------------
  ;; Setup Benchmark Measurement
  ;; ---------------------------
  ;; Load benchmark right away using the file name explicitly so we can use it
  ;; to benchmark the complete package loading mechanism.
  ;; CAUTION:
  ;;          - Copy the following files downloaded from MELPA into your PEL
  ;;            utility directory which is normally  ~/.emacs.d/utils:
  ;;                - benchmark-init-modes.el and .elc
  ;;                - benchmark-init.el and .elc
  ;;            - Do not copy the benchmark-init-autoloads.el and the
  ;;              nor the benchmark-init-pkg.el file.
  ;;              They are not needed for PEL.
  ;;          - Use to measure startup time and development of your init,
  ;;            comment this code after your investigation and want to start
  ;;            a little faster.
  ;;
  (require 'benchmark-init
           (expand-file-name \"~/.emacs.d/utils/benchmark-init\"))
  (add-hook 'after-init-hook 'benchmark-init/deactivate)

Update the path if necessary.

If the above lines are not written in your init.el this function only
prints the Emacs init time on the echo area."
  (interactive "P")
  (if log-it
      (let ((filename (expand-file-name "pel-startup-time.log.txt"
                                        user-emacs-directory)))
        (with-temp-file filename
          (auto-fill-mode -1)
          (when (file-exists-p filename)
            (insert-file-contents filename))
          (goto-char (point-max))
          (insert (format "%-20s | %-20s | %-12s | Emacs %s on %s\n"
                          (emacs-init-time)
                          (if (display-graphic-p) "graphic mode" "terminal (TTY) mode")
                          (pel--operation-mode)
                          emacs-version
                          system-type))))

    (when (and (fboundp 'benchmark-init/show-durations-tree)
               (fboundp 'benchmark-init/show-durations-tabulated)
               (memq 'benchmark-init/deactivate after-init-hook))
      (delete-other-windows)
      (split-window-below)
      (pel-2-vertical-windows)
      ;; max-specpdl-size default is 1000.
      ;; When there's a lot of packages installed benchmark hits a limit
      ;; with 1000 and stops with a warning
      ;; "Variable binding depth exceeds max-specpdl-size".
      ;; Change the value for the duration of the benchmark dump.
      (let ((max-specpdl-size 2000))
        (ignore-errors
          (benchmark-init/show-durations-tree)
          (other-window 1)
          (benchmark-init/show-durations-tabulated)))))
  (message "Emacs startup time: %s" (emacs-init-time)))

;;; ----------------------------------------------------------------------------
(provide 'pel-benchmark)

;;; pel-benchmark.el ends here
