;;; pel-frame-control.el --- PEL terminal/graphic mode Emacs support utilities

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau.swd@gmail.com>

;; This file is part of the PEL package
;; This file is not part of GNU Emacs.

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

;; -----------------------------------------------------------------------------
;;; Commentary:
;;
;; Some functions only work in graphic mode, but the macOS Terminal.app based
;; operation is available via the macOS specific keystrokes.  The following
;; functions detect the mode, and perform the operation in graphic mode, but
;; display a message in Terminal mode to remind how it can be performed with
;; macOS keystrokes.

;; TODO: Add support for other environments to  pel-toggle-frame-fullscreen.
;;       Currently it only supports macOS and two of the terminal applications
;;       available on it.

;;; Code:
(require 'pel--base)                    ; use: pel-count-string

;;-pel-autoload
(defun pel-toggle-frame-fullscreen ()
  "Toggle frame fullscreen mode on/off in graphics mode.

In Terminal mode, issue error to show how to use the OS keystrokes
to toggle the fullscreen mode."
  (interactive)
  (if (display-graphic-p)
      (toggle-frame-fullscreen)
    (if (eq system-type 'darwin)
      (let* ((terminal_name (pel-val-or-default
                             (getenv "TERM_PROGRAM") "this unknown terminal"))
             (keys (cond ((string= terminal_name "Apple_Terminal")
                          "use 'Control-Command f'")
                         ((string= terminal_name "iTerm.app")
                          "use 'Command RET'")
                         (t "consult the manual"))))
        (error
         "When using Emacs inside %s, %s to toggle fullscreen mode"
         terminal_name keys))
      (error "When using Emacs in Terminal mode, use your OS supported command to toggle fullscreen mode"))))

;;-pel-autoload
(defun pel-show-frame-count ()
  "Display number of active Emacs frames in the mini-buffer."
  (interactive)
  (message "Emacs has %s, of which %s visible."
           (pel-count-string
            (length (frame-list))
            "frame")
           (pel-count-string
            (if (display-graphic-p)
                (length (visible-frame-list))
              1)
            "is" "are")))

;;-pel-autoload
(defun pel-next-frame (arg)
  "Make next frame visible.
In text terminal mode: iterate through all frames.
In graphics mode:
 - with no/nil argument (the default): iterate through visible frames,
 - with ARG non nil: iterate through all frames (included iconified frames)."
  (interactive "p")
  (raise-frame (next-frame
                nil
                (if (and (display-graphic-p) arg)
                    0
                  'visible))))

;;-pel-autoload
(defun pel-previous-frame (arg)
  "Make previous frame visible.
In text terminal mode: iterate through all frames.
In graphics mode:
 - with no/nil argument (the default): iterate through visible frames,
 - with ARG non nil: iterate through all frames (included iconified frames)."
  (interactive "p")
  (raise-frame (previous-frame
                nil
                (if (and (display-graphic-p) arg)
                    0
                  'visible))))

;; -----------------------------------------------------------------------------
(provide 'pel-frame-control)

;;; pel-frame-control.el ends here
