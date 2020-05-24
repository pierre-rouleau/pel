;;; pel-speedbar.el --- PEL (Sr-)Speedbar support -*-lexical-binding: t-*-

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

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
;; A set of utilities to help work with the native Emacs speedbar and
;; sr-speedbar which displays in the same frame, which is useful in
;; terminal mode.
;;
;; In graphics mode, both are equally functional, but in terminal mode
;; Sr-speedbar is clearly superior because Speedbar will take over the
;; entire frame while SR-Speedbar uses only one of the windows.
;;
;; If SR-speedbar is not available, Speedbar is used.
;;
;; If SR-speedbar is available, when Emacs runs in graphics mode, then
;; `pel-open-close-speedbar' prompts the first time it's called to select
;; which one to use.
;; When Emacs runs in terminal mode, `pel-open-close-speedbar' prompts
;; only if the customizable variable `pel-prefer-sr-speedbar-in-terminal'
;; is nil, otherwise it automatically selects Sr-Speedbar, which is more
;; convenient.
;;
;; The mechanism selected is remembered inside the variable
;; `pel-speedbar-type-used'.  If you want to prevent the first prompt
;; set this variable to 'sr-speedbar or 'speedbar.
;;
;; Note:
;;
;; The 2014 version of the sr-speedbar.el file attempts to access the
;; `helm-alive-p' variable, but does not require helm nor check if that
;; variable is bounded, causing byte-compilation warnings in pel.el or
;; here.
;; The 2016 version of sr-speedbar fixes this problem.
;;
;; If you get this warning while byte compiling any of the PEL code,
;; make sure to use the 2016 version of sr-speedbar, available at MELPA.

;;; Code:

;; require the libraries that either come with Emacs or are part of PEL
(require 'speedbar)
(require 'pel--base)
(require 'pel--macros)
(require 'pel--options)              ; use: pel-prefer-sr-speedbar-in-terminal
;; sr-speedbar may not be installed.
;; Allow compilation if it's not installed:
;; later code checks if its symbols are bound
(require 'sr-speedbar nil :noerror)

;; PEL: Speedbar/Sr-Speedbar management
;; ------------------------------------
;;
;; variables:
;; speedbar-frame     : holds frame of speedbar, nil if not opened.
;; sr-speedbar-window : nil when there is no sr-speedbar,
;;                      otherwise holds it window.

(defvar pel-window-before-sr-speedbar
  nil
  "If non-nil, holds the window to return to from sr-speedbar.")

(defvar pel-speedbar-type-used
  (if (not (fboundp 'sr-speedbar-toggle))
      'speedbar
    (if (display-graphic-p)
        nil
      (if pel-prefer-sr-speedbar-in-terminal
          'sr-speedbar
        nil)))
  "Identifies the type of Speedbar component used.
The values are:
- nil:          nothing used so far, prompt first time used.
- 'speedbar:    Speedbar opened in another frame.
                Available in graphics mode only.
- 'sr-speedbar: Sr-Speedbar, a speedbar in the same frame.

'sr-speedbar is only allowed if available.
When available, 'sr-speedbar is preferred in terminal mode,
unless `pel-prefer-sr-speedbar-in-terminal' is nil.")

(defvar pel--speedbar-active
  nil
  "Identifies whether Speedbar was already opened.
t if Speedbar was opened, nil otherwise.
Do *not* change its value manually.")

;; --

(defun pel--speedbar-toggle ()
  "Open/close Speedbar appropriately, remember we're using it."
  (setq pel-speedbar-type-used 'speedbar)
  (if (display-graphic-p)
      (speedbar)
    (speedbar-get-focus)))

(defun pel--sr-speedbar-toggle ()
  "Open/close Sr-Speedbar and remember we're using it."
  (pel-when-fbound 'sr-speedbar-toggle
    (setq pel-speedbar-type-used 'sr-speedbar)
    (sr-speedbar-toggle)))

;;-pel-autoload
(defun pel-open-close-speedbar ()
  "Use/close appropriate speedbar: Speedbar or Sr-Speedbar.
- If Sr-Speedbar is not available, use Speedbar.
- If Sr-Speedbar is available, then:
  - In text mode, if `pel-prefer-sr-speedbar-in-terminal'
    is non-nil use Sr-Speedbar (other window, same frame),
    otherwise prompt and use what user selects.
  - In graphics mode, always prompt, and use what user selects.
Once Speedbar or Sr-Speedbar has been used, keep using the same
in subsequent calls of the Emacs session."
  (interactive)
  (cond ((not pel-speedbar-type-used)
         (if (yes-or-no-p
              (if (display-graphic-p)
                  "Use separate frame? "
                "Use entire terminal frame instead of a dedicated window? "))
             (pel--speedbar-toggle)
           (pel--sr-speedbar-toggle)))
        ((equal pel-speedbar-type-used 'speedbar)
         (pel--speedbar-toggle))
        ((equal pel-speedbar-type-used 'sr-speedbar)
         (pel--sr-speedbar-toggle)))
  (setq pel--speedbar-active t))

;; --
;;
;; * pel-toggle-to-speedbar
;;   - pel--toggle-to-sr-speedbar-window

(defun pel--toggle-to-sr-speedbar-window ()
  "Move point to/out of sr-speedbar.
If point is not in sr-speedbar and sr-speedbar exists move to it.
Otherwise return to previously visited window if it still exists,
otherwise move to the other window."
  (pel-when-fbound 'sr-speedbar-window-p
    (pel-when-fbound 'sr-speedbar-select-window
      (if pel--speedbar-active
          (if (sr-speedbar-window-p)
              (when pel-window-before-sr-speedbar
                ;; Return point back to window that had focus
                ;; before moving to sr-speedbar
                ;; If that window is still alive, otherwise move to other window.
                (if (window-live-p pel-window-before-sr-speedbar)
                    (select-window pel-window-before-sr-speedbar)
                  (other-window 1))
                (setq pel-window-before-sr-speedbar nil))
            ;; else: remember current window and move to sr-speedbar
            (setq pel-window-before-sr-speedbar (selected-window))
            (sr-speedbar-select-window))
        (user-error "Open SR-Speedbar first with pel-open-close-speedbar")))))

;;-pel-autoload
(defun pel-toggle-to-speedbar ()
  "Move point to speedbar frame or sr-speedbar window or back.
If no speedbar is used, open one."
  (interactive)
  (if (not pel-speedbar-type-used)
      (pel-open-close-speedbar)
    (if (equal pel-speedbar-type-used 'sr-speedbar)
        (pel--toggle-to-sr-speedbar-window)
      (user-error "The speedbar used is inside a separate frame "))))

;; --

;;-pel-autoload
(defun pel-speedbar-toggle-refresh ()
  "Toggle automatic refresh of used Speedbar."
  (interactive)
  (if pel--speedbar-active
      (cond ((equal pel-speedbar-type-used 'speedbar)
             (speedbar-toggle-updates)
             (message "Speedbar automatic refresh is now: %s"
                      (pel-symbol-on-off-string 'speedbar-update-flag)))
            ((equal pel-speedbar-type-used 'sr-speedbar)
             (pel-when-fbound 'sr-speedbar-refresh-toggle
               (sr-speedbar-refresh-toggle)
               (message "Sr-Speedbar automatic refresh is now: %s"
                        (pel-symbol-on-off-string 'sr-speedbar-auto-refresh))))
            (t
             (error "Logic error, please report")))
    (user-error "Open Speedbar first")))

;; --

;;-pel-autoload
(defun pel-speedbar-refresh ()
  "Force refresh of Speedbar content."
  (interactive)
  (if pel--speedbar-active
      (cond ((equal pel-speedbar-type-used 'speedbar)
             (speedbar-refresh t))
            ((equal pel-speedbar-type-used 'sr-speedbar)
             (pel-when-fbound 'sr-speedbar-refresh
               (sr-speedbar-refresh)))
            (t
             (error "Logic error, please report")))
    (user-error "Open Speedbar first")))

;;-pel-autoload
(defun pel-speedbar-toggle-show-all-files ()
  "Execute `speedbar-toggle-show-all-files' if loaded, warn otherwise."
  (interactive)
  (if (fboundp 'speedbar-toggle-show-all-files)
      (speedbar-toggle-show-all-files)
    (user-error "Open Speedbar first")))

;;-pel-autoload
(defun pel-speedbar-toggle-sorting ()
  "Execute `speedbar-toggle-sorting' if loaded, warn otherwise."
  (interactive)
  (if (fboundp 'speedbar-toggle-sorting)
      (speedbar-toggle-sorting)
    (user-error "Open Speedbar first")))

;;-pel-autoload
(when (display-graphic-p)
  (defun pel-speedbar-toggle-images ()
    "Execute `speedbar-toggle-images' if loaded, warn otherwise."
    (interactive)
    (if (fboundp 'speedbar-toggle-images)
        (speedbar-toggle-images)
      (user-error "Open Speedbar first"))))

;; -----------------------------------------------------------------------------
(provide 'pel-speedbar)

;;; pel-speedbar.el ends here

;; LocalWords:  speedbar
