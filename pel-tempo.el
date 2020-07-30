;;; pel-tempo.el --- Specialized tempo mode  -*- lexical-binding: t; -*-

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
;; This file defines the very simple pel-tempo-mode, a mode that does only one
;; thing: it activates a key-map to ease execution of the `tempo-forward-mark'
;; and `tempo-backward-mark' commands.  These commands move point to the next or
;; previous template "hot-spot" (officially called tempo marks): the location
;; where extra text of the template must be filled in by the user.
;;
;; While active the pel-tempo-mode displays its short lighter: " ‡".

;; -----------------------------------------------------------------------------
;;; Code:

(defvar pel-tempo-mode nil
  "Non-nil when `pel-tempo-mode' is active, nil otherwise.")

(defvar pel-tempo-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Use the standard go next/previous keys
    (define-key map "\C-c."    'tempo-forward-mark)
    (define-key map "\C-c,"    'tempo-backward-mark)
    (when (display-graphic-p)
      (define-key map (kbd "C-c C-.") 'tempo-forward-mark)
      (define-key map (kbd "C-c C-,") 'tempo-backward-mark))
    map)
  "Key-map for pel-tempo minor mode.")

(define-minor-mode pel-tempo-mode
  "Minor mode to ease navigation in tempo template hot-spots.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

When pel-tempo-mode is on, extra, easy-to-use key bindings are
bound to the `tempo-forward-mark' and `tempo-backward-mark'
commands to help navigate to the next and previous tempo mark.

Key1 bindings:
\\{pel-tempo-minor-mode-map}"
  :lighter " ‡"
  :keymap pel-tempo-minor-mode-map
  )

;; -----------------------------------------------------------------------------
(provide 'pel-tempo)

;;; pel-tempo.el ends here
