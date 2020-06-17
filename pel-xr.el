;;; pel-xr.el --- Regexp Interpretation  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

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
;; Utilities to interpret regular expressions.
;;
;;  * `pel-xr'
;;  * `pel-xr-at-point'
;;    - pel-interpret-regexp

;;; Code:

;;(require 'xr)
(require 'pel-read)                     ; use: pel-string-at-point

;; -----------------------------------------------------------------------------
;; Regular Expression Expansion
;; ----------------------------

(defun pel-interpret-regexp (regexp &optional dialect)
  "Print interpretation of REGEXP string in *Regexp Eval Log* buffer.
See `xr' for a description of DIALECT."
  (let ((bufname (get-buffer-create "*regexp-eval*")))
    (with-current-buffer bufname
      (goto-char (point-max))
      (insert (format "\n- Interpretation of: \"%s\":\n" regexp))
      (xr-pp regexp dialect))
    (display-buffer bufname)))

;;-pel-autoload
(defun pel-xr-at-point (&optional dialect)
  "Grab regexp at point and print its interpretation in *regexp-eval* buffer.
If region is marked, grab content of region instead.
DIALECT is selected by numeric argument:
- nil, 1 := medium verbose
- < 0    := terse
- 4      := brief   : short keywords
- 16     := verbose : verbose keywords.
To get 4 use \\[universal-argument],
and to get 16 type \\[universal-argument] \\[universal-argument].

LIMITATION:
 does not support double quote inside a regexp taken at point
 even if it is quoted.  To grab it mark the region, excluding the
 delimiting quotes."
  (interactive "p")
  (let ((dialect (cond ((eq dialect 1) 'medium)
                       ((< dialect 0)   'terse)
                       ((>= dialect 16) 'verbose)
                       ((>= dialect 2)  'brief)))
        (regexp (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (pel-string-at-point "\"" :allow-space))))
    (pel-interpret-regexp regexp dialect)))

;;-pel-autoload
(defun pel-xr-regxp ()
  "Prompt for regexp and print its interpretation in *regexp-eval* buffer."
  (interactive)
  (let ((regexp (read-string "Regexp: ")))
    (pel-interpret-regexp regexp)))

;; -----------------------------------------------------------------------------
(provide 'pel-xr)

;;; pel-xr.el ends here
