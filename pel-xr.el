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
;;  * `pel-xr-lint'
;;  * `pel-xr-lint-at-point'
;;    - `pel--xr-lint-expand'
;;  * `pel-xr-regexp'
;;  * `pel-xr-at-point'
;;    - `pel-interpret-regexp'

;;; Code:

(require 'pel--base)
(require 'pel-read)                     ; use: pel-string-at-point

;; -----------------------------------------------------------------------------
;; Regular Expression Expansion
;; ----------------------------

(defun pel-interpret-regexp (regexp &optional dialect)
  "Print interpretation of REGEXP string in *regexp-eval* buffer.
See `xr' for a description of DIALECT."
  (if (and (require 'xr nil :noerror)
           (fboundp 'xr-pp))
      (let ((bufname (get-buffer-create "*regexp-eval*")))
        (with-current-buffer bufname
          (goto-char (point-max))
          (insert (format "\n- Interpretation of: \"%s\":\n" regexp))
          (xr-pp regexp dialect))
        (display-buffer bufname))
    (user-error "Command xr-pp is not available!")))

;;-pel-autoload
(defun pel-xr-at-point (&optional dialect)
  "Grab regexp at point and print its rx interpretation in *regexp-eval* buffer.
Uses `xr-pp' to expand regexp in rx notation.

If region is marked, grab content of region instead.
DIALECT is selected by numeric argument:
- nil, 1 := medium verbose
- < 0    := terse
- 4      := brief   : short keywords
- 16     := verbose : verbose keywords.
To get 4 use \\[universal-argument],
and to get 16 type \\[universal-argument] \\[universal-argument].

LIMITATION:
 Does not support double quote inside a regexp taken at point
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
  "Prompt for regexp and print its rx  interpretation in *regexp-eval* buffer.
Uses `xr-pp' to expand regexp in rx notation."
  (interactive)
  (let ((regexp (read-string "Regexp: ")))
    (pel-interpret-regexp regexp)))

;; -----------------------------------------------------------------------------
;; Linting
;; -------

(defun pel--xr-lint-expand (regexp for-file-match)
  "Lint the REGEXP that might be FOR-FILE-MATCH.
Print evaluation in the *regexp-eval* buffer."
  (if (and (require 'xr nil :noerror)
           (fboundp 'xr-lint))
      (let ((bufname (get-buffer-create "*regexp-eval*"))
            warnings)
        (with-current-buffer bufname
          (goto-char (point-max))
          (insert (format "\n- Lint of%s: \"%s\":\n"
                          (if for-file-match " (file match)" "")
                          regexp))
          (setq warnings
                (xr-lint regexp (when for-file-match 'file)))
          (if warnings
              (dolist (offset.comment warnings)
                (insert (format "  - @%2d: %s\n"
                                (car offset.comment)
                                (cdr offset.comment))))
            (insert (if pel-can-display-special-chars-p
                        "️  👍\n"
                      "  OK\n")))
          (display-buffer bufname)))
    (user-error "Command xr-lint is not available!")))

;;-pel-autoload
(defun pel-xr-lint (&optional for-file-match)
  "Prompt for a regexp, lint it and display results.
If FOR-FILE-MATCH argument is non-nil, perform additional checkings to see if
the regexp is OK for matching file name."
  (interactive "P")
  (let ((regexp (read-string "Regexp: ")))
    (pel--xr-lint-expand regexp (when for-file-match 'file))))

;;-pel-autoload
(defun pel-xr-lint-at-point (&optional for-file-match)
  "Lint the regexp at point or inside region if region is marked.

If FOR-FILE-MATCH argument is non-nil, perform additional checkings to see if
the regexp is OK for matching file name.

LIMITATION:
 Does not support double quote inside a regexp taken at point
 even if it is quoted.  To grab it mark the region, excluding the
 delimiting quotes."
  (interactive "P")
  (let ((regexp (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (pel-string-at-point "\"" :allow-space))))
    (pel--xr-lint-expand regexp (when for-file-match 'file))))

;; -----------------------------------------------------------------------------
(provide 'pel-xr)

;;; pel-xr.el ends here
