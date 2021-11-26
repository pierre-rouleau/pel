;;; pel-graphviz-dot.el --- Graphviz Dot utilities. -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2021  Pierre Rouleau

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
;; This file contains code that extends support for Graphviz Dot
;;
;; The functions are:
;;
;; * pel-render-commented-graphviz-dot
;;   - pel--graphviz-dot-region
;; -----------------------------------------------------------------------------

;;; Code:
(require 'pel--base)                    ; use: pel-region-for
(require 'pel-ccp)                      ; use: pel-delete-whole-line

(defun pel--graphviz-dot-region (&optional pos)
  "Return the position of the beginning & end of graphviz dot code block.
Search at POS if specified, otherwise search around point.
Include whole lines.
Return a (start . end) cons cell if found, otherwise return nil."
  (pel-region-for "@start-gdot" "@end-gdot" pos))


;;-pel-autoload
(defun pel-render-commented-graphviz-dot (&optional pos)
  "Render the Graphviz-Dot markup embedded in current mode comment.
Search at POS if specified, otherwise search around point.
Use region if identified otherwise use Graphviz-Dot block.
NOTE: creates an image file in a temporary directory."
  (interactive)
  (if (and (require 'graphviz-dot-mode nil :noerror)
           (fboundp 'graphviz-dot-mode)
           (fboundp 'graphviz-dot-preview))
      (let ((beg nil)
            (end nil)
            (text nil)
            (f-comment-start comment-start)
            (f-comment-end   comment-end)
            (f-comment-continue comment-continue))
        (if (use-region-p)
            (progn
              (setq beg (region-beginning))
              (setq end (region-end)))
          (let ((beg.end (pel--graphviz-dot-region pos)))
            (setq beg (car beg.end))
            (setq end (cdr beg.end))))
        (if (and beg end)
            ;; Creates a temporary file, open that file, write the text
            ;; in it, then remove comments, remove markers are surrounding
            ;; empty lines.  Then use graphviz-dot-preview to render the DOT
            ;; code inside a new buffer and clean up.
            ;; Unfortunately the buffer for the temp file is visible during the
            ;; operation. I could not get this to work by using a temp buffer;
            ;; it had to be a file otherwise graphviz-dot-preview would prompt
            ;; for a file name.
            (progn
              (setq text (buffer-substring-no-properties beg end))
              (let ((temp-fname (make-temp-file
                                 (format "pel-gdot-%s-"
                                         (file-name-nondirectory
                                          (file-name-sans-extension
                                           (buffer-file-name))))
                                 nil ".dot")))
                (find-file temp-fname)
                  (insert text)
                  (unwind-protect
                      (progn
                        ;; Remove comments : TODO: won't work for comments like C
                        ;; if the beginning and end of the comment is not in buffer.
                        (setq comment-start f-comment-start)
                        (setq comment-end   f-comment-end)
                        (setq comment-continue f-comment-continue)
                        (uncomment-region (point-min) (point-max))
                        ;; Remove the marker lines and blank lines around the
                        ;; graph definition
                        (goto-char (point-min))
                        (pel-delete-whole-line)
                        (delete-blank-lines)
                        (goto-char (point-max))
                        (pel-delete-whole-line)
                        (delete-blank-lines)
                        ;; activate Graphviz-Dot mode and render the diagram in
                        ;; other window
                        (graphviz-dot-mode)
                        (goto-char (point-min))
                        (forward-line 2)
                        (graphviz-dot-preview))
                    ;; cleanup - remove buffer and temp file.
                    ;; But does NOT remove the generated PNG file.
                    (and (file-exists-p temp-fname)
                         (kill-buffer (get-file-buffer temp-fname))
                         (delete-file temp-fname)))))
              (user-error "Missing `@start-gdot' & `@end-gdot' markers!")))
    (user-error "Requires missing graphviz-dot-mode!")))

;; -----------------------------------------------------------------------------
(provide 'pel-graphviz-dot)

;;; pel-graphviz-dot.el ends here
