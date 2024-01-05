;;; pel-hide-docstring.el --- Hide docstrings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2023, 2024  Pierre Rouleau

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

;;; Commentary:
;;
;; This provides the ability to hide and show again the docstring the current,
;; previous or next definition.
;;
;; When hiding the docstring, the code leaves the opening and end quotes visible
;; as well as the first 4 characters.  It does not hide docstrings with less
;; than 5 characters.
;;
;;  LIMITATIONS:  work in progress, could be enhanced.
;;     - No visual feedback or prevention of hidden docstring modification.
;;     - Does not properly handle the ability to hide several docstrings, then
;;       show back some of them.  That will work as long as you do not use
;;       another command that uses the visible property, such as comment hiding.
;;       Docstring and comment hiding co-exists without problem when only hiding
;;       one docstring at a time.
;;
;;  Despite those current limitations, it is useful.  Be careful when using it.
;;  I am planning to submit enhancements soon to handle most of the points
;;  described in the limitations.
;;

;; The file defines the following commands (*) and functions (-):
;;
;; * pel-toggle-all-docstrings
;; * pel-hide/show-all-docstrings
;;   * pel-toggle-docstring
;;     - pel--docstring-visible-p
;;       - pel--docstring-char-invisible-p
;;   * pel-hide/show-docstring
;;     - pel--hs-docstring
;;       - pel--hs-docstring-chars
;; * pel-show-docstring-positions
;;   - pel--docstring-positions
;;

;;; Code:
;;
(require 'pel-navigate)
(require 'pel-face-ut)

(defconst pel-regexp-python-beg
  "^ +\\(\\([uU]\\)?\\|\\([rRfF]\\)?\\|\\([rRfF][rRfF]\\)?\\)['\\\"]\\{3\\}"
  "Regexp to detect the beginning of a Python docstring.

Python string literals
- ref: URL https://docs.python.org/3/reference/lexical_analysis.html#literals

Match with optional string prefix
and when 3 single or double quotes are used.

stringprefix::=  r | u | R | U | f | F | fr | Fr | fR | FR | rf | rF | Rf | RF

Note that regexp match group 0 ends with the 3 quote characters
that must end the Python docstring.")

(defconst pel-regexp-python-module-beg
  "^\\(\\([uU]\\)?\\|\\([rRfF]\\)?\\|\\([rRfF][rRfF]\\)?\\)['\\\"]\\{3\\}"
  "Regexp for Python module docstring: starts on first character of line.")

;; --

(defun pel--docstring-positions (next &optional limit)
  "Identify the start and end position of definition's docstring.

Identify the position of the next definition docstring when NEXT
is non-nil otherwise check the docstring positions of the current or previous
definition.

Optional LIMIT is maximum end position, not specified end of buffer
is used instead.
Does not move point.
Return cons cell of (start . end) positions if found, nil otherwise."
  (let (searchfor-docstring
        (regexp-beg (if (eq major-mode 'python-mode)
                        pel-regexp-python-beg
                      "^ +\\\"")))
  (save-excursion
    (unless (condition-case nil
                (setq searchfor-docstring
                      (if next
                          (pel-beginning-of-next-defun :silent :dont-push-mark)
                        (beginning-of-defun 1)))
              (error nil))
      (progn
        ;; For Python on detection of definition, try module
        ;; docstring
        (when (eq major-mode 'python-mode)
          (setq searchfor-docstring t)
          (goto-char (point-min))
          (setq regexp-beg pel-regexp-python-module-beg))))
    (when searchfor-docstring
      (let (beg-pos end-pos)
        ;; search for entry quote : must start at beginning of line
        (setq beg-pos (re-search-forward regexp-beg (or limit (point-max)) :noerror 1))
        (when beg-pos
          ;; Found candidate.  Check the face in case this is a string inside the
          ;; argument list. If it is not the font-lock-doc-face search for it.
          (unless (pel-face-at-pos-is beg-pos 'font-lock-doc-face)
            (let ((new-pos (pel-pos-of-first-char-with-face-in
                            'font-lock-doc-face beg-pos limit)))
              (when new-pos
                (setq beg-pos (1+ new-pos)) ; since we report the position past the quote.
                (goto-char new-pos))))
          ;; search next quote that is not escaped
          (let ((regexp-end (if (eq major-mode 'python-mode)
                                (if (string= "'''"  (substring
                                                     (match-string-no-properties 0)
                                                     -3 nil))
                                    "'''"
                                  "[^\\]\\\"\\\"\\\"")
                              "[^\\\"]\\\"")))
            (setq end-pos (re-search-forward regexp-end
                                             (or limit (point-max)) :noerror 1))))
          (when end-pos (cons beg-pos end-pos)))))))


(defun pel-show-docstring-positions (&optional next)
  "Show the positions of docstring.
By default show positions of the docstring of the current or previous
defintion.  If NEXT argument is specified, show positions of the docstring of
next definition instead."
  (interactive "P")
  (let ((beg.end (pel--docstring-positions (and next 'next))))
    (message "Position of the %s definition docstring: [%d, %d]"
             (if next "next" "current/previous")
             (car beg.end)
             (cdr beg.end))))

;; --

(defsubst pel--hs-docstring-chars (start-pos end-pos show)
  "Hide or show docstring characters from START-POS to END-POS inclusively.
Hide when SHOW is nil.
Show when SHOW is non-nil."
  (put-text-property start-pos end-pos
                     'invisible
                     (if show nil 'hide-docstring)))

(defun pel--hs-docstring (show next &optional limit)
  "Hide or show the next or previous docstring.

Hide the docstring by default.  If SHOW is non-nil show it instead.

Identify the position of the next definition docstring when NEXT
is non-nil otherwise check the docstring positions of the current or previous
definition.

Optional LIMIT is maximum end position, not specified end of buffer
is used instead.
Does not move point.
Return cons cell of (start . end) positions if found, nil otherwise."
  (let ((beg.end (pel--docstring-positions next limit)))
    (when beg.end
      ;; adjust invisibility spec to allow other criterias to hide things
      ;; TODO this does not take into account multiple hidings then showing back some
      (if show
          (remove-from-invisibility-spec 'hide-docstring)
        (add-to-invisibility-spec 'hide-docstring))
      ;;
      (let ((beg (car beg.end))
            (end (cdr beg.end)))
        (if show
            ;; show the complete doc-string
            (pel--hs-docstring-chars beg end show)
          ;; request to hide docstring
          (if (> (- end beg) 5)
              (progn
                (setq beg (+ beg 4))
                (setq end (1- end))
                ;; hide docstring, leave the quotes and 4 characters visible
                (pel--hs-docstring-chars beg end nil))
            (user-error "The docstring at %d,%d is too small (%s), kept visible"
                        beg
                        end
                        (buffer-substring (1- beg) end))))
      beg.end))))

;;-pel-autoload
(defun pel-hide/show-docstring (&optional show silent)
  "Hide or show the docstring of current or previous definition.
Hide the docstring by default.  If SHOW is non-nil show it instead.
Return (beg.end) on success.
If no docstring detected issue a user-error by default.
But if SILENT is non-nil, instead of issuing an error return nil instead."
  (interactive "P")
  ;; (let ((start-point (if (use-region-p) (region-beginning) (point-min)))
  ;;       (end-point   (if (use-region-p) (region-end) (point-max))))
  ;; Hiding/showing docstrings is not a buffer modification.
  (with-silent-modifications
    (condition-case nil
        (progn
          (restore-buffer-modified-p nil)
          (pel--hs-docstring show nil nil))
      (error (if silent
                 nil
               (user-error "No docstring detected around point!"))))))

;; --

(defun pel--docstring-char-invisible-p (pos)
  "Return t if character at POS is an invisible docstring, nil otherwise.
Therefore nil might be because the character does not have the invisible
attribute or because it has the invisible attribute but its value is not
\\='hide-docstring."
  (eq (get-text-property pos 'invisible) 'hide-docstring))

(defun pel--docstring-visible-p (&optional next)
  "Return t if docstring is visible, nil otherwise.
Check the docstring of the current/previous definition by default, but if
the NEXT argument is non-nil then check the docstring of the next definition."
  (let* ((beg.end (pel--docstring-positions next))
         (beg (car beg.end))
         (end (cdr beg.end))
         (hide-count 0))
    (while (< beg end)
      (when (pel--docstring-char-invisible-p beg)
        (setq hide-count (1+ hide-count)))
      (setq beg (1+ beg)))
    (= hide-count 0)))

;;-pel-autoload
(defun pel-toggle-docstring (&optional next silent)
  "Toggle the visibility of the docstring.

By default it affects the current or previous definition,
but if the NEXT argument is non-nil it affects the next definition.

Return t on success.  If no docstring detected issue a user-error by default.
But if SILENT is non-nil, instead of issuing an error return nil instead."
  (interactive "P")
  (condition-case nil
      (pel-hide/show-docstring
       (if (pel--docstring-visible-p next) nil t)
       silent)
    (error (if silent
               nil
             (user-error "No docstring detected around point!")))))

;; --

;;-pel-autoload
(defun pel-hide/show-all-docstrings (&optional show)
  "Hide all docstrings in buffer.
With optional SHOW argument, show them all instead.
Display the number of docstrings affected.
The visibility of docstring is affected, but the buffer content is unchanged."
  (interactive "P")
  (with-silent-modifications
    (restore-buffer-modified-p nil)
    (save-excursion
      (let ((docstring-count 0))
        (goto-char (point-min))
        (while (progn
                 (when (pel-hide/show-docstring show :silent)
                   (setq docstring-count (1+ docstring-count)))
                 (pel-beginning-of-next-defun :silent :dont-push-mark)))
        (if (> docstring-count 0)
            (message "%s now %s."
                     (pel-count-string docstring-count "docstring")
                     (if show "displayed" "hidden"))
          (user-error "No docstring found in buffer."))))))

;; --

;;-pel-autoload
(defun pel-toggle-all-docstrings ()
  "Toggle visibility of all docstrings in buffer.
Display the number of docstrings affected.
The visibility of docstring is affected, but the buffer content is unchanged."
  (interactive)
  (with-silent-modifications
    (restore-buffer-modified-p nil)
    (save-excursion
      (let ((docstring-count 0))
        (goto-char (point-min))
        (while (progn
                 (when (pel-toggle-docstring nil :silent)
                   (setq docstring-count (1+ docstring-count)))
                 (pel-beginning-of-next-defun :silent :dont-push-mark)))
        (if (> docstring-count 0)
            (message "Toggled visibility of %s."
                     (pel-count-string docstring-count"docstring"))
          (user-error "No docstring found in buffer."))))))

;; -----------------------------------------------------------------------------
(provide 'pel-hide-docstring)

;;; pel-hide-docstring.el ends here
