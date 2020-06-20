;;; pel-hide-docstring.el --- Hide docstrings.  -*- lexical-binding: t; -*-

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

;;; Commentary:
;;
;; This provides the ability to hide and show again the docstring the current,
;; previous or next definition.
;;
;; When hiding the docstring, the code leaves the opening and end quotes visible
;; as well as the first 4 characters.  It does not hide docstrings with less
;; than 5 characters.
;;
;;  LIMITATIONS:  work in progress, naïve implementation, enhancements required.
;;     - Only partially supports Emacs Lisp code.
;;     - Does not always recognize the true docstring if another string is found
;;       before the docstring.  Will have to check if face is 'font-lock-doc-face.
;;     - Only supports handling one definition, not a region or entire buffer.
;;     - No visual feedback or prevention of hidden docstring modification.
;;     - Does not properly handle multiple hidings followed with a sub-set of
;;       those shown back in presence of other hiding mechanisms (like hiding
;;       comments).
;;
;;  Despite those current limitations, it is useful.  Be careful when using it.
;;  I am planning to submit enhancements soon to handle most of the points
;;  described in the limitations.
;;

;;; Code:
;;
(require 'pel-navigate)

(defun pel--docstring-char-invisible-p (pos)
  "Return t if character at POS is an invisible docstring, nil otherwise.
Therefore nil might be because the character does not have the invisible
attribute or because it has the invisible attribute but its value is not
'hide-docstring."
  (eq (get-text-property pos 'invisible) 'hide-docstring))

(defsubst pel--hide/show-docstring-chars (start-pos end-pos show)
  "Hide or show docstring characters from START-POS to END-POS inclusively.
Hide when SHOW is nil.
Show when SHOW is non-nil."
  (put-text-property start-pos end-pos
                     'invisible
                     (if show nil 'hide-docstring)))


(defun pel--docstring-positions (next &optional limit)
  "Identify the start and end position of definition's docstring.

Identify the position of the next definition docstring when NEXT
is non-nil otherwise check the docstring positions of the current or previous
definition.

Optional LIMIT is maximum end position, not specified end of buffer
is used instead.
Does not move point.
Return cons cell of (start . end) positions if found, nil otherwise."
  (save-excursion
    (when (if next
              (pel-beginning-of-next-defun :silent :dont-push-mark)
            (beginning-of-defun 1))
      (let (beg-pos end-pos)
        ;; saerch for entry quote : must start at beginning of line
        (setq beg-pos (re-search-forward "^ +\\\"" (or limit (point-max)) :noerror 1))
        (when beg-pos
          ;; search next quote that is not escaped
          (setq end-pos (re-search-forward "[^\\\"]\\\""
                                           (or limit (point-max)) :noerror 1))
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

(defun pel--hide/show-docstring (show next &optional limit)
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
            (pel--hide/show-docstring-chars beg end show)
          ;; request to hide docstring
          (if (> (- end beg) 5)
              (progn
                (setq beg (+ beg 4))
                (setq end (1- end))
                ;; hide docstring, leave the quotes and 4 characters visible
                (pel--hide/show-docstring-chars beg end nil))
            (user-error "The docstring at %d,%d is too small (%s), kept visible"
                        beg
                        end
                        (buffer-substring (1- beg) end))))
      beg.end))))

;;-pel-autoload
(defun pel-hide/show-docstring (&optional show)
  "Hide or show the docstring.
Hide the docstring by default.  If SHOW is non-nil show it instead."
  (interactive "P")
  ;; (let ((start-point (if (region-active-p) (region-beginning) (point-min)))
  ;;       (end-point   (if (region-active-p) (region-end) (point-max))))
  ;; Hiding/showing docstrings is not a buffer modification.
  (with-silent-modifications
    (restore-buffer-modified-p nil)
    (pel--hide/show-docstring show nil nil)))

;;-pel-autoload
(defun pel-hide/show-docstring-toggle (&optional next)
  "Toggle the visibility of the docstring.
By default it affects the current or previous definition,
but if the NEXT argument is non-nil it affects the next definition."
  (interactive "P")
  (pel-hide/show-docstring (if (pel--docstring-visible-p next) nil t)))

;; -----------------------------------------------------------------------------
(provide 'pel-hide-docstring)

;;; pel-hide-docstring.el ends here
