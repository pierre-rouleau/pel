;;; pel-imenu-ido.el --- Navigation over imenu symbols with Ido prompting.  -*- lexical-binding: nil; -*-

;; Original Authors : shjk, updated by Matt Keller and Vergard Oye
;; Evolution in PEL:  Pierre Rouleau
;; Time-stamp: <2021-02-03 15:50:17, updated by Pierre Rouleau>

;; This file is an evolution of the single pel-goto-symbol function
;; taken from https://www.emacswiki.org/emacs/ImenuMode#h5o-14
;; written by shjk (updated by MattKeller to handle overlays as “positions”;
;; updated by VegardOye (to set the mark before jumping).

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;;


;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;; The current (original) implementation uses dynamic binding and recursion.
;; The following forms prevent byte compiler warnings.
;; TODO: convert this code to semantic binding to be able to be able to run it
;;       under gccemacs.
(eval-when-compile
  (require 'cl-lib))                    ; use: cl-eval-when
(cl-eval-when 'compile (require 'ido   nil  :no-error))
(cl-eval-when 'compile (require 'imenu nil  :no-error))

(defvar imenu--index-alist)             ; prevent compiler warning
(defvar imenu--rescan-item)             ; prevent compiler warning
(defvar selected-symbol)                ; prevent compiler warning
(defvar symbol-names)                   ; prevent compiler warning
(defvar name-and-pos)                   ; prevent compiler warning

;;; --------------------------------------------------------------------------
;;; Code:
;;
;;-pel-autoload
(defun pel-goto-symbol (&optional symbol-list)
  "Prompt using Ido for imenu symbol and move point to it.

Refresh imenu and jump to a place in the buffer using Ido.
Supports all Ido flex and tab completion."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (pel-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ;;
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (pel-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

;;; --------------------------------------------------------------------------
(provide 'pel-imenu-ido)

;;; pel-imenu-ido.el ends here
