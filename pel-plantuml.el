;;; pel-plantuml.el --- PlantUML additional utilities

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
;; This file contains code that extends plantuml-mode support of PlantUML markup.
;;
;; The functions are:
;;
;; * pel-render-commented-plantuml
;;   - pel--plantuml-region
;;
;;
;; -----------------------------------------------------------------------------

;;; Code:

(defun pel--plantuml-region (&optional pos)
  "Return the position of the beginning and end PlantUML code block.
Search at POS if specified, otherwise search around point.
Include whole lines.
Return a (start . end) cons cell if found, otherwise return nil."
  (setq pos (or pos (point)))
  (save-excursion
    (let (beg end)
      (when (search-backward "@startuml" nil :noerror)
        (beginning-of-line 1)
        (setq beg (point))
        (when (search-forward "@enduml" nil :noerror)
          (end-of-line 1)
          (setq end (point))
          (cons beg end))))))

;;-pel-autoload
(defun pel-render-commented-plantuml (prefix &optional pos)
  "Render the PlantUML markup embedded in current mode comment.
Use region if identified otherwise use PlantUML block.
Uses prefix (as PREFIX) to choose where to display it:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer."
  (interactive "p")
  (if (and (require 'plantuml-mode nil :noerror)
           (fboundp 'plantuml-mode)
           (fboundp 'plantuml-preview-current-block))
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
          (let ((beg.end (pel--plantuml-region pos)))
            (setq beg (car beg.end))
            (setq end (cdr beg.end))))
        (setq text (buffer-substring-no-properties beg end))
        (let ((temp-buffer (generate-new-buffer " *temp-plantuml*")))
          (with-current-buffer temp-buffer
            (unwind-protect
                (progn
                  (insert text)
                  ;; Remove comments : TODO: won't work for comments like C
                  ;; if the beginning and end of the comment is not in buffer.
                  (setq comment-start f-comment-start)
                  (setq comment-end   f-comment-end)
                  (setq comment-continue f-comment-continue)
                  (uncomment-region (point-min) (point-max))
                  ;; activate PlantUML mode and render the diagram in other window
                  (plantuml-mode)
                  (goto-char (point-min))
                  (forward-line 2)
                  (plantuml-preview-current-block prefix))
              (and
               (buffer-name temp-buffer)
               (kill-buffer temp-buffer))))))
    (user-error "Requires missing plantuml-mode!")))

;; -----------------------------------------------------------------------------
(provide 'pel-plantuml)

;;; pel-plantuml.el ends here
