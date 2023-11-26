;;; pel-ibuffer.el --- Ibuffer utilities.  -*- lexical-binding: t; -*-

;; Created   : Friday, November 24 2023.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2023-11-25 18:38:35 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; This file is heavily based on code posted by jpkotta, October 29, 2015 on:
;; https://emacs.stackexchange.com/questions/10621/how-to-get-ibuffer-to-use-directory-tree-as-filter-groups/17731#17731
;; I cleaned it up a little, ensured it byte compiles cleanly and built an
;; interface for PEL.


;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'ibuffer)
(require 'ibuf-ext)                     ; for: `ibuffer-filtering-alist'
(require 'cl-lib)
;;; --------------------------------------------------------------------------
;;; Code:
;;

;; Define `ibuffer-do-sort-by-pathname', a ibuffer sorter that sorts by
;; directory.

(define-ibuffer-sorter pathname
  "Sort by pathname"
  (:description "path")
  (cl-flet ((get-pathname
             (data)
             (with-current-buffer (car data)
               (or buffer-file-name
                   (if (eq major-mode 'dired-mode)
                       (expand-file-name dired-directory))
                   ;; so that all non pathnames are at the end
                   ""))))
    (string< (get-pathname a) (get-pathname b))))

;; ---------------------------------------------------------------------------
;; Define a directory filter

(defun get-all-buffer-directories ()
  "Return a list of all directories that have at least one
       file being visited."
  (interactive)
  (let (lst)
    (dolist (e (sort (mapcar 'file-name-directory
                             (cl-remove-if-not 'identity
                                            (mapcar 'buffer-file-name
                                                    (buffer-list))))
                     'string<))
      (unless (string= (car lst) e)
        (setq lst (cons e lst))))
    lst))

(define-ibuffer-filter dirname
    "Toggle current view to buffers with in a directory DIRNAME."
  (:description "directory name"
                :reader
                (intern
                 (completing-read "Filter by directory: "
                                  (get-all-buffer-directories)
                                  'identity
                                  t nil nil nil nil)))
  (string= qualifier
           (and (buffer-file-name buf)
                (file-name-directory (buffer-file-name buf)))))


;; ---------------------------------------------------------------------------
;; Define a filter that groups per directory

(defun ibuffer-set-filter-groups-by-directory ()
  "Set the current filter groups to filter by directory."
  (interactive)
  (setq ibuffer-filter-groups
        (mapcar (lambda (dir)
                  (cons (format "%s" dir) `((dirname . ,dir))))
                (get-all-buffer-directories)))
  (ibuffer-update nil t))


;; ---------------------------------------------------------------------------

(defun pel-map-ibuffer-mode-filters ()
  "Map the keys of extra ibuffer filters."

  (define-key ibuffer-mode-map (kbd "s p")   'ibuffer-do-sort-by-pathname)
  (define-key ibuffer-mode-map (kbd "/ D")   'ibuffer-set-filter-groups-by-directory)
  (define-key ibuffer-mode-map (kbd "/ d")   'ibuffer-filter-by-dirname)
  (define-key ibuffer-mode-map (kbd "/ M-d") 'ibuffer-decompose-filter))

;;; --------------------------------------------------------------------------
(provide 'pel-ibuffer)

;;; pel-ibuffer.el ends here
