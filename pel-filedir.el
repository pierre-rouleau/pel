;;; pel-filedir.el --- File, Directory and Dirpath Management  -*- lexical-binding: t; -*-

;; Created   : Thursday, February 25 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-02-02 22:26:07 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021  Pierre Rouleau
;;
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

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; A collection of functions that manipulate file system file names and
;; directories.
;;
;;
;; The calling hierarchy of the provided functions follow:
;;
;; - `pel-file-in-dir'
;; - `pel-file-in-dir-upwards'
;;    - `pel-dir-is-root'
;;    - `pel-parent-directory'
;; - `pel-symlink-is-relative-p'
;; - `pel-symlink-is-absolute-p'
;; * `pel-show-broken-symlinks'
;;   - `pel-broken-symlinks'
;;     - `pel-iter-directory-files'
;;     - `pel-symlink-broken-p'
;; - `pel-duplicate-dir'
;; - `pel-subdir-count'
;;   - `pel--dirspec-for-dir-p'
;;
;; See Emacs conventions:
;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Names.html
;;
;; - Directory:
;;   - directory file name: does not end with a slash. eg.: "~/dev/elisp/pel"
;;   - directory name: ends with a slash.              eg.: "~/dev/elisp/pel/"
;;
;; Argument name conventions:
;;
;; - filename  : Name of a file, no directory path, no slash.
;; - filepath  : Absolute path of a file.  Has a dirpath, a slash, a filename.
;; - dirpath   : Directory relative or absolute path.
;;               Optionally ends with slash.
;;               May use special symbols like "~", "." and ".."
;; - dirname   : Emacs conventional directory name that ends wit ha slash.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)    ; use pel-system-is-windows-p
(require 'generator)    ; use `iter-defun', `iter-yield-from', `iter-yield',
;;                      ;     `iter-end-of-sequence'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-file-in-dir (filename dirpath)
  "Return t if FILENAME is present in directory DIRPATH, nil otherwise.
DIRPATH can be a dirpath name (ending with separator)
or a dirpath file name (not ending with separator)."
  (file-exists-p (expand-file-name filename dirpath)))

(defun pel-dir-is-root (dirpath)
  "Return t is DIRPATH is the root of the file-system."
  (if pel-system-is-windows-p
      (not (null (string-match "\\([a-zA-Z]:\\)?[/\\]\\'"  dirpath)))
    (string= dirpath "/")))

(defun pel-parent-directory (dirpath)
  "Return the absolute path of DIRPATH's parent directory.

Return a directory name, which by Emacs convention, always ends
with a slash.  Return nil if DIRPATH is already the root."
  (unless (pel-dir-is-root dirpath)
    (file-name-directory
     (directory-file-name
      (file-name-as-directory
       (file-truename dirpath))))))

(defun pel-file-in-dir-upwards (filename dirpath &optional root)
  "Return path of FILENAME found in directory DIRPATH or its parents.

Return nil if FILENAME is not present in any of the directories.
By default search in current directory and all its parents up to
the file system's root unless another ROOT is specified.

If ROOT is specified it must be a parent of the directory identified by
dirpath, otherwise the function generates an error.

If FILE is not found in DIRPATH, the parent of DIRPATH is searched."
  (let* ((dirpath  (file-truename dirpath))
         (root     (and root (file-name-as-directory (file-truename root))))
         (filepath (expand-file-name filename dirpath))
         file-found)
    (while (and (not (setq file-found (file-exists-p filepath)))
                (not (pel-dir-is-root dirpath))
                (not (and root (string= dirpath root))))
      (setq dirpath (pel-parent-directory dirpath))
      (setq filepath (expand-file-name filename dirpath)))
    (when file-found
      filepath)))

;; --
(defun pel-symlink-is-relative-p (link-path)
  "Return t if a LINK-PATH symlink is a relative symlink, nil otherwise."
  (let ((target (file-symlink-p link-path)))
    (and target
         (not (file-name-absolute-p target)))))

(defun pel-symlink-is-absolute-p (link-path)
  "Return t if a LINK-PATH symlink is an absolute symlink, nil otherwise."
  (let ((target (file-symlink-p link-path)))
    (and target
         (file-name-absolute-p target))))


;; --
(defun pel-symlink-broken-p (fpath)
  "Return t if FPATH is a broken symlink, nil otherwise."
  (let ((target (file-symlink-p fpath)))
    (when target
      (not (file-exists-p
            (if (file-name-absolute-p target)
                target
              (expand-file-name target (file-name-directory fpath))))))))


(iter-defun pel-iter-directory-files (directory &optional regexp pred sorted)
  "Generator.  Yield absolute file names in DIRECTORY matching REGEXP and PRED.
REGEXP is a file name regular expression. If REGEXP is nil, yield all file
names.
PRED is a predicate function that accepts the file name and return non-nil if
the file must be included.
If SORTED is non-nil the files in each directory are given in sorting order."
  (let ((fpaths (directory-files directory t nil t))) ; Get absolute names
    (when sorted
      (sort fpaths #'string<))    ; Sort in place, support Emacs 26 and later.
    (dolist (fpath fpaths)
      (let ((base (file-name-nondirectory fpath)))
        (unless (member base '("." ".."))
          (if (file-directory-p fpath)
              ;; Recursively yield from sub-directories
              (iter-yield-from (pel-iter-directory-files fpath regexp pred sorted))
            ;; Yield fpath if it matches regexp and the predicate
            (when (and (or (null regexp)
                           (string-match-p regexp base))
                       (or (null pred)
                           (funcall pred fpath)))
              (iter-yield fpath))))))))

(defun pel-broken-symlinks (dirpath)
  "Return a list of broken symlinks inside the DIRPATH directory tree."
  (let ((broken-links nil)
        (it (pel-iter-directory-files dirpath nil #'pel-symlink-broken-p)))
    (condition-case nil
        (while t
          (push (iter-next it) broken-links))
      (iter-end-of-sequence nil))
    (sort broken-links #'string<)))

;;-pel-autoload
(defun pel-show-broken-symlinks ()
  "Find and list broken symbolic links in directory tree.
Prompts for a directory and list found broken symlinks in the special
*Broken Symlinks* buffer."
  (interactive)
  (let* ((dirpath (read-directory-name "Search broken symlinks in: "
                                       nil
                                       nil
                                       t
                                       nil))
         (buffer (get-buffer-create "*Broken Symlinks*"))
         (broken-links (pel-broken-symlinks dirpath))
         (title (pel-count-string (length broken-links) "broken symlink")))
    (when broken-links
      (with-current-buffer buffer
        (erase-buffer)
        (pel-insert-bold
         (format "At %s, found %s in %s:\n"
                 (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))
                 title
                 (expand-file-name dirpath)))
        (dolist (fname (nreverse broken-links))
          (insert (format "%s\t-> %s\n" fname (file-symlink-p fname))))
        (display-buffer buffer)))
    (message "Found %s." title)))


;; --
(defun pel-duplicate-dir (source destination &optional keep-time
                                 preserve-uid-gid preserve-permissions)
  "Copy all files of SOURCE directory into DESTINATION directory.
Reproduce the symbolic links in the copy: if the original symlinks are
relative, the symlinks in the copied directory are also relative (to the
copy).

- KEEP-TIME non-nil means give the destination files have the same
  last-modified time as the original ones.  (This works on only some systems.)
- If PRESERVE-UID-GID is non-nil, try to transfer the uid and gid of
  files from SOURCE directory tree to DESTINATION directory tree.
- If PRESERVE-PERMISSIONS is non-nil, copy permissions of files
  from SOURCE directory tree to DESTINATION directory tree;
  this includes the file modes, along with ACL entries and SELinux
  context if present.  Otherwise, the file permission bits of the copied files
  are those of the original files, masked by the default file permissions."
  (let (source-fn destination-fn)
    ;; First create destination directory
    ;; Directory: create peer if possible and needed.
    (let ((destination-fn (expand-file-name destination)))
      (cond
       ((and (file-exists-p destination-fn)
             (file-symlink-p destination-fn))
        (error "A destination target but is a symlink: %s" destination-fn))
       ((and (file-exists-p destination-fn)
             (not (file-directory-p destination-fn)))
        (error "A destination target but is a file: %s" destination-fn))
       ((not (file-exists-p destination-fn))
        (make-directory destination-fn))))

    ;; Then copy the directory content
    (dolist (file-name (directory-files source))
      (setq source-fn (expand-file-name file-name source))
      (setq destination-fn (expand-file-name file-name destination))
      (cond
       ;; Do no copy Emacs temporary files  (files with a name that starts
       ;; with #
       ((pel-string-starts-with-p file-name "#")
        nil)
       ;;
       ;; skip ". and ".." "
       ((member file-name '("." ".."))
        nil)
       ;;
       ;; A directory: recurse copy it
       ((file-directory-p source-fn)
        (pel-duplicate-dir source-fn destination-fn
                           keep-time
                           preserve-uid-gid
                           preserve-permissions))
       ;;
       ;; A symlink: copy it keeping it relative or absolute as the original
       ((file-symlink-p source-fn)
        (when (file-regular-p (file-truename source-fn))
          (make-symbolic-link
           (file-symlink-p source-fn) ; immediate target (relative or absolute)
           destination-fn)))
       ;;
       ;; a file: copy it
       (t (when (file-regular-p source-fn)
            (copy-file source-fn destination-fn nil keep-time
                       preserve-uid-gid
                       preserve-permissions)))))))

;; --

(defun pel--dirspec-for-dir-p (dirspec)
  "Return dirname when DIRSPEC is for a Elpa package directory, nil otherwise.
DIRSPEC is the data structure returned by `directory-files-and-attributes'.
Exclude the directory entries that start with a period."
  (when (and (cadr dirspec)                          ; is a directory that
             (not (eq (string-to-char (car dirspec)) ; doesn't start with '.'
                      ?.)))
    (car dirspec)))

(defun pel-subdir-count (dir-path-name)
  "Return number of sub-directories of DIR-PATH-NAME directory."
  (length (mapcar #'car (seq-filter
                         (function pel--dirspec-for-dir-p)
                         (directory-files-and-attributes dir-path-name)))))

;;; --------------------------------------------------------------------------
(provide 'pel-filedir)

;;; pel-filedir.el ends here
