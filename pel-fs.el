;;; pel-fs.el --- File System Operations  -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2021, 2022, 2023  Pierre Rouleau

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
;;
;; Calling external processes:
;; - `pel-exec-pel-bin'
;;   - `pel-check-pel-bin-cmd'
;;      - `pel-file-content-md5'
;;         - `pel-file-content'
;;      - `pel-bin-path'
;;   - `pel-exec-cmd'


;; -----------------------------------------------------------------------------
;; MD5 hash digests for pel/bin executable files
;; ---------------------------------------------
(defconst pel-bin-md5-alist
  '(("version-erl"     . "a6719d7cabaaca23516ebfae95fecc6b")
    ("erlang-root-dir" . "b87c653df5f8ce85fa3cd9aecc1e8bda"))
  "Maps a pel/bin file name to its MD5 hash.
These hash digests are used by `pel-exec-pel-bin' to ensure that the
file content was not modified.")

;; -----------------------------------------------------------------------------
;; Calling external processes
;; --------------------------

(defun pel-exec-cmd (cmd &rest args)
  "Execute synchronous process CMD with ARGS.

Return the list (exit-code stdout-string stderr-string).
- The stdout-string and stderr-string trailing newline are removed if present.
- If stdout or stderr is empty, an empty string is placed in the list."
  (condition-case err
      (let ((tmp-fname (make-temp-file "pel-exec-cmd"))
            exit-code
            stdout-string
            stderr-string)
        (unwind-protect
            (with-temp-buffer
              (setq exit-code
                    (apply 'call-process cmd nil
                           (list (current-buffer) tmp-fname) ; store stderr in tmp-fname
                           nil args))
              (setq stdout-string
                    (replace-regexp-in-string "\n\\'" "" (buffer-string)))
              (when (file-exists-p tmp-fname)
                (with-temp-buffer
                  (insert-file-contents tmp-fname)
                  (setq stderr-string
                        (replace-regexp-in-string "\n\\'" "" (buffer-string)))))
              (list exit-code
                    stdout-string
                    stderr-string))
          (and (file-exists-p tmp-fname)
               (delete-file tmp-fname))))
    (error (cons nil err))))


(defun pel-bin-path (&optional file-name)
  "Return the absolute path of the PEL's bin FILE-NAME or directory.
If FILE_NAME is specified return its full path name in PEL's bin,
otherwise return PEL's bin directory name only (with a trailing slash)."
  (let ((pel-bin (format "%sbin" (file-name-directory (locate-library "pel")))))
    (if file-name (format "%s/%s" pel-bin file-name)
      pel-bin)))

(defun pel-file-content (file-path)
  "Return the content of the file identified by FILE-PATH in a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun pel-file-content-md5 (file-path)
  "Return the MD5 hash of a the content of file FILE-PATH."
  (md5 (pel-file-content file-path)))

(defun pel-check-pel-bin-cmd (pel-bin-command)
  "Verify presence, validity and access of PEL-BIN-COMMAND.
The PEL-BIN-COMMAND must be a path-less filename that must be in the PEL's
bin directory.
Check if the file content corresponds to its original content
by checking the MD5 of its content against the on stored in `pel-bin-md5-alist'.
If the file is not executable, try to make it executable.
Verify that it has executable access and try to make it executable if
it's not.
Return t if it's OK, issue a user-error otherwise."
  (let ((fname (pel-bin-path pel-bin-command)))
    (if (file-exists-p fname)
        (if (string= (pel-file-content-md5 fname)
                     (cdr (assoc pel-bin-command pel-bin-md5-alist)))
            (if (file-executable-p fname)
                t
              ;; file is currently not executable
              ;; set the exec bit for owner, group, other
              (set-file-modes fname (logior (file-modes fname) #o111))
              (if (file-executable-p fname)
                  t
                (user-error "Error: can't make %s executable!" fname)))
          (user-error "Error: %s MD5 differs from pel-bin-md5-alist value!" fname))
      (user-error "Error: %s does not exist. Check your PEL installation!" fname))))

(defun pel-exec-pel-bin (cmd &rest args)
  "Execute a PEL bin executable CMD with ARGS.

CMD must not have any path an must be one of the commands provided in the PEL
bin directory.
located in the
The `pel-exec-pel-bin' checks if the file exists and was not tempered
with by checking its MD5 hash digest.
If the command file is invalid or does not exist, raise an error.
If the file is OK, make the file executable if it is not already
executable, then run the command and return the list (exit-code
stdout-string stderr-string).
- The stdout-string and stderr-string trailing newline are removed if present.
- If stdout or stderr is empty, an empty string is placed in the list."
  (when (pel-check-pel-bin-cmd cmd)
    (apply 'pel-exec-cmd (cons (pel-bin-path cmd) args))))

;; -----------------------------------------------------------------------------
(provide 'pel-fs)

;;; pel-fs.el ends here
