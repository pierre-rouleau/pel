;;; pel-dtreplace-test.el --- ERT tests for pel-dtreplace  -*- lexical-binding: t; -*-

;; Created   : Sunday, March 22 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-22 12:32:57 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2026  Pierre Rouleau
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
;; ERT tests covering all testable functions in pel-dtreplace.el:
;;
;;   pel-find-replace              - core per-file text replacement
;;   pel--allow-descent-in         - forbidden-directory predicate
;;   pel--dt                       - recursive file listing with guards
;;   pel-dirtree-replace-undo      - undo / restore from backup
;;   pel-dt-fr-changed-files-in-dired - dired display, sort-fix guard
;;
;; Interactive commands that require a live minibuffer
;; (pel-dirtree-find-replace, pel-dt-fr-set-backup-suffix,
;;  pel-dt-fr-toggle-fixedcase, pel-dt-fr-toggle-literal)
;; are not covered by unit tests here, as they cannot be meaningfully
;; exercised without a live minibuffer.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-dtreplace)
(require 'ert)
(require 'cl-lib)
(require 'seq)     ; use: `seq-filter' (not autoloaded in Emacs 26)
;;; --------------------------------------------------------------------------
;;; Code:

;; ---------------------------------------------------------------------------
;; Helpers

(defmacro pel-dtr-with-temp-dir (dir-sym &rest body)
  "Evaluate BODY with DIR-SYM bound to a fresh temporary directory.
The directory and all its contents are removed after BODY completes,
even if BODY signals an error."
  (declare (indent 1))
  `(let ((,dir-sym (make-temp-file "pel-dtreplace-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,dir-sym t))))

(defmacro pel-dtr-with-clean-state (&rest body)
  "Evaluate BODY with pel-dirtree global state variables reset to nil.
Prevents cross-test contamination of `pel-dirtree-replaced-files',
`pel-dirtree-rootdir', and `pel--dt-old-version-warning-done'."
  (declare (indent 0))
  `(let ((pel-dirtree-replaced-files nil)
         (pel-dirtree-rootdir nil)
         (pel--dt-old-version-warning-done nil))
     ,@body))

;; ---------------------------------------------------------------------------
;; pel-find-replace — core per-file replacement

(ert-deftest pel-find-replace-basic-test ()
  "Replace all occurrences of a simple pattern in a file."
  (pel-dtr-with-temp-dir tmp
    (pel-dtr-with-clean-state
      (let* ((fname (expand-file-name "test.txt" tmp))
             (pel-dirtree-replace-files-is-verbose nil)
             (pel-dirtree-replace-file-newtext-is-fixedcase t)
             (pel-dirtree-replace-file-newtext-is-literal t)
             (pel-dirtree-replace-file-backup-suffix nil))
        (write-region "hello world\nhello emacs\n" nil fname)
        (pel-find-replace fname "hello" "goodbye")
        (with-temp-buffer
          (insert-file-contents fname)
          (should (string= (buffer-string) "goodbye world\ngoodbye emacs\n")))
        (should (member fname pel-dirtree-replaced-files))))))

(ert-deftest pel-find-replace-no-match-test ()
  "pel-find-replace leaves the file unchanged when the pattern is absent."
  (pel-dtr-with-temp-dir tmp
    (pel-dtr-with-clean-state
      (let* ((fname (expand-file-name "test.txt" tmp))
             (original "hello world\n")
             (pel-dirtree-replace-file-newtext-is-fixedcase t)
             (pel-dirtree-replace-file-newtext-is-literal   t)
             (pel-dirtree-replace-files-is-verbose nil)
             (pel-dirtree-replace-file-backup-suffix nil))
        (write-region original nil fname)
        (pel-find-replace fname "NOTPRESENT" "replacement")
        (with-temp-buffer
          (insert-file-contents fname)
          (should (string= (buffer-string) original)))
        ;; File must NOT be pushed to the replaced list
        (should-not (member fname pel-dirtree-replaced-files))))))

(ert-deftest pel-find-replace-multiple-replacements-test ()
  "pel-find-replace replaces every occurrence in the file."
  (pel-dtr-with-temp-dir tmp
    (pel-dtr-with-clean-state
      (let* ((fname (expand-file-name "multi.txt" tmp))
             (pel-dirtree-replace-files-is-verbose nil)
             (pel-dirtree-replace-file-newtext-is-fixedcase t)
             (pel-dirtree-replace-file-newtext-is-literal t)
             (pel-dirtree-replace-file-backup-suffix nil))
        (write-region "a a a a" nil fname)
        (pel-find-replace fname "a" "b")
        (with-temp-buffer
          (insert-file-contents fname)
          (should (string= (buffer-string) "b b b b")))))))

(ert-deftest pel-find-replace-backup-created-test ()
  "pel-find-replace creates a backup file when backup suffix is set."
  (pel-dtr-with-temp-dir tmp
    (pel-dtr-with-clean-state
      (let* ((fname  (expand-file-name "orig.txt" tmp))
             (suffix ".bak")
             (bkp    (format "%s%s" fname suffix))
             (pel-dirtree-replace-files-is-verbose nil)
             (pel-dirtree-replace-file-newtext-is-fixedcase t)
             (pel-dirtree-replace-file-newtext-is-literal t)
             (pel-dirtree-replace-file-backup-suffix suffix))
        (write-region "old content" nil fname)
        (pel-find-replace fname "old" "new")
        ;; Backup must contain original content
        (should (file-exists-p bkp))
        (with-temp-buffer
          (insert-file-contents bkp)
          (should (string= (buffer-string) "old content")))
        ;; Modified file must contain new content
        (with-temp-buffer
          (insert-file-contents fname)
          (should (string= (buffer-string) "new content")))))))

(ert-deftest pel-find-replace-no-backup-when-suffix-nil-test ()
  "pel-find-replace does not create a backup file when suffix is nil."
  (pel-dtr-with-temp-dir tmp
    (pel-dtr-with-clean-state
      (let* ((fname (expand-file-name "test.txt" tmp))
             (pel-dirtree-replace-files-is-verbose nil)
             (pel-dirtree-replace-file-newtext-is-fixedcase t)
             (pel-dirtree-replace-file-newtext-is-literal t)
             (pel-dirtree-replace-file-backup-suffix nil))
        (write-region "alpha beta" nil fname)
        (pel-find-replace fname "alpha" "gamma")
        ;; Only the single modified file should exist; no backup
        (let ((files (seq-filter #'file-regular-p (directory-files tmp t))))
          (should (= 1 (length files))))))))

(ert-deftest pel-find-replace-regexp-newtext-test ()
  "pel-find-replace with newtext-is-literal=nil interprets new-text as regexp."
  (pel-dtr-with-temp-dir tmp
    (pel-dtr-with-clean-state
      (let* ((fname (expand-file-name "test.txt" tmp))
             (pel-dirtree-replace-files-is-verbose nil)
             (pel-dirtree-replace-file-newtext-is-fixedcase t)
             (pel-dirtree-replace-file-newtext-is-literal nil)
             (pel-dirtree-replace-file-backup-suffix nil))
        ;; "foo(bar)" → back-reference \1 captures "bar" → "[bar]"
        (write-region "foo(bar)" nil fname)
        (pel-find-replace fname "foo(\\(.+\\))" "[\\1]")
        (with-temp-buffer
          (insert-file-contents fname)
          (should (string= (buffer-string) "[bar]")))))))

(ert-deftest pel-find-replace-case-adjusted-test ()
  "pel-find-replace with fixedcase=nil adjusts replacement capitalisation."
  (pel-dtr-with-temp-dir tmp
    (pel-dtr-with-clean-state
      (let* ((fname (expand-file-name "test.txt" tmp))
             (pel-dirtree-replace-files-is-verbose nil)
             (pel-dirtree-replace-file-newtext-is-fixedcase nil)
             (pel-dirtree-replace-file-newtext-is-literal t)
             (pel-dirtree-replace-file-backup-suffix nil))
        ;; "HELLO" is all-caps; with fixedcase=nil replacement is capitalised
        (write-region "HELLO world" nil fname)
        (pel-find-replace fname "HELLO" "goodbye")
        (with-temp-buffer
          (insert-file-contents fname)
          ;; Emacs replace-match with fixedcase=nil and all-caps match
          ;; upcases the replacement text ("goodbye" → "GOODBYE").
          (should (string= (buffer-string) "GOODBYE world")))))))

(ert-deftest pel-find-replace-accumulates-replaced-files-test ()
  "Multiple pel-find-replace calls accumulate entries in pel-dirtree-replaced-files."
  (pel-dtr-with-temp-dir tmp
    (pel-dtr-with-clean-state
      (let* ((f1 (expand-file-name "a.txt" tmp))
             (f2 (expand-file-name "b.txt" tmp))
             (pel-dirtree-replace-files-is-verbose nil)
             (pel-dirtree-replace-file-newtext-is-fixedcase t)
             (pel-dirtree-replace-file-newtext-is-literal t)
             (pel-dirtree-replace-file-backup-suffix nil))
        (write-region "foo" nil f1)
        (write-region "foo" nil f2)
        (pel-find-replace f1 "foo" "bar")
        (pel-find-replace f2 "foo" "bar")
        (should (= 2 (length pel-dirtree-replaced-files)))
        (should (member f1 pel-dirtree-replaced-files))
        (should (member f2 pel-dirtree-replaced-files))))))

;; ---------------------------------------------------------------------------
;; pel--allow-descent-in — forbidden-directory predicate

(ert-deftest pel--allow-descent-in-default-test ()
  "Default pattern /\\. forbids hidden directories."
  (let ((pel-dirtree-replace-file-forbidden-dir-re '("/\\.")))
    (should-not (pel--allow-descent-in "/some/path/.git"))
    (should-not (pel--allow-descent-in "/some/path/.hidden"))
    (should-not (pel--allow-descent-in "/project/.svn"))
    (should      (pel--allow-descent-in "/some/path/src"))
    (should      (pel--allow-descent-in "/project/lib"))
    (should      (pel--allow-descent-in "/home/user/documents"))))

(ert-deftest pel--allow-descent-in-custom-forbidden-test ()
  "Custom forbidden regexps block matching directories."
  (let ((pel-dirtree-replace-file-forbidden-dir-re '("node_modules" "vendor")))
    (should-not (pel--allow-descent-in "/project/node_modules"))
    (should-not (pel--allow-descent-in "/project/vendor"))
    (should      (pel--allow-descent-in "/project/src"))
    (should      (pel--allow-descent-in "/project/lib"))))

(ert-deftest pel--allow-descent-in-empty-list-test ()
  "An empty forbidden list allows every directory."
  (let ((pel-dirtree-replace-file-forbidden-dir-re nil))
    (should (pel--allow-descent-in "/any/.hidden"))
    (should (pel--allow-descent-in "/any/node_modules"))
    (should (pel--allow-descent-in "/"))))

(ert-deftest pel--allow-descent-in-multiple-patterns-test ()
  "First matching pattern in a multi-pattern list is enough to forbid."
  (let ((pel-dirtree-replace-file-forbidden-dir-re '("/\\." "build" "dist")))
    (should-not (pel--allow-descent-in "/proj/.git"))
    (should-not (pel--allow-descent-in "/proj/build"))
    (should-not (pel--allow-descent-in "/proj/dist"))
    (should      (pel--allow-descent-in "/proj/src"))))

;; ---------------------------------------------------------------------------
;; pel--dt — recursive file listing

(ert-deftest pel--dt-finds-matching-files-test ()
  "pel--dt returns files whose names match FN-RE anywhere under ROOT-DIR."
  (skip-unless (>= emacs-major-version 27)) ; 'pel--dt' does not support Emacs 26.
  (pel-dtr-with-temp-dir tmp
    (let ((pel-dirtree-replace-file-forbidden-dir-re '("/\\."))
          (pel--dirtree-allow-operation-in-forbidden-directories nil))
      (let* ((sub  (expand-file-name "sub" tmp))
             (f1   (expand-file-name "alpha.txt" tmp))
             (f2   (expand-file-name "beta.el"   tmp))
             (f3   (expand-file-name "gamma.txt" sub)))
        (make-directory sub)
        (write-region "1" nil f1)
        (write-region "2" nil f2)
        (write-region "3" nil f3)

        ;; .txt pattern: two hits
        (let ((result (pel--dt tmp "\\.txt\\'")))
          (should (= 2 (length result)))
          (should (member f1 result))
          (should (member f3 result))
          (should-not (member f2 result)))

        ;; .el pattern: one hit
        (let ((result (pel--dt tmp "\\.el\\'")))
          (should (= 1 (length result)))
          (should (member f2 result)))

        ;; Non-existent extension: empty result
        (let ((result (pel--dt tmp "\\.xyz\\'")))
          (should (null result)))))))

(ert-deftest pel--dt-respects-forbidden-dirs-test ()
  "pel--dt does not recurse into directories matched by the forbidden regexp."
  (skip-unless (>= emacs-major-version 27)) ; 'pel--dt' does not support Emacs 26.
  (pel-dtr-with-temp-dir tmp
    (let ((pel-dirtree-replace-file-forbidden-dir-re '("/\\."))
          (pel--dirtree-allow-operation-in-forbidden-directories nil))
      (let* ((hidden (expand-file-name ".git" tmp))
             (normal (expand-file-name "src"  tmp))
             (fh     (expand-file-name "secret.txt"  hidden))
             (fn     (expand-file-name "visible.txt" normal)))
        (make-directory hidden)
        (make-directory normal)
        (write-region "secret"  nil fh)
        (write-region "visible" nil fn)
        (let ((result (pel--dt tmp "\\.txt\\'")))
          (should (= 1 (length result)))
          (should (member fn result))
          (should-not (member fh result)))))))

(ert-deftest pel--dt-empty-directory-test ()
  "pel--dt returns nil for a directory with no matching files."
  (skip-unless (>= emacs-major-version 27)) ; 'pel--dt' does not support Emacs 26.
  (pel-dtr-with-temp-dir tmp
    (let ((pel-dirtree-replace-file-forbidden-dir-re '("/\\.")))
      (should (null (pel--dt tmp "\\.txt\\'"))))))

;; ---------------------------------------------------------------------------
;; pel-dirtree-replace-undo

(ert-deftest pel-dirtree-replace-undo-no-files-test ()
  "Signals user-error when pel-dirtree-replaced-files is nil."
  (let ((pel-dirtree-replaced-files nil)
        (pel-dirtree-replace-file-backup-suffix ".original"))
    (should-error (pel-dirtree-replace-undo :no-prompt)
                  :type 'user-error)))

(ert-deftest pel-dirtree-replace-undo-no-suffix-test ()
  "Signals user-error when backup suffix is nil (no backups were made)."
  (pel-dtr-with-temp-dir tmp
    (let* ((fname (expand-file-name "file.txt" tmp))
           (pel-dirtree-replaced-files (list fname))
           (pel-dirtree-replace-file-backup-suffix nil))
      (write-region "content" nil fname)
      (should-error (pel-dirtree-replace-undo :no-prompt)
                    :type 'user-error))))

(ert-deftest pel-dirtree-replace-undo-restores-single-file-test ()
  "Undo restores a single file from its backup and clears the replaced list."
  (pel-dtr-with-temp-dir tmp
    (pel-dtr-with-clean-state
      (let* ((fname  (expand-file-name "data.txt" tmp))
             (suffix ".original")
             (bkp    (format "%s%s" fname suffix))
             (pel-dirtree-replace-file-backup-suffix suffix)
             (pel-dirtree-replace-files-is-verbose nil))
        ;; Simulate a prior find-replace: modified file + backup
        (write-region "new content" nil fname)
        (write-region "old content" nil bkp)
        (setq pel-dirtree-replaced-files (list fname))

        (let ((result (pel-dirtree-replace-undo :no-prompt)))
          (should (listp result))
          (should (= 1 (car result)))
          (should (null (cadr result))))

        ;; File must now contain the original content
        (with-temp-buffer
          (insert-file-contents fname)
          (should (string= (buffer-string) "old content")))

        ;; Intermediate --backup temp must be gone
        (should-not (file-exists-p (format "%s--backup" fname)))

        ;; State must be cleared
        (should (null pel-dirtree-replaced-files))))))

(ert-deftest pel-dirtree-replace-undo-multiple-files-test ()
  "Undo restores multiple files and returns the correct count."
  (pel-dtr-with-temp-dir tmp
    (pel-dtr-with-clean-state
      (let* ((suffix ".bak")
             (pel-dirtree-replace-file-backup-suffix suffix)
             (pel-dirtree-replace-files-is-verbose nil)
             (files (mapcar (lambda (n)
                              (expand-file-name (format "file%d.txt" n) tmp))
                            '(1 2 3))))
        (dolist (f files)
          (write-region "new" nil f)
          (write-region "old" nil (format "%s%s" f suffix)))
        (setq pel-dirtree-replaced-files (copy-sequence files))

        (let ((result (pel-dirtree-replace-undo :no-prompt)))
          (should (= 3 (car result)))
          (should (null (cadr result))))

        (dolist (f files)
          (with-temp-buffer
            (insert-file-contents f)
            (should (string= (buffer-string) "old"))))

        (should (null pel-dirtree-replaced-files))))))

(ert-deftest pel-dirtree-replace-undo-missing-backup-test ()
  "Signals user-error and reports files whose backup is absent."
  (pel-dtr-with-temp-dir tmp
    (pel-dtr-with-clean-state
      (let* ((fname  (expand-file-name "nobackup.txt" tmp))
             (suffix ".bak")
             (pel-dirtree-replace-file-backup-suffix suffix)
             (pel-dirtree-replace-files-is-verbose nil))
        ;; Modified file exists but backup does NOT
        (write-region "modified" nil fname)
        (setq pel-dirtree-replaced-files (list fname))
        (should-error (pel-dirtree-replace-undo :no-prompt)
                      :type 'user-error)))))

;; ---------------------------------------------------------------------------
;; pel-dt-fr-changed-files-in-dired

(ert-deftest pel-dt-fr-changed-files-in-dired-no-files-test ()
  "Signals user-error when no files have been modified."
  (let ((pel-dirtree-replaced-files nil))
    (should-error (pel-dt-fr-changed-files-in-dired)
                  :type 'user-error)))

(ert-deftest pel-dt-fr-changed-files-in-dired-sort-test ()
  "The file list passed to dired must be sorted (guards the in-place sort bug).

In the original code `(sort fnames #'string<)' was called without
capturing the return value, so `fnames' remained unsorted.
After the fix (`(setq fnames (sort fnames #'string<))') the list
passed to `dired' must be in ascending string order."
  (pel-dtr-with-temp-dir tmp
    (pel-dtr-with-clean-state
      (let* ((f1 (expand-file-name "zebra.txt" tmp))
             (f2 (expand-file-name "alpha.txt" tmp))
             (f3 (expand-file-name "mango.txt" tmp))
             (pel-dirtree-rootdir tmp)
             (pel-dirtree-replace-file-backup-suffix nil)
             (captured nil))
        (dolist (f (list f1 f2 f3))
          (write-region "x" nil f))
        ;; Intentionally unsorted order to expose the bug
        (setq pel-dirtree-replaced-files (list f1 f2 f3))

        ;; Intercept the `dired' call to inspect its argument
        (cl-letf (((symbol-function 'dired)
                   (lambda (arg) (setq captured arg))))
          (pel-dt-fr-changed-files-in-dired))

        (let ((file-list (cdr captured)))
          (should (listp file-list))
          (should (equal file-list
                         (sort (copy-sequence file-list) #'string<))))))))

(ert-deftest pel-dt-fr-changed-files-in-dired-with-backups-test ()
  "Backup filenames are appended and the combined list is sorted."
  (pel-dtr-with-temp-dir tmp
    (pel-dtr-with-clean-state
      (let* ((f1     (expand-file-name "a.txt" tmp))
             (f2     (expand-file-name "b.txt" tmp))
             (suffix ".original")
             (pel-dirtree-rootdir tmp)
             (pel-dirtree-replace-file-backup-suffix suffix)
             (captured nil))
        (dolist (f (list f1 f2))
          (write-region "data" nil f)
          (write-region "orig" nil (format "%s%s" f suffix)))
        (setq pel-dirtree-replaced-files (list f1 f2))

        (cl-letf (((symbol-function 'dired)
                   (lambda (arg) (setq captured arg))))
          (pel-dt-fr-changed-files-in-dired))

        (let ((file-list (cdr captured)))
          ;; 2 original files + 2 backup files = 4 entries
          (should (= 4 (length file-list)))
          (should (member f1 file-list))
          (should (member f2 file-list))
          (should (member (format "%s%s" f1 suffix) file-list))
          (should (member (format "%s%s" f2 suffix) file-list))
          ;; Must be sorted
          (should (equal file-list
                         (sort (copy-sequence file-list) #'string<))))))))

(ert-deftest pel-dt-fr-changed-files-in-dired-dired-buffer-title-test ()
  "The dired buffer title includes the root directory name."
  (pel-dtr-with-temp-dir tmp
    (pel-dtr-with-clean-state
      (let* ((fname (expand-file-name "file.txt" tmp))
             (pel-dirtree-rootdir tmp)
             (pel-dirtree-replace-file-backup-suffix nil)
             (captured nil))
        (write-region "x" nil fname)
        (setq pel-dirtree-replaced-files (list fname))
        (cl-letf (((symbol-function 'dired)
                   (lambda (arg) (setq captured arg))))
          (pel-dt-fr-changed-files-in-dired))
        ;; car of the cons is the buffer title string
        (should (stringp (car captured)))
        (should (string-match-p (regexp-quote tmp) (car captured)))))))

;; ---------------------------------------------------------------------------
;; Integration: pel-find-replace + pel-dirtree-replace-undo roundtrip

(ert-deftest pel-find-replace-undo-roundtrip-test ()
  "Full roundtrip: find-replace followed by undo restores the original file."
  (pel-dtr-with-temp-dir tmp
    (pel-dtr-with-clean-state
      (let* ((fname  (expand-file-name "roundtrip.txt" tmp))
             (suffix ".bak")
             (pel-dirtree-replace-files-is-verbose nil)
             (pel-dirtree-replace-file-newtext-is-fixedcase t)
             (pel-dirtree-replace-file-newtext-is-literal t)
             (pel-dirtree-replace-file-backup-suffix suffix))
        (write-region "original content" nil fname)

        ;; Step 1: replace
        (pel-find-replace fname "original" "replaced")
        (with-temp-buffer
          (insert-file-contents fname)
          (should (string= (buffer-string) "replaced content")))
        (should (member fname pel-dirtree-replaced-files))

        ;; Step 2: undo
        (pel-dirtree-replace-undo :no-prompt)
        (with-temp-buffer
          (insert-file-contents fname)
          (should (string= (buffer-string) "original content")))
        (should (null pel-dirtree-replaced-files))))))

(ert-deftest pel-find-replace-tree-then-undo-test ()
  "Roundtrip across multiple files discovered by pel--dt."
  (skip-unless (>= emacs-major-version 27)) ; 'pel--dt' does not support Emacs 26.
  (pel-dtr-with-temp-dir tmp
    (pel-dtr-with-clean-state
      (let* ((sub    (expand-file-name "sub" tmp))
             (f1     (expand-file-name "one.txt"     tmp))
             (f2     (expand-file-name "two.txt"     sub))
             (suffix ".bak")
             (pel-dirtree-replace-files-is-verbose nil)
             (pel-dirtree-replace-file-newtext-is-fixedcase t)
             (pel-dirtree-replace-file-newtext-is-literal t)
             (pel-dirtree-replace-file-backup-suffix suffix)
             (pel-dirtree-replace-file-forbidden-dir-re '("/\\.")))
        (make-directory sub)
        (write-region "original text" nil f1)
        (write-region "original text" nil f2)

        ;; Replace in every .txt file under tmp
        (mapc (lambda (fname)
                (pel-find-replace fname "original" "modified"))
              (pel--dt tmp "\\.txt\\'"))

        ;; Both files must be modified
        (should (= 2 (length pel-dirtree-replaced-files)))
        (dolist (f (list f1 f2))
          (with-temp-buffer
            (insert-file-contents f)
            (should (string= (buffer-string) "modified text"))))

        ;; Undo restores both
        (pel-dirtree-replace-undo :no-prompt)
        (dolist (f (list f1 f2))
          (with-temp-buffer
            (insert-file-contents f)
            (should (string= (buffer-string) "original text"))))
        (should (null pel-dirtree-replaced-files))))))

;;; --------------------------------------------------------------------------
(provide 'pel-dtreplace-test)

;;; pel-dtreplace-test.el ends here
