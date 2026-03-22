;;; pel-filedir-test.el --- Test pel-filedir  -*- lexical-binding: t; -*-

;; Created   : Thursday, February 26 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-22 16:24:39 EDT, updated by Pierre Rouleau>

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
;;
;; ERT tests for all public and internal functions in pel-filedir.el.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel-filedir)                  ; tested file
(require 'pel--base)                    ; use `pel-system-is-macos-p'
(require 'ert)
(require 'subr-x)    ; use `string-join' (became autoloaded in Emacs 28.1)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; ---------------------------------------------------------------------------
;; Helper: temporary directory fixture

(defmacro pel-with-temp-dir (dir-sym &rest body)
  "Evaluate BODY with DIR-SYM bound to a fresh temporary directory.
The directory and all its contents are removed after BODY completes,
even if BODY signals an error."
  (declare (indent 1))
  `(let ((,dir-sym (make-temp-file "pel-filedir-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,dir-sym t))))

;; ---------------------------------------------------------------------------
;; pel-dpath-of

(ert-deftest pel-dpath-of-test ()
  "Test `pel-dpath-of'."

  ;; Without a list of sub-directories return normalized the parent directory
  (should (string= (pel-dpath-of ".") (expand-file-name ".")))
  (should-not (string= (pel-dpath-of ".") "."))

  ;; With a list of sub-directories, they are concatenated to the parent
  (let ((sep (substring (file-name-as-directory "a") -1))) ; for Unix: "/"
                                        ; for Windows: "\\"
    (let ((root-dir (format "%sroot" sep)))
      ;; The end of the string is not a directory separator
      (should (string= (pel-dpath-of root-dir)  root-dir))

      ;; The parent directory can end with or without a sep, it does not have
      ;; an impact on the returned string.
      ;; - without: /root
      (should (string= (pel-dpath-of root-dir "a" "b")
                       (string-join (list root-dir "a" "b") sep)))
      ;; - with: /root/
      (should (string= (pel-dpath-of (format "%s%s" root-dir sep) "a" "b")
                       (string-join (list root-dir "a" "b") sep)))

      ;; Three levels of sub-directories
      (should (string= (pel-dpath-of root-dir "x" "y" "z")
                       (string-join (list root-dir "x" "y" "z") sep))))))

;; ---------------------------------------------------------------------------
;; pel-file-in-dir

(ert-deftest pel-file-in-dir-test ()
  "Test `pel-file-in-dir'."
  (pel-with-temp-dir tmp
    (let ((file (expand-file-name "hello.txt" tmp)))
      (write-region "data" nil file)

      ;; File exists — dirpath without trailing slash
      (should (pel-file-in-dir "hello.txt" tmp))

      ;; File exists — dirpath with trailing slash
      (should (pel-file-in-dir "hello.txt" (file-name-as-directory tmp)))

      ;; File does not exist
      (should-not (pel-file-in-dir "no-such-file.txt" tmp))

      ;; Directory name itself is not a file
      (should-not (pel-file-in-dir "nonexistent" tmp)))))

;; ---------------------------------------------------------------------------
;; pel-dir-is-root

(ert-deftest pel-dir-is-root-test ()
  "Test `pel-dir-is-root'."
  (if pel-system-is-windows-p
      (progn
        (should (pel-dir-is-root "C:\\"))
        (should (pel-dir-is-root "c:/"))
        (should (pel-dir-is-root "D:\\"))
        (should-not (pel-dir-is-root "C:\\Users"))
        (should-not (pel-dir-is-root "C:\\Windows\\System32")))
    ;; Unix / macOS
    (should      (pel-dir-is-root "/"))
    (should-not  (pel-dir-is-root "/tmp"))
    (should-not  (pel-dir-is-root "/home/user"))
    (should-not  (pel-dir-is-root "."))
    (should-not  (pel-dir-is-root ""))))

;; ---------------------------------------------------------------------------
;; pel-parent-directory

(ert-deftest pel-parent-directory-test ()
  "Test `pel-parent-directory'."
  (unless pel-system-is-windows-p
    ;; Root has no parent
    (should-not (pel-parent-directory "/"))

    ;; /tmp's parent is the root
    (if pel-system-is-macos-p
        ;; On macOS /tmp → /private/tmp, so /tmp's true parent is /private/,
        ;; not /.  Assert that the parent of /private/ is the root instead.
        (let* ((real-tmp    (file-truename "/tmp"))
               (real-parent (pel-parent-directory real-tmp)))
          (should (pel-dir-is-root (pel-parent-directory (directory-file-name
                                                          real-parent)))))
      ;; on other Unix systems
      (should (string= (pel-parent-directory "/tmp") "/")))

    ;; Result always ends with a slash (Emacs directory-name convention)
    (pel-with-temp-dir tmp
      (let* ((sub (expand-file-name "child" tmp)))
        (make-directory sub)
        (let ((parent (pel-parent-directory sub)))
          (should (stringp parent))
          ;; Ends with separator
          (should (string= (substring parent -1)
                           (substring (file-name-as-directory "x") -1)))
          ;; Stripped parent equals tmp (resolved)
          (should (string= (directory-file-name parent)
                           (file-truename tmp))))))))

;; ---------------------------------------------------------------------------
;; pel-file-in-dir-upwards

(ert-deftest pel-file-in-dir-upwards-test ()
  "Test `pel-file-in-dir-upwards'."
  (pel-with-temp-dir tmp
    ;; Structure: tmp/a/b/c/
    (let* ((tmpdir   (file-truename tmp))
           (dir-a    (expand-file-name "a"   tmpdir))
           (dir-b    (expand-file-name "b"   dir-a))
           (dir-c    (expand-file-name "c"   dir-b))
           (marker   "MARKER.txt")
           (marker-fpath  (expand-file-name marker tmpdir)))

      (make-directory dir-c t)
      ;; Place marker only at tmp level
      (write-region "marker" nil (expand-file-name marker tmpdir))

      ;; Found when starting from dir-c (must walk up three levels)
      (let ((found (pel-file-in-dir-upwards marker dir-c)))
        (should (stringp found))
        (should (file-exists-p found))
        (should (string= found marker-fpath)))

      ;; Found when starting from dir-a (one level up)
      (let ((found (pel-file-in-dir-upwards marker dir-a)))
        (should (stringp found))
        (should (file-regular-p found))
        (should (file-exists-p found))
        (should (string= found marker-fpath)))

      ;; Found when starting from the containing directory itself
      (let ((found (pel-file-in-dir-upwards marker tmpdir)))
        (should (stringp found))
        (should (file-exists-p found))
        (should (string= found marker-fpath)))

      ;; Not found when file simply does not exist anywhere
      (should-not (pel-file-in-dir-upwards "DOES_NOT_EXIST" dir-c))

      ;; ROOT restriction: searching up from dir-c with ROOT = dir-a
      ;; marker is in tmp (above dir-a), so must return nil
      (should-not (pel-file-in-dir-upwards marker dir-c dir-a))

      ;; Place a second marker inside dir-b to confirm restricted search succeeds
      (write-region "inner" nil (expand-file-name marker dir-b))
      (let ((found (pel-file-in-dir-upwards marker dir-c dir-a)))
        (should (stringp found))
        (should (file-regular-p found))
        (should (string= found (expand-file-name marker dir-b)))
        (should (file-exists-p found))))))

;; ---------------------------------------------------------------------------
;; pel-symlink-is-relative-p  /  pel-symlink-is-absolute-p

(ert-deftest pel-symlink-is-relative-p-test ()
  "Test `pel-symlink-is-relative-p'."
  (unless pel-system-is-windows-p
    (pel-with-temp-dir tmp
      (let* ((target   (expand-file-name "target.txt" tmp))
             (rel-link (expand-file-name "rel-link"   tmp))
             (abs-link (expand-file-name "abs-link"   tmp)))
        (write-region "hello" nil target)
        ;; Relative symlink  ("target.txt")
        (make-symbolic-link "target.txt" rel-link)
        ;; Absolute symlink  ("/full/path/target.txt")
        (make-symbolic-link target abs-link)

        (should      (pel-symlink-is-relative-p rel-link))
        (should-not  (pel-symlink-is-absolute-p rel-link))

        (should      (pel-symlink-is-absolute-p abs-link))
        (should-not  (pel-symlink-is-relative-p abs-link))

        ;; Regular file — not a symlink at all
        (should-not  (pel-symlink-is-relative-p target))
        (should-not  (pel-symlink-is-absolute-p target))

        ;; Non-existent path
        (should-not  (pel-symlink-is-relative-p (expand-file-name "ghost" tmp)))
        (should-not  (pel-symlink-is-absolute-p (expand-file-name "ghost" tmp)))))))

;; ---------------------------------------------------------------------------
;; pel-symlink-broken-p

(ert-deftest pel-symlink-broken-p-test ()
  "Test `pel-symlink-broken-p'."
  (unless pel-system-is-windows-p
    (pel-with-temp-dir tmp
      (let* ((target       (expand-file-name "target.txt"   tmp))
             (good-link    (expand-file-name "good-link"    tmp))
             (broken-link  (expand-file-name "broken-link"  tmp))
             (regular      (expand-file-name "regular.txt"  tmp)))
        (write-region "content" nil target)
        ;; Good symlink (relative, target exists)
        (make-symbolic-link "target.txt"    good-link)
        ;; Broken symlink (relative, target missing)
        (make-symbolic-link "no-such-file"  broken-link)
        (write-region "data" nil regular)

        (should-not  (pel-symlink-broken-p good-link))
        (should      (pel-symlink-broken-p broken-link))
        ;; Regular file is not a symlink; must not be considered broken
        (should-not  (pel-symlink-broken-p regular))
        ;; Non-symlink, non-existent path
        (should-not  (pel-symlink-broken-p (expand-file-name "ghost" tmp)))))))

;; ---------------------------------------------------------------------------
;; pel-iter-directory-files

(ert-deftest pel-iter-directory-files-test ()
  "Test `pel-iter-directory-files' generator."
  (pel-with-temp-dir tmp
    ;; Structure:
    ;;   tmp/
    ;;     file1.txt
    ;;     file2.el
    ;;     sub/
    ;;       file3.txt
    (let* ((file1  (expand-file-name "zebra.txt" tmp))
           (file2  (expand-file-name "alpha.el"  tmp))
           (sub    (expand-file-name "sub"        tmp))
           (file3  (expand-file-name "mango.txt"  sub))
           (collect (lambda (it)
                      (let (acc)
                        (condition-case nil
                            (while t (push (iter-next it) acc))
                          (iter-end-of-sequence nil))
                        (reverse acc)))))
      (write-region "a" nil file1)
      (write-region "b" nil file2)
      (make-directory sub)
      (write-region "c" nil file3)

      ;; No filter: all 3 files
      (let ((results (funcall collect (pel-iter-directory-files tmp))))
        (should (= 3 (length results)))
        (should (member file1 results))
        (should (member file2 results))
        (should (member file3 results)))

      ;; Regexp filter: only .txt files (2 hits)
      (let ((results (funcall collect
                              (pel-iter-directory-files tmp "\\.txt\\'"))))
        (should (= 2 (length results)))
        (should (member file1 results))
        (should (member file3 results))
        (should-not (member file2 results)))

      ;; Predicate filter: only .el files
      (let ((results (funcall collect
                              (pel-iter-directory-files
                               tmp nil
                               (lambda (f)
                                 (string= (file-name-extension f) "el"))))))
        (should (= 1 (length results)))
        (should (member file2 results)))

      ;; Sorted flag: results are yielded in ascending order
      (let ((results (funcall collect
                              (pel-iter-directory-files tmp nil nil t))))
        ;; check that result is already sorted
        (should (equal results
                       (sort (list file1 file2 file3) #'string<))))

      ;; Empty directory yields no files
      (pel-with-temp-dir empty-tmp
        (let ((results (funcall collect (pel-iter-directory-files empty-tmp))))
          (should (null results)))))))

;; ---------------------------------------------------------------------------
;; pel-broken-symlinks

(ert-deftest pel-broken-symlinks-test ()
  "Test `pel-broken-symlinks'."
  (unless pel-system-is-windows-p
    (pel-with-temp-dir tmp
      (let* ((target   (expand-file-name "target.txt" tmp))
             (good     (expand-file-name "good-link"  tmp))
             (broken1  (expand-file-name "broken-a"   tmp))
             (broken2  (expand-file-name "broken-b"   tmp)))
        (write-region "hi" nil target)
        (make-symbolic-link "target.txt"  good)
        (make-symbolic-link "missing-1"   broken1)
        (make-symbolic-link "missing-2"   broken2)

        (let ((result (pel-broken-symlinks tmp)))
          ;; Exactly two broken symlinks
          (should (= 2 (length result)))
          (should (member broken1 result))
          (should (member broken2 result))
          (should-not (member good result))
          ;; Result is sorted ascending
          (should (equal result (sort (copy-sequence result) #'string<))))

        ;; Directory with no broken symlinks
        (pel-with-temp-dir clean-tmp
          (write-region "x" nil (expand-file-name "plain.txt" clean-tmp))
          (should (null (pel-broken-symlinks clean-tmp))))

        ;; Symlink to an existing directory must not be reported as broken
        (pel-with-temp-dir dir-tmp
          (let* ((dir-target (expand-file-name "realdir"   dir-tmp))
                 (dir-link   (expand-file-name "link-to-dir" dir-tmp)))
            (make-directory dir-target)
            (make-symbolic-link dir-target dir-link)
            (let ((result (pel-broken-symlinks dir-tmp)))
              (should (null result))
              (should-not (member dir-link result)))))))))

;; ---------------------------------------------------------------------------
;; pel-show-broken-symlinks

(ert-deftest pel-show-broken-symlinks-smoke-test ()
  "Smoke test: pel-show-broken-symlinks creates *Broken Symlinks* buffer
containing the paths of all broken symbolic links found."
  (unless pel-system-is-windows-p
    (pel-with-temp-dir tmp
      (let* ((target  (expand-file-name "target.txt" tmp))
             (broken1 (expand-file-name "broken-a"   tmp))
             (broken2 (expand-file-name "broken-b"   tmp)))
        (write-region "hi" nil target)
        (make-symbolic-link "missing-1" broken1)
        (make-symbolic-link "missing-2" broken2)

        ;; Start clean
        (when (get-buffer "*Broken Symlinks*")
          (kill-buffer "*Broken Symlinks*"))

        ;; Mock the directory prompt to return our temp directory
        (cl-letf (((symbol-function 'read-directory-name)
                   (lambda (&rest _) tmp)))
          (pel-show-broken-symlinks))

        ;; Buffer must exist and contain both broken symlink paths
        (let ((buf (get-buffer "*Broken Symlinks*")))
          (should buf)
          (with-current-buffer buf
            (should (string-match-p (regexp-quote broken1) (buffer-string)))
            (should (string-match-p (regexp-quote broken2) (buffer-string)))))

        ;; Clean up
        (when (get-buffer "*Broken Symlinks*")
          (kill-buffer "*Broken Symlinks*"))))))

(ert-deftest pel-show-broken-symlinks-no-links-test ()
  "When there are no broken symlinks the *Broken Symlinks* buffer is
not created (the `(when broken-links ...)' guard prevents it)."
  (ert-skip "Temporary disable test under development. Currently failing ")
  (unless pel-system-is-windows-p
    (pel-with-temp-dir tmp
      (write-region "content" nil (expand-file-name "file.txt" tmp))

      ;; Start clean
      (when (get-buffer "*Broken Symlinks*")
        (kill-buffer "*Broken Symlinks*"))

      (cl-letf (((symbol-function 'read-directory-name)
                 (lambda (&rest _) tmp)))
        (pel-show-broken-symlinks))

      ;; Buffer must NOT have been (re-)created
      (should-not (get-buffer "*Broken Symlinks*")))))

;; ---------------------------------------------------------------------------
;; pel--dirspec-for-dir-p  (internal helper)

(ert-deftest pel--dirspec-for-dir-p-test ()
  "Test `pel--dirspec-for-dir-p'.

DIRSPEC elements mirror the structure returned by
`directory-files-and-attributes': (NAME IS-DIR ...)."

  ;; Normal directory name — returns name
  (should (string= "mydir"       (pel--dirspec-for-dir-p '("mydir"       t))))
  (should (string= "another-dir" (pel--dirspec-for-dir-p '("another-dir" t))))

  ;; Starts with '.' — filtered out
  (should-not (pel--dirspec-for-dir-p '("."       t)))
  (should-not (pel--dirspec-for-dir-p '(".."      t)))
  (should-not (pel--dirspec-for-dir-p '(".hidden" t)))
  (should-not (pel--dirspec-for-dir-p '(".git"    t)))

  ;; Not a directory (cadr is nil) — filtered out
  (should-not (pel--dirspec-for-dir-p '("file.txt"  nil)))
  (should-not (pel--dirspec-for-dir-p '("README.md" nil))))

;; ---------------------------------------------------------------------------
;; pel-subdir-count

(ert-deftest pel-subdir-count-test ()
  "Test `pel-subdir-count'."
  (pel-with-temp-dir tmp
    ;; Initially no sub-directories
    (should (= 0 (pel-subdir-count tmp)))

    ;; Add two ordinary sub-directories
    (make-directory (expand-file-name "alpha" tmp))
    (make-directory (expand-file-name "beta"  tmp))
    (should (= 2 (pel-subdir-count tmp)))

    ;; Regular files must not be counted
    (write-region "data" nil (expand-file-name "file.txt" tmp))
    (should (= 2 (pel-subdir-count tmp)))

    ;; Hidden directories (start with '.') must not be counted
    (make-directory (expand-file-name ".cache" tmp))
    (should (= 2 (pel-subdir-count tmp)))

    ;; Add one more visible sub-directory
    (make-directory (expand-file-name "gamma" tmp))
    (should (= 3 (pel-subdir-count tmp)))

    ;; Symlink to a directory: behaviour depends on whether it is counted
    ;; On most systems directory-files-and-attributes follows the symlink
    ;; so a symlink to a dir is counted as a directory.
    (unless pel-system-is-windows-p
      (let ((link-target (expand-file-name "gamma" tmp)) ; already created above
            (dir-link    (expand-file-name "link-to-gamma" tmp)))
        (make-symbolic-link link-target dir-link)
        ;; A symlink to a visible directory is counted as one more subdir
        (should (= 4 (pel-subdir-count tmp)))))))

;; ---------------------------------------------------------------------------
;; pel-duplicate-dir

(ert-deftest pel-duplicate-dir-test ()
  "Test `pel-duplicate-dir'."
  (unless pel-system-is-windows-p
    (pel-with-temp-dir tmp
      (let* ((src      (expand-file-name "source" tmp))
             (dst      (expand-file-name "dest"   tmp))
             (file1    (expand-file-name "file1.txt" src))
             (file2    (expand-file-name "file2.txt" src))
             (sub      (expand-file-name "sub"        src))
             (subfile  (expand-file-name "sub/deep.txt" src))
             (rel-link (expand-file-name "rel-link"   src)))
        (make-directory sub t)
        (write-region "content1"   nil file1)
        (write-region "content2"   nil file2)
        (write-region "subcontent" nil subfile)
        ;; Relative symlink inside source
        (make-symbolic-link "file1.txt" rel-link)

        ;; ── basic copy ──────────────────────────────────────────────────
        (pel-duplicate-dir src dst)

        (should (file-directory-p dst))
        (should (file-exists-p (expand-file-name "file1.txt" dst)))
        (should (file-exists-p (expand-file-name "file2.txt" dst)))
        (should (file-directory-p (expand-file-name "sub" dst)))
        (should (file-exists-p (expand-file-name "sub/deep.txt" dst)))
        ;; confirm it actually resolves to an existing file from within dst
        (should (file-exists-p (file-truename (expand-file-name "rel-link" dst))))

        ;; Relative symlink must be preserved as relative
        (let ((dst-link (expand-file-name "rel-link" dst)))
          (should (file-symlink-p dst-link))
          (should (pel-symlink-is-relative-p dst-link)))

        ;; Emacs temp files (#...) are skipped
        (write-region "tmp" nil (expand-file-name "#auto-save#" src))
        (let ((dst2 (expand-file-name "dest2" tmp))) ; ← fresh destination
          (pel-duplicate-dir src dst2)
          (should-not (file-exists-p (expand-file-name "#auto-save#" dst2))))

        ;; ── error: destination is an existing symlink ───────────────────
        (let ((sym-dst (expand-file-name "sym-dst" tmp)))
          (make-symbolic-link src sym-dst)
          (should-error (pel-duplicate-dir src sym-dst)))

        ;; ── error: destination is an existing regular file ──────────────
        (let ((file-dst (expand-file-name "file-dst" tmp)))
          (write-region "x" nil file-dst)
          (should-error (pel-duplicate-dir src file-dst)))

        ;; Broken symlink in source is silently skipped (target missing)
        (let* ((src2 (expand-file-name "source2" tmp))
               (dst3 (expand-file-name "dest3"   tmp)))
          (make-directory src2)
          (make-symbolic-link "no-target" (expand-file-name "broken" src2))
          (let ((broken-copy (expand-file-name "broken" dst3)))
            (pel-duplicate-dir src2 dst3)
            (should (file-directory-p dst3))
            (should-not (file-symlink-p broken-copy))
            (should-not (file-exists-p broken-copy))))))))


;;; --------------------------------------------------------------------------
(provide 'pel-filedir-test)

;;; pel-filedir-test.el ends here
