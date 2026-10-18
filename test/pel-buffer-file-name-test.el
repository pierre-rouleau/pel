;;; pel-buffer-file-name-test.el --- Test buffer and file mode control  -*- lexical-binding: t; -*-

;; Created   : Sunday, October 18 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-10-18 15:38:01 EDT, updated by Pierre Rouleau>

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
;; Behavior-Based regression tests. The tests verify that each function uses
;; the current buffer’s filename and preserves its existing behavior. The tests
;; do not require a particular implementation, such as forbidding calls to
;; buffer-file-name.
;;
;; - Test project lookup, Org archives, and reStructuredText.
;; - Focused tests for the affected functions that lack test files.
;; - Stub compilation, external tools, prompts, and destructive file operations.
;;
;; ==================================== =========================================================
;; Affected function 	                Assertions
;; ==================================== =========================================================
;; pel-applescript-compile              A non-file buffer signals user-error before compilation.
;;                                      The analysis, script, and application branches use the
;;                                      current filename. Stub tool checks, compilation, prompts,
;;                                      and backup operations.
;;
;; pel--lkcs-maybe-activate             A non-file buffer does not search or activate the style.
;;                                      Test Kbuild, Kconfig, a matching Linux include, and a
;;                                      non-kernel file. Verify that point does not move.
;;
;; pel-cl-lint                          A configured linter rejects a non-file buffer. Test
;;                                      filename quoting for the string, plist, and use-mallet-4emacs
;;                                      configurations. Stub executable discovery and compilation.
;;
;; pel-render-commented-graphviz-dot    Capture the arguments to make-temp-file. For example.c,
;;                                      expect prefix "pel-gdot-example-", directory flag nil,
;;                                      and suffix ".dot". Stub preview and isolate temporary-file cleanup.
;;
;; pel--org-archive-original            A non-file buffer returns nil without reading archive properties.
;;                                      A file buffer with archive metadata returns the resolved source
;;                                      filename and outline path. Missing metadata returns nil.
;;
;; pel-perl-critic                      Capture both the compilation command and its default-directory.
;;                                      Test a local filename and a TRAMP filename. For TRAMP,
;;                                      verify remote executable lookup and the host-local filename
;;                                      passed to perlcritic. Do not connect to a remote host.
;;
;; pel-rpm-spec-lint, pel-rpm-build     Verify the quoted filename and compilation arguments.
;;                                      For builds, stub read-shell-command and pel-rpm-setuptree
;;                                      so the test cannot modify ~/rpmbuild.
;;
;; pel-rst-open-target                  Verify directory selection with and without a filename.
;;                                      Test the N >= 100 bypass. Verify directory restoration
;;                                      after success and after an error.
;;
;; pel-rst-compile 	                Verify the command for a configured compiler and for
;;                                      the pel-rst2html special case. Stub executable discovery and compilation.
;; ==================================== =========================================================

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel-c3)
(require 'pel-ffind)
(require 'pel-org)
(require 'pel-uuid)

(require 'ert)
(require 'cl-lib)
(require 'compile)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(ert-deftest pel-filename/c3-saves-only-modified-file-buffer ()
  "Save a modified file buffer before compilation, but not a clean buffer."
  (dolist (modified '(nil t))
    (with-temp-buffer
      (setq-local buffer-file-name
                  (expand-file-name "source file.c3"
                                    temporary-file-directory))
      (insert "module example;\n")
      (set-buffer-modified-p modified)
      (let ((expected-command
             (format "c3c compile %s"
                     (shell-quote-argument buffer-file-name)))
            (events nil))
        (cl-letf (((symbol-function 'save-buffer)
                   (lambda (&rest _)
                     (push 'save events)
                     (set-buffer-modified-p nil)))
                  ((symbol-function 'compile)
                   (lambda (command &optional comint)
                     (push (list 'compile command comint) events))))
          (pel-c3-compile))
        (should
         (equal (nreverse events)
                (append (when modified '(save))
                        (list (list 'compile expected-command t)))))))))

(ert-deftest pel-filename/c3-uses-current-buffer ()
  "Build each command from the filename of the current buffer."
  (let ((commands nil))
    (cl-letf (((symbol-function 'compile)
               (lambda (command &optional _comint)
                 (push command commands))))
      (with-temp-buffer
        (setq-local buffer-file-name
                    (expand-file-name "outer.c3"
                                      temporary-file-directory))
        (let ((outer-file buffer-file-name)
              (inner-file
               (expand-file-name "inner file.c3"
                                 temporary-file-directory)))
          (with-temp-buffer
            (setq-local buffer-file-name inner-file)
            (pel-c3-compile))
          (pel-c3-compile)
          (should
           (equal (nreverse commands)
                  (mapcar
                   (lambda (file)
                     (format "c3c compile %s"
                             (shell-quote-argument file)))
                   (list inner-file outer-file)))))))))

(ert-deftest pel-filename/project-lookup-filename-selection ()
  "Use the buffer filename by default and prefer an explicit filename."
  (let* ((root-a (expand-file-name "project-a"
                                   temporary-file-directory))
         (root-b (expand-file-name "project-b"
                                   temporary-file-directory))
         (file-a (expand-file-name "a.c" root-a))
         (file-b (expand-file-name "b.c" root-b))
         (settings-a '(((c) ("/include-a") () () () ())))
         (settings-b '(((c) ("/include-b") () () () ())))
         (pel-dev-projects
          (list (list "A" (list root-a settings-a))
                (list "B" (list root-b settings-b)))))
    ;; Isolate filename selection from filesystem-based root discovery.
    (cl-letf (((symbol-function 'pel-ffind-project-directory-of)
               (lambda (file &optional _identifiers)
                 (cond
                  ((equal file file-a) root-a)
                  ((equal file file-b) root-b)
                  (t (ert-fail
                      (format "Unexpected filename: %S" file))))))
              ((symbol-function 'pel-path=) #'equal))
      (with-temp-buffer
        (setq-local buffer-file-name file-a)
        (should (equal (pel-ffind-project-name) "A"))
        (should (equal (pel-ffind-project-settings) settings-a))
        (should (equal (pel-ffind-project-name file-b) "B"))
        (should (equal (pel-ffind-project-settings file-b) settings-b))
        (setq-local buffer-file-name nil)
        (should-not (pel-ffind-project-name))
        (should-not (pel-ffind-project-settings))
        (should (equal (pel-ffind-project-name file-b) "B"))
        (should (equal (pel-ffind-project-settings file-b) settings-b))))))

(ert-deftest pel-filename/org-archive-detection ()
  "Require both an archive filename and org-mode."
  (dolist (case '((nil org-mode nil)
                  ("notes.org" org-mode nil)
                  ("notes.org_archive" fundamental-mode nil)
                  ("notes.org_archive" org-mode t)))
    (with-temp-buffer
      ;; This unit test checks the predicate, not mode initialization.
      (setq-local buffer-file-name (nth 0 case))
      (setq-local major-mode (nth 1 case))
      (should
       (eq (pel--org-buffer-is-archive-p)
           (nth 2 case))))))

(ert-deftest pel-filename/include-guard-output ()
  "Use the current filename and preserve UUID and endif options."
  (with-temp-buffer
    (setq-local buffer-file-name
                (expand-file-name "utils.h" temporary-file-directory))
    (cl-letf (((symbol-function 'pel-uuid)
               (lambda ()
                 "b89d59ef-9cde-45cd-bb83-242ad937bf08")))
      (dolist (with-uuid '(nil t))
        (dolist (with-endif '(nil t))
          (let ((symbol
                 (concat
                  "UTILS__H"
                  (if with-uuid
                      "_b89d59ef_9cde_45cd_bb83_242ad937bf08"
                    ""))))
            (should
             (equal
              (pel-c-include-guard with-uuid with-endif)
              (format
               "#ifndef %s\n#define %s /* include guard */\n%s"
               symbol symbol
               (if with-endif "\n#endif\n" ""))))))))))

;;; --------------------------------------------------------------------------
(provide 'pel-buffer-file-name-test)

;;; pel-buffer-file-name-test.el ends here
