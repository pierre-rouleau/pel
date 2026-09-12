;;; pel-org-test.el --- ERT tests for pel-org.el  -*- lexical-binding: t; -*-

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for Org archive utilities in pel-org.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-archive)
(require 'pel-org)

(defmacro pel-org-test--with-temp-dir (directory &rest body)
  "Bind DIRECTORY to a temporary directory and execute BODY."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,directory (make-temp-file "pel-org-test-" t)))
     (unwind-protect
         (progn ,@body)
       ;; Kill buffers that visit files in the temporary directory.
       (dolist (buffer (buffer-list))
         (let ((file-name (buffer-file-name buffer)))
           (when (and file-name
                      (file-in-directory-p file-name ,directory))
             (kill-buffer buffer))))
       (ignore-errors (delete-directory ,directory t)))))

(defun pel-org-test--write-file (file-name content)
  "Write CONTENT to FILE-NAME."
  (with-temp-file file-name
    (insert content)))

(ert-deftest pel-org/archive-file-first-property/returns-first-value ()
  "`pel--org-archive-file-first-property' returns the first matching value."
  (with-temp-buffer
    (org-mode)
    (insert
     "* First\n"
     ":PROPERTIES:\n"
     ":ARCHIVE_FILE: /tmp/first.org\n"
     ":END:\n"
     "* Second\n"
     ":PROPERTIES:\n"
     ":ARCHIVE_FILE: /tmp/second.org\n"
     ":END:\n")
    (goto-char (point-min))
    (should
     (equal (pel--org-archive-file-first-property "ARCHIVE_FILE")
            "/tmp/first.org"))))

(ert-deftest pel-org/archive-file-first-property/returns-nil-when-absent ()
  "`pel--org-archive-file-first-property' returns nil when no property exists."
  (with-temp-buffer
    (org-mode)
    (insert "* No archive metadata\n")
    (should-not
     (pel--org-archive-file-first-property "ARCHIVE_FILE"))))

(ert-deftest pel-org/heading-and-pos/returns-leaf-and-position ()
  "`pel--org-heading-and-pos' finds a valid structural path."
  (with-temp-buffer
    (org-mode)
    (insert "* Project\n** Area\n*** Task\n")
    (let ((location (pel--org-heading-and-pos "Project/Area/Task")))
      (should (equal (car location) "Task"))
      (should (integer-or-marker-p (cdr location)))
      (goto-char (cdr location))
      (should (looking-at-p "\\*\\*\\* Task")))))

(ert-deftest pel-org/heading-and-pos/returns-nil-for-missing-path ()
  "`pel--org-heading-and-pos' returns nil for a missing structural path."
  (with-temp-buffer
    (org-mode)
    (insert "* Project\n** Area\n")
    (should-not
     (pel--org-heading-and-pos "Project/Unknown/Task"))))

(ert-deftest pel-org/clean-archive-properties/clears-only-during-restore ()
  "`pel--org-clean-archive-properties-on-refile' protects normal refiles."
  (with-temp-buffer
    (org-mode)
    (insert
     "* Archived task\n"
     ":PROPERTIES:\n"
     ":ARCHIVE_TIME: [2026-01-01 Thu]\n"
     ":ARCHIVE_FILE: /tmp/source.org\n"
     ":ARCHIVE_OLPATH: Project/Task\n"
     ":ARCHIVE_CATEGORY: source\n"
     ":ARCHIVE_TODO: DONE\n"
     ":ARCHIVE_ITAGS: :tag:\n"
     ":KEEP: yes\n"
     ":END:\n")
    (goto-char (point-min))
    (let ((pel-refile-is-archive-restore nil))
      (pel--org-clean-archive-properties-on-refile)
      (should (equal (org-entry-get nil "ARCHIVE_FILE")
                     "/tmp/source.org")))
    (let ((pel-refile-is-archive-restore t))
      (pel--org-clean-archive-properties-on-refile)
      (dolist (property '("ARCHIVE_TIME" "ARCHIVE_FILE" "ARCHIVE_OLPATH"
                          "ARCHIVE_CATEGORY" "ARCHIVE_TODO" "ARCHIVE_ITAGS"))
        (should-not (org-entry-get nil property)))
      (should (equal (org-entry-get nil "KEEP") "yes")))))

(ert-deftest pel-org/archive-preserve-hierarchy/creates-missing-parents ()
  "The advice creates the missing archive parent hierarchy."
  (pel-org-test--with-temp-dir directory
    (let* ((archive-file (expand-file-name "source.org_archive" directory))
           (source-file (expand-file-name "source.org" directory))
           captured-location)
      (pel-org-test--write-file archive-file "")
      (pel-org-test--write-file
       source-file
       "* Project\n** Area\n*** Task\n")
      (with-current-buffer (find-file-noselect source-file)
        (org-mode)
        (goto-char (point-min))
        (re-search-forward "^\\*\\*\\* Task$")
        (cl-letf (((symbol-function 'org-archive--compute-location)
                   (lambda (_location) (list archive-file))))
          (pel--org-archive-preserve-hierarchy-adv
           (lambda (&rest _args)
             (setq captured-location org-archive-location))
           nil)))
      (with-current-buffer (find-file-noselect archive-file)
        (should
         (string-match-p "^\\* Project\n\\*\\* Area\n"
                         (buffer-string))))
      (should (equal captured-location
                     (format "%s::** Area" archive-file))))))

(ert-deftest pel-org/archive-preserve-hierarchy/does-not-duplicate-parents ()
  "The advice retains an existing archive parent hierarchy."
  (pel-org-test--with-temp-dir directory
    (let* ((archive-file (expand-file-name "source.org_archive" directory))
           (source-file (expand-file-name "source.org" directory)))
      (pel-org-test--write-file
       archive-file
       "* Project\n** Area\n** Existing archived task\n")
      (pel-org-test--write-file
       source-file
       "* Project\n** Area\n*** Task\n")
      (with-current-buffer (find-file-noselect source-file)
        (org-mode)
        (goto-char (point-min))
        (re-search-forward "^\\*\\*\\* Task$")
        (cl-letf (((symbol-function 'org-archive--compute-location)
                   (lambda (_location) (list archive-file))))
          (pel--org-archive-preserve-hierarchy-adv
           (lambda (&rest _args) nil)
           nil)))
      (with-current-buffer (find-file-noselect archive-file)
        (goto-char (point-min))
        (should (= (how-many "^\\* Project$" (point-min) (point-max)) 1))
        (should (= (how-many "^\\*\\* Area$" (point-min) (point-max)) 1))))))

(provide 'pel-org-test)

;;; pel-org-test.el ends here
