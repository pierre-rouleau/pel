;;; pel-file-test.el --- pel-file Emacs Regression Test -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2026  Pierre Rouleau

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

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; Currently the test code only verifies the extraction of the file name, line
;; number and column numbers, nothing else.  The behaviour with respect of window
;; selection is not yet tested.

(require 'pel-file)                     ; tested code file.
(require 'pel-ert)

;;; --------------------------------------------------------------------------
;;; Code:

(defconst ok-url-strings '( "https://www.gnu.org/"
                            "https://github.com/pierre-rouleau/pel"))

(ert-deftest ert-test-pel-file-extract-url ()
  (dolist (url ok-url-strings)
    (with-temp-buffer
      (insert url)
      (goto-char (point-min))
      (should (string= (pel-filename-at-point) url)))))

;; ----

(defconst fn-example-strings
;;    tested pattern                   | expected results
;;    fnlc                             |---- ok-fn-spec --------------------------------|
;;    fnlc                             ok-kind      ok-fn                           ok-line ok-column
  '(( "a-simple-file-name.txt"        (fname        "a-simple-file-name.txt"        nil nil))
    ( "a-simple-file-name.txt:"       (fname        "a-simple-file-name.txt"        nil nil))
    ( "/abc/def/ghi/jkl/abc.txt"      (fname        "/abc/def/ghi/jkl/abc.txt"      nil nil))
    ( "/abc/def/ghi/jkl/abc.txt:"     (fname        "/abc/def/ghi/jkl/abc.txt"      nil nil))
    ( "/abc/def/ghi/jkl/abc.txt   "   (fname        "/abc/def/ghi/jkl/abc.txt"      nil nil))
    ( "/abc/def/ghi/jkl/abc.txt:  "   (fname        "/abc/def/ghi/jkl/abc.txt"      nil nil))
    ( "\\abc\\def\\ghi\\jkl\\abc.txt" (fname        "\\abc\\def\\ghi\\jkl\\abc.txt" nil nil))
    ( "\\abc\\def\\ghi\\jkl\\abc.txt:" (fname       "\\abc\\def\\ghi\\jkl\\abc.txt" nil nil))
    ( "Où-êtes-vous_tous?"            (fname        "Où-êtes-vous_tous?"            nil nil))
    ( "Où-êtes-vous_tous?:"           (fname        "Où-êtes-vous_tous?"            nil nil))
    ( "Ici!"                          (fname        "Ici!"                          nil nil))
    ( "Ici!:"                         (fname        "Ici!"                          nil nil))
    ( "Ici!        "                  (fname        "Ici!"                          nil nil))
    ( "C:\\Windows\\files\\c.cpp"     (fname-w-ddrv "C:\\Windows\\files\\c.cpp"     nil nil))
    ( "C:\\Windows\\files\\c.cpp:"    (fname-w-ddrv "C:\\Windows\\files\\c.cpp"     nil nil))
    ( "C:\\Windows\\files\\c.cpp  "   (fname-w-ddrv "C:\\Windows\\files\\c.cpp"     nil nil))
    ( "C:/Windows/files/c.cpp"        (fname-w-ddrv "C:/Windows/files/c.cpp"        nil nil))
    ( "C:/Windows/files/c.cpp:"       (fname-w-ddrv "C:/Windows/files/c.cpp"        nil nil))
    ( "C:/Windows/files/c.cpp  "      (fname-w-ddrv "C:/Windows/files/c.cpp"        nil nil))
    ( "C:/Windows/files/c.cpp:  "     (fname-w-ddrv "C:/Windows/files/c.cpp"        nil nil))
    ( "/usr/me/src/pel/pel.el"        (fname        "/usr/me/src/pel/pel.el"        nil nil))
    ( "\\usr\\me\\src\\pel\\pel.el"   (fname        "\\usr\\me\\src\\pel\\pel.el"   nil nil))
    ( "\\usr\\me\\src\\pel\\pel.el:"  (fname        "\\usr\\me\\src\\pel\\pel.el"   nil nil))
    ( "\\usr/me/src/pel/pel.el"       (fname        "\\usr/me/src/pel/pel.el"       nil nil))
    ( "\\usr/me/src/pel/pel.el:"      (fname        "\\usr/me/src/pel/pel.el"       nil nil))
    ( "/abc/def/"                     (fname        "/abc/def/"                     nil nil))
    ( "/abc/def/:"                    (fname        "/abc/def/"                     nil nil))
    ( "./pel.el"                      (fname        "./pel.el"                      nil nil))
    ( "./pelo.el:1:2"                 (fname        "./pelo.el"                     1 2))
    ( "./pel.el:1001:4"               (fname        "./pel.el"                      1001 4))
    ( "./pel.el:1001:4"               (fname        "./pel.el"                      1001 4))
    ( "./pel.el@1001:4"               (fname        "./pel.el"                      1001 4))
    ( "./pel.el:3"                    (fname        "./pel.el"                      3 nil))
    ( "./pel.el:123456:987654"        (fname        "./pel.el"                      123456 987654))
    ( "../dev/pel.el"                 (fname        "../dev/pel.el"                 nil nil))
    ( "~/src/pel/pel.cpp"             (fname        "~/src/pel/pel.cpp"             nil nil))
    ( "~/../other/src/python/d.py"    (fname        "~/../other/src/python/d.py"    nil nil))
    ( "../dev/pel.el:"                (fname        "../dev/pel.el"                 nil nil))
    ( "~/src/pel/pel.cpp:"            (fname        "~/src/pel/pel.cpp"             nil nil))
    ( "~/../other/src/python/d.py:"   (fname        "~/../other/src/python/d.py"    nil nil))))

(ert-deftest ert-test-pel-file-extract-filename ()
  (dolist (test-scenario fn-example-strings)    ; test all file name patterns listed above.
    (let* ((fnlc (car test-scenario))
           (ok-fn-spec (cadr test-scenario))
           ;;(ok-kind    (car ok-fn-spec))
           (ok-fn      (cadr ok-fn-spec))
           ;;(ok-line    (caddr ok-fn-spec))
           ;;(ok-column  (cadddr ok-fn-spec))
           )
      (with-temp-buffer
        ;; test extraction of a file name under different scenarios:
        ;; - 1:  with nothing after the pattern
        ;; - 2:  with white-spaces and some other ASCII characters
        ;;       that may follow the file name.
        (dolist (extra-chars '(""
                               " "
                               "  "
                               "   "
                               "\t"  "\n"
                               "  ."
                               " )"
                               ")" "("
                               "]" "["
                               "}" "{"
                               ">" "<"
                               "|"
                               "\"" "'" "`"))
          (goto-char (point-min))
          (insert (format "%s%s" ok-fn extra-chars))
          (goto-char (point-min))
          (should (pel-string= (pel-filename-at-point) ok-fn
                               test-scenario
                               extra-chars))
          (goto-char (point-min))
          (kill-line 2))

        ;; Now test line and column number extraction
        (goto-char (point-min))
        (insert fnlc)
        (goto-char (point-min))
        (should (pel-equal (pel-filename-parts-at-point) ok-fn-spec
                           test-scenario))
        (kill-line 2)

        ;; Now test a file name enclosed in quotes with embedded spaces
        (goto-char (point-min))
        (insert "\"/a/with spaces/ name.txt\"")
        (goto-char (point-min))
        (should (pel-string= (pel-filename-at-point)
                             "/a/with spaces/ name.txt"
                             test-scenario))))))


;; ---------------------------------------------------------------------------
;; pel--dir-name-if
;;
;; Returns PATH-NAME as-is when DIRECTORY-ONLY is nil;
;; returns the directory part of PATH-NAME when DIRECTORY-ONLY is non-nil.

(ert-deftest pel--dir-name-if-directory-only-nil-test ()
  "When DIRECTORY-ONLY is nil, returns the full path unchanged."
  (should (string= (pel--dir-name-if "/some/path/file.txt" nil)
                   "/some/path/file.txt"))
  (should (string= (pel--dir-name-if "/some/path/file.txt" nil)
                   "/some/path/file.txt"))
  (should (string= (pel--dir-name-if "/only-dir/" nil)
                   "/only-dir/"))
  (should (string= (pel--dir-name-if "relative/path.el" nil)
                   "relative/path.el")))

(ert-deftest pel--dir-name-if-directory-only-t-test ()
  "When DIRECTORY-ONLY is non-nil, returns only the directory component."
  (should (string= (pel--dir-name-if "/some/path/file.txt" t)
                   "/some/path/"))
  (should (string= (pel--dir-name-if "relative/path.el" t)
                   "relative/"))
  ;; A path that is already a directory (ends with /) returns itself
  (should (string= (pel--dir-name-if "/only-dir/" t)
                   "/only-dir/")))

(ert-deftest pel--dir-name-if-non-nil-truthy-test ()
  "Any non-nil DIRECTORY-ONLY value activates directory extraction."
  ;; 1, :flag, "yes" are all non-nil
  (should (string= (pel--dir-name-if "/a/b/c.txt" 1)    "/a/b/"))
  (should (string= (pel--dir-name-if "/a/b/c.txt" :yes) "/a/b/"))
  (should (string= (pel--dir-name-if "/a/b/c.txt" "on") "/a/b/")))

;; ---------------------------------------------------------------------------
;; pel--file-window-info-for
;;
;; Parses the raw "P" interactive prefix argument into
;; (raw-n  n-value  directory-only  use-browser).

(ert-deftest pel--file-window-info-for-nil-test ()
  "nil prefix arg: n=nil, n-value=1 (prefix-numeric-value of nil), directory-only=nil, browser=nil."
  (let ((result (pel--file-window-info-for nil)))
    (should (null  (nth 0 result)))           ; raw-n = nil
    (should (= 1   (nth 1 result)))           ; n-value = 1
    (should (null  (nth 2 result)))           ; directory-only = nil
    (should (null  (nth 3 result)))))         ; use-browser = nil

(ert-deftest pel--file-window-info-for-9-browser-test ()
  "N=9: use-browser must be t."
  (let ((result (pel--file-window-info-for 9)))
    (should (= 9  (nth 0 result)))
    (should (= 9  (nth 1 result)))
    (should (null (nth 2 result)))
    (should (eq t (nth 3 result)))))

(ert-deftest pel--file-window-info-for-positive-normal-test ()
  "N in 0..8 (not 9): directory-only=nil, browser=nil."
  (dolist (n '(0 1 2 3 4 5 6 7 8))
    (let ((result (pel--file-window-info-for n)))
      (should (= n   (nth 1 result)))
      (should (null  (nth 2 result)))
      (should (null  (nth 3 result))))))

(ert-deftest pel--file-window-info-for-directory-only-positive-test ()
  "N >= 20: directory-only=t; effective n-value = N - 20."
  (let ((result (pel--file-window-info-for 20)))
    (should (eq t (nth 2 result)))            ; directory-only
    (should (= 0  (nth 1 result))))           ; 20 - 20 = 0
  (let ((result (pel--file-window-info-for 21)))
    (should (eq t (nth 2 result)))
    (should (= 1  (nth 1 result))))           ; 21 - 20 = 1
  (let ((result (pel--file-window-info-for 25)))
    (should (eq t (nth 2 result)))
    (should (= 5  (nth 1 result)))))          ; 25 - 20 = 5

(ert-deftest pel--file-window-info-for-directory-only-negative-test ()
  "N <= -20: directory-only=t; effective n-value = N + 20."
  (let ((result (pel--file-window-info-for -20)))
    (should (eq t  (nth 2 result)))
    (should (= 0   (nth 1 result))))          ; -20 + 20 = 0
  (let ((result (pel--file-window-info-for -21)))
    (should (eq t  (nth 2 result)))
    (should (= -1  (nth 1 result))))          ; -21 + 20 = -1
  (let ((result (pel--file-window-info-for -25)))
    (should (eq t  (nth 2 result)))
    (should (= -5  (nth 1 result)))))         ; -25 + 20 = -5

(ert-deftest pel--file-window-info-for-negative-normal-test ()
  "Negative N in (-19..-1): directory-only=nil, browser=nil."
  (dolist (n '(-1 -5 -10 -19))
    (let ((result (pel--file-window-info-for n)))
      (should (= n   (nth 1 result)))
      (should (null  (nth 2 result)))
      (should (null  (nth 3 result))))))

;; ---------------------------------------------------------------------------
;; pel-filename-parts-at-point — additional cases not covered by fn-example-strings
;;
;; The existing ert-test-pel-file-extract-filename covers the fn-example-strings table.
;; These tests cover HTTP/file URLs and the shebang (#!) special case.

(ert-deftest pel-filename-parts-at-point-http-url-test ()
  "HTTP/HTTPS strings are returned as (http . url-string)."
  (dolist (url '("https://example.com/path"
                 "http://example.com/"
                 "https://github.com/user/repo"))
    (with-temp-buffer
      (insert url)
      (goto-char (point-min))
      (let ((result (pel-filename-parts-at-point)))
        (should (consp result))
        (should (eq 'http (car result)))
        (should (string= url (cdr result)))))))

(ert-deftest pel-filename-parts-at-point-file-url-test ()
  "file:// URIs are treated as HTTP-class entries by default."
  (with-temp-buffer
    (insert "file:///tmp/some-file.txt")
    (goto-char (point-min))
    ;; Without keep-file-url=t the file:// prefix is stripped and
    ;; the path is treated as a regular filename.
    (let ((result (pel-filename-parts-at-point nil)))
      (should (listp result))
      ;; kind must be 'fname (not 'http) after stripping
      (should (eq 'fname (car result))))))

(ert-deftest pel-filename-parts-at-point-file-url-kept-test ()
  "With KEEP-FILE-URL=t, file:// strings are returned as (http . url)."
  (with-temp-buffer
    (insert "file:///tmp/some-file.txt")
    (goto-char (point-min))
    (let ((result (pel-filename-parts-at-point t)))
      (should (consp result))
      (should (eq 'http (car result))))))

(ert-deftest pel-filename-parts-at-point-shebang-line-test ()
  "A filename followed by ':#!' is recognised as a shebang hit at line 1."
  ;; Simulates what a recursive-grep result line looks like when the
  ;; match is the shebang line of a script.
  (with-temp-buffer
    (insert "somedir/myscript:#! /usr/bin/perl")
    (goto-char (point-min))
    (let ((result (pel-filename-parts-at-point)))
      (should (listp result))
      (should (eq 'fname (car result)))
      (should (string= "somedir/myscript" (nth 1 result)))
      ;; Line number must be 1 for the shebang case
      (should (= 1 (nth 2 result)))
      ;; Column must be nil
      (should (null (nth 3 result))))))

(ert-deftest pel-filename-parts-at-point-nil-for-empty-test ()
  "An empty buffer yields nil from pel-filename-parts-at-point."
  (with-temp-buffer
    (should (null (pel-filename-parts-at-point)))))

;; ---------------------------------------------------------------------------
;; pel-filename-at-point — additional mode-specific delimiter cases

(ert-deftest pel-filename-at-point-rst-mode-no-parens-delimiters-test ()
  "In rst-mode, parentheses and square brackets are NOT delimiters."
  (with-temp-buffer
    (rst-mode)
    (insert "/path/to/file(1).txt")
    (goto-char (point-min))
    ;; In rst-mode the '(' is not a delimiter, so it remains in the filename
    (let ((result (pel-filename-at-point)))
      (should (string-match-p "file(1)" result)))))

(ert-deftest pel-filename-at-point-standard-mode-parens-delimiters-test ()
  "In non-rst, non-markdown modes, parentheses ARE delimiters."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "/path/to/file(1).txt")
    (goto-char (point-min))
    ;; '(' is a delimiter, so the filename stops before it
    (let ((result (pel-filename-at-point)))
      (should (string= "/path/to/file" result)))))

(ert-deftest pel-filename-at-point-quoted-with-spaces-test ()
  "Point at opening double-quote extracts a filename with embedded spaces."
  (with-temp-buffer
    (insert "\"/a path/with spaces/file.txt\"")
    (goto-char (point-min))  ; point is at the opening quote
    (should (string= (pel-filename-at-point)
                     "/a path/with spaces/file.txt"))))

(ert-deftest pel-filename-at-point-region-overrides-extraction-test ()
  "When a region is active, pel-filename-at-point returns the region content."
  (with-temp-buffer
    (insert "some /ignored/path here /actual/file.txt extra")
    ;; Mark /actual/file.txt as the region
    (goto-char (point-min))
    (re-search-forward "/actual/file\\.txt")
    (let ((end (point)))
      (re-search-backward "/actual")
      (set-mark (point))
      (goto-char end)
      (should (string= (pel-filename-at-point) "/actual/file.txt")))))

;; ---------------------------------------------------------------------------
;; pel-shell-command-on-current-file — mock-based smoke test

(ert-deftest pel-shell-command-on-current-file-smoke-test ()
  "pel-shell-command-on-current-file formats the command and passes it to
`shell-command' with the current buffer's filename substituted for %s.

Note: the function calls `pel-current-buffer-filename' internally (not
`buffer-file-name' directly), so that helper must be mocked.
`tramp-tramp-file-p' and `find-file-name-handler' are also mocked so
the test does not depend on Tramp being loaded."
  (let (captured-cmd)
    (cl-letf (((symbol-function 'pel-current-buffer-filename)
               (lambda (&rest _) "/tmp/test-file.txt"))
              ((symbol-function 'find-file-name-handler)
               (lambda (&rest _) nil))
              ((symbol-function 'tramp-tramp-file-p)
               (lambda (&rest _) nil))
              ((symbol-function 'shell-command)
               (lambda (cmd) (setq captured-cmd cmd))))
      (pel-shell-command-on-current-file "wc -l %s")
      (should (string= captured-cmd "wc -l /tmp/test-file.txt"))

      (pel-shell-command-on-current-file "echo %s")
      (should (string= captured-cmd "echo /tmp/test-file.txt")))))

(ert-deftest pel-shell-command-on-current-file-format-test ()
  "The %s in COMMAND-FORMAT is substituted with the full file path."
  (let (captured-cmd)
    (cl-letf (((symbol-function 'pel-current-buffer-filename)
               (lambda (&rest _) "/usr/local/src/myproject/hello.py"))
              ((symbol-function 'find-file-name-handler)
               (lambda (&rest _) nil))
              ((symbol-function 'tramp-tramp-file-p)
               (lambda (&rest _) nil))
              ((symbol-function 'shell-command)
               (lambda (cmd) (setq captured-cmd cmd))))
      (pel-shell-command-on-current-file "python3 -m py_compile %s")
      (should (string= captured-cmd
                       "python3 -m py_compile /usr/local/src/myproject/hello.py")))))

;;; --------------------------------------------------------------------------
(provide 'pel-file-test)

;;; pel-file-test.el ends here
