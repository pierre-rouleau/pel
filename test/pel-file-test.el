;;; pel-file-test.el --- pel-file Emacs Regression Test -*-lexical-binding: t-*-

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

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; Currently the test code only verifies the extraction of the file name, line
;; number and column numbers, nothing else.  The behaviour with respect of window
;; selection is not yet tested.

(require 'pel-file)                     ; tested code file.
(require 'pel-ert)

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

(defconst fn-strings
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
  (dolist (test-scenario fn-strings)
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


;;; --------------------------------------------------------------------------
(provide 'pel-file-test)

;;; pel-file-test.el ends here
