;;; pel-file-test.el --- pel-file Emacs Regression Test

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau.swd@gmail.com>

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
;; Currently the test code only verifies the extraction of the file name, line
;; number and column numbers, nothing else. The behaviour with respect of window
;; selection is not yet tested.

(require 'pel-file)
(require 'ert)

;;; Code:

(defconst ok-url-strings '( "https://www.gnu.org/"
                            "https://github.com/pierre-rouleau/pel"))

;;                       fnlc                             |---- ok-fn-spec --------------------------------|
;;                       fnlc                             ok-kind      ok-fn                           ok-line ok-column
(defconst fn-strings '(( "a-simple-file-name.txt"        (fname        "a-simple-file-name.txt"        1 0))
                       ( "/abc/def/ghi/jkl/abc.txt"      (fname        "/abc/def/ghi/jkl/abc.txt"      1 0))
                       ( "/abc/def/ghi/jkl/abc.txt   "   (fname        "/abc/def/ghi/jkl/abc.txt"      1 0))
                       ( "\\abc\\def\\ghi\\jkl\\abc.txt" (fname        "\\abc\\def\\ghi\\jkl\\abc.txt" 1 0))
                       ( "Où-êtes-vous_tous?"            (fname        "Où-êtes-vous_tous?"            1 0))
                       ( "Ici!"                          (fname        "Ici!"                          1 0))
                       ( "Ici!        "                  (fname        "Ici!"                          1 0))
                       ( "C:\\Windows\\files\\c.cpp"     (fname-w-ddrv "C:\\Windows\\files\\c.cpp"     1 0))
                       ( "C:\\Windows\\files\\c.cpp  "   (fname-w-ddrv "C:\\Windows\\files\\c.cpp"     1 0))
                       ( "C:/Windows/files/c.cpp"        (fname-w-ddrv "C:/Windows/files/c.cpp"        1 0))
                       ( "C:/Windows/files/c.cpp  "      (fname-w-ddrv "C:/Windows/files/c.cpp"        1 0))
                       ( "/usr/me/src/pel/pel.el"        (fname        "/usr/me/src/pel/pel.el"        1 0))
                       ( "\\usr\\me\\src\\pel\\pel.el"   (fname        "\\usr\\me\\src\\pel\\pel.el"   1 0))
                       ( "\\usr/me/src/pel/pel.el"       (fname        "\\usr/me/src/pel/pel.el"       1 0))
                       ( "/abc/def/"                     (fname        "/abc/def/"                     1 0))
                       ( "./pel.el"                      (fname        "./pel.el"                      1 0))
                       ( "./pelo.el:1:2"                 (fname        "./pelo.el"                     1 2))
                       ( "./pel.el:1001:4"               (fname        "./pel.el"                      1001 4))
                       ( "./pel.el:1001:4"               (fname        "./pel.el"                      1001 4))
                       ( "./pel.el@1001:4"               (fname        "./pel.el"                      1001 4))
                       ( "./pel.el:3"                    (fname        "./pel.el"                      3 0))
                       ( "./pel.el:123456:987654"        (fname        "./pel.el"                      123456 987654))
                       ( "../dev/pel.el"                 (fname        "../dev/pel.el"                 1 0))
                       ( "~/src/pel/pel.cpp"             (fname        "~/src/pel/pel.cpp"             1 0))
                       ( "~/../other/src/python/d.py"    (fname        "~/../other/src/python/d.py"    1 0))
                       ))


(ert-deftest ert-test-pel-file-extract-url ()
  (dolist (url ok-url-strings)
    (with-temp-buffer
      (insert url)
      (goto-char (point-min))
      (should (string= (pel-filename-at-point) url)))))



(ert-deftest ert-test-pel-file-extract-filename ()
  (dolist (entry fn-strings)
    (let* ((fnlc (car entry))
           (ok-fn-spec (cadr entry))
           ;;(ok-kind    (car ok-fn-spec))
           (ok-fn      (cadr ok-fn-spec))
           ;;(ok-line    (caddr ok-fn-spec))
           ;;(ok-column  (cadddr ok-fn-spec))
           )
      (with-temp-buffer
        ;; test extraction of a file name without and with following white-spaces
        ;; and some other ASCII characters that may follow the file name.
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
          (should (string= (pel-filename-at-point) ok-fn))
          (goto-char (point-min))
          (kill-line 2))

        ;; Now test line and column number extraction
        (goto-char (point-min))
        (insert fnlc)
        (goto-char (point-min))
        (should (equal (pel-filename-parts-at-point) ok-fn-spec))
        (kill-line 2)

        ;; Now test a file name enclosed in quotes with embedded spaces
        (goto-char (point-min))
        (insert "\"/a/with spaces/ name.txt\"")
        (goto-char (point-min))
        (should (string= (pel-filename-at-point)  "/a/with spaces/ name.txt"))))))


;; -----------------------------------------------------------------------------
(provide 'pel-file-test)

;;; pel-file-test.el ends here
