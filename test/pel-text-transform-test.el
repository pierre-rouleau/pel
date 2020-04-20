;;; pel-text-transform-test.el --- PEL text transformation test -*-lexical-binding: t-*-

;;; Commentary:
;;


(require 'pel-text-transform)
(require 'ert)

;;; Code:

;; -----------------------------------------------------------------------------
;; Test with no region: one or several words at a time
;; ---------------------------------------------------
;;
;; It is possible to only use the
;; `pel-downcase-word-or-region' and
;; `pel-upcase-word-or-region' functions
;; to convert  one or several words from lowercase,
;; uppercase and capitalized.  At the most 2 commands are used
;; and since the commands are both mapped to M-c with out without
;; the Shift key it would be possible to free the key (M-T) currently
;; used for capitalization, but PEL leaves it for convenience.
;;

(defconst pel--tt-test1
  '( ;; Start string for each test
    "one two three four five six seven eight nine ten"
    ;;
    ;; Expected result after execution of test lambda
    ("One Two Three Four Five Six Seven Eight Nine Ten"
     (lambda ()
       (goto-char 1)
       (dotimes (_i 10)
         (pel-downcase-word-or-region))))
    ("One Two Three Four Five Six Seven Eight Nine Ten"
     (lambda ()
       (goto-char 1)
       (pel-downcase-word-or-region 10)))
    ("one two Three four five six seven eight nine ten"
     (lambda ()
       (goto-char 8)
       (pel-downcase-word-or-region)))
    ("one two Three four five six seven eight nine ten"
     (lambda ()
       (goto-char 8)
       (pel-downcase-word-or-region 0)))
    ("one Two three four five six seven eight nine ten"
     (lambda ()
       (goto-char 8)
       (pel-downcase-word-or-region -1)))
    ("One Two three four five six seven eight nine ten"
     (lambda ()
       (goto-char 8)
       (pel-downcase-word-or-region -2)))
    ("one two tHree four five six seven eight nine ten"
     (lambda ()
       (goto-char 10)
       (pel-downcase-word-or-region)))
    ("one two Three four five six seven eight nine ten"
     (lambda ()
       (goto-char 10)
       (pel-downcase-word-or-region -1)))
    ("one two Three four five six seven eight nine ten"
     (lambda ()
       (goto-char 12)
       (pel-downcase-word-or-region -1)))
    ("One Two Three four five six seven eight nine ten"
     (lambda ()
       (goto-char 12)
       (pel-downcase-word-or-region -3)))
    ("One Two ThrEe Four five six seven eight nine ten"
     (lambda ()
       (goto-char 12)
       (pel-downcase-word-or-region -3)
       (pel-downcase-word-or-region 2)))
    ("One Two Three Four Five Six Seven Eight Nine Ten"
     (lambda ()
       (goto-char 47)
       (pel-downcase-word-or-region -10)))
    ("One Two Three Four Five Six Seven Eight Nine ten"
     (lambda ()
       (goto-char 45)
       (pel-downcase-word-or-region -10)))
    ("One Two Three Four Five Six Seven Eight Nine ten"
     (lambda ()
       (goto-char 45)
       (pel-downcase-word-or-region -100)))))


(defconst pel--tt-test2
  '(;; initial string
    "One Two Three Four Five Six Seven Eight Nine Ten"
    ;;
    ("one two three four five six seven eight nine ten"
     (lambda ()
       (goto-char 1)
       (dotimes (_i 10)
         (pel-downcase-word-or-region))))
    ("one two three four five six seven eight nine ten"
     (lambda ()
       (goto-char 1)
       (pel-downcase-word-or-region 10)))
    ("One two Three Four Five Six Seven Eight Nine Ten"
     (lambda ()
       (goto-char 4)
       (pel-downcase-word-or-region))
    ("One two Three Four Five Six Seven Eight Nine Ten"
     (lambda ()
       (goto-char 8)
       (pel-downcase-word-or-region -1)))
    ("One Two Three Four Five Six SeVen Eight Nine Ten"
     (lambda ()
       (goto-char 30)
       (pel-downcase-word-or-region))))))

(defconst pel--tt-test3
  '(;; initial string
    "ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
    ;;
    ("one two three four five six seven eight nine ten"
     (lambda ()
       (goto-char 1)
       (dotimes (_i 10)
         (pel-downcase-word-or-region))))
    ("one two three four five six seven eight nine ten"
     (lambda ()
       (goto-char 1)
       (pel-downcase-word-or-region 10)))
    ;;            .
    ("ONE TWO THREe FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     (lambda ()
       (goto-char 13)
       (pel-downcase-word-or-region)))
    ("ONE TWO threE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     (lambda ()
       (goto-char 13)
       (pel-downcase-word-or-region -1)))
    ("ONE TWO three FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     (lambda ()
       (goto-char 14)
       (pel-downcase-word-or-region -1)))
    ("one two threE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     (lambda ()
       (goto-char 13)
       (pel-downcase-word-or-region -3)))
    ;;             .
    ("one two three FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     (lambda ()
       (goto-char 14)
       (pel-downcase-word-or-region -3)))
    ;;                              .
    ("ONE TWO THREE FOUR FIVE SIX SEven EIGHT NINE TEN"
     (lambda ()
       (goto-char 31)
       (pel-downcase-word-or-region)))
    ("ONE TWO THREE FOUR FIVE SIX SEven eight NINE TEN"
     (lambda ()
       (goto-char 31)
       (pel-downcase-word-or-region 2)))
    ("ONE TWO THREE FOUR FIVE SIX seVEN EIGHT NINE TEN"
     (lambda ()
       (goto-char 31)
       (pel-downcase-word-or-region -1)))
    ("ONE TWO THREE FOUR FIVE six seVEN EIGHT NINE TEN"
     (lambda ()
       (goto-char 31)
       (pel-downcase-word-or-region -2)))))

(defconst pel--tt-test4
  '(;; initial string
    "one two three four five six seven eight nine ten"
    ;;
    ;;.
    ("ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     (lambda ()
       (goto-char 1)
       (dotimes (_i 10)
         (pel-upcase-word-or-region))))
    ;;.
    ("ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     (lambda ()
       (goto-char 1)
       (pel-upcase-word-or-region 10)))
    ;;             .
    ("one two THREE four five six seven eight nine ten"
     (lambda ()
       (goto-char 14)
       (pel-upcase-word-or-region -1)))
    ;;                              .
    ("one two three four five six seVEN eight nine ten"
     (lambda ()
       (goto-char 31)
       (pel-upcase-word-or-region)))
    ;;                              .
    ("one two three four five six SEVEN eight nine ten"
     (lambda ()
       (goto-char 31)
       (pel-upcase-word-or-region 0)))
    ;;                              .
    ("one two three four five six seVEN EIGHT nine ten"
     (lambda ()
       (goto-char 31)
       (pel-upcase-word-or-region 2)))))

(defconst pel--tt-test5
  '(;; initial string
    "One Two Three Four Five Six Seven Eight Nine Ten"
    ;;
    ("ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     (lambda ()
       (goto-char 1)
       (dotimes (_i 10)
         (pel-upcase-word-or-region))))
    ("ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     (lambda ()
       (goto-char 1)
       (pel-upcase-word-or-region 10)))))

(defconst pel--tt-test6
  '(;; initial string
    "ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
    ;;.
    ("One Two Three Four Five Six Seven Eight Nine Ten"
     (lambda ()
       (goto-char 1)
       (dotimes (_i 10)
         (pel-upcase-word-or-region))))
    ;;.
    ("One Two Three Four Five Six Seven Eight Nine Ten"
     (lambda ()
       (goto-char 1)
       (pel-upcase-word-or-region 10)))
    ;;             .
    ("ONE TWO Three FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     (lambda ()
       (goto-char 14)
       (pel-upcase-word-or-region -1)))
    ;;                              .
    ("ONE TWO THREE FOUR FIVE SIX SEVen EIGHT NINE TEN"
     (lambda ()
       (goto-char 31)
       (pel-upcase-word-or-region)))
    ;;                              .
    ("ONE TWO THREE FOUR FIVE SIX SEven EIGHT NINE TEN"
     (lambda ()
       (goto-char 31)
       (pel-downcase-word-or-region)))
    ;;                              .
    ("ONE TWO THREE FOUR FIVE SIX SEVen Eight Nine TEN"
     (lambda ()
       (goto-char 31)
       (pel-upcase-word-or-region 3)))
    ;;                              .
    ("ONE TWO THREE FOUR FIVE SIX SEven eight nine TEN"
     (lambda ()
       (goto-char 31)
       (pel-downcase-word-or-region 3)))))


(defun pel--tt-word-test (test-data)
  "Run the test defined by TEST-DATA.
A list of 2 elements:
- the original string for each test
- a list of tests:
  - the expected result string
  - the lambda used to transform the original string into the expected result."
  (let ((orig-string (car test-data))
        (test-defs   (cdr test-data)))
    (dolist (tdef test-defs)
      (let ((expected-string (car tdef))
            (tfct           (cadr tdef)))
        (with-temp-buffer
          (insert orig-string)
          (funcall tfct)
          (goto-char (point-min))
          (should (string= (buffer-string) expected-string)))))))

(ert-deftest er-test-text-transform-tests ()
  "Perform word conversion tests."
  (pel--tt-word-test pel--tt-test1)
  (pel--tt-word-test pel--tt-test2)
  (pel--tt-word-test pel--tt-test3)
  (pel--tt-word-test pel--tt-test4)
  (pel--tt-word-test pel--tt-test5)
  (pel--tt-word-test pel--tt-test6))

;; -----------------------------------------------------------------------------
;; Test with region
;; ----------------
;;
;; It is possible to only use the
;; `pel-downcase-word-or-region' and
;; `pel-upcase-word-or-region' functions
;; to convert all words in a region from all lowercase,
;; uppercase and capitalized.  At the most 2 commands are used
;; and since the commands are both mapped to M-c with out without
;; the Shift key it would be possible to free the key (M-T) currently
;; used for capitalization, but PEL leaves it for convenience.
;;

(defconst pel--tt-downcase-with-regions
  ;; list of before-string, after-string
  '(("one two three four five six seven eight nine ten"
     "One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("One Two Three Four Five Six Seven Eight Nine Ten"
     "one two three four five six seven eight nine ten")
    ;;
    ("one two THREE four five SIX seven eight nine ten"
     "One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     "one two three four five six seven eight nine ten")
    ;;
    ("ONE TWO THREE FOUR Five SIX seven EIght Nine TEN"
     "one two three four five six seven eight nine ten")
    ("oNE tWO tHREE fOUR fIVE sIX sEVEN eIGHT nINE tEN"
     "One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("oNE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     "One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("One Two Three Four Five Six Seven Eight Nine Ten"
     "one two three four five six seven eight nine ten")
    ;;
    ("OnE TwO ThREE FOUr FivE SIx SEveN Eight Nine Ten"
     "one two three four five six seven eight nine ten")
    ;;
    ("ONe TwO ThREE FOUr FivE SIx SEveN Eight Nine Ten"
     "one two three four five six seven eight nine ten")
    ;; same thing with leading spaces
    ("  one two three four five six seven eight nine ten"
     "  One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("  One Two Three Four Five Six Seven Eight Nine Ten"
     "  one two three four five six seven eight nine ten")
    ;;
    ("  one two THREE four five SIX seven eight nine ten"
     "  One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("  ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     "  one two three four five six seven eight nine ten")
    ;;
    ("  ONE TWO THREE FOUR Five SIX seven EIght Nine TEN"
     "  one two three four five six seven eight nine ten")
    ("  oNE tWO tHREE fOUR fIVE sIX sEVEN eIGHT nINE tEN"
     "  One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("  oNE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     "  One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("  One Two Three Four Five Six Seven Eight Nine Ten"
     "  one two three four five six seven eight nine ten")
    ;;
    ("  OnE TwO ThREE FOUr FivE SIx SEveN Eight Nine Ten"
     "  one two three four five six seven eight nine ten")
    ;;
    ("  ONe TwO ThREE FOUr FivE SIx SEveN Eight Nine Ten"
     "  one two three four five six seven eight nine ten"))
  ;;
  "The function `pel-downcase-word-or-region' checks the letters
of the first word in the region to determine what action to perform.
If the first word is:
- all uppercase: it converts to lowercase.
- first uppercase then some lowercase: it converts to lowercase
- first lowercase then some uppercase: it capitalizes all words
- all lowercase: it capitalizes all words.")

(defconst pel--tt-upcase-with-regions
  ;; list of before-string, after-string
  '(("one two three four five six seven eight nine ten"
     "ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN")
    ;;
    ("One Two Three Four Five Six Seven Eight Nine Ten"
     "ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN")
    ;;
    ("one two THREE four five SIX seven eight nine ten"
     "ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN")
    ;;
    ("ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     "One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("ONE TWO THREE FOUR Five SIX seven EIght Nine TEN"
     "One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("oNE tWO tHREE fOUR fIVE sIX sEVEN eIGHT nINE tEN"
     "ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN")
    ;;
    ("oNE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     "ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN")
    ;;
    ("One Two Three Four Five Six Seven Eight Nine Ten"
     "ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN")
    ;;
    ("OnE TwO ThREE FOUr FivE SIx SEveN Eight Nine Ten"
     "ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN")
    ;; same thing with leading spaces
    ("  one two three four five six seven eight nine ten"
     "  ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN")
    ;;
    ("  One Two Three Four Five Six Seven Eight Nine Ten"
     "  ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN")
    ;;
    ("  one two THREE four five SIX seven eight nine ten"
     "  ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN")
    ;;
    ("  ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     "  One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("  ONE TWO THREE FOUR Five SIX seven EIght Nine TEN"
     "  One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("  oNE tWO tHREE fOUR fIVE sIX sEVEN eIGHT nINE tEN"
     "  ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN")
    ;;
    ("  oNE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     "  ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN")
    ;;
    ("  One Two Three Four Five Six Seven Eight Nine Ten"
     "  ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN")
    ;;
    ("  OnE TwO ThREE FOUr FivE SIx SEveN Eight Nine Ten"
     "  ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN"))
  ;;
  "The function `pel-upcase-word-or-region' checks the letters
of the first word in the region to determine what action to perform.
If the first word is:
- all lowercase: it converts all to upper-case
- first lowercase: it converts all to upper-case
- first uppercase then some lowercase: it converts all to upper-case
- all uppercase: it capitalizes all words. ")

(defconst pel--tt-capitalize-with-regions
  ;; list of before-string, after-string
  '(("one two three four five six seven eight nine ten"
     "One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("One Two Three Four Five Six Seven Eight Nine Ten"
     "One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("one two THREE four five SIX seven eight nine ten"
     "One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     "One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("ONE TWO THREE FOUR Five SIX seven EIght Nine TEN"
     "One Two Three Four Five Six Seven Eight Nine Ten")
    ;; same thing with leading spaces
    ("  one two three four five six seven eight nine ten"
     "  One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("  One Two Three Four Five Six Seven Eight Nine Ten"
     "  One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("  one two THREE four five SIX seven eight nine ten"
     "  One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("  ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN"
     "  One Two Three Four Five Six Seven Eight Nine Ten")
    ;;
    ("  ONE TWO THREE FOUR Five SIX seven EIght Nine TEN"
     "  One Two Three Four Five Six Seven Eight Nine Ten"))
  ;;
  "The function `pel-capitalize-word-or-region' always
capitalizes all words in the region.")



(defun pel--tt-region-test (fct test-data-list)
  "Apply FCT on TEST-DATA-LIST to test region text transformation.
The TEST-DATA is a list of list of list of two strings:
- the original string
- the string with expected result after the first string is
  transformed with the specified FCT applied to the complete string
  as a region."
  (dolist (test-data test-data-list)
    (let ((original-string (car test-data))
          (expected-string (cadr test-data)))
      (with-temp-buffer
        (insert original-string)
        (goto-char (point-min))
        (set-mark-command nil)
        (end-of-line)
        (funcall fct)
        (should (string= (buffer-string) expected-string))))))

(ert-deftest er-test-region-transform-tests ()
  "Perform word conversion tests."
  (pel--tt-region-test 'pel-downcase-word-or-region   pel--tt-downcase-with-regions)
  (pel--tt-region-test 'pel-upcase-word-or-region     pel--tt-upcase-with-regions)
  (pel--tt-region-test 'pel-capitalize-word-or-region pel--tt-capitalize-with-regions))

;; -----------------------------------------------------------------------------
(provide 'pel-text-transform-test)

;;; pel-text-transform-test.el ends here
