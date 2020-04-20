;;; pel-text-transform-test.el --- PEL text transformation test -*-lexical-binding: t-*-

;;; Commentary:
;;


(require 'pel-text-transform)
(require 'ert)

;;; Code:

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


(defun pel--tt-test (test-data)
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
  (pel--tt-test pel--tt-test1)
  (pel--tt-test pel--tt-test2)
  (pel--tt-test pel--tt-test3))

;; -----------------------------------------------------------------------------
(provide 'pel-text-transform-test)

;;; pel-text-transform-test.el ends here
