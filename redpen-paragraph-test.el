;;; redpen-paragraph-test.el --- test for redpen-paragraph

;;; Commentary:
;; test for redpen-paragraph by ert
;; $ emacs -Q -batch --directory ~/.emacs.d -l redpen-paragraph-test.el \
;;     -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'redpen-paragraph)

;;; Code:
(ert-deftest detect-english ()
  "Detect english."
  (should (redpen-paragraph-is-english "abc")))

(ert-deftest detect-not-english ()
  "Detect the other language."
  (should-not (redpen-paragraph-is-english "あいう")))

(ert-deftest detect-prefer-english ()
  "Detect english with the other language."
  (should (redpen-paragraph-is-english "abcdあいう")))

(ert-deftest detect-prefer-not-english ()
  "Detect the other language with english."
  (should-not (redpen-paragraph-is-english "abcあいうえ")))

(require 'compile)
(ert-deftest redpen-plain-regexp ()
  "Test of regular expression on compilation."
  (let* ((str "redpen.15364:10: ValidationError[SpaceBetweenAlphabeticalWord],")
         (pat (assoc 'redpen-plain compilation-error-regexp-alist-alist))
         (regexp (nth 1 pat)))
    (string-match regexp str)
    (should (equal (match-string 1 str) "redpen.15364"))
    (should (equal (match-string 2 str) "10"))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; redpen-paragraph-test.el ends here
