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

(ert-deftest list-errors-message ()
  "List errors from minimal json."
  (with-temp-buffer
    (let ((redpen-paragraph-compilation-buffer-name (current-buffer))
          (message "test"))
      (redpen-paragraph-list-errors
       `((errors . [((message . ,message))])))
      (should
       (equal
        (buffer-string)
        (format redpen-paragraph-input-pattern 1 1 1 1 "" message ""))))))

(require 'json)
(ert-deftest read-the-process-stdout-as-json ()
  "Read the process stdout as JSON."
  (with-temp-buffer
    (let ((redpen-paragraph-compilation-buffer-name
           (current-buffer)))
      (async-shell-command
       (concat "echo "
               (shell-quote-argument
                (json-encode '((errors . [((lineNum . 1))])))))
       (current-buffer))
      (let ((proc (get-buffer-process (current-buffer)))
            (desc "dummy")
            (nop (lambda (FORMAT-STRING &rest ARGS) nil)))
        (advice-add 'message :around nop)
        (sleep-for 1) ;; wait until exit of echo process.
        (redpen-paragraph-sentinel proc desc)
        (advice-remove 'message nop))))
  (should t))

(ert-deftest read-the-process-stdout-as-not-json ()
  "Read the process stdout as not JSON."
  (with-temp-buffer
    (let ((redpen-paragraph-compilation-buffer-name
           (current-buffer)))
      (async-shell-command
       "echo test"
       (current-buffer))
      (let ((proc (get-buffer-process (current-buffer)))
            (desc "dummy")
            (nop (lambda (FORMAT-STRING &rest ARGS) nil)))
        (advice-add 'message :around nop)
        (sleep-for 1) ;; wait until exit of echo process.
        (should-error
         (redpen-paragraph-sentinel proc desc)
         :type 'json-unknown-keyword)
        (advice-remove 'message nop)))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; redpen-paragraph-test.el ends here
