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

(ert-deftest list-errors-message ()
  "List errors from minimal json."
  (with-temp-buffer
    (let ((redpen-paragraph-compilation-buffer-name (current-buffer))
          (message "message")
          (validator "validator"))
      (redpen-paragraph-list-errors
       `((errors . [((message . ,message) (validator . ,validator))])))
      (should
       (equal
        (buffer-string)
        (concat (format redpen-paragraph-input-pattern
                        validator 1 1 1 1 message)
                "\n"))))))

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

(ert-deftest invoke-redpen-paragraph ()
  "Invoke redpen-paragraph."
  (with-temp-buffer
    (insert "test")
    (let ((nop (lambda (FORMAT-STRING &rest ARGS) nil))
          (redpen-commands
           `(,(concat
              "echo "
              (shell-quote-argument
               (json-encode '((errors . [((lineNum . 1))]))))))))
      (advice-add 'message :around nop)
      (redpen-paragraph)
      (sleep-for 1) ;; wait until exit of echo process.
      (advice-remove 'message nop)
      (with-current-buffer redpen-paragraph-compilation-buffer-name
        (should (eq (point) 1))
        (should (eq major-mode 'compilation-mode))))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; redpen-paragraph-test.el ends here
