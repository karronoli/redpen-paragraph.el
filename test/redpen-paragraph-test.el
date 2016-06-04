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
            (desc "dummy"))
        (sleep-for 1) ;; wait until exit of echo process.
        (redpen-paragraph-sentinel proc desc))))
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
            (desc "dummy"))
        (sleep-for 1) ;; wait until exit of echo process.
        (should-error
         (redpen-paragraph-sentinel proc desc)
         :type 'json-unknown-keyword)))))

(ert-deftest invoke-redpen-paragraph ()
  "Invoke redpen-paragraph."
  (with-temp-buffer
    (insert "test")
    (let ((redpen-commands
           `(,(concat
              "echo "
              (shell-quote-argument
               (json-encode '((errors . [((lineNum . 1))]))))))))
      (redpen-paragraph)
      (sleep-for 1) ;; wait until exit of echo process.
      (with-current-buffer redpen-paragraph-compilation-buffer-name
        (should (eq (point) 1))
        (should (eq major-mode 'compilation-mode))))))

(ert-deftest check-cursor-position ()
  "Check cursor position to paragraph."
  (with-temp-buffer
    (insert "test1\n\ntest2\n\ntest3")
    (let ((redpen-commands
           `(,(concat
              "echo "
              (shell-quote-argument
               (json-encode '((errors . []))))))))
      ;; 1st line
      (goto-char (point-min))
      (redpen-paragraph)
      (sleep-for 1)
      (with-current-buffer (find-file-noselect redpen-temporary-filename)
        (should (equal "test1\n" (buffer-string))))
      (kill-buffer (find-file-noselect redpen-temporary-filename))

      (goto-char (point-min))
      (move-end-of-line 1)
      (redpen-paragraph)
      (sleep-for 1)
      (with-current-buffer (find-file-noselect redpen-temporary-filename)
        (should (equal "test1\n" (buffer-string))))
      (kill-buffer (find-file-noselect redpen-temporary-filename))

      ;; 2nd line
      (goto-char (point-min))
      (forward-line 1)
      (redpen-paragraph)
      (sleep-for 1)
      (with-current-buffer (find-file-noselect redpen-temporary-filename)
        (should (equal "test1\n" (buffer-string))))
      (kill-buffer (find-file-noselect redpen-temporary-filename))

      ;; 3rd line
      (goto-char (point-min))
      (forward-line 2)
      (redpen-paragraph)
      (sleep-for 1)
      (with-current-buffer (find-file-noselect redpen-temporary-filename)
        (should (equal "test2\n" (buffer-string))))
      (kill-buffer (find-file-noselect redpen-temporary-filename))

      (goto-char (point-min))
      (forward-line 2)
      (move-end-of-line 1)
      (redpen-paragraph)
      (sleep-for 1)
      (with-current-buffer (find-file-noselect redpen-temporary-filename)
        (should (equal "test2\n" (buffer-string))))
      (kill-buffer (find-file-noselect redpen-temporary-filename))

      ;; 4th line
      (goto-char (point-min))
      (forward-line 1)
      (redpen-paragraph)
      (sleep-for 1)
      (with-current-buffer (find-file-noselect redpen-temporary-filename)
        (should (equal "test2\n" (buffer-string))))
      (kill-buffer (find-file-noselect redpen-temporary-filename))

      ;; 5th line
      (goto-char (point-min))
      (forward-line 4)
      (redpen-paragraph)
      (sleep-for 1)
      (with-current-buffer (find-file-noselect redpen-temporary-filename)
        (should (equal "test3" (buffer-string))))
      (kill-buffer (find-file-noselect redpen-temporary-filename))

      (goto-char (point-min))
      (forward-line 4)
      (move-end-of-line 1)
      (redpen-paragraph)
      (sleep-for 1)
      (with-current-buffer (find-file-noselect redpen-temporary-filename)
        (should (equal "test3" (buffer-string)))))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; redpen-paragraph-test.el ends here
