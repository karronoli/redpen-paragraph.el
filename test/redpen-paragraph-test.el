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
  (should (redpen-paragraph-is-english ""))
  (should (redpen-paragraph-is-english "abc"))
  (should (redpen-paragraph-is-english "abcdあいう")))

(ert-deftest detect-not-english ()
  "Detect the other language."
  (should-not (redpen-paragraph-is-english "あいう"))
  (should-not (redpen-paragraph-is-english "abcあいう"))
  (should-not (redpen-paragraph-is-english "abcあいうえ")))

(ert-deftest target-filename ()
  "Return target filename."
  (should (eq redpen-target-filename (redpen-target-filename))))

(ert-deftest list-all-parameter-error ()
  "List the all parameter error."
  (with-temp-buffer
    (let ((redpen-paragraph-compilation-buffer-name (current-buffer))
          (validator "validator")
          (startLineNum 1) (startOffset 2)
          (endLineNum 4) (endOffset 5)
          (message "message")
          (sentence "sentence"))
      (redpen-paragraph-list-errors
       `((errors . [((validator . ,validator)
                     (startPosition . ((lineNum . ,startLineNum)
                                       (offset . ,startOffset)))
                     (endPosition . ((lineNum . ,endLineNum)
                                     (offset . , endOffset)))
                     (message . ,message)
                     (sentence . ,sentence))])))
      (should
       (equal
        (concat (format redpen-paragraph-input-pattern
                        validator
                        startLineNum (1+ startOffset)
                        endLineNum endOffset
                        message)
                sentence "\n\n")
        (buffer-string))))))

(ert-deftest list-sorted-errors ()
  "List the sorted errors."
  (with-temp-buffer
    (let ((redpen-paragraph-compilation-buffer-name (current-buffer))
          (startLineNum1 1) (startLineNum2 2) (startLineNum3 3))
      (redpen-paragraph-list-errors
       `((errors . [((lineNum . ,startLineNum2))
                    ((lineNum . ,startLineNum3))
                    ((lineNum . ,startLineNum1))])))
      (should
       (equal
        (concat
         (format redpen-paragraph-input-pattern
                 "" startLineNum1 1 startLineNum1 1 "")
         "\n"
         (format redpen-paragraph-input-pattern
                 "" startLineNum2 1 startLineNum2 1 "")
         "\n"
         (format redpen-paragraph-input-pattern
                 "" startLineNum3 1 startLineNum3 1 "")
         "\n")
        (buffer-string))))))

(require 'json)
(ert-deftest read-the-process-stdout-as-json ()
  "Read the process stdout as JSON."
  (with-temp-buffer
    (let ((redpen-paragraph-compilation-buffer-name (current-buffer))
          (proc
           (progn
             (async-shell-command
              (concat "echo "
                      (shell-quote-argument
                       (json-encode '((errors . [])))))
              (current-buffer))
             (get-buffer-process (current-buffer))))
          (desc "dummy"))
      (sleep-for 1) ;; wait until exit of echo process.
      (redpen-paragraph-sentinel proc desc)))
  (should t))

(ert-deftest read-the-process-stdout-as-not-json ()
  "Read the process stdout as not JSON."
  (with-temp-buffer
    (let ((redpen-paragraph-compilation-buffer-name (current-buffer))
          (proc
           (progn
             (async-shell-command
              "echo test"
              (current-buffer))
             (get-buffer-process (current-buffer))))
          (desc "dummy"))
      (sleep-for 1) ;; wait until exit of echo process.
      (should-error
       (redpen-paragraph-sentinel proc desc)
       :type 'json-unknown-keyword))))

(ert-deftest invoke-redpen-paragraph ()
  "Invoke redpen-paragraph."
  (with-temp-buffer
    (let ((redpen-commands
           `(,(concat
               "echo "
               (shell-quote-argument
                (json-encode '((errors . [()]))))))))
      (redpen-paragraph)
      (sleep-for 1) ;; wait until exit of echo process.
      (with-current-buffer redpen-paragraph-compilation-buffer-name
        (should (equal
                 " at start 1.1, end 1.1: \n\n"
                 (buffer-string)))
        (should (eq (point) (point-min)))
        (should (eq major-mode 'compilation-mode))))))

(ert-deftest check-cursor-position ()
  "Check cursor position to paragraph."
  (with-temp-buffer
    (insert "test1\n\ntest2\n\ntest3")
    (let ((redpen-commands
           `(,(concat
               "echo "
               (shell-quote-argument
                (json-encode '((errors . [])))))))
          ;; lineNum position-on-the-line expected-result
          (tests '((1 nil "test1\n") (1 'end "test1\n")
                   (2 nil "\ntest2\n")
                   (3 nil "\ntest2\n") (3 'end "\ntest2\n")
                   (4 nil "\ntest3")
                   (5 nil "\ntest3") (5 'end "\ntest3"))))
      (mapc
       (lambda (test)
         (cl-destructuring-bind (lineNum position expected) test
           (goto-char (point-min))
           (if (> lineNum 1) (forward-line (1- lineNum)))
           (if (eq position 'end) (move-end-of-line 1))
           (redpen-paragraph)
           (sleep-for 1) ;; wait until exit of echo process.
           (with-current-buffer
               (find-file-noselect redpen-temporary-filename)
             (should (equal expected (buffer-string))))
           (kill-buffer
            (find-file-noselect redpen-temporary-filename))))
       tests)

      (mark-whole-buffer)
      ;; for (use-region-p) on emacs --batch
      (let ((transient-mark-mode t))
        (redpen-paragraph))
      (sleep-for 1) ;; wait until exit of echo process.
      (with-current-buffer
          (find-file-noselect redpen-temporary-filename)
        (should
         (equal "test1\n\ntest2\n\ntest3" (buffer-string)))))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; redpen-paragraph-test.el ends here
