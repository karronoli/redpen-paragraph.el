;;; redpen-paragraph.el --- RedPen interface. -*- lexical-binding: t; -*-

;; Copyright (C) 2015 karronoli

;; Author:  karronoli
;; Created: 2016/05/09
;; Version: 0.4
;; Keywords: document, proofreading, help
;; X-URL: https://github.com/karronoli/redpen-paragraph.el
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (json "1.4"))

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
;; or implied. See the License for the specific language governing
;; permissions and limitations under the License.

;;; Commentary:
;;
;; This package proofread some paragraph or a file by RedPen,
;; parse the json Output Format.
;;
;; You can get some paragraph by the priority.
;; 1. by customization on specific major mode
;; 2. active region
;; 3. (mark-paragraph)
;;
;; Or you can get a file regardless of the priority.

;;; Usage:
;;
;; You should install redpen!
;;   http://redpen.cc/
;;
;; Install from package.el & put these lines in your init file.
;; `redpen-commands' is for demo by default.
;; '%s' is replaced by `redpen-temporary-filename'.
;; With C-u, replaced by `buffer-file-name'.
;;
;;   (defvar redpen-commands
;;       ;; for english command
;;     '("redpen -r json -c /path/to/redpen-conf-en.xml %s 2>/dev/null"
;;       ;; for not english command
;;       "redpen -r json -c /path/to/redpen-conf-ja.xml %s 2>/dev/null"))
;;   (define-key global-map (kbd "C-c C-r") 'redpen-paragraph)
;;   (add-hook 'kill-emacs-hook
;;             (lambda ()
;;               (if (file-exists-p redpen-temporary-filename)
;;                   (delete-file redpen-temporary-filename))))
;;
;; You can add how to get paragraph by `redpen-paragraph-alist'.
;; `org-mode' setting is enabled by default.
;;
;;   (with-eval-after-load "org"
;;     (defvar org-mode-map)
;;     ;; Override `org-reveal' by `global-map' or set other key.
;;     (define-key org-mode-map (kbd "C-c C-r") nil))
;;
;; You may need extra setting for convenient.
;; - `popwin-mode' for closing buffer. (but often not work...)
;;
;;   (require 'popwin)
;;   (popwin-mode 1)
;;   (push '(compilation-mode :noselect t) popwin:special-display-config)
;;
;; - save hook
;;
;;   (add-hook 'after-save-hook
;;             (lambda ()
;;               (if (eq major-mode 'org-mode)
;;                   (let ((redpen-paragraph-force-reading-whole t))
;;                     (redpen-paragraph)))))

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'json)

(defgroup redpen-paragraph nil
  "RedPen interface for proofreading paragraph."
  :group 'redpen-paragraph)

(defvar redpen-commands
  ;; This setting is demo use only.
  `(,(concat
      "curl -s --data-urlencode document@%s"
      " --data documentParser=PLAIN --data format=json"
      " --data lang=en" ;; for english
      ;; " --data-urlencode config@/path/to/redpen-conf-en.xml"
      " http://redpen-paragraph-demo.herokuapp.com/rest/document/validate/")
    ,(concat
      "curl -s --data-urlencode document@%s"
      " --data documentParser=PLAIN --data format=json"
      " --data lang=ja" ;; for not english
      ;; " --data-urlencode config@/path/to/redpen-conf-ja.xml"
      " http://redpen-paragraph-demo.herokuapp.com/rest/document/validate/"))
  "Define redpen commands. 1st is for english, 2nd is for other language.")

(defvar redpen-paragraph-force-english nil
  "Force English without detecting.")

(defvar redpen-paragraph-force-reading-whole nil
  "Force reading the whole file.")

(defvar redpen-encoding 'utf-8
  "Encoding for redpen I/O.")

(defvar redpen-temporary-filename
  (expand-file-name
   (format "redpen.%s" (emacs-pid)) temporary-file-directory)
  "Filename passed to rendpen.")

(defvar redpen-target-filename ""
  "Editing filename.")

(defvar redpen-paragraph-compilation-buffer-name
  "*compilation*" "Compilation buffer name.")

(defvar redpen-paragraph-beginning-position
  '(0 . 0) "Position of the paragraph beginning. (lineNum . offset)")

;; eg. 1:1:1:1: ValidationError[StartWithCapitalLetter], Sentence starts with a lowercase character "t". at line: test
(defvar redpen-paragraph-input-pattern
  "%s at start %d.%d, end %d.%d: %s\n"
  "Adjust to suit the input regexp.")
(defvar redpen-paragraph-input-regexp
  "^\\sw+ at start \\([0-9]+\\).\\([0-9]+\\), end \\([0-9]+\\).\\([0-9]+\\): .*$"
  "Adjust to suit the input pattern.

regexp capture & bind list
1st: lineNum | startPosition.lineNum
2nd: lineNum | endPosition.lineNum
3rd: 1 | startPosition.offset
4th: 1 | endPosition.offset")

(autoload 'org-backward-paragraph "org")
(autoload 'org-forward-paragraph "org")
(defvar redpen-paragraph-alist
  (list
   `(org-mode
     . ,(lambda () "get visible string on current paragraph."
          (let ((end (if (use-region-p) (1- (region-end))
                       (org-forward-paragraph) (1- (point))))
                (begin (if (use-region-p) (region-beginning)
                         (org-backward-paragraph) (point))))
            (apply 'string
                   (cl-loop
                    for pos from begin to end
                    when (not (get-text-property pos 'invisible))
                    collect (char-after pos)))))))
  "Define how to get paragraph on specific major mode.")

(defun redpen-paragraph-is-english (text)
  "Detect language by TEXT."
  (let* ((full (length text))
         (not-english
          (length (replace-regexp-in-string "[\x21-\x7e]" "" text)))
         (english (- full not-english)))
    (> english not-english)))

;;;###autoload
(defun redpen-paragraph (&optional flag)
  "Profread some paragraphs by redpen.

if FLAG is not nil, use second command in `redpen-commands'."
  (interactive "P")
  (setq redpen-target-filename buffer-file-name)
  (let* ((coding-system-for-write redpen-encoding) ; for writing file
         (coding-system-for-read redpen-encoding) ; for reading stdout
         (is-whole (or redpen-paragraph-force-reading-whole
                       (not (null flag)))) ;; for C-u flag
         (handler (cdr (assq major-mode redpen-paragraph-alist)))
         (default-handler
           (lambda ()
             (unless (use-region-p) (mark-paragraph))
             (buffer-substring-no-properties (region-beginning) (region-end))))
         (string (save-excursion
                   (funcall (or handler default-handler))))
         (is-english (or redpen-paragraph-force-english
                         (redpen-paragraph-is-english string)))
         (command
          (let ((template (nth (if is-english 0 1) redpen-commands)))
            (format template
                    (if is-whole
                        buffer-file-name redpen-temporary-filename)))))
    (with-temp-file redpen-temporary-filename (insert string))
    (setq redpen-paragraph-beginning-position
          (if is-whole '(0 . 0)
            (save-excursion
              (if (use-region-p) (goto-char (region-beginning))
                (backward-paragraph))
              (cons (1- (line-number-at-pos))
                    (current-column)))))
    (with-current-buffer
        (get-buffer-create redpen-paragraph-compilation-buffer-name)
      (async-shell-command command (current-buffer))
      (set-process-sentinel
       (get-buffer-process (current-buffer))
       'redpen-paragraph-sentinel))))

(defun redpen-paragraph-sentinel (proc desc)
  "Sentinel for redpen-paragraph compilation buffers."
  (message "Compilation %s at %s"
           (replace-regexp-in-string "\n?$" "" desc)
           (substring (current-time-string) 0 19))
  (if (memq (process-status proc) '(exit signal))
      (with-current-buffer
          redpen-paragraph-compilation-buffer-name
        ;; Erase for showing the errors after reading the raw result.
        (let ((json (json-read-from-string (buffer-string))))
          (erase-buffer)
          (redpen-paragraph-list-errors json)))))

(defun redpen-paragraph-list-errors (json)
  "Show the error list for the current buffer by RedPen."
  ;; Split window as well as usual compilation-mode.
  (switch-to-buffer-other-window (current-buffer))
  (set-buffer redpen-paragraph-compilation-buffer-name)
  (mapc
   (lambda (err)
     (let* ((validator (or (cdr (assoc 'validator err)) ""))
            (message (or (cdr (assoc 'message err)) ""))
            (sentence (or (cdr (assoc 'sentence err)) ""))
            ;; lineNum is still used on RedPen
            (lineNum (or (cdr (assoc 'lineNum err)) 0))
            (start-pos (cdr (assoc 'startPosition err)))
            (start-lineNum
             (+ (car redpen-paragraph-beginning-position)
                (or (cdr (assoc 'lineNum start-pos))
                    lineNum)))
            (start-offset
             (+ 1 (cdr redpen-paragraph-beginning-position)
                (or (cdr (assoc 'offset start-pos))
                    ;; sentenceStartColumnNum is still used on RedPen
                    (if (assoc 'sentenceStartColumnNum err)
                        (1+ (cdr (assoc 'sentenceStartColumnNum err))))
                    0)))
            (end-pos (cdr (assoc 'endPosition err)))
            (end-lineNum
             (+ (car redpen-paragraph-beginning-position)
                (or (cdr (assoc 'lineNum end-pos))
                    lineNum)))
            (end-offset
             (+ (cdr redpen-paragraph-beginning-position)
                (or (cdr (assoc 'offset end-pos)) 0))))
       (insert (format
                redpen-paragraph-input-pattern
                validator
                ;; Emacs displays from the 1st line.
                (if (eq start-lineNum 0) 1 start-lineNum)
                start-offset
                (if (eq end-lineNum 0) 1 end-lineNum)
                (if (> start-offset end-offset) start-offset end-offset)
                message))
       (if (> (length sentence) 0)
           (insert sentence "\n"))
       (insert "\n")))
   (cl-sort
    (cdr (assoc 'errors json))
    (lambda (a b)
      (< (cdr (assoc 'lineNum a)) (cdr (assoc 'lineNum b))))))
  ;; According to `redpen-paragraph-input-regexp',
  ;; Parse `redpen-paragraph-input-pattern' in `compilation-mode'.
  (compilation-mode)
  (goto-char (point-min)))

(eval-after-load "compile"
  '(progn
     (add-to-list 'compilation-error-regexp-alist 'redpen-paragraph)
     (add-to-list
      'compilation-error-regexp-alist-alist
      `(redpen-paragraph
        ,redpen-paragraph-input-regexp
        redpen-target-filename
        (1 . 3) (2 . 4)))))

(defun redpen-target-filename ()
  "Return `redpen-target-filename'."
  redpen-target-filename)

(provide 'redpen-paragraph)

;; Local Variables:
;; coding: utf-8
;; End:

;;; redpen-paragraph.el ends here
