;;; readline-complete.el --- shell completion using readline style interactions  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 by Troy Hinckley

;; Author: Troy Hinckley <troy.hinckley@gmail.com>
;; URL: https://github.com/CeleritasCelery/readline-complete
;; Version: 0.1.0
;; Package-Requires: ((emacs "25"))

(defvar readline-command "")
(defvar readline-prefix "")
(defvar readline-common "")
(defvar readline-redirection-command "")
(defvar readline-buffer " *readline redirect*")
(defcustom readline-exclude-regex (rx (not (in alnum "-_~/*.+"))))
(defcustom readline-style-regex-alist `(("╰─→ " . bash)
                                        (,(rx (or "% " "[0-9]+> ")) . csh)
                                        ("[A-Z]+> " . tab)))

(defun readline--excluded (x)
  "Remove unwanted candidates from list."
  (string-match-p readline-exclude-regex x))

(defun readline-setup ()
  "Setup output redirection to query the source shell."
  (let* ((redirect-buffer (get-buffer-create readline-buffer))
         (proc (get-buffer-process (current-buffer)))
         (beg (process-mark proc))
         (end (point))
         (str (buffer-substring-no-properties beg end))
         (word-start (or (cl-search " " str :from-end t) -1))
         (env-start (or (cl-search "$" str :from-end t) -1))
         (path-start (or (cl-search "/" str :from-end t) -1))
         (prefix-start (1+ (max word-start env-start path-start)))
         (style (cl-letf (((point) beg))
                  (cl-loop for (regex . style) in readline-style-regex-alist
                           if (looking-back regex)
                           return style)))
         (comint-redirect-perform-sanity-check nil))
    (with-current-buffer redirect-buffer (erase-buffer))
    (setq readline-common (substring str (1+ word-start) prefix-start))
    (setq readline-command str)
    (setq readline-prefix (substring str prefix-start))
    (setq readline-redirection-command
          (concat str (pcase style
                        (`bash "\e* echo ")
                        (`csh "")
                        (_ "\t"))))))

(defun readline-get-completions ()
  "Using the redirection output get all completion candidates."
  (let* ((cmd (string-remove-suffix
               readline-prefix
               readline-command))
         (echo-cmd (concat (regexp-quote readline-command) "[]"))
         (buffer (with-current-buffer readline-buffer
                   (buffer-string))))
    (when (or (string-match-p "There are [0-9]+ rows, list them anyway" buffer)
              (string-match-p "Display all [0-9]+ possibilities" buffer))
      (error "Too many candidates to display"))
    (thread-last (split-string buffer "\n\n")
      (car)
      (ansi-color-filter-apply)
      (setq foo)
      (replace-regexp-in-string echo-cmd "")
      (string-remove-prefix cmd)
      (split-string)
      (cl-remove-if 'readline--excluded)
      (mapcar (lambda (x) (string-remove-prefix readline-common x)))
      (mapcar (lambda (x) (string-remove-suffix "*" x)))
      (delete-dups))))


(defun readline-capf ()
  "Get the candidates that would be triggered by using TAB on an
interactive shell."
  (readline-setup)
  (comint-redirect-send-command
   readline-redirection-command
   readline-buffer nil t)
  (unwind-protect
      (while (or quit-flag (null comint-redirect-completed))
        (accept-process-output nil 0.1))
    (unless comint-redirect-completed
      (comint-redirect-cleanup)))
  (list (- (point) (length readline-prefix))
        (point)
        (readline-get-completions)))


(defun company-readline-candidates (callback)
  "Get candidates for company-readline"
  (add-hook 'comint-redirect-hook
            (lambda ()
              (setq comint-redirect-hook nil)
              (funcall callback (readline-get-completions))))
  (comint-redirect-send-command
   readline-redirection-command
   readline-buffer nil t))

(defun company-readline-prefix ()
  "Get prefix for company-readline"
  (readline-setup)
  (cond
   ((string-prefix-p "-" readline-prefix)
    (cons readline-prefix t))
   ((string-match-p "/" readline-common)
    (cons readline-prefix
          (+ (length readline-common)
             (length readline-prefix))))
   (t readline-prefix)))

(defun company-readline (command &optional arg &rest ignored)
  "Completion for native readline functionality."
  (interactive '(interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-readline))
    (prefix (company-readline-prefix))
    (candidates (cons :async 'company-readline-candidates))
    (ignore-case t)))

