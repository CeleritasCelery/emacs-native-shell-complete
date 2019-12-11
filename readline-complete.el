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
(defcustom readline-exclude-regex (rx (not (in alnum "-_~/*.+")))
  "Regex of elements to ignore when generating candidates.
Any candidates matching this regex will not be included in final
  list of candidates.")
(defcustom readline-style-regex-alist `(("╰─→ " . bash)
                                        (,(rx (or "% " "[0-9]+> ")) . zsh)
                                        ("[A-Z]+> " . tab))
  "An alist of prompt regex and their completion mechanisms.
the car of each alist element is a regex matching the prompt for
a particular shell type. The cdr is one of the following symbols
`bash', `zsh', or `tab'.

- `bash' style uses `M-*' and `echo'
- `zsh' style uses `C-D'
- `tab' style uses `TAB'

You may need to test this on an line editing enabled shell to see
which of these options a particular shell supports. Most shells
support basic TAB completion, but some will not echo the
candidate to output when it is the sole completion. Hence the
need for the other methods as well.")

(defun readline-setup-bash ()
  "Setup support for readline-enabled bash shells.
This involves not sending the --noediting argument as well as not
setting the `INSIDE_EMACS' environment variable."
  (interactive)
  (advice-add 'comint-term-environment
              :filter-return 'readline-unset-inside-emacs)
  (with-eval-after-load 'shell
    (setq explicit-bash-args
          (delete "--noediting" explicit-bash-args))))

(defun readline-unset-inside-emacs (env)
  "Remove INSIDE_EMACS from term envrionment."
  (cons "INSIDE_EMACS=" env))

(defun readline--excluded (x)
  "Remove unwanted candidates from list."
  (string-match-p readline-exclude-regex x))

(defun readline-get-completion-style ()
  "Get the completion style based on current prompt."
  (cl-loop for (regex . style) in readline-style-regex-alist
           if (looking-back regex)
           return style))

(defun readline-get-prefix ()
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
         (style (cl-letf (((point) beg)) (readline-get-completion-style)))
         ;; sanity check makes sure the input line is empty, which is
         ;; not useful when doing input completion
         (comint-redirect-perform-sanity-check nil))
    (with-current-buffer redirect-buffer (erase-buffer))
    (setq readline-common (substring str (1+ word-start) prefix-start))
    (setq readline-command str)
    (setq readline-prefix (substring str prefix-start))
    (setq readline-redirection-command
          (concat str (pcase style
                        (`bash "\e* echo ")
                        (`zsh "")
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
      ;; In this case the solution is to increase the number of
      ;; candidates that can be displayed without query.
      (error "Too many candidates to display"))
    (thread-last (split-string buffer "\n\n")
      (car)
      (ansi-color-filter-apply)
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
  (readline-get-prefix)
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
  (readline-get-prefix)
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

