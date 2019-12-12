;;; native-complete.el --- shell completion using native complete mechanisms -*- lexical-binding: t; -*-

;; Copyright (C) 2019 by Troy Hinckley

;; Author: Troy Hinckley <troy.hinckley@gmail.com>
;; URL: github.com/CeleritasCelery/native-complete
;; Version: 0.1.0
;; Package-Requires: ((emacs "25"))

(defvar native-complete-command "")
(defvar native-complete-prefix "")
(defvar native-complete-common "")
(defvar native-complete-redirection-command "")
(defvar native-complete-buffer " *native-complete redirect*")

(defcustom native-complete-exclude-regex (rx (not (in alnum "-_~/*.+")))
  "Regex of elements to ignore when generating candidates.
Any candidates matching this regex will not be included in final
  list of candidates.")

(defcustom native-complete-style-regex-alist
  `(("╰─→ " . bash)
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

(defun native-complete-setup-bash ()
  "Setup support for native-complete-enabled bash shells.
This involves not sending the --noediting argument as well as not
setting the `INSIDE_EMACS' environment variable."
  (interactive)
  (advice-add 'comint-term-environment
              :filter-return 'native-complete-unset-inside-emacs)
  (with-eval-after-load 'shell
    (setq explicit-bash-args
          (delete "--noediting" explicit-bash-args))))

(defun native-complete-unset-inside-emacs (env)
  "Remove INSIDE_EMACS from term envrionment."
  (cons "INSIDE_EMACS=" env))

(defun native-complete--excluded (x)
  "Remove unwanted candidates from list."
  (string-match-p native-complete-exclude-regex x))

(defun native-complete-get-completion-style ()
  "Get the completion style based on current prompt."
  (cl-loop for (regex . style) in native-complete-style-regex-alist
           if (looking-back regex)
           return style))

(defun native-complete-get-prefix ()
  "Setup output redirection to query the source shell."
  (let* ((redirect-buffer (get-buffer-create native-complete-buffer))
         (proc (get-buffer-process (current-buffer)))
         (beg (process-mark proc))
         (end (point))
         (str (buffer-substring-no-properties beg end))
         (word-start (or (cl-search " " str :from-end t) -1))
         (env-start (or (cl-search "$" str :from-end t) -1))
         (path-start (or (cl-search "/" str :from-end t) -1))
         (prefix-start (1+ (max word-start env-start path-start)))
         (style (cl-letf (((point) beg)) (native-complete-get-completion-style)))
         ;; sanity check makes sure the input line is empty, which is
         ;; not useful when doing input completion
         (comint-redirect-perform-sanity-check nil))
    (with-current-buffer redirect-buffer (erase-buffer))
    (setq native-complete-common (substring str (1+ word-start)
                                            prefix-start))
    (setq native-complete-command str)
    (setq native-complete-prefix (substring str prefix-start))
    (setq native-complete-redirection-command
          (concat str (pcase style
                        (`bash "\e* echo ")
                        (`zsh "")
                        (_ "\t"))))))

(defun native-complete-get-completions ()
  "Using the redirection output get all completion candidates."
  (let* ((cmd (string-remove-suffix
               native-complete-prefix
               native-complete-command))
         (echo-cmd (concat (regexp-quote native-complete-command) "[]"))
         (buffer (with-current-buffer native-complete-buffer
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
      (cl-remove-if 'native-complete--excluded)
      (mapcar (lambda (x) (string-remove-prefix native-complete-common x)))
      (mapcar (lambda (x) (string-remove-suffix "*" x)))
      (delete-dups))))

(defun native-complete-at-point ()
  "Get the candidates that would be triggered by using TAB on an
interactive shell."
  (native-complete-get-prefix)
  (comint-redirect-send-command
   native-complete-redirection-command
   native-complete-buffer nil t)
  (unwind-protect
      (while (or quit-flag (null comint-redirect-completed))
        (accept-process-output nil 0.1))
    (unless comint-redirect-completed
      (comint-redirect-cleanup)))
  (list (- (point) (length native-complete-prefix))
        (point)
        (native-complete-get-completions)))


(defun company-native-complete-candidates (callback)
  "Get candidates for company-native-complete"
  (add-hook 'comint-redirect-hook
            (lambda ()
              (setq comint-redirect-hook nil)
              (funcall callback (native-complete-get-completions))))
  (comint-redirect-send-command
   native-complete-redirection-command
   native-complete-buffer nil t))

(defun company-native-complete-prefix ()
  "Get prefix for company-native-complete"
  (native-complete-get-prefix)
  (cond
   ((string-prefix-p "-" native-complete-prefix)
    (cons native-complete-prefix t))
   ((string-match-p "/" native-complete-common)
    (cons native-complete-prefix
          (+ (length native-complete-common)
             (length native-complete-prefix))))
   (t native-complete-prefix)))

(defun company-native-complete (command &optional arg &rest ignored)
  "Completion for native native-complete functionality."
  (interactive '(interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-native-complete))
    (prefix (company-native-complete-prefix))
    (candidates (cons :async 'company-native-complete-candidates))
    (ignore-case t)))

