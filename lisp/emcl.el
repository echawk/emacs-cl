;;;; -*- mode: emacs-lisp; lexical-binding: nil -*-
;;; emcl.el --- command-line driver for emacs-cl, used by bin/emcl

;;; Commentary:

;; Loaded by bin/emcl as `emacs --batch -l emcl.el -- ARGS...'.  Processes
;; ARGS the way SBCL processes its toplevel options, then either runs a
;; REPL on stdin or exits.  Never returns to Emacs's own command-line
;; processing.

;;; Code:

(defconst emcl-version "0.5")

(defconst emcl-usage "Usage: emcl [options]

Options are processed left to right:
  --load FILE         Load a Common Lisp source file.
  --eval FORM         Read and evaluate FORM.
  --script FILE       Load FILE and exit (a leading #! line is ignored).
                      Arguments after FILE are not processed.
  --non-interactive   Exit after processing options instead of starting
                      a REPL; errors exit with status 1.
  --quit              Same as --non-interactive.
  --noinform          Don't print the startup banner.
  --version           Print the version and exit.
  --help              Print this message and exit.
")

(defvar emcl-script-args nil
  "Arguments following --script FILE.")

(defvar emcl--interactive t
  "When nil, an unhandled error exits with status 1.")

(defun emcl--die (format-string &rest args)
  (princ (concat "emcl: " (apply #'format format-string args) "\n")
	 #'external-debugging-output)
  (kill-emacs 1))

;;; Loading the system quietly.

(defun emcl--load-system ()
  (let ((inhibit-message t)
	(standard-output #'ignore))
    (load (expand-file-name "load-cl.el"
			    (file-name-directory load-file-name))
	  nil t)))

;;; Streams.

(defvar emcl--stdin-pending "")
(defvar emcl--stdin-eof nil)

(defun emcl--stdin-read (stream)
  (when (and (equal emcl--stdin-pending "") (not emcl--stdin-eof))
    ;; In batch mode, read-from-minibuffer reads a line from stdin and
    ;; signals an error at end of file.
    (condition-case nil
	(setq emcl--stdin-pending (concat (read-from-minibuffer "") "\n"))
      (error (setq emcl--stdin-eof t))))
  (if (equal emcl--stdin-pending "")
      :eof
    (prog1 (cl-char (aref emcl--stdin-pending 0))
      (setq emcl--stdin-pending (substring emcl--stdin-pending 1)))))

(defun emcl--make-stderr-stream ()
  (MAKE-STREAM (kw direction) (kw OUTPUT)
	       (kw write-fn)
	       (lambda (char stream)
		 (external-debugging-output (el-char char)))))

(defun emcl--setup-streams ()
  ;; A nil position makes UNREAD-CHAR use the unread buffer rather than
  ;; rewinding, which a line-at-a-time stdin can't do.
  (setq *STANDARD-INPUT* (MAKE-STREAM (kw direction) (kw INPUT)
				      (kw read-fn) #'emcl--stdin-read
				      (kw position) nil)
	*STANDARD-OUTPUT* (make-princ-stream)
	*ERROR-OUTPUT* (emcl--make-stderr-stream)
	*TRACE-OUTPUT* *ERROR-OUTPUT*
	*TERMINAL-IO* (MAKE-TWO-WAY-STREAM *STANDARD-INPUT* *STANDARD-OUTPUT*)
	*QUERY-IO* *TERMINAL-IO*
	*DEBUG-IO* *TERMINAL-IO*))

;;; Errors.  The built-in debugger is interactive and can't work in batch
;;; mode, so report the condition and unwind instead, like SBCL's
;;; --disable-debugger.

(defun emcl--condition-message (condition)
  (or (ignore-errors
	(cond
	  ((TYPEP condition 'UNDEFINED-FUNCTION)
	   (FORMAT nil "The function ~S is undefined."
		   (CELL-ERROR-NAME condition)))
	  ((TYPEP condition 'UNBOUND-VARIABLE)
	   (FORMAT nil "The variable ~S is unbound."
		   (CELL-ERROR-NAME condition)))
	  ((TYPEP condition 'TYPE-ERROR)
	   (FORMAT nil "The value ~S is not of type ~S."
		   (TYPE-ERROR-DATUM condition)
		   (TYPE-ERROR-EXPECTED-TYPE condition)))))
      (ignore-errors
	(apply #'FORMAT nil
	       (SIMPLE-CONDITION-FORMAT-CONTROL condition)
	       (SIMPLE-CONDITION-FORMAT-ARGUMENTS condition)))
      (ignore-errors (PRINC-TO-STRING condition))
      (format "%S" (TYPE-OF condition))))

(defun emcl--report (condition)
  (FORMAT *ERROR-OUTPUT* "~&Unhandled ~A: ~A~%"
	  (TYPE-OF condition) (emcl--condition-message condition))
  (FINISH-OUTPUT *ERROR-OUTPUT*))

(defun emcl--debugger-hook (condition hook)
  (emcl--report condition)
  (if emcl--interactive
      (throw 'emcl-toplevel nil)
    (kill-emacs 1)))

(defmacro emcl--protect (&rest body)
  "Run BODY, unwinding to here after a reported error."
  `(catch 'emcl-toplevel
     (condition-case err
	 (progn ,@body)
       (error
	(princ (format "emcl: %s\n" (error-message-string err))
	       #'external-debugging-output)
	(unless emcl--interactive
	  (kill-emacs 1))))))

;;; Actions.

(defun emcl--eval-string (string)
  (EVAL (READ-FROM-STRING string)))

(defun emcl--load-file (file)
  (unless (file-exists-p file)
    (emcl--die "no such file: %s" file))
  (LOAD (expand-file-name file)))

(defun emcl--banner ()
  (FORMAT T "Emacs Common Lisp ~A, running on GNU Emacs ~A~%"
	  emcl-version emacs-version))

(defun emcl--repl ()
  (let ((eof (make-symbol "eof")))
    (while t
      (emcl--protect
       (FRESH-LINE)
       (WRITE-STRING "* ")
       (FINISH-OUTPUT)
       (let ((form (READ *STANDARD-INPUT* nil eof)))
	 (when (eq form eof)
	   (FRESH-LINE)
	   (kill-emacs 0))
	 (dolist (value (emacs-cl-eval-interactively form))
	   (PRIN1 value)
	   (TERPRI)))))))

;;; Command line.

(defun emcl-main (args)
  (when (equal (car args) "--")
    (pop args))
  (let ((actions nil)
	(repl t)
	(banner t))
    (while args
      (let ((arg (pop args)))
	(cond
	  ((member arg '("--help" "-h"))
	   (princ emcl-usage)
	   (kill-emacs 0))
	  ((equal arg "--version")
	   (princ (format "emcl %s\n" emcl-version))
	   (kill-emacs 0))
	  ((member arg '("--load" "--eval"))
	   (unless args
	     (emcl--die "%s requires an argument" arg))
	   (push (cons arg (pop args)) actions))
	  ((equal arg "--script")
	   (unless args
	     (emcl--die "--script requires a file"))
	   (push (cons arg (pop args)) actions)
	   (setq emcl-script-args args
		 args nil
		 repl nil
		 banner nil))
	  ((member arg '("--non-interactive" "--quit"))
	   (setq repl nil))
	  ((equal arg "--noinform")
	   (setq banner nil))
	  (t
	   (emcl--die "unknown option: %s (try --help)" arg)))))
    (setq emcl--interactive repl)
    (emcl--load-system)
    (emcl--setup-streams)
    (setq *DEBUGGER-HOOK* #'emcl--debugger-hook)
    (dolist (action (nreverse actions))
      (emcl--protect
       (pcase (car action)
	 ("--load" (emcl--load-file (cdr action)))
	 ("--eval" (emcl--eval-string (cdr action)))
	 ("--script" (emcl--load-file (cdr action))))))
    (when repl
      (when banner
	(emcl--banner))
      (emcl--repl))
    (FINISH-OUTPUT)
    (kill-emacs 0)))

(emcl-main (prog1 command-line-args-left
	     (setq command-line-args-left nil)))

;;; emcl.el ends here
