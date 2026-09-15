;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; Functions for loading and compiling the whole system.
;;; Loading this file also loads the system as a side effect.

(let ((inhibit-message t))
  (require 'cl))
(require 'byte-compile "bytecomp")

(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

;;; Fake IN-PACKAGE and FIND-PACKAGE until they are defined properly
;;; in cl-packages.el.
(defmacro IN-PACKAGE (name) nil)
(defun FIND-PACKAGE (name) nil)

(unless (fboundp 'cl-mapcar-many)
  (fset 'cl-mapcar-many (symbol-function 'cl--mapcar-many)))

(defvar *cl-files*
  '("utils"
    "func"

    "cl-evaluation"
    "cl-flow"
    "cl-numbers"
    "cl-conses"
    "cl-characters"
    "cl-strings"
    "cl-arrays"
    "cl-sequences"
    "cl-structures"
    "cl-iteration"

    "cl-symbols"
    "cl-packages"

    "cl-types"
    "cl-typep"
    "cl-subtypep"

    "cl-hash"
    "cl-streams"
    "cl-reader"
    "cl-printer"
    "cl-environment"
    "cl-filenames"
    "cl-files"
    "interaction"
    "cl-eval"
    "cl-system"

    "cl-loop"
    "cl-format"
    "cl-compile"
    "cl-objects"
    "cl-conditions"

    "populate"))

(defvar *cl-eval-buffer-files*
  '("cl-compile"
    "cl-objects"
    "cl-conditions"))

(defun load-cl-file (file)
  (if (member file *cl-eval-buffer-files*)
      (let* ((path (locate-library (concat file ".el")))
	     (load-file-name path)
	     (load-in-progress t))
	(unless path
	  (error "Cannot find %s" file))
	(message "Loading %s (source)..." path)
	(with-temp-buffer
	  (insert-file-contents path)
	  (condition-case nil
	      (while t
		(eval (read (current-buffer))))
	    (end-of-file t))))
    (load file)
    (message "Loaded %s..." file)))

(defun load-cl ()
  (interactive)
  (let ((load-path (cons (file-name-directory load-file-name) load-path))
	(debug-on-error t)
	(byte-compile-warnings nil)
	(macroexp-inhibit-compiler-macros t)
	(warning-minimum-level :error)
	(warning-suppress-types '((obsolete))))
    (let ((old-byte-compile-warn-obsolete
	   (when (fboundp 'byte-compile-warn-obsolete)
	     (symbol-function 'byte-compile-warn-obsolete)))
	  (old-macroexp--obsolete-warning
	   (when (fboundp 'macroexp--obsolete-warning)
	     (symbol-function 'macroexp--obsolete-warning))))
      (unwind-protect
	  (progn
	    (when old-byte-compile-warn-obsolete
	      (fset 'byte-compile-warn-obsolete (lambda (&rest args) nil)))
	    (when old-macroexp--obsolete-warning
	      (fset 'macroexp--obsolete-warning (lambda (&rest args) nil)))
	    (mapc #'load-cl-file *cl-files*)
	    (populate-packages)
	    (garbage-collect))
	(when old-byte-compile-warn-obsolete
	  (fset 'byte-compile-warn-obsolete old-byte-compile-warn-obsolete))
	(when old-macroexp--obsolete-warning
	  (fset 'macroexp--obsolete-warning old-macroexp--obsolete-warning))))))

(defun compile-cl ()
  (interactive)
  (let ((byte-compile-warnings '(not cl-functions)))
    (dolist (file *cl-files*)
      (byte-compile-file (concat file ".el")))))

(when (string-match "^19" emacs-version)
  (dolist (x '(:required :optional-rest :weakness :type :read-only
	       :constituent :whitespace :single-escape
	       :multiple-escape :terminating-macro
	       :non-terminating-macro :eof :special-operator
	       :lexical :special :macro :symbol-macro))
    (set x x)))

(setq *global-environment*
      (vector 'environment
	      ;; Variable information
	      nil nil nil
	      ;; Function information
	      nil nil nil
	      ;; Block and tagbody information
	      nil nil))

(load-cl)
(IN-PACKAGE "CL-USER")
