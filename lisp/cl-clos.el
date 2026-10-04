;;;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; cl-clos.el --- fit closette's CLOS into the rest of emacs-cl

;;; Commentary:

;; Loaded by `load-closette' right after closette.lisp.  closette is a
;; self-contained CLOS; this file connects it to emacs-cl's type system,
;; structures, and conditions.

;;; Code:

(setq *std-instance-tag* (FIND-SYMBOL "STD-INSTANCE" "CLOSETTE"))

;;;; MAKE-LOAD-FORM.  Its T method, built-in-make-load-form in
;;;; cl-compile.el, is needed by COMPILE-FILE for literals that aren't
;;;; trivially printable (e.g. shared or circular structure).

(EVAL '(PROGN
	(DEFGENERIC MAKE-LOAD-FORM (OBJECT &OPTIONAL ENV))
	(DEFMETHOD MAKE-LOAD-FORM ((OBJECT T) &OPTIONAL ENV)
	  (built-in-make-load-form OBJECT ENV))
	(DEFMETHOD MAKE-LOAD-FORM ((OBJECT STANDARD-OBJECT) &OPTIONAL ENV)
	  (ERROR 'ERROR))))

;;;; Metaobject type names.  closette makes every class, built-in ones
;;;; included, an instance of STANDARD-CLASS and has no CLASS,
;;;; BUILT-IN-CLASS, GENERIC-FUNCTION or METHOD classes; provide the ANSI
;;;; type names on top of its model.

(defconst clos-built-in-class-names
  '(T SYMBOL SEQUENCE ARRAY NUMBER CHARACTER FUNCTION HASH-TABLE PACKAGE
    PATHNAME READTABLE STREAM LIST NULL CONS VECTOR BIT-VECTOR STRING
    COMPLEX INTEGER FLOAT REAL RATIONAL RATIO RANDOM-STATE LOGICAL-PATHNAME
    BROADCAST-STREAM CONCATENATED-STREAM ECHO-STREAM FILE-STREAM
    STRING-STREAM SYNONYM-STREAM TWO-WAY-STREAM))

(ensure-type 'CLASS (lambda (obj _env) (and (classp obj) 'T)))
(ensure-type 'BUILT-IN-CLASS
	     (lambda (obj _env)
	       (and (classp obj)
		    (memq (clos-call "CLASS-NAME" obj) clos-built-in-class-names)
		    'T)))
(ensure-type 'GENERIC-FUNCTION
	     (lambda (obj _env) (TYPEP obj 'STANDARD-GENERIC-FUNCTION)))
(ensure-type 'METHOD (lambda (obj _env) (TYPEP obj 'STANDARD-METHOD)))
(ensure-type 'STRUCTURE-CLASS (lambda (_obj _env) nil))

;;;; Conditions.  The built-in conditions are structures (see
;;;; cl-conditions.el), while DEFINE-CONDITION in Common Lisp code makes
;;;; CLOS classes.  Give every ANSI condition type a class, with its
;;;; standard slots, so Common Lisp conditions can inherit from them;
;;;; MAKE-CONDITION, TYPEP and the accessors below accept instances of
;;;; either kind.  CONDITION inherits from STANDARD-OBJECT, which ANSI
;;;; permits, because closette's instance protocol is defined on it.

(defconst clos-condition-classes
  '((CONDITION (STANDARD-OBJECT))
    (WARNING (CONDITION))
    (STYLE-WARNING (WARNING))
    (SERIOUS-CONDITION (CONDITION))
    (ERROR (SERIOUS-CONDITION))
    (STORAGE-CONDITION (SERIOUS-CONDITION))
    (SIMPLE-CONDITION (CONDITION)
     ("FORMAT-CONTROL" . SIMPLE-CONDITION-FORMAT-CONTROL)
     ("FORMAT-ARGUMENTS" . SIMPLE-CONDITION-FORMAT-ARGUMENTS))
    (SIMPLE-ERROR (SIMPLE-CONDITION ERROR))
    (SIMPLE-WARNING (SIMPLE-CONDITION WARNING))
    (TYPE-ERROR (ERROR)
     ("DATUM" . TYPE-ERROR-DATUM)
     ("EXPECTED-TYPE" . TYPE-ERROR-EXPECTED-TYPE))
    (SIMPLE-TYPE-ERROR (SIMPLE-CONDITION TYPE-ERROR))
    (PROGRAM-ERROR (ERROR))
    (CONTROL-ERROR (ERROR))
    (CELL-ERROR (ERROR) ("NAME" . CELL-ERROR-NAME))
    (UNBOUND-VARIABLE (CELL-ERROR))
    (UNDEFINED-FUNCTION (CELL-ERROR))
    (UNBOUND-SLOT (CELL-ERROR) ("INSTANCE" . UNBOUND-SLOT-INSTANCE))
    (PACKAGE-ERROR (ERROR) ("PACKAGE" . PACKAGE-ERROR-PACKAGE))
    (STREAM-ERROR (ERROR) ("STREAM" . STREAM-ERROR-STREAM))
    (END-OF-FILE (STREAM-ERROR))
    (PARSE-ERROR (ERROR))
    (READER-ERROR (PARSE-ERROR STREAM-ERROR))
    (FILE-ERROR (ERROR) ("PATHNAME" . FILE-ERROR-PATHNAME))
    (ARITHMETIC-ERROR (ERROR)
     ("OPERATION" . ARITHMETIC-ERROR-OPERATION)
     ("OPERANDS" . ARITHMETIC-ERROR-OPERANDS))
    (DIVISION-BY-ZERO (ARITHMETIC-ERROR))
    (FLOATING-POINT-INVALID-OPERATION (ARITHMETIC-ERROR))
    (FLOATING-POINT-INEXACT (ARITHMETIC-ERROR))
    (FLOATING-POINT-OVERFLOW (ARITHMETIC-ERROR))
    (FLOATING-POINT-UNDERFLOW (ARITHMETIC-ERROR))
    (PRINT-NOT-READABLE (ERROR) ("OBJECT" . PRINT-NOT-READABLE-OBJECT)))
  "Each entry is (NAME SUPERCLASSES (SLOT-NAME . ACCESSOR)...).")

(defun clos-slot-name (name)
  (INTERN name "EMACS-CL"))

(dolist (entry clos-condition-classes)
  (EVAL `(DEFCLASS ,(car entry) ,(cadr entry)
	   ,(mapcar (lambda (slot)
		      (list (clos-slot-name (car slot))
			    (keyword "INITARG") (keyword (car slot))
			    (keyword "INITFORM") nil))
		    (cddr entry)))))

;;; Make the accessors work on Common Lisp conditions too.
(dolist (entry clos-condition-classes)
  (dolist (slot (cddr entry))
    (let ((slot-name (clos-slot-name (car slot)))
	  (accessor (cdr slot)))
      (if (fboundp accessor)
	  (advice-add accessor :around
		      (lambda (orig object)
			(if (clos-instance-p object)
			    (clos-call "SLOT-VALUE" object slot-name)
			  (funcall orig object))))
	(fset accessor (lambda (object)
			 (clos-call "SLOT-VALUE" object slot-name)))))))

;;;; Structures.  Mirror each structure type as a class, so methods can be
;;;; specialized on it and CLASS-OF works on structure instances.  The
;;;; structures defined so far were recorded in *initial-classes* by the
;;;; ENSURE-CLASS stub in cl-structures.el; wrap the stub to mirror the
;;;; rest as they're defined.

(defun clos-mirror-structure (name keys)
  (unless (or (eq name *std-instance-tag*) (clos-find-class name))
    (let ((supers (or (plist-get keys (keyword "DIRECT-SUPERCLASSES"))
		      '(STRUCTURE-OBJECT))))
      (EVAL `(DEFCLASS ,name
	       ,(mapcar (lambda (super)
			  (if (clos-find-class super) super 'STRUCTURE-OBJECT))
			supers)
	       ())))))

(dolist (entry (reverse *initial-classes*))
  (clos-mirror-structure (car entry) (cdr entry)))

(let ((ensure-class (symbol-function 'ENSURE-CLASS)))
  (fset 'ENSURE-CLASS
	(lambda (name &rest keys)
	  (prog1 (apply ensure-class name keys)
	    (clos-mirror-structure name keys)))))

;;; cl-clos.el ends here
