
;;; Copyright (c) 2006 John Maraist
;;;
;;; This code is being made available in accordance with the LGPL, and
;;; the lisp-specific preamble to that license, both of which are
;;; included with the code for this library.

(in-package :common-lisp-user)

(defpackage :sift.nst
    (:documentation "Unit and regression testing for Common Lisp")
    (:nicknames :nst)
    (:use :common-lisp #+sbcl sb-mop #+allegro mop)
    #+(or sbcl allegro)
    (:import-from #+sbcl sb-mop #-sbcl mop
		  #:generic-function-methods #:method-specializers
		  #:eql-specializer-object)
    #+(or openmcl clozure)
    (:import-from ccl
		  #:extract-lambda-list
		  #:generic-function-methods #:method-specializers
		  #:eql-specializer-object)
    (:export #:def-fixtures
	     #:def-capture/restore-fixtures
	     #:def-test-group
	     #:def-test
	     #:def-check
	     #:def-check-alias
	     #:def-value-check
	     #:def-value-check
	     #:def-control-check
	     #:continue-check
	     #:run-nst-commands
	     #:emit-failure
	     #:emit-warning
	     
	     #:*default-report-stream*))

(defpackage :sift.nst-test
    (:nicknames :nst-test :nst-t)
    (:use :common-lisp :sift.nst))

(defpackage :sift.nst-interact
    (:nicknames :nst-interact :nst-i)
    (:use :common-lisp :sift.nst))

(defun nst::make-package-documentation ()
  "Write documentation for this package, using system package-doc."
  (asdf:oos 'asdf:load-op 'package-doc)
  (funcall (symbol-function (intern (symbol-name 'package-doc)
				    (find-package :package-doc)))
	   (find-package :nst)))
