
;;; Copyright (c) 2006 John Maraist
;;;
;;; This code is being made available in accordance with the LGPL, and
;;; the lisp-specific preamble to that license, both of which are
;;; included with the code for this library.

(in-package :common-lisp-user)

(defpackage :sift.nst
    (:nicknames :nst)
    (:use :common-lisp :jm-utils #+sbcl sb-mop #-sbcl mop)
    (:import-from jm-utils #:def-string-constant #:defgeneric-when)
    (:import-from #+sbcl sb-mop #-sbcl mop
		  #:generic-function-methods #:method-specializers
		  #:eql-specializer-object)
    (:export #:def-fixtures
	     #:def-capture/restore-fixtures
	     #:def-test-group
	     #:def-test
	     #:def-check
	     #:def-check-criterion
	     #:def-value-check
	     #:def-check-alias
	     #:def-check-form
	     #:def-check-form-manip
	     #:continue-check
	     #:run-nst-commands
	     #:emit-failure
	     #:emit-warning
	     
	     #:*default-report-stream*))

(defpackage :sift.nst-test
    (:nicknames :nst-test)
    (:use :common-lisp :sift.nst))
