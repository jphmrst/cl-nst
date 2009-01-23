
;;; Copyright (c) 2006 John Maraist
;;;
;;; This code is being made available in accordance with the LGPL, and
;;; the lisp-specific preamble to that license, both of which are
;;; included with the code for this library.

(in-package :common-lisp-user)

(defpackage :sift.nst
    (:nicknames :nst)
    (:use :common-lisp)
    (:export #:def-fixtures
	     #:def-capture/restore-fixtures
	     #:def-test-group
	     #:def-test
	     #:def-check
	     #:def-check-criterion
	     #:def-check-form
	     #:def-check-form-manip
	     #:continue-check
	     #:run-nst-commands))

(defpackage :sift.nst-test
    (:nicknames :nst-test)
    (:use :common-lisp :sift.nst))
