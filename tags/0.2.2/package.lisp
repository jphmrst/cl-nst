;;; File package.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006, 2007, 2008 Smart Information Flow Technologies.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with NST.  If not, see
;;; <http://www.gnu.org/licenses/>.
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
