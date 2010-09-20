;;; File package.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2010 Smart Information Flow Technologies.
;;; Written by John Maraist.
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
    (:use :closer-common-lisp :defdoc)
    #+(or sbcl allegro)
    (:import-from #+sbcl sb-mop #-sbcl mop
                  #:generic-function-methods #:method-specializers
                  #:eql-specializer-object)
    #+(or openmcl clozure)
    (:import-from ccl
                  #:extract-lambda-list
                  #:generic-function-methods #:method-specializers
                  #:eql-specializer-object)

    (:export #:def-fixtures #:def-test-group #:def-test
             #:nst-cmd

             #:nst-junit-dump #:junit-results-by-group

             #:*nst-output-stream*
             #:*default-report-verbosity*
             #:*debug-on-error*
             #:*debug-on-fail*

             #:arbitrary
             #:compound-structure
             #:def-arbitrary-instance-type

             #:def-criterion #:def-criterion-unevaluated #:def-criterion-alias
             #:check-criterion-on-value #:check-criterion-on-form
             #:make-failure-report #:make-error-report
             #:make-warning-report #:make-success-report
             #:add-failure #:add-error #:add-info
             ;; Deprecated:
             #:emit-failure #:emit-warning #:emit-success
             #:def-form-criterion #:def-values-criterion

             #:with-fixtures

             #:nst-results
             #:def-test-generic
             #:def-test-method
             #:def-test-method-criterion))

(defdoc:def-documentation (package :nst)
    (:short "NST unit testing package"))

(defun nst::make-package-documentation ()
  "Write documentation for this package, using system package-doc."
  (asdf:oos 'asdf:load-op 'package-doc)
  (funcall (symbol-function (intern (symbol-name 'package-doc)
                                    (find-package :package-doc)))
           (find-package :nst)))

;;; -----------------------------------------------------------------
;;; Internal packages.

(defpackage :nst-name-use-in-packages
    (:documentation "Internal package for names' record-keeping."))
