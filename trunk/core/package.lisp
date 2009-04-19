;;; File package.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2009 Smart Information Flow Technologies.
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
    (:use :common-lisp :closer-mop)
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
             #:def-test-group
             #:def-check

             #:def-check-alias
             #:def-value-check
             #:def-control-check

             #:continue-check
             #:emit-failure
             #:emit-warning
             #:emit-success
             #:check-result

             #:run-package #:run-group #:run-test
             #:report-multiple #:report-package #:report-group #:report-test

             #:nst-junit-dump

             #:*nst-output-stream*
             #:*nst-report-default-verbosity*
             #:*nst-verbosity*
             #:*debug-on-error*

             #:junit-results-by-group))

(defun nst::make-package-documentation ()
  "Write documentation for this package, using system package-doc."
  (asdf:oos 'asdf:load-op 'package-doc)
  (funcall (symbol-function (intern (symbol-name 'package-doc)
                                    (find-package :package-doc)))
           (find-package :nst)))

;;; The packages below are internal --- user symbols map to NST
;;; internal symbols which live in thse packages.  By "hiding" these
;;; symbols here, we avoid the need to gensym into user packages.
(defpackage :nst-suite-class-names
    (:documentation "A test suite defines local names for the class corresponding to the suite itself; this package holds these names."))
(defpackage :nst-standalone-class-names
    (:documentation "A test suite defines local names for the class corresponding to the object instantiated for standalone execution of a single test in that suite; this package holds these names."))
(defpackage :nst-test-config-class-names
    (:documentation "A test suite defines local names for the class corresponding to the object instantiated for the in-group execution of tests in that suite; this package holds these names."))
(defpackage :nst-test-in-group-class-names)

(defpackage :nst-fixture-group-class-names)
(defpackage :nst-fixture-test-class-names)

(defpackage :group-class-name-package)
(defpackage :test-in-group-class-name-package)
(defpackage :standalone-test-in-group-class-name-package)
