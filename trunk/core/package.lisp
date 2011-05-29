;;; File package.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2011 Smart Information Flow Technologies.
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
  #+allegro (:import-from excl #:named-function)
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
           #:*max-compound-structure-depth*
           #:def-arbitrary-instance-type

           #:def-criterion #:def-criterion-unevaluated #:def-criterion-alias
           #:check-criterion-on-value #:check-criterion-on-form
           #:make-failure-report #:make-error-report
           #:make-warning-report #:make-success-report
           #:add-failure #:add-error #:add-info #:add-warning
           #:wrap-thrown-lisp-warning
           ;; Deprecated:
           #:emit-failure #:emit-warning #:emit-success
           #:def-form-criterion #:def-values-criterion

           #:with-fixtures

           #:assert-criterion
           #:def-unary-predicate-assert #:def-unary-negated-predicate-assert
           #:def-binary-predicate-assert #:def-binary-negated-predicate-assert

           #:assert-null #:assert-zero #:assert-non-nil
           #:assert-eq #:assert-eql #:assert-equal #:assert-equalp
           #:assert-not-eq #:assert-not-eql #:assert-not-equal
           #:assert-not-equalp

           #:*assert-null-format-string* #:*assert-zero-format-string*
           #:*assert-nonnil-format-string* #:*assert-eq-format-string*
           #:*assert-eql-format-string* #:*assert-equal-format-string*
           #:*assert-equalp-format-string* #:*assert-not-eq-format-string*
           #:*assert-not-eql-format-string* #:*assert-not-equal-format-string*
           #:*assert-not-equalp-format-string*

           #:nst-results
           #:def-test-generic
           #:def-test-method
           #:def-test-method-criterion))

(defdoc:def-documentation (package :sift.nst)
    (:blurb "NST unit testing package")
  (:descriptive "NST"))

(defdoc:def-doc-tag nst::primary (:package :nst)
  :sort 0
  :formatter (lambda (sy pk tg sr)
               (declare (ignore sy pk tg))
               (format sr "Primary macros")))
(defdoc:def-doc-tag nst::criteria (:package :nst)
  :sort 3
  :formatter (lambda (sy pk tg sr)
               (declare (ignore sy pk tg))
               (format sr "Functions used in criteria definitions")))
(defdoc:def-doc-tag nst::control (:package :nst)
  :sort 5
  :formatter (lambda (sy pk tg sr)
               (declare (ignore sy pk tg))
               (format sr "Programmatic control of testing and output")))
(defdoc:def-doc-tag nst::sample (:package :nst)
  :sort 8
  :formatter (lambda (sy pk tg sr)
               (declare (ignore sy pk tg))
               (format sr "Testing randomized samples")))
(defdoc:def-doc-tag nst::object (:package :nst)
  :sort 11
  :formatter (lambda (sy pk tg sr)
               (declare (ignore sy pk tg))
               (format sr "Method-based tests on objects")))
(defdoc:def-doc-tag &rest (:package :nst)
  :sort 15
  :formatter (lambda (sy pk tg sr)
               (declare (ignore sy pk tg))
               (format sr "Other symbols")))
(defdoc:def-doc-tag nst::deprecated (:package :nst)
  :sort 20
  :formatter (lambda (sy pk tg sr)
               (declare (ignore sy pk tg))
               (format sr "Deprecated macros, functions and variable")))

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
