;;; File package.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lisp Lesser General Public License,
;;; which consists of the preamble published by Franz Incorporated,
;;; plus the LGPL published by the Free Software Foundation, either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lisp Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the Preamble to the Gnu Lesser
;;; General Public License and the GNU Lesser General Public License
;;; along with NST.  If not, see respectively
;;; <http://opensource.franz.com/preamble.html> and
;;; <http://www.gnu.org/licenses/>.
(in-package :common-lisp-user)

(defpackage :sift.nst
  (:documentation "Unit and regression testing for Common Lisp")
  (:nicknames :nst)
  (:use :common-lisp)
  #+allegro (:import-from excl #:named-function)

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

           #:def-eval-test #:assert-criterion
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

;;; -----------------------------------------------------------------
;;; Internal packages.

(defpackage :nst-name-use-in-packages
    (:documentation "Internal package for names' record-keeping."))
