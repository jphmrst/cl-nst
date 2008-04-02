;;; File format.lisp
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
(in-package :sift.nst)

;;; Pretty-printing the standard classes.
(set-pprint-dispatch 'test
  '#(lambda (stream info)
     "Formatter for test info records."
     (with-slots (group test-name documentation) info
       (format stream "~@<~s (~s)~@[ ~_(~a)~]~:>"
	 test-name (get-name group) documentation))))

(set-pprint-dispatch 'group
  '#(lambda (stream info)
     "Formatter for group info records."
     (with-slots (package group-name
		  setup cleanup documentation) info
       (format stream "~s ~@<in package ~a~
                             ~@[ ~_(~a)~]~
                             ~@[, ~_setup ~s~]~
                             ~@[, ~_cleanup ~s~]~
                          ~:>"
	 group-name (package-name package)
	 documentation
	 (caddr setup) (caddr cleanup)))))

(set-pprint-dispatch 'error-or-failure-report
  '#(lambda (stream info)
     "Default formatter for unspecialized non-success records."
     (declare (ignorable info))
     (format stream "Unspecified non-success")))

(set-pprint-dispatch 'error-report
  '#(lambda (stream info)
     "Default formatter for unspecialized error report records."
     (with-slots (caught) info
       (format stream "Error: ~s" caught))))

(set-pprint-dispatch 'fixture-error-report
  '#(lambda (stream info)
     "Default formatter for error reports from fixture setup"
     (with-slots (caught fixture-name var-name) info
       (format stream
	   "~@<Error ~:_setting ~:_up ~:_fixture ~:_~a ~
               ~:_element ~:_~a ~:_(package ~:_~a):~
             ~_ ~s~:>"
	 (symbol-name fixture-name) (symbol-name var-name)
	 (package-name (symbol-package fixture-name))
	 caught))))

(set-pprint-dispatch 'failure-report
  '#(lambda (stream info)
     "Default formatter for unspecialized failure report records."
     (declare (ignorable info))
     (format stream "Test failure")))
