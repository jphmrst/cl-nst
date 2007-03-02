;;; File format
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :nst)

(defmethod print-object ((obj nst-class) stream)
  "We route print-object calls to a wrapped use of nst-format"
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~/nst::nst-format/" obj)))


;;; Formatting the standard classes.

(defgeneric nst-format (stream obj c s)
  (:documentation
   "Format-string compatible function for this package's classes")

  (:method (stream (info test) colon at-sign)
     "Formatter for test info records."
     (declare (ignorable colon) (ignorable at-sign))
     (with-slots (group test-name documentation) info
       (format stream "~@<~s (~s)~@[ ~_(~a)~]~:>"
	       test-name (get-name group) documentation)))

  (:method (stream (info group) colon at-sign)
     "Formatter for group info records."
     (declare (ignorable colon) (ignorable at-sign))
     (with-slots (package group-name
			  setup cleanup documentation) info
       (format stream "~s ~@<in package ~a~
                             ~@[ ~_(~a)~]~
                             ~@[, ~_setup ~s~]~
                             ~@[, ~_cleanup ~s~]~
                          ~:>"
	       group-name (package-name package)
	       documentation
	       (caddr setup) (caddr cleanup))))

  (:method (stream (info error-or-failure-report) colon at-sign)
     "Default formatter for unspecialized non-success records."
     (declare (ignorable colon) (ignorable at-sign))
     (format stream "Unspecified non-success"))

  (:method (stream (info error-report) colon at-sign)
     "Default formatter for unspecialized error report records."
     (declare (ignorable colon) (ignorable at-sign))
     (with-slots (caught) info
       (format stream "Error: ~s" caught)))

  (:method (stream (info fixture-error-report) colon at-sign)
     "Default formatter for error reports from fixture setup"
     (declare (ignorable colon) (ignorable at-sign))
     (with-slots (caught fixture-name var-name) info
       (format stream
	       "~@<Error setting up fixture ~s element ~s: ~_~s~:>"
	       fixture-name var-name caught)))

  (:method (stream (info failure-report) colon at-sign)
     "Default formatter for unspecialized failure report records."
     (declare (ignorable colon) (ignorable at-sign))
     (format stream "Test failure"))

  (:method (stream misc colon at-sign)
     "Fall-through for non-NST stuff.  Shouldn't be called."
     (declare (ignorable colon) (ignorable at-sign))
     (format stream "Non-NST object ~s" misc)))
