;;; File classes.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :sift.nst)


;;; Class and generic function definitions for the core of test
;;; execution.

(defclass nst-class () ()
  (:documentation
   "This superclass consolidates print-object and format pretty-
printing for all of the classes we define here."))

(defclass fixture (nst-class) ()
  (:documentation
   "Class of bindings usable in tests.  This class is used only for
dispatch in groups and tests which use these fixtures."))

(defclass test (nst-class)
     ((group     :initarg :group :type group
		 :documentation
		 "Record of information about the group")
      (test-name :initarg :name  :type symbol :reader get-name
		 :documentation "Name of this test")
      (documentation :initarg :documentation :type (or string null)
		     :documentation
		     "Documentation associated with this test"))
  (:documentation "Information associated with one single test."))

;;; Class and generic function definitions for the core of test
;;; execution.

(defclass group (nst-class)
     ((package :initarg :package
	       :documentation "Package where this group lives")
      (group-name :initarg :name :type symbol :reader get-name
		  :documentation "Name of this group lives")
      (test-names :type (vector symbol) :reader get-test-names
		  :documentation "Name of the tests in this group")
      (tests-hash :type hash-table :reader get-tests-hash
		  :documentation "Map from test names to tests")
      (setup   :initarg :setup   :initform nil :reader get-setup
	       :documentation
	       "Form to evaluate before running this group's tests")
      (cleanup :initarg :cleanup :initform nil :reader get-cleanup
	       :documentation
	       "Form to evaluate after running this group's tests")
      (testclass :initarg :testclass :type symbol
		 :documentation
		 "Symbolic name of the class which all test classes \
associated with this group should subclass.")
      (fixtures  :initarg :fixtures  :type (or (cons symbol) null)
		 :reader get-fixtures
		 :documentation
		 "Names of fixtures to be used in this group's tests")
      (documentation :initarg :documentation :type (or string null)
		     :documentation
		     "Documentation associated with this group"))
  (:documentation "Information associated with one group of tests."))

(define-condition unknown-fixture ()
  ((name :initarg :name :reader name)))

(defgeneric open-fixture (fixture)
  (:documentation
   "Adds the bindings defined by each fixture to the runtime namespace.
This function is defined via eql-methods for each fixture name.")
  (:method (unc) (error 'unknown-fixture :name unc)))

(defgeneric get-fixture-bindings (name)
  (:documentation
   "Return the declaration form for the named fixture.  For user echo
of fixture forms and other debugging."))

;;; Packets of information stored as reasons why a test or group errs
;;; or fails.
 
(defclass error-or-failure-report ()
     ()
  (:documentation
   "Top-level class of error and failure reports for tests and\
 groups"))

(defclass error-report (error-or-failure-report)
     ((caught :initarg :caught))
  (:documentation
   "Top-level class of error reports for tests and groups."))

(defclass setup-error-report (error-report)
     ((form :initarg :form))
  (:documentation "Error reports arising from setup forms"))

(defclass fixture-error-report (error-report)
     ((fixture-name :initarg :fixture-name)
      (var-name :initarg :var-name))
  (:documentation "Error reports arising from setup forms"))

(defclass failure-report (error-or-failure-report)
     ()
  (:documentation
   "Top-level class of failure reports for tests"))
