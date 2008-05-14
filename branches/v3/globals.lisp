;;; File globals.lisp
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

;;; This file contains settings, global variables and flags.

;;; Options for output in the interactive system.
;;;
(defmacro def-flag (flag-name flag-value implying-flags
			      &key (documentation nil doc-sup-p)
			      runtime-macro function-name)
  `(progn
     (defparameter ,flag-name ,flag-value
       ,(when doc-sup-p documentation))
     ,(when function-name
	`(defun ,function-name () (or ,flag-name ,@implying-flags)))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       ,(when runtime-macro
	  `(defmacro ,runtime-macro (&rest forms)
	     (let ((name-echo ',flag-name)
		   (flags-echo ',implying-flags))
	       `(when (or ,name-echo ,@flags-echo) ,@forms)))))
     t))

(def-flag *debug-forms-manip* nil ()
	  :runtime-macro forms-dbg
	  :documentation "Set to t to generate debugging information \
                          about certain macro expansions")
(def-flag *debug-fixtures* nil ()
	  :runtime-macro fixture-dbg
	  :documentation "Set to t to generate debugging information \
                          about fixtures")
(def-flag *debug-class-hierarchy* nil ()
	  :runtime-macro class-dbg
	  :documentation "Set to t to generate debugging information \
                          about the class hierarchy of tests and \
                          groups")
(def-flag *debug-macrotime* nil
  (*debug-class-hierarchy* *debug-fixtures* *debug-forms-manip*)
	  :runtime-macro macro-dbg
	  :documentation "Set to t for extensive macro expansion \
                          debugging output")
(def-flag *debug-compile* nil (*debug-forms-manip* *debug-fixtures*)
	  :runtime-macro compile-dbg
	  :documentation "Set to t for extensive debugging output for \
                          expanded macros")
(def-flag *debug-bindings* nil (*debug-fixtures*)
	  :runtime-macro bind-dbg
	  :documentation "Set to t to generate debugging information \
                          about fixture bindings in tests and groups")
(def-flag *debug-output* nil (*debug-class-hierarchy* *debug-bindings*)
	  :runtime-macro run-dbg
	  :documentation "Set to t for extensive runtime debugging \
                          output")
(def-flag *verbose-output* nil (*debug-output*)
	  :function-name use-verbose-output
	  :runtime-macro verbose-out
	  :documentation "Set to t for verbose output during test\
                          execution.  This setting is implied by\
                          *debug-output*.")

(def-flag *scheduled-summary-output* t ()
	  :documentation "Set to t for summaries of runs of scheduled\
                          tests.")
(def-flag *scheduled-single-output* nil ()
	  :documentation "Set to t for summaries of single test, group\
                          or package runs.")
(def-flag *defer-test-compile* t ()
	  :documentation "Set to t to defer compilation of test forms\
                          until runtime.")

;;; -----------------------------------------------------------------

(defclass group-base-class () ()
  (:documentation "Base class of group behavior."))

(defclass standalone-test-base-class () ()
  (:documentation "Base class of standalone test execution behavior."))

;;; -----------------------------------------------------------------

;;; The fixtures, groups and tests that have been defined.
;;;

(defgeneric test-names (fixture-or-group)
  (:documentation "The names of tests in a group.  Will be given an eql-method
by the macros which expand tests and groups."))

(defgeneric group-name (group-instance)
  (:documentation "Map from a group instance back to its symbolic name."))

(defgeneric bound-names (fixture-or-group)
  (:documentation "The names defined by each fixture.  Will be given
an eql-method by the macros which expand tests and groups."))

(defgeneric groups-package (public-package)
  (:documentation
   "Map from packages to the private package NST associates with each for
housing the names of the groups in each package.")
  (:method (default) (declare (ignorable default)) nil))

(defgeneric group-class-name (group-name)
  (:documentation
   "Map from groups to the private name with which NST associates the class of
group-specific activities.")
  (:method (default) (declare (ignorable default)) nil))

(defgeneric group-fixture-classes (group-name)
  (:documentation
   "Map from groups to the private names of the group's fixtures."))

(defgeneric test-in-group-class-name (group-name)
  (:documentation
   "Map from groups to the private name with which NST associates the class of
")
  (:method (default) (declare (ignorable default)) nil))

(defgeneric standalone-test-in-group-class-name (group-name)
  (:documentation
   "Map from groups to the private name with which NST associates the class of
")
  (:method (default) (declare (ignorable default)) nil))

(defgeneric suite-class-name (group-name test-name)
  (:documentation
   "Map from tests to the private name with which NST associates the class of
the instance of this test for runs within a group run.")
  (:method (group class) (declare (ignorable group class)) nil))

(defgeneric standalone-class-name (group-name test-name)
  (:documentation
   "Map from tests to the private name with which NST associates the class of
the instance of this test for standalone runs, not part of a run with a group.")
  (:method (group class) (declare (ignorable group class)) nil))

(defgeneric test-config-class-name (group-name test-name)
  (:documentation
   "Map from tests to the private name with which NST associates the class of
")
  (:method (group class) (declare (ignorable group class)) nil))

(defgeneric fixture-class-name (fixture-name)
  (:documentation
   "Map from fixture names to the private name with which NST associates the
corresponding internal name-binding NST class.")
  (:method (default) (declare (ignorable default)) nil))

(defgeneric open-fixture (fixture-name &optional package)
  (:documentation
   "Inject the names defined by the named fixture into the current package."))

(defgeneric trace-fixture (fx)
  (:method (fx) (format t "No known fixture ~s~%" fx)))
(defgeneric trace-group (gr)
  (:method (gr) (format t "No known group ~s~%" gr)))
(defgeneric trace-test (gr ts)
  (:method (gr ts) (format t "No known test ~s in group ~s~%" ts gr)))

(defgeneric run (test)
  (:documentation
   "Fixtures provide name-binding :around methods to this generic function")
  (:method ((group-inst group-base-class))
     (let ((group-name (group-name group-inst)))
       (format t "Starting run loop for ~s~%" group-inst)
       (loop for test in (test-names group-inst) do
	 (format t "  Starting loop entry ~s~%" test)
	 (let ((in-suite-class-name (suite-class-name group-name test)))
	   ;; (format t "    Suite class name ~s~%" suite-class-name)
	   ;; (format t "    Actual class ~s~%" (find-class suite-class-name))
	   ;; (describe (find-class suite-class-name))
	   (let ((test-inst (make-instance in-suite-class-name)))
	     ;; (format t "    Instance ~s~%" test-inst)
	     (run test-inst)))
	 (format t "  Exiting loop entry ~s~%" test))
       (format t "Exiting run loop for ~s~%" group-inst))))

;;; This is probably disused.
;;;
;;;(defvar +fixtures+ nil
;;;  "For user echo of fixture forms and other debugging." )

;;; -----------------------------------------------------------------

(defgeneric blurb-context-line (stream id args forms)
  (:documentation "Give a short description of a context."))

(defgeneric detail-context-line (stream id args forms)
  (:documentation "Give a longer blurb of a context."))

(defgeneric stack-transformer (id)
  (:documentation "Check form-specific stack transformation."))

;;; -----------------------------------------------------------------

(defun first-symbol (name-or-name-and-args)
  (cond ((symbolp name-or-name-and-args) name-or-name-and-args)
	((listp name-or-name-and-args) (first name-or-name-and-args))
	(t (cerror "Return nil" "Unable to parse ~S to find the name in it."
		   name-or-name-and-args)
	   nil)))

(defun cdr-or-nil (name-or-name-and-args)
  (cond ((symbolp name-or-name-and-args) nil)
	((listp name-or-name-and-args) (cdr name-or-name-and-args))
	(t (cerror "Return nil" "Unable to parse ~S to find the name in it."
		   name-or-name-and-args)
	   nil)))

;;; -----------------------------------------------------------------

;;; Functions supporting tests on numbers.

(defmacro log10 (v) `(/ (log ,v) (log 10)))

(defun sig-place (n value)
  "Returns the n-th significant place of value"
  (let* ((xlog (if (zerop value) 0 (log10 (abs value))))
	 (xlog-up (floor xlog)))
    (expt 10 (- xlog-up (- n 1)))))

(defun eql-for-sigdigits (digits n1 n2)
  (and (numberp n1)
       (numberp n2)
       (let ((rounder (sig-place digits n1)))
	 (eql (round n1 rounder) (round n2 rounder)))))

(defun lambda-list-names (lambda-list)
  (let ((generic-list (extract-lambda-list lambda-list))
	(result))
    (labels ((descend (list)
	        (unless (null list)
		  (let ((item (car list)))
		    (cond 
		     ((listp item)
		      (descend item))
		     ((symbolp item)
		      (unless (member item
				      #+allegro '(&allow-other-keys &aux
						  &body &environment &key
						  &optional &rest &whole)
				      #-allegro lambda-list-keywords)
			(push item result))))
		    (descend (cdr list))))))
      (descend generic-list)
      (nreverse result))))
