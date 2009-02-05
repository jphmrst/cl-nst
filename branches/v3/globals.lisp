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

;;; ----------------------------------------------------------------------

;;;
;;; Base classes.
;;;

(defclass group-base-class () ()
  (:documentation "Base class of group behavior."))

(defclass standalone-test-base-class () ()
  (:documentation "Base class of standalone test execution behavior."))

;;;
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
	  :documentation
	  "Set to t to generate debugging information about fixtures")
(def-flag *debug-class-hierarchy* nil ()
	  :runtime-macro class-dbg
	  :documentation "Set to t to generate debugging information about the \
                          class hierarchy of tests and groups")
(def-flag *debug-macrotime* nil
  (*debug-class-hierarchy* *debug-fixtures* *debug-forms-manip*)
	  :runtime-macro macro-dbg
	  :documentation
	  "Set to t for extensive macro expansion debugging output")
(def-flag *debug-compile* nil (*debug-forms-manip* *debug-fixtures*)
	  :runtime-macro compile-dbg
	  :documentation
	  "Set to t for extensive debugging output for expanded macros")
(def-flag *debug-bindings* nil (*debug-fixtures*)
	  :runtime-macro bind-dbg
	  :documentation "Set to t to generate debugging information \
                          about fixture bindings in tests and groups")
(def-flag *debug-output* nil (*debug-class-hierarchy* *debug-bindings*)
	  :runtime-macro run-dbg
	  :documentation "Set to t for extensive runtime debugging output")
(def-flag *verbose-output* nil (*debug-output*)
	  :function-name use-verbose-output
	  :runtime-macro verbose-out
	  :documentation "Set to t for verbose output during test execution. \
                          This setting is implied by *debug-output*.")

(def-flag *scheduled-summary-output* t ()
	  :documentation "Set to t for summaries of runs of scheduled tests.")
(def-flag *scheduled-single-output* nil ()
	  :documentation
	  "Set to t for summaries of single test, group or package runs.")
(def-flag *defer-test-compile* t ()
	  :documentation
	  "Set to t to defer compilation of test forms until runtime.")

(defun get-verbosity-level ()
  :verbose)

(defvar *nst-local-verbosity* :default)
(defvar *nst-output-stream* *standard-output*)

(defvar *debug-on-error* nil)

;;;
;;; Generic functions whose methods are defined by the various macros.
;;;

;; Properties of groups.

(defgeneric group-name (group-instance)
  (:documentation "Map from a group instance back to its symbolic name."))

(defgeneric test-names (fixture-or-group)
  (:documentation "The names of tests in a group.  Will be given an eql-method
by the macros which expand tests and groups."))

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
   "Map from groups to the private name with which NST associates a class with
which every test in the group is associated for testing the whole group of
tests.")
  (:method (default) (declare (ignorable default)) nil))

(defgeneric standalone-test-in-group-class-name (group-name)
  (:documentation
   "Map from groups to the private name with which NST associates a class with
which every test in the group is associated for a standalone test.")
  (:method (default) (declare (ignorable default)) nil))

;; Information by Lisp package.

(defgeneric groups-package (public-package)
  (:documentation
   "Map from packages to the private package NST associates with each for
housing the names of the groups in each package.")
  (:method (default) (declare (ignorable default)) nil))

;; Properties of checks.

(defgeneric check-name (check-instance)
  (:documentation "Map from a check instance back to its symbolic name."))

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
   "Map from tests to the private name with which NST associates a class for
each test for methods to apply whether the test is called standalone or as part
of a group.")
  (:method (group class) (declare (ignorable group class)) nil))

(defgeneric canonical-storage-name (test-name)
  (:documentation
   "Map from various test names and instances to the private name against which NST
associates test results."))

;; Fixture properties and operations.

(defgeneric bound-names (fixture-or-group)
  (:documentation "The names defined by each fixture.  Will be given
an eql-method by the macros which expand tests and groups."))

(defgeneric group-fixture-class-name (fixture-name)
  (:documentation
   "Map from fixture names to the private name with which NST associates the
corresponding internal name-binding NST class for adding fixtures to a group.")
  (:method (default) (declare (ignorable default)) nil))

(defgeneric test-fixture-class-name (fixture-name)
  (:documentation
   "Map from fixture names to the private name with which NST associates the
corresponding internal name-binding NST class for adding fixtures to a test.")
  (:method (default) (declare (ignorable default)) nil))

(defgeneric open-fixture (fixture-name &optional package)
  (:documentation
   "Inject the names defined by the named fixture into the current package."))

;; Diagnostic information display.

(defgeneric blurb-context-line (stream id args forms)
  (:documentation "Give a short description of a context."))

(defgeneric detail-context-line (stream id args forms)
  (:documentation "Give a longer blurb of a context."))

(defgeneric stack-transformer (id)
  (:documentation "Check form-specific stack transformation."))

;; Extracting information for debugging.

(defgeneric trace-fixture (fx)
  (:documentation "Provide debugging information about a fixture.")
  (:method (fx) (format t "No known fixture ~s~%" fx)))

(defgeneric trace-group (gr)
  (:documentation "Provide debugging information about a group.")
  (:method (gr) (format t "No known group ~s~%" gr)))

(defgeneric trace-test (gr ts)
  (:documentation "Provide debugging information about a test.")
  (:method (gr ts) (format t "No known test ~s in group ~s~%" ts gr)))

(defun trace-results ()
  "Dump the results hash."
  (loop for ts being the hash-keys of +results-record+ using (hash-value rs) do
    (format t "~s -> ~s~%" ts rs)))

;;;
;;; More generic functions whose methods are defined by the various
;;; macros.
;;;

(defparameter *nst-info-shows-expected* nil)

(defparameter *nst-check-name* nil)

;; Internal test execution functions.

(defgeneric core-run (group-or-test)
  (:documentation
   "Group fixtures provide name-binding :around methods to this generic
function; group setup and cleanup become :before and :after methods.")
  (:method ((group-inst group-base-class))
     (let ((group-name (group-name group-inst)))
       ;; (format t "    Starting run loop for ~s~%" group-inst)
       (loop for test in (test-names group-inst) do
	 ;; (format t "      Starting loop entry ~s~%" test)
	 (let ((in-suite-class-name (suite-class-name group-name test)))
	   ;; (format t "    Suite class name ~s~%" suite-class-name)
	   ;; (format t "    Actual class ~s~%" (find-class suite-class-name))
	   ;; (describe (find-class suite-class-name))
	   (let ((test-inst (make-instance in-suite-class-name)))
	     ;; (format t "    Instance ~s~%" test-inst)
	     (core-run-test test-inst)))
	 ;; (format t "      Exiting loop entry ~s~%" test)
	     )
	 ;;(format t "    Exiting run loop for ~s~%" group-inst)
       )
     nil))

(defgeneric core-run-test (test)
  (:documentation
   "Test fixtures provide name-binding :around methods to this generic function
for individual tests.  Every-test and test-specific setup and cleanup are
encoded as :before and :after methods.")

  (:method :around (test)
    "Capture the result of the test."
    (let ((*nst-check-name* (check-name test)))
      (let ((result (call-next-method)))
	(setf (gethash (canonical-storage-name (type-of test)) +results-record+)
	  result)
	result))))

;;;
;;; Programmatic starters for a test from Lisp.  Other starters such
;;; as via ASDF and vendor-specific REPL macros call these functions;
;;; from pure Lisp these are the top-level calls.
;;;
(defun run-package (&optional (package-or-name *package*))
  "Run all groups in a package."
  (let* ((user-package (find-package package-or-name))
	 (sym-pack (groups-package user-package)))
    (do-symbols (group sym-pack)
      (run-group (intern (symbol-name group) user-package)))))

(defun run-group (group)
  "Run a group by its user-given name."
  (core-run (make-instance (group-class-name group))))

(defun run-test (group test)
  "Run a test standalone by its user-given name (and its group's name)."
  (core-run (make-instance (standalone-class-name group test))))

;;;
;;; Recording of results.  We use a hash table here --- unlike the
;;; method-based recording of test symbols, we're not worried about
;;; straddling the compile/load/run-time borders for result recording.
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+results-record+)
    (defconstant +results-record+ (make-hash-table :test 'eq)
      "Results of test runs.")))

;;;
;;; Helper functions
;;;

;; Versions of car and cdr for when we expect to have either a list or
;; a symbol.

(defun symbol-or-car (name-or-name-and-args)
  "Return the first element given a list, or return a symbol."
  (cond ((symbolp name-or-name-and-args) name-or-name-and-args)
	((consp name-or-name-and-args) (car name-or-name-and-args))
	(t (cerror "Return nil" "Unable to parse ~S to find the name in it."
		   name-or-name-and-args)
	   nil)))

(defun cdr-or-nil (name-or-name-and-args)
  "Return the cdr given a list, or return nil if given a symbol."
  (cond ((symbolp name-or-name-and-args) nil)
	((listp name-or-name-and-args) (cdr name-or-name-and-args))
	(t (cerror "Return nil" "Unable to parse ~S to find the name in it."
		   name-or-name-and-args)
	   nil)))

;; Tests on numbers.

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

;; Operations on lambda lists, for processing test specs.

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
