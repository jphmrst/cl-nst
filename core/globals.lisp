;;; File globals.lisp
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
(in-package :sift.nst)

;;; This file contains settings, global variables and flags.

;;; ----------------------------------------------------------------------

;;;
;;;  Base classes.
;;;
(defclass group-base-class () ()
  (:documentation "Base class of group behavior."))

(defclass standalone-test-base-class () ()
  (:documentation "Base class of standalone test execution behavior."))

;;;
;;;  Flags and dynamic variable declarations.
;;;
(defvar *nst-verbosity* :quiet
  "User variable determining how verbose NST's output to the REPL should be.  Recognized values in rough order from most terse to most verbose are: nil, :quiet, :default, t, :verbose, :vverbose (:quiet by default).")

(defvar *nst-local-verbosity* :default
  "Global dynamic variable used to set the level of verbosity during report printing.")

(defvar *nst-report-default-verbosity* :verbose
  "User variable determining the default value for *nst-local-verbosity*, which sets the level of verbosity during report printing (:verbose by default).")

(defvar *nst-output-stream* *standard-output*
  "User variable determining the output stream to which NST should print its output (*standard-output* by default).")

(defvar *debug-on-error* nil
  "User variable: if non-null, will break into the Lisp REPL debugger upon encountering an unexpected error.  If t, will record the error and continue with other tests.")

(defparameter *nst-info-shows-expected* nil
  "Debugging-oriented user flag: when tracing NST structures, print expected 
values as hardcoded by the macros, rather than recalled via the generic
functions whose methods the macros define.")

(defparameter *nst-check-name* nil
  "Dynamic variable used to set the name of a test in its result report.")

;;;
;;; Internal tables.
;;;
(defvar +package-groups+ (make-hash-table :test 'eq)
  "Map from packages to the test grups declared in each package.")

;;;
;;; Generic functions whose methods are defined by the various macros.
;;;
(defmacro add-class-name-static-method (fn)
  `(defmethod ,fn ((g symbol)) (,fn (find-class g))))

(defmacro add-class-name-instantiator-method (fn)
  `(defmethod ,fn ((g symbol)) (,fn (make-instance g))))

;; Properties of groups.  Many of these function have methods on
;; symbols (presumably class names) that either relay to class
;; methods, or re-dispatch after instantiating an object of the named
;; class.

(defgeneric group-name (group-instance)
  (:documentation "Map from a group instance back to its symbolic name."))

(defgeneric test-names (fixture-or-group)
  (:documentation "The names of tests in a group.  Will be given an eql-method
by the macros which expand tests and groups."))
(add-class-name-instantiator-method test-names)

(defgeneric group-class-name (group-name)
  (:documentation
   "Map from groups to the private name with which NST associates the class of
group-specific activities.")
  (:method (default) (declare (ignorable default)) nil))
(add-class-name-static-method group-class-name)

(defgeneric group-fixture-classes (group-name)
  (:documentation
   "Map from groups to the private names of the group's fixtures."))
(add-class-name-static-method group-fixture-classes)

(defgeneric test-in-group-class-name (group-name)
  (:documentation
   "Map from groups to the private name with which NST associates a class with
which every test in the group is associated for testing the whole group of
tests.")
  (:method (default) (declare (ignorable default)) nil))
(add-class-name-static-method test-in-group-class-name)

(defgeneric standalone-test-in-group-class-name (group-name)
  (:documentation
   "Map from groups to the private name with which NST associates a class with
which every test in the group is associated for a standalone test.")
  (:method (default) (declare (ignorable default)) nil))
(add-class-name-static-method standalone-test-in-group-class-name)

(defgeneric test-fixture-classes (name))
(add-class-name-static-method test-fixture-classes)

(defgeneric package-groups (package-or-symbol))
(defmethod package-groups ((s symbol))
  (package-groups (find-package s)))
(defmethod package-groups ((p package))
  (let ((group-hash (gethash p +package-groups+)))
    (when group-hash
      (loop for g being the hash-keys of group-hash collect g))))

;; Information by Lisp package.

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
   "Map from various test names and instances to the private name against which
NST associates test results."))

;; Fixture properties and operations.

(defgeneric bound-names (fixture-or-group)
  (:documentation "The names defined by each fixture.  Will be given
an eql-method by the macros which expand tests and groups."))
(add-class-name-static-method bound-names)

(defgeneric group-fixture-class-name (fixture-name)
  (:documentation
   "Map from fixture names to the private name with which NST associates the
corresponding internal name-binding NST class for adding fixtures to a group.")
  (:method (default) (declare (ignorable default)) nil))
(add-class-name-static-method group-fixture-class-name)

(defgeneric test-fixture-class-name (fixture-name)
  (:documentation
   "Map from fixture names to the private name with which NST associates the
corresponding internal name-binding NST class for adding fixtures to a test.")
  (:method (default) (declare (ignorable default)) nil))
(add-class-name-static-method test-fixture-class-name)

(defgeneric open-fixture (fixture-name &optional package)
  (:documentation
   "Inject the names defined by the named fixture into the given package, by
default the current package."))
(defmethod open-fixture ((s symbol) &optional (in-package *package*))
  (open-fixture (make-instance s) in-package))

(defgeneric anon-fixture-forms (forms))
(add-class-name-static-method anon-fixture-forms)

;; Diagnostic information display.

(defgeneric blurb-context-line (stream id args forms)
  (:documentation "Give a short description of a context."))

(defgeneric detail-context-line (stream id args forms)
  (:documentation "Give a longer blurb of a context."))

(defgeneric stack-transformer (id)
  (:documentation "Check form-specific stack transformation."))

;;;
;;; Recording of results.  We use a hash table here --- unlike the
;;; method-based recording of test symbols, we're not worried about
;;; straddling the compile/load/run-time borders for result recording.
;;;
(defvar +results-record+ (make-hash-table :test 'eq)
  "Results of test runs.")

;; Extracting information for debugging.

(defgeneric trace-fixture (fx)
  (:documentation "Provide debugging information about a fixture.")
  (:method (fx) (format t "No known fixture ~s~%" fx)))
(add-class-name-instantiator-method trace-fixture)

(defgeneric trace-group (gr)
  (:documentation "Provide debugging information about a group.")
  (:method (gr) (format t "No known group ~s~%" gr)))
(add-class-name-instantiator-method trace-group)

(defgeneric trace-test (gr ts)
  (:documentation "Provide debugging information about a test.")
  (:method (gr ts) (format t "No known test ~s in group ~s~%" ts gr)))

(defun trace-results ()
  "Internal debugging function: dump the results hash."
  (loop for ts being the hash-keys of +results-record+ using (hash-value rs) do
    (format t "~s -> ~s~%" ts rs)))

;;;
;;; Error conditions.
;;;

(define-condition nst-error () ())

(defmacro define-nst-error (name fields (stream exp) &body printer)
  `(progn
     (define-condition ,name (nst-error) ,fields
		       (:report (lambda (,exp ,stream) ,@printer)))
     (set-pprint-dispatch ',name (lambda (,stream ,exp) ,@printer))))

(define-nst-error no-nst-groups-in-package
    ((package :initarg :package :reader package-of))
  (stream exp)
  (format stream "No NST packages in package ~s" (package-of exp)))

(define-nst-error no-such-nst-group
    ((group :initarg :group :reader group))
  (stream exp)
  (format stream "No such NST group ~s" (group exp)))

(define-nst-error no-such-nst-test
    ((group :initarg :group :reader group)
     (test :initarg :test :reader test))
  (stream exp)
  (format stream "No such NST test ~s in group ~s" (test exp) (group exp)))

;;;
;;; More generic functions whose methods are defined by the various
;;; macros.
;;;

;; Internal test execution functions.

(defgeneric core-run (group-or-test)
  (:documentation
   "Group fixtures provide name-binding :around methods to this generic
function; group setup and cleanup become :before and :after methods.")
  (:method ((group-inst group-base-class))
     (let ((group-name (group-name group-inst)))
       (case *nst-verbosity*
	 ((:vverbose)
	  (format t "    Starting run loop for ~s~%" group-inst)))
       (loop for test in (test-names group-inst) do
	 (case *nst-verbosity*
	   ((:vverbose)
	    (format t "      Starting loop entry ~s~%" test)))
	 (let ((in-suite-class-name (suite-class-name group-name test)))
	   (case *nst-verbosity*
	     ((:vverbose)
	      (format t "    Suite class name ~s~%" in-suite-class-name)
	      (format t "    Actual class ~s~%"
		(find-class in-suite-class-name))))
	   (let ((test-inst (make-instance in-suite-class-name)))
	     (case *nst-verbosity*
	       ((:vverbose)
		(format t "    Instance ~s~%" test-inst)))
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
      (case *nst-verbosity*
	((:default t :verbose :vverbose)
	 (format t " - Executing test ~s~%" (check-name test))))
      (let ((result (call-next-method)))
	(setf (gethash (canonical-storage-name (type-of test)) +results-record+)
	  result)
	(case *nst-verbosity*
	  ((:default t :verbose :vverbose) (format t "   ~s~%" result)))
	result))))

;;;
;;; Programmatic starters for a test from Lisp.  Other starters such
;;; as via ASDF and vendor-specific REPL macros call these functions;
;;; from pure Lisp these are the top-level calls.
;;;
(defun run-package (&optional (package-or-name *package*))
  "Run all groups in a package."
  (let* ((user-package (find-package package-or-name))
	 (group-names (package-groups user-package)))

    ;; Print a message at the appropriate level of verbosity.
    (case *nst-verbosity*
      ((:default t :verbose :vverbose)
       (format t "~@<Running package ~s (groups ~{~s~^ ~:_~})~:>~%" 
	 (package-name user-package) group-names)))
    
    (cond
      (group-names
       (loop for group-name in group-names do (run-group group-name)))
      (t
       (error 'no-nst-groups-in-package :package package-or-name)))))

(defun run-group (group)
  "Run a group by its user-given name."
  (let ((group-class (group-class-name group)))

    ;; Print a message at the appropriate level of verbosity.
    (case *nst-verbosity*
      ((:default t :verbose)
       (format t "Running group ~s~%" group))
      ((:vverbose)
       (format t "Running group ~s --> ~s~%" group group-class)))
    
    (unless group-class (error 'no-such-nst-group :group group))
    (core-run (make-instance group-class))))

(defun run-test (group test)
  "Run a test standalone by its user-given name (and its group's name)."
  (let ((test-class (standalone-class-name group test)))

    ;; Print a message at the appropriate level of verbosity.
    (case *nst-verbosity*
      ((:default t :verbose :vverbose)
       (format t "Running test ~s (group ~s)~%" test group)))
    
    (unless test-class (error 'no-such-nst-test :group group :test test))
    (core-run (make-instance test-class))))


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
  "Test whether two numbers are eql to the given number of significant digits."
  (and (numberp n1) (numberp n2)
       (let ((rounder (sig-place digits n1)))
	 (eql (round n1 rounder) (round n2 rounder)))))

;; Operations on lambda lists, for processing test specs.

(defun lambda-list-names (lambda-list supp-p)
  "Pick out the names from a lambda-list, omitting the ampersand-prefixed
delimiters."
  (let ((generic-list (extract-lambda-list lambda-list))
	(result))
    (labels ((descend (list)
	        (unless (null list)
		  (let ((item (car list)))
		    (cond 
		      ((listp item)
		       (cond
			 (supp-p (descend item))
			 (t (push (car item) result))))
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
