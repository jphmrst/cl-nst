;;; File runners.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :sift.nst)

;;; This file defined the sequence of generic function by which each
;;; test and group is run, and onto which fixtures and setup/cleanup
;;; is hung.  The end of this sequence is a generic function which is
;;; given a concrete eql-method for each individual test.

;;; Macros we'll use in the methods for running tests.

(defmacro control-setup-errors (&rest forms)
  (let ((c (gensym)) (setup-block (gensym "setup-block")))
    `(block ,setup-block
       ;; Grab thrown errors.  If we want to debug errors right away,
       ;; then we let the usual handler sequence go.  Otherwise we
       ;; return nil from this block, and so skip the tests and
       ;; cleanup.
       (handler-bind
	   ((error
	     #'(lambda (,c)
		 (declare (ignorable ,c))
		 (unless *debug-on-error*
		   (return-from ,setup-block nil)))))
	 ,@forms
	 (return-from ,setup-block t)))))

(defmacro record-setup-error (group id record-form stream &rest forms)
  (let ((report (gensym)))
    `(handler-bind
	 ((error
	   #'(lambda (,id)
	       (let ((,report ,record-form))
		 (setf (gethash ,group *erred-groups*) ,report)
		 (format ,stream
		     "Error in setup of group ~a (package ~a):~
                      ~%  ~/nst::nst-format/~%"
		   (symbol-name (get-name ,group))
		   (package-name (symbol-package (get-name ,group)))
		   ,report)))))
       ,@forms)))

(defmacro do-setup (group group-name group-setup-form report-stream
			  &rest other-forms)
  "This macro, which we use twice below in slightly different ways for
running tests as for running groups, describes the tedious plumbing
associated with the setup form of a test group."
  (let ((c (gensym)))
  `(progn
     ;; Print the currently selecting blurbing for group setup
     ;; execution.
     (verbose-out 
      (format ,report-stream
	  "Setting up tests in group ~s..." ,group-name))

     ;; We capture the success of setup in a conditional, and proceed
     ;; with tests and cleanup only if it works.
     (control-setup-errors
	(record-setup-error
	 ,group ,c (make-instance 'setup-error-report
		     :caught ,c :form ,group-setup-form) ,report-stream
	 (eval ,group-setup-form)))

     ;; Blurb a successful setup, and run the given forms.
     (verbose-out (format ,report-stream "done~%"))
     ,@other-forms)))

(defmacro do-cleanup (group group-name group-cleanup-form report-stream
			    &rest other-forms)
  "This macro, which we use twice below, describes the tedious plumbing
associated with the cleanup for a test group.  Or at least, it will
be tedious when we finish catching errors here."
  (declare (ignorable group))
  
  ;; TO DO - catch errors here

  `(progn
     (verbose-out 
      (format ,report-stream
	  "Cleaning up after tests in group ~s..." ,group-name))
     (eval ,group-cleanup-form)
     (verbose-out (format ,report-stream "done~%"))
     ,@other-forms))

;;; Generic functions relating to test and group execution.

(defgeneric core (test &key report-stream)
  (:documentation
   "Run a test, a group of tests, or all the tests associated with a
package.  The primary methods for this generic is defined for each test
in its def-test with the :form argument to that macro.  We provide an
:around method for processing the result of the primary method
immediately below; its return value is always one of t, nil or 'err.")

  (:method :around (test &key report-stream)
   "The primary core method simply evaluates the form given in the test
definition, which could return an arbitrary value, or raise an error.
This wrapper intercepts errors and standardizes the return value to
one of t, nil or 'err.  Here we also make the calls to our hook macros
for output before and after indiviual tests."
	   
   (with-slots (test-name group) test
     (block single-test
       (handler-bind
	   ((error
	     #'(lambda (x)
		 (if (use-verbose-output)
		     (format report-stream "   Error: ~s~%" x)
		     (format report-stream
			 "Test ~s (group ~s) raised error~%   ~s~%"
		       test-name (get-name group) x))
		 (add-test *erred-tests* test x)
		 (unless *debug-on-error*
		   (return-from single-test 'err)))))
	 (multiple-value-bind (result report) (call-next-method test)
	   (if result
	       (progn
		 (add-test *passed-tests* test)
		 (setf *passed-test-count* (+ 1 *passed-test-count*)))
	       (add-test *failed-tests* test report))
	   (if (use-verbose-output) 
	       (format report-stream "   ~a~%"
		 (if result "Passed" "Failed"))
	       (unless result
		 (format report-stream "Test ~s (group ~s) failed~%"
		   test-name (get-name group))))
	   (return-from single-test (if result t nil))))))))

(defgeneric bind-for-test (test report-stream)
  (:method ((ts test) report-stream)
     (run-dbg (format report-stream
		  "       - In core of bind-for-test ~s~%" ts))
     (core ts :report-stream report-stream)))

(defun chase-superclasses (class)
  (let ((result nil) (classes (list class)))
    (loop while classes do
      (let ((c (pop classes)))
	(unless (member c result)
	  (push (mop::class-name c) result)
	  (loop for sc in (mop:class-direct-superclasses c) do
	    (push sc classes)))))
    result))

(defun setup/cleanup-test (ts report-stream)
  (run-dbg
   (format report-stream
       "    - ~@<Relaying from test setup/cleanup function ~
                                            to test bindings methods,~
                 ~:@_for test ~s~
                 ~:@_of class ~s~
                 ~:@_superclasses ~@<~{~s~^ ~_~}~:>~:>~%"
     ts (class-of ts) (chase-superclasses (class-of ts))))
  (with-slots (group test-name) ts
    (with-slots (group-name) group
      (clear-test *passed-tests* group-name test-name)
      (clear-test *failed-tests* group-name test-name)
      (clear-test *erred-tests*  group-name test-name)))
  (bind-for-test ts report-stream))

(defgeneric bind-for-group (group-or-test report-stream)
  (:method ((g group) report-stream)
     (run-dbg 
      (format report-stream
	  " - Relayed to group bindings hook on ~s~%" (type-of g)))
     (block nil
       (let ((group-result t))
	 (with-slots (group-name test-names tests-hash) g
	   (loop for test-name across test-names do
	     (verbose-out
	      (format report-stream
		  " - ~@<Running test ~s ~_of group ~s~:>~%"
		test-name group-name))
	     (let* ((test (gethash test-name tests-hash))
		    (test-result
		     (setup/cleanup-test test report-stream)))
		 
	       ;; Here we check for the *break-on-...* flags.
	       (cond
		((eq test-result 'err)
		 (if (or *break-on-error* *debug-on-error*)
		   (return 'err)
		   (setf group-result 'err)))
	   
		((not test-result)
		 (if *break-on-wrong*
		   (return nil)
		   (unless (eq group-result 'err)
		     (setf group-result nil))))))))
	 group-result)))

  (:method ((ts test) report-stream)
     (setup/cleanup-test ts report-stream)))

(defun setup/cleanup-group (item report-stream)
  (run-dbg
   (format report-stream
       " - ~@<Relaying from group setup/cleanup function ~
                                               to group binding methods~
              ~:@_for group: ~s~
              ~:@_of class: ~s~
              ~:@_superclasses ~@<~{~s~^ ~_~}~:>~:>~%"
     item (class-of item) (chase-superclasses (class-of item))))
  (bind-for-group item report-stream))

;;; Generic functions relating to test and group execution.
(defgeneric run (ptg &key report-stream)
  (:documentation "Run a test, or a group of tests.  Methods return
t if all tests completed with a non-nil return value, 'err if any tests
exited with an error, or nil if all tests completed, but some with an
unsuccessful nil result.")

  (:method ((ts test) &key (report-stream cl-user::*nst-default-report-stream*))
     "Run a single test, bracketed by its group's setup and cleanup."
     (with-slots (group test-name) ts
       (verbose-out
	(format report-stream " - Running test ~s~%" test-name))
       (with-slots (group-name setup cleanup) group
	 (do-setup group group-name setup report-stream report-stream
	     (let ((test-result))
	       (unwind-protect
		   (setf test-result
			 (setup/cleanup-group ts report-stream))
		 (do-cleanup group group-name cleanup report-stream))
	       ;; When we're only trying to run one test, the return
	       ;; value from the method run is the same as the return
	       ;; value from the method core.
	       test-result)))))
  
  (:method ((g group) &key (report-stream cl-user::*nst-default-report-stream*))
     "Run the group's tests, bracketed by its setup and cleanup."
     (remhash g *erred-groups*)
     (remhash g *erred-cleanup*)
     (let ((group-result t))
       (block group-exec
	 (with-slots (group-name setup cleanup test-names tests-hash) g
	   (do-setup g group-name setup report-stream
		     (unwind-protect
			 (setup/cleanup-group g report-stream)
		       (do-cleanup group group-name
				   cleanup report-stream))))
	 group-result))))
