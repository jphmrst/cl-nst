;;; File runners.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :nst)

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

(defmacro record-setup-error (group id record-form &rest forms)
  (let ((report (gensym)))
    `(handler-bind
	 ((error
	   #'(lambda (,id)
	       (let ((,report ,record-form))
		 (setf (gethash ,group *erred-groups*) ,report)
		 (format t "Error in group ~s setup:~%  ~
                            ~/nst::nst-format/~%"
			 (get-name ,group) ,report)))))
       ,@forms)))

(defmacro do-setup (group group-name group-setup-form
			  &rest other-forms)
  "This macro, which we use twice below in slightly different ways for
running tests as for running groups, describes the tedious plumbing
associated with the setup form of a test group."
  (let ((c (gensym)))
  `(progn
     ;; Print the currently selecting blurbing for group setup
     ;; execution.
     (verbose-out 
      (format t "Setting up tests in group ~s..." ',group-name))

     ;; We capture the success of setup in a conditional, and proceed
     ;; with tests and cleanup only if it works.
     (control-setup-errors
	(record-setup-error
	 ,group ,c
	 (make-instance 'setup-error-report
	   :caught ,c :form ,group-setup-form)
	 (eval ,group-setup-form)))

     ;; Blurb a successful setup, and run the given forms.
     (verbose-out (format t "done~%"))
     ,@other-forms)))


(defmacro do-cleanup (group group-name group-cleanup-form
			    &rest other-forms)
  "This macro, which we use twice below, describes the tedious plumbing
associated with the cleanup for a test group.  Or at least, it will
be tedious when we finish catching errors here."
  (declare (ignorable group))
  
  ;; TO DO - catch errors here

  `(progn
     (verbose-out 
      (format t "Cleaning up after tests in group ~s..." ,group-name))
     (eval ,group-cleanup-form)
     (verbose-out
      (format t "done~%"))
     ,@other-forms))

;;; Generic functions relating to test and group execution.

(defgeneric core (test)
  (:documentation
   "Run a test, a group of tests, or all the tests associated with a
package.  The primary methods for this generic is defined for each test
in its def-test with the :form argument to that macro.  We provide an
:around method for processing the result of the primary method
immediately below; its return value is always one of t, nil or 'err.")

  (:method :around (test)
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
		     (format t "   Error: ~s~%" x)
		     (format t
			     "Test ~s (group ~s) raised error~%   ~s~%"
			     test-name (get-name group) x))
		 (add-test *erred-tests* test x)
		 (unless *debug-on-error*
		   (return-from single-test 'err)))))
	 (let ((result (call-next-method test)))
	   (if result
	       (setf *passed-test-count* (+ 1 *passed-test-count*))
	       (add-test *failed-tests* test))
	   (if (use-verbose-output) 
	       (format t "   ~a~%" (if result "Passed" "Failed"))
	       (unless result
		 (format t "Test ~s (group ~s) failed~%"
			 test-name (get-name group))))
	   (return-from single-test (if result t nil))))))))

(defgeneric bind-for-test (test)
  (:method ((ts test)) (core ts)))

(defgeneric setup/cleanup-test (test)
  (:method ((ts test)) (bind-for-test ts)))

(defgeneric bind-for-group (test)
  (:method ((g group))
     (block nil
	 (let ((group-result t))
	   (with-slots (group-name test-names tests-hash) g
	     (loop for test-name across test-names do
	       (verbose-out
		(format t " - ~@<Running test ~s ~_of group ~s~:>~%"
			test-name group-name))
	       (let* ((test (gethash test-name tests-hash))
		      (test-result (setup/cleanup-test test)))
		 
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

  (:method ((ts test)) (setup/cleanup-test ts)))

(defgeneric setup/cleanup-group (item)
  (:method (item) (bind-for-group item)))

;;; Generic functions relating to test and group execution.

(defgeneric run (ptg)
  (:documentation "Run a test, or a group of tests.  Methods return
t if all tests completed with a non-nil return value, 'err if any tests
exited with an error, or nil if all tests completed, but some with an
unsuccessful nil result.")

  (:method ((ts test))
     "Run a single test, bracketed by its group's setup and cleanup."
     (with-slots (group test-name) ts
       (verbose-out (format t " - Running test ~s~%" test-name))
       (with-slots (group-name setup cleanup) group
	 (do-setup group group-name setup
	     (let ((test-result))
	       (unwind-protect
		   (setf test-result (setup/cleanup-group ts))
		 (do-cleanup group group-name cleanup))
	       ;; When we're only trying to run one test, the return
	       ;; value from the method run is the same as the return
	       ;; value from the method core.
	       test-result)))))
  
  (:method ((g group))
     "Run the group's tests, bracketed by its setup and cleanup."
     (let ((group-result t))
       (block group-exec
	 (with-slots (group-name setup cleanup test-names tests-hash) g
	   (do-setup g group-name setup
		     (unwind-protect (setup/cleanup-group g)
		       (do-cleanup group group-name cleanup))))
	 group-result))))
