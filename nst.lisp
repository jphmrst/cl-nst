;;; File nst.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman
;;;
;;; NST is Copyright (c) 2006 Smart Information Flow Technologies
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :nst)

;;; Known issues
;;;
;;; 1. Let there be comments.
;;;
;;; 2. None of the :break-on- or :debug-on- flags work yet (unless you
;;; set them all to be false).  So this will require thinking about
;;; what these things return, getting it right from the test runner to
;;; the group runner and so forth, doing something with the 'err
;;; symbols, and all that good stuff.
;;;
;;; 3. The :blurb command doesn't work yet.  This will require some
;;; additional bookkeeping to save the thrown errors.
;;;
;;; 4. Write specialized versions of def-test for checking for a
;;; symbol, checking equality, some sort of checking against a
;;; template in that way that so far exists only in my head, etc.


(defconstant +fixture-info+ (make-hash-table) "For debugging")
(defconstant +groups+ (make-hash-table) "Declared test NST groups")
(defconstant +mixin-to-group-to-wrapper+ (make-hash-table))
(defconstant +mixin-to-test-to-wrapper+ (make-hash-table))
(defconstant +groups-by-package+ (make-hash-table)
  "Hash table from packages to sets of group names, that is, to other
hash tables from group names to t or nil.")
(defconstant +tests-by-group+ (make-hash-table))

(defvar *debug-output* nil)
(defun show-debug-output () *debug-output*)

(defvar *verbose-output* nil)
(defun show-verbose-output () (or *verbose-output* *debug-output*))

(defvar *break-on-wrong* nil)
(defvar *break-on-error* nil)
(defvar *debug-on-error* nil)

;;; These should all be defvar's when NST is more finished.
(defparameter *interesting-packages* nil)
(defparameter *interesting-group-names* nil)
(defparameter +interesting-test-names+ (make-hash-table))

(defparameter *pending-packages* nil)
(defparameter *pending-group-names* nil)
(defparameter *pending-test-names* (make-hash-table)
  "The hash is against first group names, and then test names")

(defparameter *passed-test-count* 0)
(defparameter *erred-groups* nil
  "The list holds group info records, not names")
(defparameter *erred-cleanup* nil
  "The list holds group info records, not names")
(defparameter *failed-tests* (make-hash-table)
  "The hash is against test info records, not test names")
(defparameter *erred-tests* (make-hash-table)
  "The hash is against test info records, not test names")

;;; -----------------------------------------------------------------

(defclass nst-class () ())
(defgeneric format-group (stream obj c s))
(defmethod print-object ((obj nst-class) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~/nst::nst-format/" obj)))

;;; -----------------------------------------------------------------

(defclass test-group-info (nst-class)
     ((package :initarg :package)
      (name :initarg :name :reader get-name :type symbol)
      (runner :initarg :runner :type symbol)
      (fixtures :initarg :fixtures :initform nil :type (vector symbol))
      (setup :initarg :setup :initform nil :reader get-setup)
      (cleanup :initarg :cleanup :initform nil :reader get-cleanup)
      (documentation :initarg :documentation :type string))
  (:documentation
   "Record of information associated with each test group"))

(defmethod nst-format (stream (info test-group-info) colon at-sign)
  (declare (ignorable colon) (ignorable at-sign))
  (with-slots (name fixtures runner setup
		    package cleanup documentation) info
    (format stream "~s ~@<~@[~_(~a) ~_~]with ~@<~
                    ~[no fixtures,~*~
                        ~;fixture ~{~s~^, ~},~
                        ~;fixtures ~{~s~^, ~};~] ~
                    ~_runner ~s~
                    ~@[, ~_setup ~s~]~
                    ~@[, ~_cleanup ~s~]~:>, ~_~
                    in package ~a~:>"
	    name documentation
	    (length fixtures) (coerce fixtures 'list)
	    runner
	    (caddr setup)
	    (caddr cleanup)
	    (package-name package))))

(defvar *current-group* 'default)

;;;(make-instance 'test-group-info
;;;  :package (find-package 'common-lisp)
;;;  :name 'default-group
;;;  :documentation
;;;  "The default test group, for tests defined outside a
;;;def-test-group block.")

;;; -----------------------------------------------------------------

(defclass test-info (nst-class)
     ((group :initarg :group :type test-group-info)
      (name :initarg :name :reader get-name :type symbol)
      (core :initarg :core :type symbol
	    :documentation
	    "The symbolic name of the function defining the core check
of this test")
      (single :initarg :single :type symbol
	  :documentation
	  "The symbolic name of the function defining a one-off run of
this test")
      (documentation :initarg :documentation :type string))
  (:documentation
   "Record of information associated with each test"))

(defmethod nst-format (stream (info test-info) colon at-sign)
  (declare (ignorable colon) (ignorable at-sign))
  (with-slots (group name core single documentation) info
    (format stream "~@<~s~@[ ~_(~a)~]: ~
                    ~_core test ~s, ~
                    ~_single run ~s~:>"
	    name documentation core single)))

;;; -----------------------------------------------------------------

(defmacro def-fixtures (name outer-decls bindings inner-decls)
  (let* ((dynamic-decls (loop for binding in bindings
			      collect (list 'dynamic-extent
					     (car binding))))
	 (special-decls (loop for binding in bindings
			      collect (list 'special (car binding))))
	 (group-name (gensym "group-name-"))
	 (group-wrapping (gensym "group-wrapping-"))
	 (all-test-wrappings (gensym "all-test-wrappings-"))
	 (test-wrapping (gensym "test-wrapping-"))
	 (test-info (gensym "test-info-")))
    `(progn
       (declaim ,@special-decls)
       (setf (gethash ',name +fixture-info+) ',bindings)
       (def-fwrapper ,name ()
	 (declare ,@outer-decls)
	 (pre-intro-fixture-set ',name)
	 (let* ,bindings
	   (declare ,@dynamic-decls ,@inner-decls)
	   (post-intro-fixture-set ',name)
	   (call-next-fwrapper))
	 (outro-fixture-set ',name))
       (let ((,all-test-wrappings
	      (gethash ',name +mixin-to-test-to-wrapper+)))
	 (unless ,all-test-wrappings
	   (setf ,all-test-wrappings (make-hash-table)
		 (gethash ',name +mixin-to-test-to-wrapper+)
		 ,all-test-wrappings))
	 (if (gethash ',name +mixin-to-group-to-wrapper+)
	     (loop for ,group-name being the hash-keys
		   of (gethash ',name +mixin-to-group-to-wrapper+)
		   using (hash-value ,group-wrapping)
		   do
		(with-slots (runner) (gethash ,group-name +groups+)
		  (fwrap runner ,group-wrapping ',name))
		(loop for ,test-info in (gethash ,group-name
						 +tests-by-group+)
		      do
		   (with-slots (single name) ,test-info
		     (let ((,test-wrapping
			    (gethash name ,all-test-wrappings)))
		       (unless ,test-wrapping
			 (setf ,test-wrapping (gensym "test-wrapping-")
			       (gethash name ,all-test-wrappings)
			       ,test-wrapping))
		       (fwrap single ,test-wrapping ',name)))))

	   (setf (gethash ',name +mixin-to-group-to-wrapper+)
		 (make-hash-table))))
	 nil)))

(defmacro def-test-group (name fixture-names &rest forms)
  (let ((group-info (gensym "group-info-"))
	(len (length fixture-names))
	(group (gensym "group-"))
	(test (gensym "test-"))
	(fixture-name1 (gensym "fixture-"))
	(fixture-name2 (gensym "fixture-"))
	(fixture-vector (gensym "fixture-vector-"))
	(wrapping-hash (gensym "wrapping-hash-"))
	(wrapping (gensym "wrapping-"))
	(runner (gensym "runner-"))
	(doc-string nil)
	(setup-form nil)
	(cleanup-form nil)
	(tests nil))
    (loop for form in forms do
      (destructuring-bind (token &rest subforms) form
      (cond
	((eq token :documentation)
	 (setf doc-string (car subforms)))
	((eq token :setup)
	 (setf setup-form subforms))
	((eq token :cleanup)
	 (setf cleanup-form subforms))
	(t
	 (push form tests)))))
    (setf tests (nreverse tests))
    `(progn
       
       (defun ,runner ()
	 (let ((,group (gethash ',name nst::+groups+)))
	   (block group-exec
	     (with-slots (setup cleanup) ,group
	       (intro-group-setup ',name)
	       (when (block group-setup
		       (handler-bind
			   ((error
			     #'(lambda (c)
				 (push ,group *erred-groups*)
				 (format t
					 "Error in group ~s setup:
                                              ~%  ~s~%"
					 ,group c)
				 (outro-failed-group-setup ',name c)
				 (return-from group-setup nil))))
			 (eval setup)
			 (return-from group-setup t)))

		 (outro-group-setup ',name)

		 (unwind-protect
		     (block group-tests
		       (loop for ,test
			     in (gethash ',name nst::+tests-by-group+)
			     do
			  (with-slots (core) ,test
			    (funcall (symbol-function core)))))

		   (intro-group-cleanup ',name)
		   (eval cleanup)
		   (outro-group-cleanup ',name)))))))
       
       (let* ((,fixture-vector
	       (make-array ,len :initial-contents
			   (reverse ',fixture-names)))
	      (,group-info
	       (make-instance 'test-group-info
		 :package *package*
		 :name ',name
		 :fixtures ,fixture-vector
		 :setup '(block nil ,@setup-form)
		 :cleanup '(block nil ,@cleanup-form)
		 :documentation ,doc-string
		 :runner ',runner)))
	 (setf (gethash ',name nst::+tests-by-group+) nil)
	 (setf (gethash ',name nst::+groups+) ,group-info)
       
	 (loop for ,fixture-name1
		 being the hash-keys in +mixin-to-group-to-wrapper+
	       using (hash-value ,wrapping-hash)
	       do (unless ,wrapping-hash
		    (setf ,wrapping-hash (make-hash-table)
			  (gethash ,fixture-name1
				   +mixin-to-group-to-wrapper+)
			  ,wrapping-hash))
		  (remhash ',name ,wrapping-hash))

	 (loop for ,fixture-name2 across ,fixture-vector do
	   (let ((,wrapping-hash (gethash ,fixture-name2
					  +mixin-to-group-to-wrapper+))
		 (,wrapping (gensym "group-wrapping-")))
	     (unless ,wrapping-hash
	       (setf ,wrapping-hash (make-hash-table)
		     (gethash ,fixture-name2 
			      +mixin-to-group-to-wrapper+)
		     ,wrapping-hash))
	     (setf (gethash ',name ,wrapping-hash) ,wrapping)
	     (fwrap ',runner ,wrapping ,fixture-name2)))
	 
	 (let ((,wrapping-hash
		(gethash *package* +groups-by-package+)))
	   (unless ,wrapping-hash
	     (setf ,wrapping-hash (make-hash-table)
		   (gethash *package* +groups-by-package+)
		   ,wrapping-hash))
	   (setf (gethash ',name ,wrapping-hash) t))
	 
	 (let ((*current-group* ',name))
	   ,@tests)
	 
	 nil))))

(let ((*package* (find-package 'common-lisp)))
  (def-test-group default ()))

(defmacro def-test (name &key form)
  (let ((core-fn-name (gensym "core-fn-"))
	(single-fn-name (gensym "single-fn-"))
	(test-info (gensym "test-info-"))
	(group-info (gensym "group-info-"))
	(group-name (gensym "group-name-"))
	(setup-form (gensym "setup-form-"))
	(cleanup-form (gensym "cleanup-form-"))
	(fixture-name1 (gensym "fixture-info-"))
	(fixture-name2 (gensym "fixture-info-"))
	(wrapping-hash (gensym "wrapping-hash-"))
	(wrapping (gensym "wrapping-"))
	(test-result (gensym "result-"))
	(x (gensym "x")))

    `(progn
       (delete-if (lambda (,x) (eq (get-name ,x) ',name))
		  (gethash ',*current-group* +tests-by-group+))

       (defun ,core-fn-name ()
	 (intro-test-run ',name)
	 (block single-test
	   (handler-bind ((error
			   #'(lambda (,x)
			       (outro-test-error ',name ,x)
			       (return-from single-test 'err))))
	     (let ((,test-result ,form))
	       (when ,test-result
		 (setf *passed-test-count*
		       (+ 1 *passed-test-count*)))
	       (outro-test-run ',name ,test-result)
	       (return-from single-test (if ,test-result t nil))))))
       
       (defun ,single-fn-name ()
	 (let* ((,group-info (gethash ',*current-group* +groups+))
		(,group-name (get-name ,group-info))
		(,setup-form   (get-setup ,group-info))
		(,cleanup-form (get-cleanup ,group-info))
		(,test-result nil))

	   (intro-group-setup ,group-name)
	   (eval ,setup-form)
	   (outro-group-setup ,group-name)

	   (unwind-protect (setf ,test-result (,core-fn-name))
	     (intro-group-cleanup ,group-name)
	     (eval ,cleanup-form)
	     (outro-group-cleanup ,group-name))))
       
       (loop for ,fixture-name1
	       being the hash-keys in +mixin-to-test-to-wrapper+
	     using (hash-value ,wrapping-hash) 
	     do 
	  (unless ,wrapping-hash
	    (setf ,wrapping-hash (make-hash-table)
		  (gethash ,fixture-name1 +mixin-to-test-to-wrapper+)
		  ,wrapping-hash))
	  (remhash ',name ,wrapping-hash))

       (with-slots (fixtures) (gethash ',*current-group* +groups+)
	 (loop for ,fixture-name2 across fixtures do
	   (let ((,wrapping-hash
		  (gethash ,fixture-name2
			   +mixin-to-test-to-wrapper+)))
	     (unless ,wrapping-hash
	       (error "No such fixture set ~s" ,fixture-name2))
	     (let ((,wrapping (gethash ',name ,wrapping-hash)))
	       (unless ,wrapping
		 (setf ,wrapping (gensym "test-wrapping-")
		       (gethash ',name ,wrapping-hash) ,wrapping))
	       (fwrap ',single-fn-name ,wrapping ,fixture-name2)))))
       
       (let ((,test-info (make-instance 'test-info
			  :group ',*current-group*
			  :name ',name
			  :core ',core-fn-name
			  :single ',single-fn-name
			  :documentation nil)))
	 (push ,test-info
	       (gethash ',*current-group* +tests-by-group+))))))


(defmacro intro-test-run (name)
  `(when (show-verbose-output) 
     (format t " - Running test ~s..." ,name)))

(defmacro outro-test-run (name passed)
  `(if (show-verbose-output) 
       (format t "~a~%" (if ,passed "passed" "failed"))
       (unless ,passed
	 (format t "Test ~s failed~%" ,name))))

(defmacro outro-test-error (name msg)
  `(progn
     (if (show-verbose-output) 
	 (format t "error~%")
	 (format t "Test ~s raised error~%" ,name))
     (format t "   ~s~%" ,msg)))

(defmacro intro-group-setup (name)
  `(when (show-verbose-output) 
     (format t "Setting up tests in group ~s..." ,name)))

(defmacro outro-group-setup (name)
  (declare (ignorable name))
  `(when (show-verbose-output) (format t "done~%")))

(defmacro outro-failed-group-setup (name msg)
  (declare (ignorable name))
  `(when (show-verbose-output) (format t "failed:~%  ~s~%" ,msg)))

(defmacro intro-group-cleanup (name)
  `(when (show-verbose-output) 
     (format t "Cleaning up after tests in group ~s..." ,name)))

(defmacro outro-group-cleanup (name)
  (declare (ignorable name))
  `(when (show-verbose-output) (format t "done~%")))


(defmacro pre-intro-fixture-set (name)
  `(when (show-verbose-output) 
     (format t "Incorporating fixture set ~s..." ,name)))

(defmacro post-intro-fixture-set (name)
  (declare (ignorable name))
  `(when (show-verbose-output) (format t "done~%")))

(defmacro outro-fixture-set (name)
  `(when (show-verbose-output) 
     (format t "Releasing fixture set ~s.~%" ,name)))

;;; -----------------------------------------------------------------

(defmacro under-empty-pendings (&rest forms)
  `(let ((*pending-packages* ())
	 (*pending-group-names* ())
	 (*pending-test-names* (make-hash-table))
	 (*passed-test-count* 0)
	 (*erred-groups* nil)
	 (*erred-cleanup* nil)
	 (*failed-tests* (make-hash-table))
	 (*erred-tests* (make-hash-table)))
     ,@forms
     (run-pending)
     (report-last-run)))

(defmacro reset-pending ()
  (let ((group-name   (gensym "-group-name"))
	(test-set     (gensym "-test-set"))
	(new-test-set (gensym "-new-test-set"))
	(test-name    (gensym "-test-name")))
    `(progn
       (setf *pending-packages* *interesting-packages*
	     *pending-group-names* *interesting-group-names*

	     *passed-test-count* 0
	     *erred-groups* nil
	     *erred-cleanup* nil)

       (clrhash *pending-test-names*)
       (clrhash *failed-tests*)
       (clrhash	*erred-tests*)
       
       (loop for ,group-name
	       being the hash-keys of +interesting-test-names+
	     using (hash-value ,test-set)
	     do
	  (let ((,new-test-set (make-hash-table)))
	    (setf (gethash ,group-name *pending-test-names*)
		  ,new-test-set)
	    (loop for ,test-name being the hash-keys of ,test-set do
	      (setf (gethash ,test-name ,new-test-set) t)))))))

(defmacro run-pending ()
  "Run pending tests"
  (let ((package-name (gensym "package-name-"))
	(package (gensym "package-"))
	(group-name (gensym "group-name-"))
	(group-info (gensym "group-info-"))
	(group-set (gensym "group-set-"))
	(test-set (gensym "test-set-"))
	(test-name (gensym "test-name-"))
	(test-info-list (gensym "test-info-list-"))
	(test-info (gensym "test-info-"))
	(flag (gensym "flag-"))
	(more (gensym "more-")))
    `(progn
       (let ((,more t))
	 (block pending-loop
	   (loop while ,more do
	     (setf ,more nil)
	     (loop for ,group-name
		     being the hash-keys of *pending-test-names*
		   using (hash-value ,test-set)
		   do
		(loop for ,test-name being the hash-keys of ,test-set
		      using (hash-value ,flag)
		      do
		   (let* ((,test-info-list
			   (gethash ,group-name +tests-by-group+))
			  (,test-info
			   (block nil
			     (loop for cand in ,test-info-list do
			       (when (eq (get-name cand) ,test-name)
				 (return cand))))))
		     (unless ,test-info
		       (error "No such test ~s in group ~s"
			      ,test-name ,group-name))
		     (with-slots (single) ,test-info
		       (funcall (symbol-function single))))
		   (remhash ,test-name ,test-set))
		(remhash ,group-name *pending-test-names*))
	     (loop as ,group-name = (pop *pending-group-names*)
		   while ,group-name
		   do
		(let ((,group-info (gethash ,group-name +groups+)))
		  (if ,group-info
		      (with-slots (runner) ,group-info
			(funcall (symbol-function runner)))
		      (format t "WARNING: No such group ~s~%"
			      ,group-name))))
	     (loop as ,package-name = (pop *pending-packages*)
		   while ,package-name
		   do
		(let* ((,package (find-package ,package-name))
		       (,group-set (gethash ,package
					    +groups-by-package+)))
		  (if ,group-set
		      (loop for ,group-name
			      being the hash-keys of ,group-set
			    using (hash-value ,flag)
			    do
			 (when ,flag
			   (push ,group-name *pending-group-names*)
			   (setf ,more t)))
		      (format t "WARNING: no groups in package ~s~%"
			      ,package))))))))))

(defmacro report-last-run ()
  (let ((hash (gensym "hash-")))
    `(format t "~%SUMMARY~%~
                Tests passed: ~d~%~
                Tests failed: ~d~%~
                Tests raising error: ~d~%~
                Groups raising error in setup: ~d~%~
                Groups raising error in cleanup: ~d~%"
	     *passed-test-count*
	     (loop for ,hash being the hash-values of *failed-tests*
		   summing (hash-table-count ,hash))
	     (loop for ,hash being the hash-values of *erred-tests*
		   summing (hash-table-count ,hash))
	     (length *erred-groups*)
	     (length *erred-cleanup*))))

;;; -----------------------------------------------------------------

(top-level:alias "nst" (&rest args)
  "Invoke the NST tester from the top-level"
  (if (null args)
      (progn
	(format t "Argument-free behavior of :nst not implemented"))
      (block nil
	  (loop do
	    (if (null args) (return))
	    (let ((head (pop args)))
	      (macrolet
		  ((pop-arg (want have)
		     `(cond 
			((null args)
			 (format t "Command ~s requires ~d~:* ~
                               argument~[s~;~;s~] but given ~d.~%"
				 head ,want ,have)
			 (return))
			(t (pop args))))
		   (command-case (args &rest forms)
		     (let* ((want (length args))
			    (arg-bindings
			     (loop for arg in args and have from 0
				   collect
				   (list arg
					 (list 'pop-arg want have)))))
		       `(let ,arg-bindings ,@forms)))
		   (warn-unimplemented (&rest forms)
		     `(progn ,@forms
			     (format t "Command ~s not implemented~%"
				     head))))
		(cond
		  ((or (eq head ':help)
		       (eq head 'help)
		       (eq head 'h))
		   (format t "NST test framework control

OUTPUT CONTROL
  :nst :help
	Show this help message and exit.
  :nst :verbose FORM
	Set whether verbose output should be generated.
  :nst :debug BOOL
	Set whether NST debugging messages should be generated.
  :nst :dump
	Print the state of the test system.

MARKING TESTS OF INTEREST FOR EXECUTION
  :nst :p PACKAGE
	Mark a package as to be tested.
  :nst :g GROUP
	Mark a group as to be tested.
  :nst :t TEST
	Mark a single test as to be run.

CONTROLLING TEST SUITE EXECUTION BEHAVIOR
  :nst :break-on-wrong BOOL
	Set whether a failing test should cause test execution to
	pause.  Not yet implemented.
  :nst :break-on-error BOOL
	Set whether any error in a test run should cause test
	execution to pause.  Not yet implemented.
  :nst :debug-on-error
	Set whether an error in a test run should drop us into debug
	mode.  Not yet implemented.

TEST SUITE EXECUTION
  :nst :run
	Run all marked tests.
  :nst :continue
	Continue running tests after an interruption arising from one
	of the three flags above.
  :nst :retry
	Retry failed or error-raising tests from the last run.
  :nst :blurb TESTNAME
	Describe the outcome of a test in the last run.  Not yet
	implemented.

ONE-OFF EXECUTION
  :nst :run-package PACKAGE
	Run all of the tests in a package.
  :nst :run-group GROUP
	Run all of the tests in a single group.
  :nst :run-test GROUP TEST
	Run a single test.

Multiple NST commands can be combined at one prompt, e.g.
  :nst :p *package* :g aux::key-tests :run
")
		 (return))

		((eq head ':verbose)
		 (command-case (flag)
		   (setf *verbose-output* flag)
		   (format t "~:[Deactivated~;Activated~] ~
                                 verbose output.~%" flag)))

		((eq head ':debug)
		 (command-case (flag)
		   (setf *debug-output* flag)
		   (format t "~:[Deactivated~;Activated~] ~
                                 debugging output.~%" flag)))

		((or (eq head ':dump) (eq head 'dump))
		 (nst-dump t))

		((eq head ':break-on-wrong)
		 (command-case (flag)
		   (setf *break-on-wrong* flag)
		   (format t "~:[Deactivated~;Activated~] ~
                              breaking on test failure.~%" flag)))

		((eq head ':break-on-error)
		 (command-case (flag)
		   (setf *break-on-error* flag)
		   (format t "~:[Deactivated~;Activated~] ~
                                breaking on raised errors.~%" flag)))

		((eq head ':continue)
		 (command-case ()
		    (run-pending)))

		((eq head ':debug-on-error)
		 (command-case (flag)
		   (setf *debug-on-error* flag)
		   (format t "~:[Deactivated~;Activated~] ~
                                debugging on raised errors~%" flag)))

		((eq head ':blurb)
		 (command-case ()
		   (warn-unimplemented)))

		((eq head ':p)
		 (command-case (package-name)
		   (let ((package (find-package package-name)))
		     (if package
			 (progn
			   (unless (member package
					   *interesting-packages*)
			     (push package *interesting-packages*))
			   (unless (member package *pending-packages*)
			     (push package *pending-packages*))
			   (format t "Marked package ~s for testing~%"
				   package-name))
			 (format t "ERROR: cannot find package ~s~%"
				 package-name)))))

		((eq head ':g)
		 (command-case (group-name)
		   (if (gethash group-name +groups+)
		       (progn
			 (unless (member group-name
					 *interesting-group-names*)
			   (push group-name
				 *interesting-group-names*))
			 (unless (member group-name
					 *pending-group-names*)
			   (push group-name *pending-group-names*))
			 (format t "Marked group ~s for testing~%"
				 group-name))
		       (format t "ERROR: cannot find group ~s~%"
			       group-name))))

		((eq head ':t)
		 (command-case (group-name test-name)
		    (if (gethash group-name +groups+)
			(progn
			  (let ((i-tests
				 (gethash group-name
					  +interesting-test-names+))
				(p-tests
				 (gethash group-name
					  *pending-test-names*)))
			  (unless i-tests
			    (setf i-tests (make-hash-table)
				  (gethash group-name
					   +interesting-test-names+)
				  i-tests))
			  (unless p-tests
			    (setf p-tests (make-hash-table)
				  (gethash group-name
					   *pending-test-names*)
				  p-tests))
			  (setf (gethash test-name i-tests) t
				(gethash test-name p-tests) t))
			  (when (show-verbose-output)
			    (format t "Marked test ~s (group ~s) ~
                                       for testing."
				    test-name group-name)))
			(format t "ERROR: cannot find group ~s~%"
				group-name))))

		((eq head ':run)
		 (reset-pending)
		 (run-pending))

		((eq head ':run-package)
		 (command-case (package-name)
		  (under-empty-pendings
		   (push (find-package package-name)
			 *pending-packages*))
		  ))

		((eq head ':run-group)
		 (command-case (group)
		  (under-empty-pendings
		   (push group *pending-group-names*))))

		((eq head ':run-test)
		 (command-case (group test)
		  (under-empty-pendings
		   (let ((singleton (make-hash-table)))
		     (setf 
		      (gethash test singleton) t
		      (gethash group
			       *pending-test-names*) singleton)))))

		((eq head ':retry)
		 (command-case ()
		   (loop as group = (pop *erred-groups*)
			 while group
			 do
		      (push group *pending-group-names*))
		   (loop as group = (pop *erred-cleanup*)
			 while group
			 do
		      (push group *pending-group-names*))
		   (loop for test-hash
			 in (list *failed-tests* *erred-tests*)
			 do 
		      (loop for group
			      being the hash-keys in test-hash
			    using (hash-value test-set)
			    do
			 (let ((new-test-set
				(gethash group *pending-group-names*)))
			   (unless new-test-set
			     (setf new-test-set (make-hash-table)
				   (gethash group
					    *pending-group-names*)
				   new-test-set))
			   (loop for test
				   being the hash-keys in test-set
				 using (hash-value flag)
				 do
			      (when flag
				(setf (gethash test new-test-set) t))))
			 (remhash group test-hash)))))

		(t
		 (format t "Unrecognized NST command ~s~%~
                          For more options, use :nst :help~%~%"
			 head)))))))))

(defun format-binding (stream tup c s)
  (declare (ignorable c) (ignorable s))
  (format stream "~s <- ~s" (car tup) (cadr tup)))

(defun format-groups (stream groups c s)
  (declare (ignorable c) (ignorable s))
  (loop for info being the hash-values of groups
	using (hash-key name)
	do
     (format stream "Group ~/nst::nst-format/~%~
                     ~:[ - No tests in group.~%~;~:*~{ - Test ~/nst::nst-format/~%~}~]"
	     info (gethash name +tests-by-group+))
	))

(defconstant +group-test-name-formatter+
     "~:[none~;~:*~@<~{~/nst::format-group-test-list/~^, ~_~}~:>~]")
(defun format-group-test-list (stream item s c)
  (declare (ignorable s) (ignorable c))
  (format stream "~s.~s" (car item) (cadr item)))

(defun nst-dump (stream)
  (macrolet ((group-test-names-from-hashes (hash)
	       (let ((all (gensym "all-"))
		     (group (gensym "group-"))
		     (test (gensym "test-"))
		     (flag (gensym "flag-"))
		     (name-hash (gensym "name-hash-")))
		 `(let ((,all nil))
		    (loop for ,group being the hash-keys
			  of ,hash
			  using (hash-value ,name-hash)
			  do
		       (loop for ,test being the hash-keys
			     of ,name-hash
			     using (hash-value ,flag)
			     do
			  (if ,flag (push (list ,group ,test) ,all))))
		    ,all))))
    (unless (eql 0 (hash-table-count +fixture-info+))
      (format stream "~%FIXTURE SETS~%")
      (loop for name being the hash-keys of +fixture-info+
	    using (hash-value val)
	    do
	 (format stream "Set ~s binds ~
                     ~@<~{~/nst::format-binding/~^, ~}~:>~%"
		 name val)))

    (unless (eql 0 (hash-table-count +groups+))
      (format stream "~%TEST GROUPS~%")
      (format stream "~/nst::format-groups/" +groups+))

    (unless (eql 0 (hash-table-count +fixture-info+))
      (format stream "~%FIXTURE WRAPPINGS~%")
      (loop for fixture-name being the hash-keys of +fixture-info+ do
	(format stream " - Fixture ~s~%" fixture-name)
	(loop for group-name being the hash-keys
	      of (gethash fixture-name +mixin-to-group-to-wrapper+)
	      using (hash-value wrapping)
	      do (format stream "    . Group ~s by ~s~%"
			 group-name wrapping))
	(loop for test-name being the hash-keys
	      of (gethash fixture-name +mixin-to-test-to-wrapper+)
	      using (hash-value wrapping)
	      do (format stream "    . Test ~s by ~s~%"
			 test-name wrapping))))
    
    (format stream "~%~
     SETTINGS~%~
     Verbose output: ~:[off~;on~]~%~
     Debugging output: ~:[off~;on~]~%~
     Break on failed test: ~:[off~;on~]~%~
     Break on error-throwing test: ~:[off~;on~]~%~
     Debug mode on error-throw: ~:[off~;on~]~%~
     ~%~
     SCHEDULED TESTS~%~
     Scheduled packages: ~:[none~;~:*~{~a~^, ~}~]~%~
     Scheduled groups: ~:[none~;~:*~{~s~^, ~}~]~%~
     Scheduled tests: ~@?~%~
     ~%~
     CURRENTLY RUNNING TESTS~%~
     Packages pending this run: ~:[none~;~:*~{~a~^, ~}~]~%~
     Groups pending this run: ~:[none~;~:*~{~s~^, ~}~]~%~
     Tests pending this run: ~@?~%~
     ~%~
     RECENT RESULTS~%~
     Tests passed this run: ~d~%~
     Groups incurring errors this run: ~
        ~:[none~;~:*~{~s~^, ~}~]~%~
     Tests erring this run: ~@?~%~
     Tests failed this run: ~@?~%~
     ~%"
	    *verbose-output* *debug-output*
	    *break-on-wrong* *break-on-error* *debug-on-error*
	    (map 'list #'package-name *interesting-packages*)
	    *interesting-group-names*
	    +group-test-name-formatter+
	    (group-test-names-from-hashes +interesting-test-names+)
	    (map 'list #'package-name *pending-packages*)
	    *pending-group-names*
	    +group-test-name-formatter+
	    (group-test-names-from-hashes *pending-test-names*)
	    *passed-test-count*
	    *erred-groups*
	    +group-test-name-formatter+
	    (group-test-names-from-hashes *failed-tests*)
	    +group-test-name-formatter+
	    (group-test-names-from-hashes *erred-tests*))))
