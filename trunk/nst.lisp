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
;;;
;;; 5. Provide "open" methods (or macros, or whatever) to put fixtures
;;; into the CL-USER namespace.

;;; Global variables for the state of the test runner.

(defconstant +fixture-info+ (make-hash-table)
  "For user echo of fixture forms and other debugging." )
(defconstant +groups+ (make-hash-table)
  "Declared test NST groups")
(defconstant +groups-by-package+ (make-hash-table)
  "Hash table from packages to sets of groups, that is, to
other hash tables each mapping group records to t or nil.")

(defvar *debug-output* nil
  "Set to t for extensive debugging output")
(defun show-debug-output () *debug-output*)

(defvar *verbose-output* nil
  "Set to t for verbose output during test execution")
(defun show-verbose-output () (or *verbose-output* *debug-output*))

(defvar *break-on-wrong* nil
  "When set to t, directs the test runner to return to the command
line whenever a test does not succeed.")
(defvar *break-on-error* nil
  "When set to t, directs the test runner to return to the command
line whenever a test raises an error condition, rather than returning
a boolean value.")
(defvar *debug-on-error* nil
  "When set to t, directs the test runner to return in debugging mode
whenever a test raises an error condition, rather than returning a
boolean value.")

(defvar *interesting-packages* nil
  "The names of packages whose tests should be checked by the :run
command to the NST runtime system.")
(defvar *interesting-group-names* nil
  "The names of groups whose tests should be checked by the :run
command to the NST runtime system.")
(defvar +interesting-test-names+ (make-hash-table)
  "The names of groups whose tests should be checked by the :run
command to the NST runtime system.  The hash is against first group
names, and then test names.")

(defvar *pending-packages* nil
  "The names of packages whose tests remain to be checked under the
current :run session of the NST runtime system.")
(defvar *pending-group-names* nil
  "The names of groups whose tests remain to be checked under the
current :run session of the NST runtime system.")
(defvar *pending-test-names* (make-hash-table)
  "The names of groups whose tests remain to be checked under the
current :run session of the NST runtime system.  The hash is against
first group names, and then test names.")

(defparameter *passed-test-count* 0
  "The number of tests passed under the current :run session of the NST
runtime system.")
(defparameter *erred-groups* nil
  "The names of groups raising an error in setup during the current
:run session of the NST runtime system.")
(defparameter *erred-cleanup* nil
  "The names of groups raising an error in cleanup during the current
:run session of the NST runtime system.")
(defparameter *failed-tests* (make-hash-table)
  "The test info records of tests failing during the current :run
session of the NST runtime system.")
(defparameter *erred-tests* (make-hash-table)
  "The test info records of tests raising an error condition during the
current :run session of the NST runtime system.")

;;; This group of macros summarizes the control decisions for output
;;; during test execution.  It's convenient to write this logic
;;; separately from the rest of the system.

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

(defclass nst-class () ())
(defgeneric nst-format (stream obj c s))
(defmethod print-object ((obj nst-class) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~/nst::nst-format/" obj)))

(defclass fixture (nst-class) ()
  (:documentation "Class of bindings usable in tests"))

(defclass test (nst-class)
     ((group     :initarg :group :type group)
      (test-name :initarg :name  :type symbol :reader get-name)
      (documentation :initarg :documentation :type string))
  (:documentation "Information associated with one single test."))

(defmethod nst-format (stream (info test) colon at-sign)
  (declare (ignorable colon) (ignorable at-sign))
  (with-slots (group test-name documentation) info
    (format stream "~@<~s (~s)~@[ ~_(~a)~]~:>"
	    test-name (get-name group) documentation)))

(defclass group (nst-class)
     ((package :initarg :package)
      (group-name :initarg :name :type symbol :reader get-name)
      (test-names :type (vector symbol) :reader get-test-names)
      (tests-hash :type hash-table :reader get-tests-hash)
      (setup   :initarg :setup   :initform nil :reader get-setup)
      (cleanup :initarg :cleanup :initform nil :reader get-cleanup)
      (testclass :initarg :testclass :type symbol)
      (fixtures  :initarg :fixtures  :type (cons symbol)
		:reader get-fixtures)
      (documentation :initarg :documentation :type string))
  (:documentation "Information associated with one group of tests."))

(defmethod nst-format (stream (info group) colon at-sign)
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
	    (caddr setup)
	    (caddr cleanup))))

(defgeneric core (test)
  (:documentation "Run a test, a group of tests, or all the tests
associated with a package."))

(defmethod core :around (test)
  "The primary core method simply evaluates the form given in the
test definition, which could return an arbitrary value, or raise an
error.  This wrapper intercepts errors and standardizes the return
value to one of t, nil or 'err.  Here we also make the calls to our
hook macros for output before and after indiviual tests."
  (with-slots (test-name) test
    (intro-test-run test-name)
    (block single-test
      (handler-bind ((error
		      #'(lambda (x)
			  (outro-test-error test-name x)
			  (return-from single-test 'err))))
	(let ((result (call-next-method test)))
	  (when result
	    (setf *passed-test-count* (+ 1 *passed-test-count*)))
	  (outro-test-run test-name result)
	  (return-from single-test (if result t nil)))))))

(defmacro do-setup (group group-name group-setup-form
			  &rest other-forms)
  (let ((c (gensym)))
    `(progn
       (intro-group-setup ,group-name)

       (when (block group-setup
	       (handler-bind
		   ((error
		     #'(lambda (,c)
			 (push ,group *erred-groups*)
			 (format t "Error in group ~s setup:~%  ~s~%"
				 ,group-name ,c)
			 (outro-failed-group-setup ,group-name ,c)
			 (return-from group-setup nil))))
		 (eval ,group-setup-form)
		 (return-from group-setup t)))
    	 (outro-group-setup group-name)
	 ,@other-forms))))

(defgeneric run (ptg)
  (:documentation "Run a test, a group of tests, or all the tests
associated with a package.")

  (:method ((ts test))
     (with-slots (test-name group) ts
       (with-slots (group-name setup cleanup) group
	
	 (do-setup
	     group group-name setup
	 
	     (let ((test-result nil))
	       (declare (ignore test-result))
	       (unwind-protect (setf test-result (core ts))
		 (intro-group-cleanup group-name)
		 ;; TO DO - catch errors here
		 (eval cleanup)
		 (outro-group-cleanup group-name)))))))
  
  (:method ((g group))
     (block group-exec
       (with-slots (group-name setup cleanup test-names tests-hash) g
	 (do-setup
	     g group-name setup
	       
	     (unwind-protect
		 (block group-tests
		   (loop for test-name across test-names do
		     (core (gethash test-name tests-hash))))
	       
	       (intro-group-cleanup group-name)
	       (eval cleanup)
	       (outro-group-cleanup group-name)))))))

(defmacro def-fixtures (name bindings &key
			     outer inner documentation)
  (let* ((dynamic-decls
	  (loop for binding in bindings
		collect (list 'dynamic-extent (car binding))))
	 
	 (special-decls
	  (loop for binding in bindings
		collect (list 'special (car binding))))
	 
	 (doc-forms (if documentation 
			(list (list ':documentation documentation))
			nil))
	 
	 (ptg (gensym "ptg-")))

    `(progn
       (declaim ,@special-decls)
       (defclass ,name (fixture) () ,@doc-forms)
       (setf (gethash ',name +fixture-info+) ',bindings)
       (defmethod run :around ((,ptg ,name))
	 (declare ,@outer)
	 (pre-intro-fixture-set ',name)
	 (let* ,bindings
	   (declare ,@dynamic-decls ,@inner)
	   (post-intro-fixture-set ',name)
	   (call-next-method))
	 (outro-fixture-set ',name))
       nil)))

(defmacro def-capture/restore-fixtures (name variables
					     &key documentation)
  (let ((nil-bindings (loop for v in variables
			    collect (list v nil))))
    `(def-fixtures ,name ,nil-bindings
       :documentation ,documentation)))

;;; -----------------------------------------------------------------

(defmacro def-test-group (group-name fixture-names &rest forms)
  (let ((doc-string nil) (tests nil)
	(setup-form nil) (cleanup-form nil)
	
	(test-class (gensym "test-class-"))
	(tests-acc (gensym "tests-acc-"))
	(group-class (gensym "group-class-"))
	(singleton (gensym "singleton-"))
	(wrapping-hash (gensym "wrapping-hash-"))
	
	(class-doc 
	 (format nil "Class definition corresponding to test group ~s"
		 group-name)))

    (loop for form in forms do
      (destructuring-bind (token &rest subforms) form
      (cond
	((eq token :documentation) (setf doc-string (car subforms)))
	((eq token :setup)         (setf setup-form subforms))
	((eq token :cleanup)       (setf cleanup-form subforms))
	((eq token 'def-test)      (push form tests))
	(t (error "~@<Illegal form in def-test-group:~_ ~s~:>~%"
		  form)))))
    (setf tests (nreverse tests))
      
    `(progn
       (defclass ,group-class (,@fixture-names group) ()
	 (:documentation ,class-doc))
       (defclass ,test-class (,@fixture-names test) ()
	 (:documentation ,class-doc))
       (let ((,singleton (make-instance ',group-class
			   :package *package* :name ',group-name
			   :fixtures ',fixture-names
			   :setup '(block nil ,@setup-form)
			   :cleanup '(block nil ,@cleanup-form)
			   :testclass ',test-class
			   :documentation ,doc-string))
	     (,tests-acc nil))
	 (declare (ignorable ,tests-acc))
	 (defmethod run-group ((g (eql ',group-name)))
	   (run ,singleton))
	 (setf (gethash ',group-name +groups+) ,singleton)
	 (let ((,wrapping-hash (gethash *package*
					+groups-by-package+)))
	   (unless ,wrapping-hash
	     (setf ,wrapping-hash (make-hash-table)
		   (gethash *package*
			    +groups-by-package+) ,wrapping-hash))
	   (setf (gethash ',group-name ,wrapping-hash) t))

	 (let ((*test-class-symbol* ',test-class)
	       (*current-group-name* ',group-name)
	       (*current-group-info* ,singleton)
	       (*test-names-acc* nil)
	       (*test-info-hash* (make-hash-table)))
	   (declare (ignorable *test-class-symbol*)
		    (ignorable *current-group-name*)
		    (ignorable *current-group-info*))
	   ,@tests
	   (let ((tests-vector
		  (make-array (length *test-names-acc*)
		      :initial-contents (nreverse *test-names-acc*))))
	     (setf (slot-value ,singleton
			       'test-names) tests-vector
		   (slot-value ,singleton
			       'tests-hash) *test-info-hash*)))
	 nil))))

(defmacro def-test (test-name &key form)
  (let ((test-info (gensym "test-info-")))
		    
    `(progn
       ;; (unless (and (boundp '*test-class-symbol*)
       ;; 	    (boundp '*current-group-name*)
       ;; 	    (boundp '*current-group-info*)
       ;; 	    (boundp '*test-names-acc*)
       ;; 	    (boundp '*test-info-hash*))
       ;;   (error "Use of def-test outside of a test group body"))

       (let ((,test-info (make-instance *test-class-symbol*
			   :group *current-group-info*
			   :name ',test-name
			   :documentation nil)))
	 (push ',test-name *test-names-acc*)
	 (setf (gethash ',test-name *test-info-hash*) ,test-info)
	 (defmethod core ((ts (eql ,test-info))) ,form)
	 (defmethod run-test ((gr (eql *current-group-name*))
			      (ts (eql ',test-name)))
	   (run ,test-info))))))

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
	(group-test-hash (gensym "group-test-hash-"))
	(group-set (gensym "group-set-"))
	(test-set (gensym "test-set-"))
	(test-name (gensym "test-name-"))
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
		(let ((,group-test-hash
		       (get-tests-hash (gethash ,group-name
						+groups+))))
		  (loop for ,test-name
			  being the hash-keys of ,test-set
			using (hash-value ,flag)
			do
		     (remhash ,test-name ,test-set)
		     (when ,flag
		       (let* ((,test-info (gethash ,test-name
						   ,group-test-hash)))
			 (unless ,test-info
			   (error "No such test ~s in group ~s"
				  ,test-name ,group-name))
			 (run ,test-info)))))
		(remhash ,group-name *pending-test-names*))
	     (loop as ,group-name = (pop *pending-group-names*)
		   while ,group-name
		   do
		(let ((,group-info (gethash ,group-name +groups+)))
		  (if ,group-info
		      (run ,group-info)
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

(defun format-binding (stream tup c s)
  (declare (ignorable c) (ignorable s))
  (format stream "~s <- ~s" (car tup) (cadr tup)))

(defun format-groups (stream groups c s)
  (declare (ignorable c) (ignorable s))
  (loop for info being the hash-values of groups
	using (hash-key name)
	do
     (format stream "Group ~/nst::nst-format/~%" info)
     (with-slots (test-names tests-hash) info
       (if (eql (length test-names) 0)
	   (format stream " - No tests in group.~%")
	   (loop for test-name across test-names do
	     (format stream " - Test ~/nst::nst-format/~%"
			    (gethash test-name tests-hash)))))))

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
      (format stream "~%TEST GROUPS~%~/nst::format-groups/" +groups+))
    
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

(defconstant +nst-top-help+ "NST test framework control

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

(defun run-nst-commands (&rest args)
  "Top-level command interpreter for the NST tester"
  (block runner
  
    (when (null args)
      (progn
	(format t "Argument-free behavior of :nst not implemented"))
      (return-from runner))

    (loop do
      (if (null args) (return-from runner))

      (block single-command
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
	       
	       (command-case (synonyms args &rest forms)
		 (let* ((want (length args))
			(arg-bindings
			 (loop for arg in args and have from 0
			       collect
			       (list arg (list 'pop-arg want have)))))
		   `(when (member head ',synonyms)
		      (let ,arg-bindings ,@forms)
		      (return-from single-command))))

	       (command-case-flag-setter (synonyms variable blurb)
		 (let ((flag (gensym)))
		   `(command-case ,synonyms (,flag)
			(setf ,variable ,flag)
			(format t "~:[Deactivated~;Activated~] ~a.~%"
				,flag ,blurb))))
	       
	       (warn-unimplemented (&rest forms)
		 `(progn ,@forms
			 (format t "Command ~s not implemented~%"
				 head))))

	    (command-case (:help help h) ()
		(format t "~a" +nst-top-help+)
		(return-from runner))
	    
	    (command-case-flag-setter (:verbose) *verbose-output*
				      "verbose output")
	    
	    (command-case-flag-setter (:debug) *debug-output*
				      "debugging output")

	    (command-case-flag-setter (:break-on-wrong)
				      *break-on-wrong*
				      "breaking on test failure")

	    (command-case-flag-setter (:break-on-error)
				      *break-on-error*
				      "breaking on raised errors")

	    (command-case-flag-setter (:debug-on-error)
				      *debug-on-error*
				      "debugging on raised errors")

	    (command-case (:dump dump) ()
		(nst-dump t))

	    (command-case (:continue) ()
		(run-pending))

	    (command-case (:blurb) ()
		(warn-unimplemented))

	    (command-case (:p) (package-name)
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
			      package-name))))

	    (command-case (:g) (group-name)
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
			    group-name)))

	    (command-case (:t) (group-name test-name)
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
			    group-name)))

	    (command-case (:run) ()
		(reset-pending)
		(run-pending))

	    (command-case (:run-package) (package-name)
		(under-empty-pendings
		 (push (find-package package-name)
		       *pending-packages*)))

	    (command-case (:run-group) (group)
		(under-empty-pendings
		 (push group *pending-group-names*)))

	    (command-case (:run-test) (group test)
		(under-empty-pendings
		 (let ((singleton (make-hash-table)))
		   (setf 
		    (gethash test singleton) t
		    (gethash group 
			     *pending-test-names*) singleton))))

	    (command-case (:retry) ()
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
	  
	  (format t "Unrecognized NST command ~s~%~
                     For more options, use :nst :help~%~%"
		  head))))))

#+allegro
(top-level:alias "nst" (&rest args)
  (apply #'run-nst-commands args))

