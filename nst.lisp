;;; File nst.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :nst)

;;; KNOWN ISSUES
;;;
;;; 1. What's up with printing the conditions that come back from
;;; erring tests?
;;;
;;; 2. There are still, sometimes, warnings from the use of fixture
;;; names in other fixtures.
;;;
;;; 3. We are not yet catching errors arising from cleanup, and
;;; there's not great feedback from erring setups.
;;;
;;; 4. Clobbering test/group names should be caught by the macros, not
;;; by the lower-level expanded code defining methods.
;;;
;;;
;;; PLANNED FEATURES
;;;
;;; 1. Provide additional functionality to def-check --- other checks
;;; on lists, checks on other data structures.
;;;
;;; 2. Provide a def-check-method to define additional keywords to the
;;; def-check method form.
;;;
;;; 3. Maybe run-nst-commands should be turned in to a macro.
;;;
;;; 4. The def-check macro should be able to provide better feedback
;;; when failing (the kind of failing that doesn't throw an error),
;;; not just at the time, but also when :blurb'ing tests
;;; after-the-fact.
;;;
;;; 5. Texinfo the user guide.

;;; Options for output in the interactive system.
;;;
(defvar *debug-output* nil
  "Set to t for extensive debugging output")
(defvar *verbose-output* nil
  "Set to t for verbose output during test execution")
(defvar *scheduled-summary-output* t
  "Set to t for summaries of runs of scheduled tests.")
(defvar *scheduled-single-output* nil
  "Set to t for summaries of single test, group or package runs.")
(defvar *defer-test-compile* t
  "Set to t to defer compilation of test forms until runtime.")

;;; Options for breaking at failed and erroneous tests in the
;;; interactive system.
;;;
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

;;; Options for opening fictures into the interactive system.
;;;
(defvar *open-used-fixtures* t
  "If t, then (re-)opening a fixture will always (re-)open the fixtures
it uses.")
(defvar *reopen-fixtures* nil
  "If nil, then will will never open the same fixture twice.")

;;; The fixtures, groups and tests that have been defined.
;;;
(defvar +fixtures+ nil
  "For user echo of fixture forms and other debugging." )
(defvar +fixture-def-names+ (make-hash-table)
  "The names bound by each fixture.")
(defvar +group-def-names+ (make-hash-table)
  "The names bound by each group" )
(defvar +groups+ (make-hash-table)
  "Declared test NST groups")
(defvar +groups-by-package+ (make-hash-table)
  "Hash table from packages to sets of groups, that is, to
other hash tables each mapping group records to t or nil.")

;;; The packages, groups and tests that have been marked as
;;; interesting for quick execution in the runtime system.
;;;
(defvar *interesting-packages* nil
  "The names of packages whose tests should be checked by the :run
command to the NST runtime system.")
(defvar *interesting-group-names* nil
  "The names of groups whose tests should be checked by the :run
command to the NST runtime system.")
(defvar *interesting-test-names* (make-hash-table)
  "The names of groups whose tests should be checked by the :run
command to the NST runtime system.  The hash is against first group
names, and then test names.")

(defmacro have-interesting-tests ()
  "Poll the above variables to check for interesting tests."
  `(or *interesting-packages*
       *interesting-group-names*
       (> (hash-table-count *interesting-test-names*) 0)))


;;; The packages, groups and tests that remain to be run in the
;;; current :run session.
;;;
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
(defmacro have-pending-tests ()
  "Poll the above variables to check for pending tests."
  `(or *pending-packages*
       *pending-group-names*
       (> (hash-table-count *pending-test-names*) 0)))

(defparameter *passed-test-count* 0
  "The number of tests passed under the current :run session of the NST
runtime system.")

;;; The groups and tests that failed or caused an error in the current
;;; :run session.
;;;
(defparameter *erred-groups* (make-hash-table)
  "Map from groups raising an error in setup during the current
:run session of the NST runtime system to a reason for the error,
or t if none is available.")
(defparameter *erred-cleanup* (make-hash-table)
  "Map from groups raising an error in cleanup during the current
:run session of the NST runtime system to a reason for the error,
or t if none is available.")
(defparameter *failed-tests* (make-hash-table)
  "Map from names of tests failing during the current :run session of
the NST runtime system to a reason for the error, or t if none is
available.")
(defparameter *erred-tests* (make-hash-table)
  "Map from names of tests raising an error condition during the
current :run session of the NST runtime system to a reason for the
error, or t if none is available.")

(defmacro if-test (storage group-name test-name)
  "Where storage is some double hash table, return what, if anything,
is stored against group-name and test-name."
  (let ((group-hash (gensym "group-hash-")))
    `(let ((,group-hash (gethash ,group-name ,storage)))
       (when ,group-hash
	 (gethash ,test-name ,group-hash)))))

(defmacro add-test (test-record-hash test-record &optional (value t))
  (let ((group-hash (gensym "group-hash")))
    `(with-slots (group test-name) ,test-record
       (with-slots (group-name) group
	 (let ((,group-hash (gethash group-name ,test-record-hash)))
	   (unless ,group-hash
	     (setf ,group-hash (make-hash-table)
		   (gethash group-name ,test-record-hash) ,group-hash))
	   (setf (gethash test-name ,group-hash) ,value))))))
		    
(defmacro have-erred-tests ()
  "Poll the above variables to check for erred tests."
  `(or (> (hash-table-count *erred-groups*) 0)
       (> (hash-table-count *erred-cleanup*) 0)
       (> (hash-table-count *failed-tests*) 0)
       (> (hash-table-count *erred-tests*) 0)))

;;; Remembering the fixtures which have been opened.
(defvar *opened-fixtures* (make-hash-table)
  "Maps fixture names to t to show that they have been opened.")
(defvar *opening-at-top* t
  "This tag will be dynamically set to nil when recurring over opening
fixtures; this should be used for output selection only.")

;;; This group of macros summarizes the control decisions for output
;;; during test execution.  It's convenient to write this logic
;;; separately from the rest of the system.

(defmacro intro-test-run (name)
  `(when *verbose-output* 
     (format t " - Running test ~s..." ,name)))

(defmacro outro-test-run (name passed)
  `(if *verbose-output* 
       (format t "~a~%" (if ,passed "passed" "failed"))
       (unless ,passed
	 (format t "Test ~s failed~%" ,name))))

(defmacro outro-test-error (name msg)
  `(progn
     (if *verbose-output* 
	 (format t "error~%")
	 (format t "Test ~s raised error~%" ,name))
     (format t "   ~s~%" ,msg)))

(defmacro intro-group-setup (name)
  `(when *verbose-output* 
     (format t "Setting up tests in group ~s..." ,name)))

(defmacro outro-group-setup (name)
  (declare (ignorable name))
  `(when *verbose-output* (format t "done~%")))

(defmacro outro-failed-group-setup (name msg)
  `(format t "Error in group ~s setup:~%  ~/nst::nst-format/~%"
	   ,name ,msg))

(defmacro intro-group-cleanup (name)
  `(when *verbose-output* 
     (format t "Cleaning up after tests in group ~s..." ,name)))

(defmacro outro-group-cleanup (name)
  (declare (ignorable name))
  `(when *verbose-output* (format t "done~%")))


(defmacro pre-intro-fixture-set (name)
  `(when *verbose-output* 
     (format t "Incorporating fixture set ~s..." ,name)))

(defmacro post-intro-fixture-set (name)
  (declare (ignorable name))
  `(when *verbose-output* (format t "done~%")))

(defmacro outro-fixture-set (name)
  `(when *verbose-output* 
     (format t "Releasing fixture set ~s.~%" ,name)))

;;; Class and generic function definitions for the core of test
;;; execution.

(defclass nst-class () ()
  (:documentation
   "This superclass consolidates print-object and format pretty-
printing for all of the classes we define here."))

(defmethod print-object ((obj nst-class) stream)
  "We route print-object calls to a wrapped use of nst-format"
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~/nst::nst-format/" obj)))

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
      (documentation :initarg :documentation :type string
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
      (fixtures  :initarg :fixtures  :type (cons symbol)
		 :reader get-fixtures
		 :documentation
		 "Names of fixtures to be used in this group's tests")
      (documentation :initarg :documentation :type string
		     :documentation
		     "Documentation associated with this group"))
  (:documentation "Information associated with one group of tests."))

(defgeneric open-fixture (fixture)
  (:documentation
   "Adds the bindings defined by each fixture to the runtime namespace.
This function is defined via eql-methods for each fixture name."))

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
		 (outro-failed-group-setup (get-name ,group)
					   ,report)))))
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
     (intro-group-setup ,group-name)

     ;; We capture the success of setup in a conditional, and proceed
     ;; with tests and cleanup only if it works.
     (control-setup-errors
	(record-setup-error
	 ,group ,c
	 (make-instance 'setup-error-report
	   :caught ,c :form ,group-setup-form)
	 (eval ,group-setup-form)))

     ;; Blurb a successful setup, and run the given forms.
     (outro-group-setup ,group-name)
     ,@other-forms)))

(defmacro do-cleanup (group group-name group-cleanup-form
			    &rest other-forms)
  "This macro, which we use twice below, describes the tedious plumbing
associated with the cleanup for a test group.  Or at least, it will
be tedious when we finish catching errors here."
  (declare (ignorable group))
  
  ;; TO DO - catch errors here

  `(progn
     (intro-group-cleanup ,group-name)
     (eval ,group-cleanup-form)
     (outro-group-cleanup ,group-name)
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
	   
   (with-slots (test-name) test
     (intro-test-run test-name)
     (block single-test
       (handler-bind ((error #'(lambda (x)
				 (outro-test-error test-name x)
				 (add-test *erred-tests* test x)
				 (unless *debug-on-error*
				   (return-from single-test 'err)))))
	 (let ((result (call-next-method test)))
	   (if result
	       (setf *passed-test-count* (+ 1 *passed-test-count*))
	       (add-test *failed-tests* test))
	   (outro-test-run test-name result)
	   (return-from single-test (if result t nil))))))))

;;; Generic functions relating to test and group execution.

(defgeneric run (ptg)
  (:documentation "Run a test, or a group of tests.  Methods return
t if all tests completed with a non-nil return value, 'err if any tests
exited with an error, or nil if all tests completed, but some with an
unsuccessful nil result.")

  (:method ((ts test))
     "Run a single test, bracketed by its group's setup and cleanup."
     (with-slots (test-name group) ts
       (with-slots (group-name setup cleanup) group
	 (do-setup group group-name setup
	     (let ((test-result nil))
	       (unwind-protect (setf test-result (core ts))
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
	     (unwind-protect
		 (loop for test-name across test-names do
		   (let ((test-result
			  (core (gethash test-name tests-hash))))

		     ;; Here we check for the *break-on-...* flags.
		     (cond
		       ((eq test-result 'err)
			(if (or *break-on-error* *debug-on-error*)
			    (return-from group-exec 'err)
			    (setf group-result 'err)))
			
		       ((not test-result)
			(if *break-on-wrong*
			    (return-from group-exec nil)
			    (unless (eq group-result 'err)
			      (setf group-result nil)))))))
	       (do-cleanup group group-name cleanup))))
	 group-result))))

;;; Exported macro which sets up test fixtures.

(defparameter *active-group* nil)

(defmacro def-fixtures (name &key bindings
			     uses outer inner documentation)
  "Define a list of test fixtures which can then be assigned to test
groups and referenced from those groups' tests."

  (let* ((err (gensym))
	 
	 ;; A list of the names in the bindings by themselves, without
	 ;; the associated forms.
	 (safe-bindings
	  (loop for binding in bindings
		collect `(,(car binding)
			  (progn
			    (record-setup-error
			     *active-group* ,err
			     (make-instance 'fixture-error-report
			       :caught ,err
			       :fixture-name ',name
			       :var-name ',(car binding))
			     ,@(cdr binding))))))
	 
	 ;; A list of the names in the bindings by themselves, without
	 ;; the associated forms.
	 (names-only
	  (loop for binding in bindings collect (car binding)))
	 
	 ;; We use several sets of declarations about the names we
	 ;; bind for this fixture --- this one in "dynamic", below is
	 ;; "special".
	 (dynamic-decls
	  (loop for name in names-only
		collect (list 'dynamic-extent name)))
	 
	 (special-decls
	  (loop for name in names-only collect (list 'special name)))

	 ;; The names defined in the fixtures which this one uses.
	 (uses-names
	  (loop for f in uses
		append (loop for id in (gethash f +fixture-def-names+)
			     collect id)))
	 
	 ;; We also need to make declarations about the names in other
	 ;; fixtures which this fixture uses.  Here we make "special"
	 ;; and below is "ignorable".
	 (used-specials
	  (loop for id in uses-names collect (list 'special id)))

	 (ignorable-used-specials
	  (loop for id in uses-names collect (list 'ignorable id)))
	 
	 ;; Inserter for documentation, if we have any.
	 (doc-forms
	  (if documentation 
	      (list (list ':documentation documentation))
	      nil))
	 
	 ;; Documentation string for the open method
	 (open-method-doc
	  (format nil "Generated method for the ~s fixture set." name))
	 
	 ;; Converting the bindings into defparameter bindings to open
	 ;; the fixture into the interactive system.
	 (open-bindings (loop for b in bindings
			      collect (cons 'defparameter b)))
	 
	 ;; Macro joy.
	 (ptg (gensym "ptg-"))
	 (disc (gensym "disc-"))
	 (id (gensym "id-")))

    `(progn
       ;; The names we bind must be decliamed special, or they will
       ;; not be recognized.
       (declaim ,@special-decls)
       
       ;; Save this name with the other fixture names.  We check
       ;; first, since we could be re-defining the name.
       (unless (member ',name +fixtures+)
	 (push ',name +fixtures+))
       
       ;; Save the names we define in this fixture.
       (setf (gethash ',name +fixture-def-names+) ',names-only)
       
       ;; Create a class with the same name as the fixture.  Possibly,
       ;; this class name should be generated, and retrieved by a
       ;; function against the symbol ',name .
       (defclass ,name (fixture) () ,@doc-forms)
       
       ;; We put the fixture's bindings in effect with this :around
       ;; method.  All groups which use this fixture, and all of these
       ;; groups' tests, will be subclasses of the class above.  So
       ;; this :around method will give those test bodies these
       ;; bindings.
       (defmethod run :around ((,ptg ,name))
	 (declare ,@outer ,@used-specials ,@ignorable-used-specials)
	 (pre-intro-fixture-set ',name)
	 (let* ,safe-bindings
	   (declare ,@dynamic-decls ,@inner)
	   (post-intro-fixture-set ',name)
	   (call-next-method))
	 (outro-fixture-set ',name))
       
       ;; For runtime system debugging.  Returns the literal list of
       ;; name-value bindings assigned to this fixture.
       (defmethod get-fixture-bindings ((,disc (eql ',name)))
	 (declare (ignorable ,disc))
	 ',bindings)

       ;; For opening the fixture to the current namespace.
       (defmethod open-fixture ((,ptg (eql ',name)))
	 ,open-method-doc
	 (unless (and (gethash ',name *opened-fixtures*)
		      (not *reopen-fixtures*))
	   (when *open-used-fixtures*
	     (loop for ,id in ',uses do
	       (open-fixture ,id)))
	   ,@open-bindings)
	 nil)
       
       nil)))

;;; A convenience macro for specialized fixture definitions.

(defmacro def-capture/restore-fixtures (name variables
					     &key documentation)
  "Defines a simple fixtures which binds nil to each of the given
variables.  Since these bindings are all made via dynamic let's in
:around methods, the effect of this fixture will be to protect global
variables from the test suite."
  (let ((nil-bindings
	 (loop for v in variables collect (list v nil))))
    `(def-fixtures ,name ,nil-bindings :documentation ,documentation)))

;;; Exported macro for defining a group of tests.

;;; Global variables which we use in the embedded macros.
(defparameter *test-class-symbol* nil)
(defparameter *current-group-name* nil)
(defparameter *current-group-info* nil)
(defparameter *test-names-acc* nil)
(defparameter *test-info-hash* nil)
(defparameter *group-defer-test-compile* nil)
(defparameter *fixtures-for-group* nil)
(defparameter *fixtures-for-group-name* nil)

(defmacro def-test-group (group-name fixture-names &rest forms)
  "Define a group of tests associated with certain fixtures,
initialization and cleanup."

  (let ((doc-string nil) (tests nil)
	(setup-form nil) (cleanup-form nil)
	(group-defer-compile *defer-test-compile*)

	;; Unique names.
	(f (gensym "f"))
	(test-class (gensym "test-class-"))
	(group-class (gensym "group-class-"))
	(singleton (gensym "singleton-"))
	(wrapping-hash (gensym "wrapping-hash-"))
	(ptg (gensym))

	(class-doc 
	 (format nil "Class definition corresponding to test group ~s"
		 group-name)))

    ;; Run through the given forms, and sort them according to their
    ;; forst symbol.
    (loop for form in forms do
      (destructuring-bind (token &rest subforms) form
      (cond
	((eq token :documentation)  (setf doc-string (car subforms)))
	((eq token :setup)          (setf setup-form subforms))
	((eq token :cleanup)        (setf cleanup-form subforms))
	((eq token :defer-compile)  (setf group-defer-compile
					  (car subforms)))
	((or (eq token 'def-test) (eq token 'def-check))
	 (push form tests))
	(t (error "~@<Illegal form in def-test-group:~_ ~s~:>~%"
		  form)))))

    ;; Preserve the order of tests.
    (setf tests (nreverse tests))  
    
    `(progn
       
       ;; Define a uniquely-named class associated with this group.
       ;; It should extend all of the given fixture groups, plus the
       ;; "group" class.
       (defclass ,group-class (,@fixture-names group) ()
	 (:documentation ,class-doc))
       
       ;; Define a uniquely-named class which we will extend for each
       ;; test in this group.  It will also extend all of the given
       ;; fixture groups, plus the "test" class.
       (defclass ,test-class (,@fixture-names test) ()
	 (:documentation ,class-doc))
       
       ;; Extract the names which will be provided by the fixtures
       ;; which this group uses.
       (setf (gethash ',group-name +group-def-names+) 
	     (loop for ,f in ',fixture-names
		   append (gethash ,f +fixture-def-names+)))
       
       
       (let (;; The record of information about this group.
	     (,singleton (make-instance ',group-class
			   :package *package* :name ',group-name
			   :fixtures ',fixture-names
			   :setup '(block nil ,@setup-form)
			   :cleanup '(block nil ,@cleanup-form)
			   :testclass ',test-class
			   :documentation ,doc-string)))


	 ;; Calling the fixture setup might throw an error, so we need
	 ;; to catch a setup exception here as well as when calling
	 ;; the actual setup form.
	 (defmethod run :around ((,ptg ,group-class))
	   (let ((*active-group* ,singleton))
	     (control-setup-errors (call-next-method))))
	 
	 ;; Convenience method for running this group by name.
	 (defmethod run-group ((g (eql ',group-name)))
	   (run ,singleton))

	 ;; Save the group information against its name.
	 (setf (gethash ',group-name +groups+) ,singleton)
	 
	 ;; Save the group information against this package.
	 (let ((,wrapping-hash (gethash *package*
					+groups-by-package+)))
	   (unless ,wrapping-hash
	     (setf ,wrapping-hash (make-hash-table)
		   (gethash *package*
			    +groups-by-package+) ,wrapping-hash))
	   (setf (gethash ',group-name ,wrapping-hash) t))

	 ;; Set up variables that the tests defined in this group
	 ;; should see.
	 (let (;; The class which all tests of this group should
	       ;; extend; information about the group.
	       (*test-class-symbol* ',test-class)
	       (*current-group-name* ',group-name)
	       (*current-group-info* ,singleton)
	       
	       ;; Accumulators for tests in this group.
	       (*test-names-acc* nil)
	       (*test-info-hash* (make-hash-table))

	       ;; Any override for the default settings for deferring
	       ;; test compilation.
	       (*group-defer-test-compile* ,group-defer-compile))
	   
	   (declare (dynamic-extent *test-names-acc*)
		    (dynamic-extent *test-info-hash*)
		    (dynamic-extent *test-class-symbol*)
		    (dynamic-extent *current-group-name*)
		    (dynamic-extent *current-group-info*)
		    (dynamic-extent *group-defer-test-compile*)
		    (ignorable *test-class-symbol*)
		    (ignorable *current-group-name*)
		    (ignorable *current-group-info*)
		    (ignorable *group-defer-test-compile*))
	   
	   ;; Now process the tests defined for this group.
	   ,@tests
	   
	   ;; Store the accumulated test information in this group
	   ;; record.
	   (let ((tests-vector
		  (make-array (length *test-names-acc*)
		      :initial-contents (nreverse *test-names-acc*))))
	     (setf (slot-value ,singleton
			       'test-names) tests-vector
		   (slot-value ,singleton
			       'tests-hash) *test-info-hash*)))
	 nil))))

;;; Exported macro for defining a boolean test.

(defmacro def-test (test-name &key form
			      (defer-compile nil
				  defer-compile-supplied-p))
  ;;  (unless *current-group-name*
  ;;    (error "Called def-test from outside a def-test-group"))

  (let (;; Unique symbol for the macro expansion.
	(test-info (gensym "test-info-"))
	
	;; Build "special" declarations for the names defined in
	;; fixtures.
	(specials (loop for name in (gethash *current-group-name*
					     +group-def-names+)
			collect (list 'special name))))
		    
    `(progn
       (let (;; Actual information record for this test.
	     (,test-info (make-instance *test-class-symbol*
			   :group *current-group-info*
			   :name ',test-name
			   :documentation nil)))
	 ;; File away this test's name and information.
	 (push ',test-name *test-names-acc*)
	 (setf (gethash ',test-name *test-info-hash*) ,test-info)
	 
	 ;; Define a method which runs the form given for this test.
	 (defmethod core ((ts (eql ,test-info)))
	   ;; Declare the names provided by fixtures.
	   (declare ,@specials)

	   ;; Run the test expression, and return its value.
	   ,(if (if defer-compile-supplied-p
		    defer-compile
		    *group-defer-test-compile*)
		`(eval ',form)
		form))
	 
	 ;; Convenience method for running tests by name.
	 (defmethod run-test ((gr (eql ',*current-group-name*))
			      (ts (eql ',test-name)))
	   (run ,test-info))))))

;;; Some shorthand we'll use in def-check below.

(defmacro require-arguments-for-method (keyword args count
						&key at-least)
  (let ((given (gensym)))
    `(let ((,given (length ,args)))
       (unless (>= ,given ,count)
	 (error "def-check form ~s requires ~:[~;at least ~]~d target~:*~[s~;~:;s~] in method form, provided ~d, ~{~s~^ ~}"
		,keyword ,at-least ,count ,given ,args
		)))))

(defmacro warn-if-excess-arguments (keyword args)
  `(unless (null ,args)
     (warn "~@<Extra arguments to def-check form ~s ignored:~_ ~
               ~@<~{~s~^ ~_~}~:>~:>"
	   ,keyword ,args)))

(defmacro require-further-checks (cmd further)
  `(unless ,further
     (error "def-check form ~s requires further check methods" ,cmd)))

(defmacro destructure-warning-extra (cmd bindings value &rest forms)
  (let ((extra-forms (gensym))
	(minimum (length bindings)))
    `(progn
       (require-arguments-for-method ,cmd ,value ,minimum)
       (destructuring-bind (,@bindings &rest ,extra-forms) ,value
	 (warn-if-excess-arguments ,cmd ,extra-forms)
	 ,@forms))))

(defmacro destructure-prefix-last (cmd prefixes middle form value
				       &rest forms)
  (let ((minimum (+ 2 (length prefixes)))
	(raw-middle (gensym)))
    `(progn
       (require-arguments-for-method ,cmd ,value ,minimum :at-least t)
       (destructuring-bind (,@prefixes &rest ,raw-middle) ,value
	 (let ((,form (car (last ,raw-middle)))
	       (,middle (nbutlast ,raw-middle)))
	   ,@forms)))))

(defun continue-check (further)
  (destructuring-bind (method &rest details) further
    (let ((local-symbol (intern (symbol-name method)
				(find-package "NST"))))
      (apply #'check-form (cons local-symbol details)))))

;;; Exported macro providing a more expressive test-definition
;;; facility.

(defmacro def-check (name &rest commands-and-forms)
  "Define a test constructed according to the specified method."
  (unless commands-and-forms
    (error "def-check needs arguments")) 
    `(def-test ,name :form
       ,(continue-check commands-and-forms)))

(defgeneric check-form (method &rest details)
  (:documentation "Definition of the top-level check forms."))

(defmethod check-form ((cmd (eql 'symbol)) &rest details)
  "Check that the form evaluates to the given atom.  This is the
style of test provided by RT/RRT."
  (destructure-warning-extra cmd (target form) details
        `(eq ,form ',target)))
 
(defmethod check-form ((cmd (eql 'eq)) &rest details)
  "Check that the form is eq to an ideal (which may itself be
another form)."
  (destructure-warning-extra cmd (target form) details
       `(eq ,form ,target)))
   
(defmethod check-form ((cmd (eql 'eql)) &rest details)
  "Check that the form is eql to an ideal (which may itself be
another form)."
  (destructure-warning-extra cmd (target form) details
       `(eql ,form ,target)))
  
(defmethod check-form ((cmd (eql 'forms-eq)) &rest details)
  "Check that two forms are eq."
  (destructure-warning-extra cmd (form1 form2) details
       `(eq ,form1 ,form2)))
  
(defmethod check-form ((cmd (eql 'forms-eql)) &rest details)
  "Check that two forms are eql."
  (destructure-warning-extra cmd (form1 form2) details
       `(eql ,form1 ,form2)))
  
(defmethod check-form ((cmd (eql 'predicate)) &rest details)
  "Apply a boolean predicate to a form, and take the result as the
test result."
  (destructure-warning-extra cmd (predicate form) details
       `(funcall ,predicate ,form)))

(defmethod check-form ((cmd (eql 'err)) &rest details)
  "The err specifier tells the tester to expect evaluation of the
form to throw an error, and otherwise the test fails."
  (destructure-warning-extra cmd (form) details
       (let ((x (gensym "x")))
	 `(block ,x
	    (handler-bind ((error #'(lambda (,x)
				      (declare (ignorable ,x))
				      (return-from ,x t))))
	      ,form)
	    nil))))

(defmethod check-form ((cmd (eql 'not)) &rest details)
  "Require that a check fail."
  (require-further-checks cmd details)
  (let ((subform (continue-check details)))
    `(not ,subform)))

(defmethod check-form ((cmd (eql 'each)) &rest details)
  "The each specifier tells the tester to expect that the form will 
evaluate to a list, and that each element of the list will pass the
check given in the further elements of the check specification."
     
  (require-further-checks cmd details)
  (let* ((list-form (car (last details)))
	 (subdetails-rev (cdr (reverse details)))
	 (x (gensym "x"))
	 (subform (continue-check (reverse (cons x subdetails-rev)))))
    `(block ,x
       (loop for ,x in ,list-form do
	 (unless ,subform (return-from ,x nil)))
       t)))
  
(defmethod check-form ((cmd (eql 'seq)) &rest details)
  "The seq specifier takes N further specifier elements of the form\
 plus a form for evaluation.  The check expects the form to evaluate\
 to a list of N elements which match the respective specifier in the\
 further elements."

  (require-further-checks cmd details)
  (let ((form (car (last details)))
	(checks (cdr (reverse details)))
	
	(overall (gensym "overall"))
	(last-var (gensym "unused"))
	(on-last t)
	(result-form t))
    
    (loop for method in checks do
      (let ((first-form (gensym "first-form"))
	    (other-forms (gensym "other-forms")))
	(setf result-form
	      `(progn
		 (unless ,other-forms (return-from ,overall nil))
		 (destructuring-bind (,first-form &rest ,last-var)
		     ,other-forms
		   ,@(if on-last
			 `((declare (ignorable ,last-var))))
		   (unless 
		       ,(continue-check (nconc method
					       (list first-form)))
		     (return-from ,overall nil))
		   ,result-form))
		
	      last-var other-forms
	      on-last nil)))
    
    `(block ,overall
       (let ((,last-var ,form))
	 ,result-form))))

(defmethod check-form ((cmd (eql 'across)) &rest details)
  "The across specifier takes N further specifier elements and a form,\
 and expects the form to evaluate to a vector of N elements which\
 match the respective specifier in the further elements."

  (require-further-checks cmd details)
  (let ((form (car (last details)))
	(checks (nbutlast details))
	
	(result (gensym "result"))
	(l (gensym "l")))
    
    `(block ,l
       (let ((,result ,form))
	 (progn 
	   (unless (eql (length ,result) ,(length checks))
	     (return-from ,l nil))
	   ,@(loop for check in checks and idx from 0
		   collect
		   (let* ((ref `(aref ,result ,idx))
			  (item-form
			   (continue-check (nconc check (list ref)))))
		     `(unless ,item-form
			(return-from ,l nil))))))
       t)))
  
(defmethod check-form ((cmd (eql 'permute)) &rest details)
  "The permute specifier expects that the form will evaluate to a
list, some permutation of which will satisfy the further specified
check."

  (require-further-checks cmd details)
  (let ((form (car (last details)))
	(method (nbutlast details))
	
	(perms (gensym "perms-"))
	(x (gensym "x-"))
	(permute-block (gensym "permute-block-")))

    `(block ,permute-block
       (let ((,perms (make-instance 'permuter :src ,form)))
	 (loop while (has-next ,perms) do
	   (let ((,x (next-permutation ,perms)))
	     (when ,(continue-check (nconc method (list x)))
	       (return-from ,permute-block t))))))))
  
(defmethod check-form ((cmd (eql 'slots)) &rest details)
  "Apply checks to the slots of a class."

  (require-further-checks cmd details)
  (let* ((form (car (last details)))
	 (tuples (nbutlast details))
	
	 (slots-check (gensym "slots-check-"))
	 (slot-checks
	  (loop for tuple in tuples
		for slot-name = (car tuple)
		and method = (cadr tuple)
		collect
		`(unless
		     ,(continue-check (nconc method (list slot-name)))
		   (return-from ,slots-check nil))))
	    
	 (slot-names
	  (loop for tuple in tuples collect (car tuple))))
       
    `(block ,slots-check
       (with-slots ,slot-names ,form ,@slot-checks)
       t)))

(defmethod check-form ((cmd (eql 'apply)) &rest details)
  "Apply a transformation to the value of a form, and check the
resulting value"

  (destructure-prefix-last cmd (transform) methods form details
    (let ((application `(funcall ,transform ,form)))
      (continue-check (nconc methods (list application))))))
  
(defmethod check-form ((cmd (eql 'pass)) &rest details)
  "This test always passes"
  (warn-if-excess-arguments cmd details)
  t)

(defmethod check-form ((cmd (eql 'fail)) &rest details)
  "This test always fails"
  (warn-if-excess-arguments cmd details)
  nil)

(defmethod check-form (unrecognized &rest whatever)
  "Ill-specified checks are compile-time errors"
  (declare (ignorable unrecognized) (ignorable whatever))
  (error "Unrecognized def-check form ~s~%" unrecognized))


;;; This next section of code sets up how the interactive system
;;; manages nominating "interesting" tests; manages running,
;;; examining, and re-running the interesting tests; etc.

(defmacro under-empty-pendings (&rest forms)
  `(let ((*pending-packages* ())
	 (*pending-group-names* ())
	 (*pending-test-names* (make-hash-table))
	 (*passed-test-count* 0)
	 (*erred-groups* (make-hash-table))
	 (*erred-cleanup* (make-hash-table))
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
	     *passed-test-count* 0)

       (clrhash *pending-test-names*)
       (clrhash *failed-tests*)
       (clrhash	*erred-tests*)
       (clrhash *erred-groups*)
       (clrhash	*erred-cleanup*)
       
       (loop for ,group-name
	       being the hash-keys of *interesting-test-names*
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
	   (macrolet
	       ((check-break (record)
		  (let ((result (gensym "result-")))
		    `(let ((,result (run ,record)))
		       (when (or (and (or *break-on-error*
					  *debug-on-error*)
				      (eq ,result 'err))
				 (and *break-on-wrong*
				      (eq ,result nil)))
			 (return-from pending-loop))))))

	     ;; It's easiest to put this all this mess in a loop, and
	     ;; cycle until it's all done.  Sloppy maybe, but
	     ;; effective.
	     (loop while ,more do
	       (setf ,more nil)
	       
	       ;; Run individual pending tests (and tests left over
	       ;; form groups after a break, etc.).
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
			 (let* ((,test-info
				 (gethash ,test-name ,group-test-hash)))
			   (unless ,test-info
			     (error "No such test ~s in group ~s"
				    ,test-name ,group-name))
			   (check-break ,test-info)))))
		  (remhash ,group-name *pending-test-names*))

	       ;; Run pending groups.
	       (loop as ,group-name = (pop *pending-group-names*)
		     while ,group-name
		     do
		  (let ((,group-info (gethash ,group-name +groups+)))
		    (if ,group-info
			(check-break ,group-info)
			(format t "WARNING: No such group ~s~%"
				,group-name))))

	       ;; We don't actually run pending packages, we just make
	       ;; their groups pending.
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
				,package)))))))))))

(defmacro report-last-run ()
  (let ((hash (gensym "hash-")))
    `(format t "------------------------------------~%~
                SUMMARY OF TEST RUN~%~
                ~[No tests passed~:;~:*Tests passed: ~d~]~%~
                ~[No tests failed~:;~:*Tests failed: ~d~]~%~
                ~[~:;~:*Tests raising error: ~d~%~]~
                ~[~:;~:*Groups raising error in setup: ~d~%~]~
                ~[~:;~:*Groups raising error in cleanup: ~d~%~]~
                ------------------------------------~%"
	     *passed-test-count*
	     (loop for ,hash being the hash-values of *failed-tests*
		   summing (hash-table-count ,hash))
	     (loop for ,hash being the hash-values of *erred-tests*
		   summing (hash-table-count ,hash))
	     (hash-table-count *erred-groups*)
	     (hash-table-count *erred-cleanup*))))

(defmacro give-blurb (group-name test-name)
  (let ((x (gensym "x-"))
	(p (gensym "p-"))
	(package-hash (gensym "package-hash-"))
	(package-name (gensym "package-name-")))
    `(progn
       (block blurbing
	 (when (member ,group-name *pending-group-names*)
	   (format t "Group ~s is pending.~%" ,group-name)
	   (return-from blurbing))
      
	 (when (if-test *pending-test-names* ,group-name ,test-name)
	   (format t "Test ~s.~s is pending.~%"
		   ,group-name ,test-name)
	   (return-from blurbing))
	
	 (loop for ,p being the hash-keys in +groups-by-package+
	       using (hash-value ,package-hash)
	       do
	    (when (gethash ,group-name ,package-hash)
	      (let ((,package-name (package-name ,p)))
		(when (member ,package-name *pending-packages*)
		  (format t "Package ~a is pending.~%" ,package-name)
		  (return-from blurbing)))))
      
	 (when (gethash (gethash ,group-name +groups+) *erred-groups*)
	   (format t "Group ~s raised an error in setup.~%"
		   ,group-name)
	   (return-from blurbing))
      
	 (let ((,x (if-test *failed-tests* ,group-name ,test-name)))
	   (when ,x
	     (format t "Test ~s.~s failed.~%" ,group-name ,test-name)
	     (return-from blurbing)))
      
	 (let ((,x (if-test *erred-tests* ,group-name ,test-name)))
	   (when ,x
	     (format t "Test ~s.~s raised an error:~%  ~s~%"
		     ,group-name ,test-name ,x)
	     (return-from blurbing))))
       (when (gethash (gethash ,group-name +groups+) *erred-cleanup*)
	 (format t "Group ~s raised an error in cleanup.~%"
		 ,group-name)))))

;;; Output functions for lists and other collections of testing
;;; artifacts for use in the runtime system.

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

    (unless +fixtures+
      (format stream "~%FIXTURE SETS~%")
      (loop for name in +fixtures+ do
	(let ((val (get-fixture-bindings name)))
	  (format stream "Set ~s binds ~
                     ~@<~{~/nst::format-binding/~^, ~}~:>~%"
		  name val))))
    
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
	    (group-test-names-from-hashes *interesting-test-names*)
	    (map 'list #'package-name *pending-packages*)
	    *pending-group-names*
	    +group-test-name-formatter+
	    (group-test-names-from-hashes *pending-test-names*)
	    *passed-test-count*
	    (loop for g being the hash-keys of *erred-groups*
		  collect (get-name g))
	    +group-test-name-formatter+
	    (group-test-names-from-hashes *failed-tests*)
	    +group-test-name-formatter+
	    (group-test-names-from-hashes *erred-tests*))))

;;; Top-level user help message.

(defconstant +nst-top-help+ "NST test framework control

OUTPUT CONTROL
  :nst :help
	Show this help message and exit.
  :nst :verbose FORM
	Set whether verbose output should be generated.
  :nst :debug BOOL
	Set whether NST debugging messages should be generated.
  :nst :summarize-scheduled BOOL
	Set whether a summary should be printed after running
        scheduled tests with :run, :continue, etc.
  :nst :summarize-single BOOL
	Set whether a summary should be printed after one-time
        test runs with :run-test, :run-group, etc.
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
	pause.
  :nst :break-on-error BOOL
	Set whether any error in a test run should cause test
	execution to pause.
  :nst :debug-on-error BOOL
	Set whether an error in a test run should drop us into debug
	mode.

TEST SUITE EXECUTION
  :nst :run
	Run all marked tests.
  :nst :continue
	Continue running tests after an interruption arising from one
	of the three flags above.
  :nst :retry
	Retry failed or error-raising tests from the last run.
  :nst :blurb GROUPNAME TESTNAME
	Describe the outcome of a test in the last run.

TEST DEFINITION
  :nst :defer-test-compile BOOL
	Set whether tests defined subsequently should, by default,
        defer compilation of their forms until actually running the
        test.  This feature is useful when debugging code involving
        macros, but changing this feature can lead to confusion: it
        may be advisable to set this flag locally via def-test-group
        and def-test.

ONE-OFF EXECUTION
  :nst :run-package PACKAGE
	Run all of the tests in a package.
  :nst :run-group GROUP
	Run all of the tests in a single group.
  :nst :run-test GROUP TEST
	Run a single test.

OPENING FIXTURES
  :nst :open FIXTURE-NAME
        Bring the names bound in the fixture into the runtime
        environment.
  :nst :open-used BOOL
        Set whether opening a fixture should always also open the
        fixtures it uses.  Default is t.
  :nst :reopen BOOL
        Set whether fixtures should be re-opened e.g. when required
        multiple times by opening different fixtures that use them.

Multiple NST commands can be combined at one prompt, e.g.
  :nst :p *package* :g aux::key-tests :run

When the :nst command is given without arguments, the interpreter
will choose to run, continue or re-try scheduled tests based on
the state of the last run.  Its choice expects that you would be
fixing problems as they arise.
")

;;; Function version of the command-line interpreter.  The main logic
;;; is here; further below we define platform-specific command-line
;;; interfaces.

(defun run-nst-commands (&rest args)
  "Top-level command interpreter for the NST tester"
  (block runner
    
    (unless args 
      (cond
	((have-pending-tests)
	 (setf args '(:continue))
	 (format t "Running pending tests.~%"))
	
	((have-erred-tests)
	 (setf args '(:retry))
	 (format t "Rerunning failed tests and groups.~%"))
	
	((have-interesting-tests)
	 (setf args '(:run))
	 (format t "Running scheduled tests.~%"))
	
	(t
	 (format t "No tests scheduled.~%")
	 (return-from runner))))

    (loop do
      (if (null args) (return-from runner))

      (block single-command
	(let ((head (pop args)))
	  (macrolet
	      ((pop-arg (want have)
		 `(cond 
		    ((null args)
		     (format t "Command ~s requires ~d~:* ~
                               argument~[s~;~:;s~] but given ~d.~%"
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

	    (command-case-flag-setter (:summarize-scheduled)
				      *scheduled-summary-output*
				      "summaries for scheduled runs")

	    (command-case-flag-setter (:summarize-single)
				      *scheduled-single-output*
				      "summaries for single runs")

	    (command-case-flag-setter (:open-used)
				      *open-used-fixtures*
				      "opening used fixtures")

	    (command-case-flag-setter (:reopen)
				      *reopen-fixtures*
				      "reopening fixtures")

	    (command-case-flag-setter
	     (:defer-test-compile) *defer-test-compile*
	     "deferral of test form compilation by default")

	    (command-case (:dump dump) () (nst-dump t))

	    (command-case (:blurb) (group-name test-name)
			  (give-blurb group-name test-name))

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
				      *interesting-test-names*))
			    (p-tests
			     (gethash group-name
				      *pending-test-names*)))
			(unless i-tests
			  (setf i-tests (make-hash-table)
				(gethash group-name
					 *interesting-test-names*)
				i-tests))
			(unless p-tests
			  (setf p-tests (make-hash-table)
				(gethash group-name
					 *pending-test-names*)
				p-tests))
			(setf (gethash test-name i-tests) t
			      (gethash test-name p-tests) t))
		      (when *verbose-output*
			(format t "Marked test ~s (group ~s) ~
                                   for testing."
				test-name group-name)))
		    (format t "ERROR: cannot find group ~s~%"
			    group-name)))

	    (command-case (:run) ()
		(reset-pending) (run-pending) (report-last-run))

	    (command-case (:continue) ()
		(run-pending) (report-last-run))

	    (command-case (:run-package) (pkg-name)
		(under-empty-pendings
		 (push (find-package pkg-name) *pending-packages*)))

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
		(loop for group being the hash-keys of *erred-groups*
		      do (push (get-name group) *pending-group-names*))
		(clrhash *erred-groups*)
		
		(loop for group being the hash-keys of *erred-cleanup*
		      do (push (get-name group) *pending-group-names*))
		(clrhash *erred-cleanup*)
		
		(loop for test-hash
		      in (list *failed-tests* *erred-tests*)
		      do 
		   (loop for group being the hash-keys in test-hash
			 using (hash-value test-set)
			 do
		      (let ((new-test-set
			     (gethash group *pending-test-names*)))
			(unless new-test-set
			  (setf new-test-set (make-hash-table)
				(gethash group
					 *pending-test-names*)
				new-test-set))
			(loop for test being the hash-keys in test-set
			      using (hash-value flag)
			      do
			   (when flag
			     (setf (gethash test new-test-set) t))))
		      (remhash group test-hash)))
		(run-pending) (report-last-run))

	    (command-case (:open) (fixture-name)
		(open-fixture fixture-name)
		(format t "Opened fixture ~s.~%" fixture-name)))
	  
	  (format t "Unrecognized NST command ~s~%~
                     For more options, use :nst :help~%~%"
		  head))))))

;;; Platform-specific command-line interpreter interfaces.

#+allegro
(top-level:alias "nst" (&rest args)
  ;;  #.+nst-top-help+
  (apply #'run-nst-commands args))

