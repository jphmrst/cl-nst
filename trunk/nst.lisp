;;; File nst.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow Technologies.
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
;;; 3. We are not yet catching errors arising from cleanup.
;;;
;;; 4. Clobbering test/group names should be caught by the macros, not
;;; by the lower-level expanded code defining methods.
;;;
;;; 5. Have warnings or errors when multiple fixtures get the same
;;; error.
;;;
;;; 6. The testing via ASDF adds the package tests, but doesn't remove
;;; them after that run.  This is probably not how it should work.
;;;
;;; 7. There is much cruft here arising from my inexperience on
;;; getting names known at macro-expansion time communicated to other
;;; subsequently expanded macros.  I should clean up after myself.
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
(defmacro def-flag (flag-name flag-value implying-flags
			      &key (documentation nil doc-sup-p)
			      runtime-macro function-name)
  `(progn
     ,(when function-name
	`(defun ,function-name () (or ,flag-name ,@implying-flags)))
     ,(when runtime-macro
	`(defmacro ,runtime-macro (&rest forms)
	   (let ((name-echo ',flag-name) (flags-echo ',implying-flags))
	     `(when (or ,name-echo ,@flags-echo) ,@forms))))
     (defvar ,flag-name ,flag-value ,(when doc-sup-p documentation))))

(def-flag *debug-macrotime* t ()
	  :runtime-macro macro-dbg
	  :documentation "Set to t for extensive macro expansion \
                          debugging output")
(def-flag *debug-compile* t ()
	  :runtime-macro compile-dbg
	  :documentation "Set to t for extensive debugging output for \
                          expanded macros")
(def-flag *debug-class-hierarchy* t ()
	  :runtime-macro class-dbg
	  :documentation "Set to t to generate debugging information \
                          about the class hierarchy of tests and \
                          groups")
(def-flag *debug-bindings* t ()
	  :runtime-macro bind-dbg
	  :documentation "Set to t to generate debugging information \
                          about fixture bindings in tests and groups")
(def-flag *debug-output* t (*debug-class-hierarchy* *debug-bindings*)
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
      (format t "Setting up tests in group ~s..." group-name))

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
     (verbose-out (format t " - Running test ~s~%" test-name))
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
       ;; The names we bind must be declaimed special, or they will
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
       ;;
       ;;(format t "Defining names: ~s~%" ',names-only)
       (defmethod run :around ((,ptg ,name))
	 (declare ,@outer ,@used-specials ,@ignorable-used-specials)
	 (bind-dbg (format t "  Binding names:~{ ~s~} (by ~s)~%"
			   ',names-only ',name))
	 (let* ,safe-bindings
	   (declare ,@dynamic-decls ,@inner)
	   (call-next-method)))
       
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

;;; Defining a fixture anonymously.
(defun quick-fix (name binding)
  (let ((bindings (loop for x = (pop binding) while x
			for y = (pop binding)
			collect (list x y))))
    `(def-fixtures ,name :bindings ,bindings)))

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

(defun clean-fixtures-names! (fixtures-list)
  "Returns fixture declarations for anonymous fixtures"
  (let* ((fixture-decls nil))
    (loop for node on fixtures-list do
      (let ((item (car node)))
	(cond
	  ((symbolp item) t)
			      
	  ((and (listp item) (eq :fixtures (car item)))
	   (let* ((name (gensym))
		  (decl (quick-fix name (cdr item))))
	     (push decl fixture-decls)
	     (setf (car node) name)))
	  
	  (t (error "Unrecognized fixture name list item ~s"
		    item)))))
    fixture-decls))

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

;;;  (format t "Defining group ~s with fixtures ~s.~%"
;;;	  group-name fixture-names)
  (let ((doc-string nil) (tests nil)
	(setup-form nil) (cleanup-form nil)
	(group-defer-compile *defer-test-compile*)
	(anon-fixtures (clean-fixtures-names! fixture-names))

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

    (let ((actual-tests
	   (let ((*current-group-name* group-name))
	     (declare (dynamic-extent *current-group-name*)
		      (ignorable *current-group-name*))
	     (loop for test in tests collect
		   (macroexpand test)))))

    
    `(progn
       
       ;; First define any of the anonymous fixtures we found in the
       ;; original fixtures list.
       ,@anon-fixtures
       
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
       
       ;; (format t " - Creating singleton record for ~s~%"
       ;;	 ',group-name)
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
	   (setf *current-group-name* ',group-name)
	   ,@actual-tests
	   
	   ;; Store the accumulated test information in this group
	   ;; record.
	   (let ((tests-vector
		  (make-array (length *test-names-acc*)
			      :initial-contents
			      (nreverse *test-names-acc*))))
	       
	     (setf (slot-value ,singleton 'test-names)
		   tests-vector
		   
		   (slot-value ,singleton 'tests-hash)
		   *test-info-hash*)))
	 nil)))))

;;; Exported macro for defining a boolean test.

(defmacro def-test
    (test-name &key form 
	       (setup nil setup-supplied-p)
	       (cleanup nil cleanup-supplied-p)
	       (fixtures nil fixtures-supplied-p)
	       (defer-compile nil defer-compile-supplied-p))

  `(def-test-in-group ',test-name ',*current-group-name*
     :form ,form
     ,@(if setup-supplied-p `(:setup ,setup))
     ,@(if cleanup-supplied-p `(:cleanup ,cleanup))
     ,@(if fixtures-supplied-p `(:fixtures ,fixtures))
     ,@(if defer-compile-supplied-p `(:defer-compile ,defer-compile))))

(defmacro def-test-in-group
    (test-name group-name
	       &key form 
	       (setup nil setup-supplied-p)
	       (cleanup nil cleanup-supplied-p)
	       (fixtures nil fixtures-supplied-p)
	       (defer-compile nil defer-compile-supplied-p))

;;;  (format t " - Processing test ~s of group ~s (~s)~%"
;;;	  test-name group-name (eval group-name))
  (let* (;; Unique symbol for the macro expansion.
	 (test-info (gensym "test-info"))

	 (actual-defer (if defer-compile-supplied-p
			   defer-compile
			   *group-defer-test-compile*))

	 (actual-form
	  (let ((forms (list (if actual-defer `(eval ',form) form))))
	    (when setup-supplied-p
	      (setf forms (cons (if actual-defer `(eval ',setup) setup)
				forms)))
	    (when cleanup-supplied-p
	      (setf forms
		    `((unwind-protect ,(if (> (length forms) 1)
					   `(progn ,@forms)
					   (car forms))
			,(if actual-defer `(eval ',cleanup) cleanup)))))
	    (if (> (length forms) 1)
		`(progn ,@forms)
		(car forms))))
	
	 ;; Build "special" declarations for the names defined in
	 ;; fixtures.
	 (specials (loop for name in (gethash group-name
					      +group-def-names+)
			 collect (list 'special name)))
	 
	 (fixtures-forms nil)		; The list of forms used to
					; defined test-local fixtures.
	 (actual-test-class '*test-class-symbol*))

    (macro-dbg
      (format t "Expanding ~s/~s.~%" group-name test-name))
    
    (when fixtures-supplied-p
      (let* ((new-class-name (gensym "custom-test-class"))
	     (anon-fixtures (clean-fixtures-names! fixtures))
	     (class-defn (gensym)))
	(setf actual-test-class `',new-class-name)
	(setf fixtures-forms
	      `(,@anon-fixtures
		(let ((,class-defn (list 'defclass ',new-class-name
					 (cons *test-class-symbol*
					       ',fixtures)
					 ())))
		  (macro-dbg
		    (format t " - Local test class definition:~%   ~s~%"
			    ,class-defn))
		  (eval ,class-defn))))
	(macro-dbg
	  (format t " - Test-local fixture declaration~% - ~s~%"
		  fixtures-forms))))
    
    `(progn
       (compile-dbg
	 (format t "Compiling test ~s/~s~%" ,group-name ,test-name))
    
       ,@fixtures-forms

       (let (;; Actual information record for this test.
	     (,test-info (make-instance ,actual-test-class
			   :group *current-group-info*
			   :name ,test-name
			   :documentation nil)))
	 
	 ;; File away this test's name and information.
	 (push ,test-name *test-names-acc*)
	 (setf (gethash ,test-name *test-info-hash*) ,test-info)
	 
	 ;; Define a method which runs the form given for this test.
	 (defmethod core ((ts (eql ,test-info)))
	   ;; Declare the names provided by fixtures.
	   (declare ,@specials)
	   ;; Run the test expression, and return its value.
	   ,actual-form)
	 
	 ;; Convenience method for running tests by name.
	 (defmethod run-test ((gr (eql ,group-name))
			      (ts (eql ,test-name)))
	   (run ,test-info))))))

;;; Some shorthand we'll use in def-check below.

(defmacro require-arguments-for-method (keyword args count
						&key at-least)
  (let ((given (gensym)))
    `(let ((,given (length ,args)))
       (unless (>= ,given ,count)
	 (error "def-check form ~s requires ~:[~;at least ~]~d ~
                 target~:*~[s~;~:;s~] in method form, provided ~d, ~
                 ~{~s~^ ~}"
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
