;;; File interactive.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :sift.nst)

;;; Command-line facilities and interaction.

;;; This next section of code sets up how the interactive system
;;; manages nominating "interesting" tests; manages running,
;;; examining, and re-running the interesting tests; etc.

(defmacro under-empty-pendings (&rest forms)
  `(let ((*pending-packages* ())
	 (*pending-group-names* ())
	 (*pending-test-names* (make-hash-table))
	 (*passed-test-count* 0)
;;;	 (*erred-groups* (make-hash-table))
;;;	 (*erred-cleanup* (make-hash-table))
;;;	 (*failed-tests* (make-hash-table))
;;;	 (*passed-tests* (make-hash-table))
;;;	 (*erred-tests* (make-hash-table))
	 )
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
       (clrhash	*passed-tests*)
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
    `(progn
       (unless (eq cl-user::*nst-default-report-stream* t)
	 (format cl-user::*nst-default-report-stream*
		 "------------------------------------~%~
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
		 (hash-table-count *erred-cleanup*))
	 (format t "------------------------------------~%~
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
		 (hash-table-count *erred-cleanup*))))))

(defmacro give-blurb (group-name test-name)
  (let ((x (gensym "x-"))
	(p (gensym "p-"))
	(package-hash (gensym "package-hash-"))
	(package-name (gensym "package-name-")))
    `(block blurbing
       (when (member ,group-name *pending-group-names*)
	 (format t "Group ~s is pending.~%" ,group-name)
	 (return-from blurbing))
      
       (when (gethash (gethash ,group-name +groups+) *erred-groups*)
	 (format t "Group ~s raised an error in setup.~%"
		 ,group-name)
	 (return-from blurbing))

       (when (gethash (gethash ,group-name +groups+) *erred-cleanup*)
	 (format t "Group ~s raised an error in cleanup.~%"
		 ,group-name))
      
       (when (if-test *pending-test-names* ,group-name ,test-name)
	 (format t "Test ~s/~s is pending.~%"
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
      
       (let ((,x (if-test *failed-tests* ,group-name ,test-name)))
	 (when ,x
	   (format t "Test ~s/~s failed.~%" ,group-name ,test-name)
	   (return-from blurbing)))
      
       (let ((,x (if-test *erred-tests* ,group-name ,test-name)))
	 (when ,x
	   (format t "Test ~s/~s raised an error:~%  ~s~%"
		   ,group-name ,test-name ,x)
	   (return-from blurbing)))
      
       (when (if-test *passed-tests* ,group-name ,test-name)
	 (format t "Test ~s/~s passed.~%" ,group-name ,test-name)
	 (return-from blurbing))
       
       (format t
	       "Test ~s/~s not scheduled and not recently manually run~
                ~%(or, perhaps you are not querying on atoms from the ~
                   test package).~%"
	       ,group-name ,test-name)
       (return-from blurbing))))

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

(defmacro group-test-name-formatter ()
     "~:[none~;~:*~@<~{~/nst::format-group-test-list/~^, ~_~}~:>~]")
(defun format-group-test-list (stream item s c)
  (declare (ignorable s) (ignorable c))
  (format stream "~s/~s" (car item) (cadr item)))


(defun nst-dump (stream)
  (macrolet ((group-test-loop (hash group test content &rest forms)
	       (let ((name-hash (gensym "name-hash-")))
		 `(loop for ,group being the hash-keys
			  of ,hash
			  using (hash-value ,name-hash)
			  do
		       (loop for ,test being the hash-keys
			     of ,name-hash
			     using (hash-value ,content)
			     do ,@forms))))
	     (group-test-names-from-hashes (hash)
	       (let ((group (gensym "group-"))
		     (test (gensym "test-"))
		     (name-hash (gensym "name-hash-")))
		 `(loop for ,group being the hash-keys of ,hash
			using (hash-value ,name-hash)
			append
			(loop for ,test being
			      the hash-keys of ,name-hash
			      collect (list ,group ,test))))))

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
     Tests passed this run: ~@?~%~
     ~%"
	    *verbose-output* *debug-output*
	    *break-on-wrong* *break-on-error* *debug-on-error*
	    (map 'list #'package-name *interesting-packages*)
	    *interesting-group-names*
	    (group-test-name-formatter)
	    (group-test-names-from-hashes *interesting-test-names*)
	    (map 'list #'package-name *pending-packages*)
	    *pending-group-names*
	    (group-test-name-formatter)
	    (group-test-names-from-hashes *pending-test-names*)
	    *passed-test-count*
	    (loop for g being the hash-keys of *erred-groups*
		  collect (get-name g))
	    (group-test-name-formatter)
	    (group-test-names-from-hashes *failed-tests*)
	    (group-test-name-formatter)
	    (group-test-names-from-hashes *erred-tests*)
	    (group-test-name-formatter)
	    (group-test-names-from-hashes *passed-tests*))))

;;; Top-level user help message.

(defmacro nst-top-help () "NST test framework control

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
	       
	       (command-case-core (synonyms args &rest forms)
		 (let* ((want (length args))
			(arg-bindings
			 (loop for arg in args and have from 0
			       collect
			       (list arg (list 'pop-arg want have)))))
		   `(when (member head ',synonyms)
		      (let ,arg-bindings ,@forms))))
	       
	       (command-case (synonyms args &rest forms)
		   `(command-case-core ,synonyms ,args ,@forms
				       (return-from single-command)))

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

	    (command-case-core (:help help h) ()
		(format t "~a" (nst-top-help))
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

	    (command-case (:run-packages) (pkg-name-list)
	      (under-empty-pendings
	       (setf *pending-packages*
		     (loop for pkg-name in pkg-name-list
			   collect (find-package pkg-name)))))

	    (command-case (:run-group) (group)
	      (under-empty-pendings
	       (push group *pending-group-names*)))

	    (command-case (:run-groups) (group-name-list)
	      (under-empty-pendings
	       (loop for group-name in group-name-list do
		 (push group-name *pending-group-names*))))

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
	      (handler-case (open-fixture fixture-name)
		(unknown-fixture (cnd)
		  (format t "Can't find fixture ~s ~
                             ~_(check current package)." (name cnd))
		  (return-from run-nst-commands)))))
	  
	  (format t "Unrecognized NST command ~s~%~
                     For more options, use :nst :help~%~%"
		  head))))))

;;; Platform-specific command-line interpreter interfaces.

#+(or allegro sbcl)
(#+allegro top-level:alias #+sbcl sb-aclrepl:alias "nst" (&rest args)
  (apply #'run-nst-commands args))

