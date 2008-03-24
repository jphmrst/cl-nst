;;; File tests.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :sift.nst)

;;; Macros defining tests and groups.

;;; Global variables which we use in the embedded macros.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *test-class-symbol* nil)
  (defvar *current-group-name* nil)
  (defvar *expanding-test-for-group* nil)
  (defvar *test-names-acc* nil)
  (defvar *test-info-hash* nil)
  (defvar *group-defer-test-compile* nil)
  (defvar *fixtures-for-group* nil)
  (defvar *fixtures-for-group-name* nil))

#+allegro (excl::define-simple-parser def-test-group second :nst-group)
(defmacro def-test-group (group-name given-fixtures &body forms)
  "Define a group of tests associated with certain fixtures,
initialization and cleanup."

  (macro-dbg
   (format t "~@<Expanding test group ~s~_ with fixtures ~s.~:>~%"
	   group-name given-fixtures))
  (let ((doc-string nil) (tests nil)
	(setup-form nil) (cleanup-form nil)
	(group-defer-compile *defer-test-compile*))

    (multiple-value-bind (anon-fixtures fixture-names)
	(process-anonymous-fixtures given-fixtures)
      (let (;; Unique names.
	    (f (gensym "f"))
	    (group-class (gensym (concatenate 'string "group-class-"
					      (symbol-name group-name)
					      "-")))
	    (wrapping-hash (gensym "wrapping-hash-"))
	    (ptg (gensym))
	    
	    (class-doc 
	     (format nil " - Class definition corresponding ~
                             to test group ~s"
		     group-name)))

	;; Run through the given forms, and sort them according to
	;; their first symbol.
	(loop for form in forms do
	  (destructuring-bind (token &rest subforms) form
	    (cond
	      ((eq token :documentation) (setf doc-string
					       (car subforms)))
	      ((eq token :setup)         (setf setup-form subforms))
	      ((eq token :cleanup)       (setf cleanup-form subforms))
	      ((eq token :defer-compile) (setf group-defer-compile
					       (car subforms)))
	      ((or (eq token 'def-test) (eq token 'def-check))
	       (push form tests))
	      (t
	       (error "~@<Illegal form in def-test-group:~_ ~s~:>~%"
		      form)))))

	;; Preserve the order of tests.
	(setf tests (nreverse tests))  

	(let ((actual-tests) (test-fixture-defs)
	      (*test-class-symbol*)
	      (*current-group-name* group-name)
	      (*expanding-test-for-group* t))
	  (declare (dynamic-extent *test-class-symbol*)
		   (dynamic-extent *current-group-name*)
		   (dynamic-extent *expanding-test-for-group*)
		   (ignorable *test-class-symbol*)
		   (ignorable *current-group-name*)
		   (ignorable *expanding-test-for-group*))

	  (eval-when (:compile-toplevel :load-toplevel :execute)
	    (setf *test-class-symbol* (gensym "test-class-")))
	  
	  (loop for ts in (reverse tests) do
	    (let ((processed-test (macroexpand ts)))
	      (loop for d in (reverse (car processed-test)) do
		(push d test-fixture-defs))
	      (loop for pt in (reverse (cadr processed-test)) do
		(push pt actual-tests))))
	  (forms-dbg
	   (format t " * Final list of fixtures defs:~
                    ~%   ~S~
                    ~% * Final list of tests:~
                    ~%   ~S~%"
		   test-fixture-defs actual-tests))
    
	  `(progn
	     #+allegro (excl:record-source-file ',group-name :type :nst-group)
	     (eval-when (:compile-toplevel :load-toplevel :execute)
	       ,@test-fixture-defs

	       (compile-dbg
		(format t
		    "Compiling anonymous fixtures to test group ~s~%"
		  ',group-name))

	       ;; First define any of the anonymous fixtures we found in
	       ;; the original fixtures list.
	       ,@anon-fixtures

	       (compile-dbg
		(format t "~@<Compiling test group ~s~_ (class ~s)~:>~%"
		  ',group-name ',group-class)
		(format t " * fixtures ~s~%" ',fixture-names)
		,@(when fixture-names
		    `((format t " * map to classes ~s~%"
			(loop for f in ',fixture-names
			    collect
			      (gethash f *fixture-to-group-class*))))))

	       ;; Define a uniquely-named class associated with this
	       ;; group.  It should extend all of the given fixture
	       ;; groups, plus the "group" class.
	       (let ((form
                      (list 'defclass ',group-class
                            `(,@(loop for f in ',fixture-names
				    collect 
                                      (gethash
				       f *fixture-to-group-class*))
                                group)
                            () '(:documentation ,class-doc))))
		 (bind-dbg 
		  (format t " * Form is:~%   ~s~%" form))
		 (eval form))
       
	       ;; Define a uniquely-named class which we will extend
	       ;; for each test in this group.  It will also extend
	       ;; all of the given fixture groups, plus the "test"
	       ;; class.
	       (let ((form
		      (list 'defclass ',*test-class-symbol*
			    `(,@(loop for f in ',fixture-names
				      collect
					(gethash
					 f *fixture-to-test-class*))
				test)
			    () '(:documentation ,class-doc))))
		 (forms-dbg
		  (format t " - Test class form:~%   ~s~%" form))
		 (eval form)
		 (forms-dbg (format t "   compiled~%")))
	       
	       ;; Extract the names which will be provided by the
	       ;; fixtures which this group uses.
	       (setf (gethash ',group-name +group-def-names+) 
		     ,(when fixture-names
			`(loop for ,f in ',fixture-names
			    append (gethash ,f +fixture-def-names+)))))
       
	     ;; (format t " - Creating singleton record for ~s~%"
	     ;;	 ',group-name)
	     (let ((current-group-info))
	       (eval-when (:load-toplevel :execute)
		 (setf current-group-info
		   (make-instance ',group-class
		     :package *package*
		     :name ',group-name
		     :fixtures ',fixture-names
		     :setup '(block nil ,@setup-form)
		     :cleanup '(block nil ,@cleanup-form)
		     :testclass ',*test-class-symbol*
		     :documentation ,doc-string))
		 ;; Save the group information against its name.
		 (setf (gethash ',group-name +groups+)
		   current-group-info))
	   
	       ;; Calling the fixture setup might throw an error, so
	       ;; we need to catch a setup exception here as well as
	       ;; when calling the actual setup form.
	       (defmethod run :around ((,ptg ,group-class)
				       &key report-stream)
		 (declare (ignorable report-stream))
		 (let ((*active-group* ,ptg))
		   (control-setup-errors (call-next-method))))
	 
	       ;; Convenience method for running this group by name.
	       (defmethod run-group ((g (eql ',group-name))
					&key (report-stream
					      cl-user::*nst-default-report-stream*))
		 (let ((group-info (gethash g +groups+)))
		   (run group-info :report-stream report-stream)))

	       ;; Save the group information against this package.
	       (let ((,wrapping-hash (gethash *package*
					      +groups-by-package+)))
		 (unless ,wrapping-hash
		   (setf ,wrapping-hash (make-hash-table)
			 (gethash *package*
				  +groups-by-package+) ,wrapping-hash))
		 (setf (gethash ',group-name ,wrapping-hash) t))
	     
	       ;; Set up variables that the tests defined in this
	       ;; group should see.
	       (let (;; Accumulators for tests in this group.
		     (*test-names-acc* nil)
		     (*test-info-hash* (make-hash-table))

		     ;; Any override for the default settings for
		     ;; deferring test compilation.
		     (*group-defer-test-compile* ,group-defer-compile))
	   
		 ;; Now process the tests defined for this group.
		 (setf *current-group-name* ',group-name)
		 ,@actual-tests
	   
		 ;; Store the accumulated test information in this
		 ;; group record.
		 (let ((tests-vector
			(make-array (length *test-names-acc*)
				    :initial-contents
				    (nreverse *test-names-acc*))))
		   (compile-dbg
		    (format t "   ~@<Saving compiled test names ~s ~
                                 ~_for class ~s~:>~%"
		      tests-vector (get-name current-group-info)))
		   (setf (slot-value current-group-info 'test-names)
		     tests-vector
		     
		     (slot-value current-group-info 'tests-hash)
		     *test-info-hash*)))
	       nil)))))))

;;; Exported macro for defining a boolean test.

#+allegro (excl::define-simple-parser def-test second :nst-test)
(defmacro def-test
    (test-name &key form 
		    (setup nil setup-supplied-p)
		    (cleanup nil cleanup-supplied-p)
		    (fixtures nil fixtures-supplied-p)
		    (defer-compile nil defer-compile-supplied-p))

  (macro-dbg (format t " - ~@<Expanding test ~s of group ~s ~
                              ~_(test class ~s)~:>~%"
		     test-name *current-group-name*
		     *test-class-symbol*))
  
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
	 (specials (loop for name in (gethash *current-group-name*
					      +group-def-names+)
		       collect (list 'special name)))
	 
	 (fixtures-forms nil)		; The list of forms used to
					; defined test-local fixtures.
	 (actual-test-class *test-class-symbol*))

    (when fixtures-supplied-p
      (fixture-dbg
       (format t "    - Fixtures before cleanup: ~s~%" fixtures))
      (let* ((new-class-name (gensym "custom-test-class"))
	     (class-defn (gensym)))
	(multiple-value-bind (anon-fixtures fixture-class-list)
	    (process-anonymous-fixtures fixtures)
	  (fixture-dbg
	   (format t "    - Fixtures after cleanup: ~s~%"
		   fixture-class-list))
	  (setf actual-test-class new-class-name)
	  (setf fixtures-forms
	    `(,@anon-fixtures
	      (let ((,class-defn
		     (list 'defclass ',new-class-name
			   (cons ',*test-class-symbol*
				 ',(loop for f in fixture-class-list
					 collect
					 (gethash
					   f *fixture-to-test-class*)))
			   ())))
		(forms-dbg
		 (format t "    - Local test class definition:~
                          ~%      Fixtures: ~s ~
                          ~%      Form: ~s ~%"
			 ',fixture-class-list
			 ,class-defn))
		(eval ,class-defn)
		(forms-dbg (format t "      compiled.~%"))))))))

    (let ((final-test-forms
	   `((compile-dbg
	      (format t "Compiling test ~s/~s (class ~s)~%"
		      ',*current-group-name* ',test-name
		      ',actual-test-class))
	     #+allegro (excl:record-source-file ',test-name :type :nst-test)
	     (let (;; Actual information record for this test.
		   (,test-info (make-instance ',actual-test-class
				 :group (gethash ',*current-group-name*
						 +groups+)
				 :name ',test-name
				 :documentation nil)))
	 
	       ;; File away this test's name and information.
	       (push ',test-name *test-names-acc*)
	       (setf (gethash ',test-name *test-info-hash*) ,test-info)
	 
	       ;; Define a method which runs the form given for this
	       ;; test.
	       (defmethod core ((ts (eql ,test-info)) &key report-stream)
		 ;; Declare the names provided by fixtures.
		 (declare ,@specials (ignorable report-stream))
		 ;; Run the test expression, and return its value.
		 
		 (multiple-value-bind (primary report)
		     ,actual-form
		   (when (null report) (setf report t))
		   (values primary report)))
	 
	       ;; Convenience method for running tests by name.
	       (defmethod run-test ((gr (eql ',*current-group-name*))
				    (ts (eql ',test-name))
				    &key (report-stream
					  cl-user::*nst-default-report-stream*))
		 (run ,test-info :report-stream report-stream))))))
    
      (if *expanding-test-for-group*
	  `(,fixtures-forms ,final-test-forms)
	  `(progn ,@fixtures-forms ,@final-test-forms)))))
