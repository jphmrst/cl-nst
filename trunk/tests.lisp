;;; File tests.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :nst)

;;; Macros defining tests and groups.

;;; Global variables which we use in the embedded macros.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *test-class-symbol* nil)
  (defvar *current-group-name* nil)
  (defvar *current-group-info* nil)
  (defvar *test-names-acc* nil)
  (defvar *test-info-hash* nil)
  (defvar *group-defer-test-compile* nil)
  (defvar *fixtures-for-group* nil)
  (defvar *fixtures-for-group-name* nil))

(defmacro def-test-group (group-name fixture-names &rest forms)
  "Define a group of tests associated with certain fixtures,
initialization and cleanup."

  (macro-dbg
   (format t "Expanding test group ~s with fixtures ~s.~%"
	   group-name fixture-names))
  (let ((doc-string nil) (tests nil)
	(setup-form nil) (cleanup-form nil)
	(group-defer-compile *defer-test-compile*)
	(anon-fixtures (clean-fixtures-names! fixture-names
					      *fixture-to-group-class*))
	
	;; Unique names.
	(f (gensym "f"))
	(test-class (gensym "test-class-"))
	(group-class (gensym (concatenate 'string "group-class-"
					  (symbol-name group-name)
					  "-")))
	(singleton (gensym "singleton-"))
	(wrapping-hash (gensym "wrapping-hash-"))
	(ptg (gensym))

	(class-doc 
	 (format nil
		 " - Class definition corresponding to test group ~s"
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
       
       (compile-dbg
	(format t "Compiling anonymous fixtures to test group ~s~%"
		',group-name))

       ;; First define any of the anonymous fixtures we found in the
       ;; original fixtures list.
       ,@anon-fixtures
       
       (compile-dbg
	(format t "Compiling test group ~s~%" ',group-name))

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
       (eval-when (:compile-toplevel :load-toplevel :execute)       
	 (setf (gethash ',group-name +group-def-names+) 
	       (loop for ,f in ',fixture-names
		     append (gethash ,f +fixture-def-names+))))
       
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

  (macro-dbg (format t " - Processing test ~s of group ~s~%"
		     test-name *current-group-name*))
  
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
	 (actual-test-class '*test-class-symbol*))

    (when fixtures-supplied-p
      (fixture-dbg
       (format t "    - Fixtures before cleanup: ~s~%" fixtures))
      (let* ((new-class-name (gensym "custom-test-class"))
	     (anon-fixtures
	      (clean-fixtures-names! fixtures *fixture-to-test-class*))
	     (class-defn (gensym)))
	(fixture-dbg
	 (format t "    - Fixtures after cleanup: ~s~%"
		 fixtures))
	(setf actual-test-class `',new-class-name)
	(setf fixtures-forms
	      `(,@anon-fixtures
		(let ((,class-defn (list 'defclass ',new-class-name
					 (cons *test-class-symbol*
					       ',fixtures)
					 ())))
		  (macro-dbg
		   (format t
			   "    - Local test class definition:~%   ~s~%"
			   ,class-defn))
		  (eval ,class-defn))))
	;;(macro-dbg
	;;  (format t "    - Test-local fixture declaration:~%     ~s~%"
	;;	    fixtures-forms))
	))
    
    `(progn
       (compile-dbg
	(format t "Compiling test ~s/~s~%"
		',*current-group-name* ',test-name))
    
       ,@fixtures-forms

       (let (;; Actual information record for this test.
	     (,test-info (make-instance ,actual-test-class
			   :group ',*current-group-info*
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
	   ,actual-form)
	 
	 ;; Convenience method for running tests by name.
	 (defmethod run-test ((gr (eql ',*current-group-name*))
			      (ts (eql ',test-name)))
	   (run ,test-info))))))
