;;; File fixtures.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :sift.nst)


;;; Exported macro which sets up test fixtures.

(defparameter *active-group* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fixture-to-group-class* (make-hash-table :test 'eq))
  (defparameter *fixture-to-test-class* (make-hash-table :test 'eq)))

(defmacro def-fixtures (name
			(&key uses assumes outer inner documentation)
			     &body bindings)
  (macro-dbg
   (format t "Expanding declaration of fixture ~s:~%" name))
  (let* ((err (gensym "err"))
	 
	 ;; A list of the names in the bindings by themselves, without
	 ;; the associated forms.
	 (names (loop for b in bindings collect (car b)))
	 
	 ;; The names defined in the other fixtures which this fixture
	 ;; is declared to use.
	 (fix-used
	  (loop for f in uses
		append (loop for id in (gethash f +fixture-def-names+)
			     collect id)))
	 
	 ;; Names for the classes we define.
	 (class-for-group (gensym (format nil "~a-class-" name)))
	 (class-for-test  (gensym (format nil "~a-test-" name)))
	 
	 ;; Name-capture avoidance.
	 (ptg (gensym "ptg-"))
	 (disc (gensym "disc-"))
	 (id (gensym "id-"))
	 (report-stream (gensym "stream-")))

    (macro-dbg (format t " + Assembling form~%"))

    (setf (gethash name *fixture-to-group-class*) class-for-group
	  (gethash name *fixture-to-test-class*) class-for-test)
	 
    (class-dbg
     (format t
	 " - Classes from fixture ~s:~
        ~%    . For groups, ~s~%    . For tests, ~s~
        ~%    . Check: -> ~s, -> ~s~%"
       name class-for-group class-for-test
       (gethash name *fixture-to-group-class*)
       (gethash name *fixture-to-test-class*)))
       
    ;; Save this name with the other fixture names.  We check first,
    ;; since we could be re-defining the name.
    (unless (member name +fixtures+) (push name +fixtures+))
       
    ;; Save the names we define in this fixture.
    (setf (gethash name +fixture-def-names+) names)    
    
    `(progn
       (declaim ,@(loop for n in names collect `(special ,n)))
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (gethash ',name *fixture-to-group-class*)
	       ',class-for-group
	       (gethash ',name *fixture-to-test-class*)
	       ',class-for-test)
	 
	 ;; Below is all done after expanding the macro.
	 (compile-dbg
	  (format t
	      "Compiling declaration of fixture ~s:~
             ~% - Classes ~s, ~s~%"
	    ',name ',class-for-group ',class-for-test))
	      
	 ;; Create classes for using this fixture with groups and with
	 ;; individual tests.
	 (defclass ,class-for-group (fixture) ()
	   ,@(if documentation `((:documentation ,documentation))))
	 (defclass ,class-for-test  (fixture) ()
	   ,@(if documentation `((:documentation ,documentation))))
       
	 ;; We put the fixture's bindings in effect with this :around
	 ;; method.  All groups which use this fixture, and all of
	 ;; these groups' tests, will be subclasses of the class
	 ;; above.  So this :around method will give those test bodies
	 ;; these bindings.
	 (defmethod bind-for-group :around ((,ptg ,class-for-group)
					    ,report-stream)
	   (declare ,@outer
		    ,@(loop for f in fix-used collect `(special ,f))
		    ,@(loop for f in fix-used collect `(ignorable ,f))
		    ,@(loop for v in assumes collect `(special ,v)))
	   (bind-dbg
	    (format ,report-stream
		"       ~@<Binding names:~{ ~s~} ~_(by ~s via ~s)~:>~%"
	      ',names ',name ',class-for-group))
	   (let* ,(loop for b in bindings
			collect
			(let ((var (car b)) (body (cdr b)))
			  `(,var
			    (record-setup-error
			     *active-group* ,err
			     (make-instance 'fixture-error-report
			       :caught ,err
			       :fixture-name ',name
			       :var-name ',var) ,report-stream
			     ,@body))))
	     (declare
	      ,@(loop for n in names collect `(ignorable ,n))
	      ,@inner)
	     (call-next-method)))

	 (defmethod bind-for-test :around ((,ptg ,class-for-test)
					   ,report-stream)
	   (declare ,@outer
		    ,@(loop for f in fix-used collect `(special ,f))
		    ,@(loop for v in assumes collect `(special ,v))
		    ,@(loop for f in fix-used collect `(ignorable ,f)))
	   (bind-dbg
	    (format ,report-stream
		"       ~@<Binding names:~{ ~s~} ~_(by ~s via ~s)~:>~%"
		    ',names ',name ',class-for-test))
	   (let* ,(loop for b in bindings
			collect
			(let ((var (car b)) (body (cdr b)))
			  `(,var
			    (record-setup-error
			     *active-group* ,err
			     (make-instance 'fixture-error-report
			       :caught ,err
			       :fixture-name ',name
			       :var-name ',var) ,report-stream
			     ,@body))))
	     (declare
	      ,@(loop for n in names collect `(ignorable ,n))
	      ,@inner)
	     (call-next-method)))
       
	 ;; For runtime system debugging.  Returns the literal list of
	 ;; name-value bindings assigned to this fixture.
	 (defmethod get-fixture-bindings ((,disc (eql ',name)))
	   (declare (ignorable ,disc))
	   ',bindings)

	 ;; For opening the fixture to the current namespace.
	 (defmethod open-fixture ((,ptg (eql ',name)))
	   (declare ,@outer
		    ,@(loop for f in fix-used collect `(special ,f))
		    ,@(loop for v in assumes collect `(special ,v))
		    ,@(loop for f in fix-used collect `(ignorable ,f)))
	   ,(format nil "Generated method for the ~s fixture set." name)
	   (unless (and (gethash ',name *opened-fixtures*)
			(not *reopen-fixtures*))
	     ,@(when uses
		 `((when *open-used-fixtures*
		     (loop for ,id in ',uses do (open-fixture ,id)))))
	     ,@(loop for b in bindings append
		     `((verbose-out
			(format t
			    ,(format nil
				 "~~@<Defining ~s as~~_ ~~s...~~:>~~%"
			       (car b))
			  ',(cadr b)))
		       (defparameter ,@b)
		       (verbose-out
			(format t 
			    ,(format nil "~~@<Set ~s to~~_ ~~s~~:>~~%"
			       (car b))
			  ,(car b))))))
	   nil)
       
	 (compile-dbg
	  (format t
	      "Ending compilation of fixture ~s.~%" ',name))))))

;;; A convenience macro for specialized fixture definitions.

(defmacro def-capture/restore-fixtures (name variables
					     &key documentation)
  "Defines a simple fixtures which binds nil to each of the given
variables.  Since these bindings are all made via dynamic let's in
:around methods, the effect of this fixture will be to protect global
variables from the test suite."
  (let ((nil-bindings
	 (loop for v in variables collect (list v nil))))
    `(def-fixtures ,name (:documentation ,documentation)
       ,@nil-bindings)))

(defun process-anonymous-fixtures (fixtures-list)
  "Returns fixture declarations for anonymous fixtures"
  (let* ((fixture-decls nil)
	 (class-list nil))
    (fixture-dbg
     (format t "    >> Resolving fixture list ~s~%" fixtures-list))
    (loop for node on fixtures-list do
      (let ((item (car node)))
	(cond
	  ((symbolp item)
	   (push item class-list))

	  ((and (listp item) (eq :fixtures (car item)))
	   (let* ((name (gensym "anon-fixtures-"))
		  (decl-binding (cdr item))
		  (decl (macroexpand-1
			 `(def-fixtures ,name ()
			    ,@(loop for x = (pop decl-binding)
				   while x
				   for y = (pop decl-binding)
				   collect (list x y))))))
	     (push decl fixture-decls)
	     (push name class-list)
	     (fixture-dbg (format t "       ~s -> ~s~%" item name))))

	  (t (error "Unrecognized fixture name list item ~s"
		    item)))))
    (fixture-dbg
     (format t "    >>  - ~d anonymous fixtures found~
              ~%    >>  - Names (reversed): ~s~%"
	     (length fixture-decls) class-list))
    (values fixture-decls (nreverse class-list))))
