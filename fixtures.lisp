;;; File fixtures.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :nst)


;;; Exported macro which sets up test fixtures.

(defparameter *active-group* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fixture-to-group-class* (make-hash-table :test 'eq))
  (defparameter *fixture-to-test-class* (make-hash-table :test 'eq)))

(defmacro def-fixtures (name &key bindings
			     uses outer inner documentation)
  (macro-dbg (format t "Expanding declaration of fixture ~s:~%" name))
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
	 
	 ;; Names for the classes we define.
	 (class-for-group (gensym))
	 (class-for-test  (gensym))
	 
	 ;; Macro joy.
	 (ptg (gensym "ptg-"))
	 (disc (gensym "disc-"))
	 (id (gensym "id-")))

    
    (let ((result
	   `(progn
	      (eval-when (:compile-toplevel :load-toplevel :execute)
		(setf (gethash ',name *fixture-to-group-class*)
		      ',class-for-group
		      (gethash ',name *fixture-to-test-class*)
		      ',class-for-test)
		(class-dbg
		 (format t " - Classes from fixture ~s:~
              ~%    . For groups, ~s~%    . For tests, ~s~%"
			 ',name ',class-for-group ',class-for-test)))
	      
	      (compile-dbg
	       (format t "Compiling declaration of fixture ~s:~%"
		       ',name))
	      
	      ;; The names we bind must be declaimed special, or they
	      ;; will not be recognized.
	      (declaim ,@special-decls)
       
	      ;; Save this name with the other fixture names.  We
	      ;; check first, since we could be re-defining the name.
	      (unless (member ',name +fixtures+)
		(push ',name +fixtures+))
       
	      ;; Save the names we define in this fixture.
	      (setf (gethash ',name +fixture-def-names+) ',names-only)
       
	      ;; Create classes for using this fixture with groups and
	      ;; with individual tests.
	      (defclass ,class-for-group (fixture) () ,@doc-forms)
	      (defclass ,class-for-test  (fixture) () ,@doc-forms)
       
	      ;; We put the fixture's bindings in effect with this
	      ;; :around method.  All groups which use this fixture,
	      ;; and all of these groups' tests, will be subclasses of
	      ;; the class above.  So this :around method will give
	      ;; those test bodies these bindings.
	      (defmethod bind-for-group
		  :around ((,ptg ,class-for-group))
		(declare ,@outer ,@used-specials
			 ,@ignorable-used-specials)
		(bind-dbg
		 (format t "  ~@<Binding names:~{ ~s~} ~
                               ~_(by ~s via ~s)~:>~%"
			 ',names-only ',name ',class-for-group))
		(let* ,safe-bindings
		  (declare ,@dynamic-decls ,@inner)
		  (call-next-method)))
	      (defmethod bind-for-test :around ((,ptg ,class-for-test))
		(declare ,@outer ,@used-specials
			 ,@ignorable-used-specials)
		(bind-dbg
		 (format t "     ~@<Binding names:~{ ~s~} ~
                                  ~_(by ~s via ~s)~:>~%"
			 ',names-only ',name ',class-for-test))
		(let* ,safe-bindings
		  (declare ,@dynamic-decls ,@inner)
		  (call-next-method)))
       
	      ;; For runtime system debugging.  Returns the literal
	      ;; list of name-value bindings assigned to this fixture.
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
	      
	      (compile-dbg
	       (format t "Ending compilation of fixture ~s.~%"
		       ',name)))))
      (macro-dbg (format t "End expansion of fixture ~s~%" name))
      result)))

;;; Defining a fixture anonymously.
;;;(defmacro quick-fix (name binding)
;;;  (let ((bindings (loop for x = (pop binding) while x
;;;			for y = (pop binding)
;;;			collect (list x y))))
;;;    `(def-fixtures ,name :bindings ,bindings)))

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

(defun clean-fixtures-names! (fixtures-list fixture-to-class)
  "Returns fixture declarations for anonymous fixtures"
  (let* ((fixture-decls nil))
    (loop for node on fixtures-list do
      (let ((item (car node)))
	(cond
	  ((symbolp item)
	   (setf (car node) (gethash item fixture-to-class))
	   t)

	  ((and (listp item) (eq :fixtures (car item)))
	   (let* ((name (gensym))
		  (decl-binding (cdr item))
		  (decl (macroexpand-1
			 `(def-fixtures ,name :bindings
			    ,(loop for x = (pop decl-binding)
				   while x
				   for y = (pop decl-binding)
				   collect (list x y))))))
	     ;; (fixture-dbg
	     ;;  (format t "      * From ~s~%        generated ~s~%"
	     ;;          (car node) decl))
	     (push decl fixture-decls)
	     (setf (car node) (gethash name fixture-to-class))))

	  (t (error "Unrecognized fixture name list item ~s"
		    item)))))
    fixture-decls))
