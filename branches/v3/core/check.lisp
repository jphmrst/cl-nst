;;; File check.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006, 2007, 2008 Smart Information Flow Technologies.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with NST.  If not, see
;;; <http://www.gnu.org/licenses/>.
(in-package :sift.nst)

(defun decode-defcheck-name-and-args (name-or-name-and-args)
  "This function unpacks the information inside the first form of a def-check
block, which can be either a single symbol naming the test, or a list whose
first element is that symbol and whose remaining elements are options."
  
  (cond
   ((symbolp name-or-name-and-args)
    (return-from decode-defcheck-name-and-args
      (values name-or-name-and-args nil nil nil nil nil nil)))
   ((listp name-or-name-and-args)
    (destructuring-bind (name &key (setup nil setup-supp-p)
				   (cleanup nil cleanup-supp-p)
				   (fixtures nil fixtures-supp-p))
	name-or-name-and-args
      (return-from decode-defcheck-name-and-args
	(values name
		setup setup-supp-p
		cleanup cleanup-supp-p
		fixtures fixtures-supp-p))))
   (t
    (error "~@<Expected symbol or list for def-check argument~_ ~s~:>"
	   name-or-name-and-args))))
  
(defgeneric build-check-form (criterion args formals)
  (:documentation
   "Assemble a Lisp expression corresponding to the logic of a single test.")
  (:method (unknown-criterion args formals)
     (declare (ignorable args formals))
     (error "Undefined criterion ~s" unknown-criterion)))

(defvar *error-checking* nil
  "Criteria such as :check-err set this variable to t (and declare it special)
to suppress error-handling in continue-check, and thus become able to handle
all further errors themselves.")

(defun continue-check (criterion forms)
  "This function is available from within the check-defining forms to process
subsequences of a current check definition.
 - criterion is an expression denoting a check to be made.
 - forms is an expression which at runtime will evaluate to the stack of values
   to be tested."
  
  (declare (special *nst-context*))
  (let (criterion-name criterion-args)
    (cond ((symbolp criterion)
	   (setf criterion-name criterion criterion-args nil))
	  ((listp criterion)
	   (setf criterion-name (car criterion)
		 criterion-args (cdr criterion)))
	  (t
	   (error "Malformed criterion in def-check: ~s" criterion)))
    (let ((*nst-context* (cons (cons criterion-name criterion-args)
			       *nst-context*))
	  (*nst-stack* forms)
	  (checker-block (gensym "block.")))
      (declare (special *nst-context* *nst-stack*))
      (let ((body (build-check-form criterion-name criterion-args forms)))
	(cond
	 (*error-checking*
	  body)
	 (t
	  `(block ,checker-block
	     (handler-bind
		 ((error #'(lambda (e)
			     (unless *debug-on-error*
			       (return-from ,checker-block
				 (make-check-result
				  :errors (list (make-error-check-note
						 :context *nst-context*
						 :stack *nst-stack*
						 :format "~a"
						 :args (list (format nil
								 "~w" e))
						 :error e))))))))
	       ,body))))))))

#+allegro (excl::define-simple-parser def-value-check caadr :nst-criterion)
(defmacro def-value-check ((name criterion-args check-args &key
				 (blurb-format nil blurb-format-supp-p)
				 (full-format nil full-format-supp-p)
				 (stack-transformer nil))
			   &body expansion)
  (let* ((stream (gensym "stream")) (id (gensym "id"))
	 (args (gensym "args")) (forms (gensym "forms"))
	 (criterion-formals (lambda-list-names criterion-args))
	 (check-formals (lambda-list-names check-args)))
    (unless blurb-format-supp-p
      (setf blurb-format
	(list "~s ~@<~{~s~^ ~:_~}~:>" name
	      (cons 'list criterion-formals))))
    (unless full-format-supp-p
      (setf full-format
	(if stack-transformer
	  (list "~@<~?~_~@<~:~{@_  ~s~}~:>~:>"
		(car blurb-format) (cdr blurb-format) forms)
	  blurb-format)))
    `(eval-when (:compile-toplevel :load-toplevel)
       #+allegro (eval-when (:load-toplevel)
		   (excl:record-source-file ',name :type :nst-criterion))
       (defmethod blurb-context-line (,stream (,id (eql ',name)) ,args ,forms)
	 (destructuring-bind ,criterion-args ,args
	   (declare (ignorable ,@criterion-formals))
	   (destructuring-bind ,check-args ,forms
	     (declare (ignorable ,@check-formals))
	     (format ,stream ,@blurb-format))))
       (defmethod detail-context-line (,stream (,id (eql ',name)) ,args ,forms)
	 (destructuring-bind ,criterion-args ,args
	   (declare (ignorable ,@criterion-formals))
	   (destructuring-bind ,check-args ,forms
	     (declare (ignorable ,@check-formals))
	     (format ,stream ,@full-format))))
       (defmethod stack-transformer ((,id (eql ',name)))
	 ,stack-transformer)
       (defmethod build-check-form ((,id (eql ',name)) ,args ,forms)
	 (destructuring-bind ,criterion-args ,args
	   ;; `(destructuring-bind ,',check-args ,',forms ,@',expansion)
	   (list 'destructuring-bind ',check-args ,forms ,@expansion)
	   )))))

#+allegro (excl::define-simple-parser def-control-check caadr :nst-criterion)
(defmacro def-control-check ((name criterion-args forms-formal
			      &key (stack-transformer t)
				   (blurb-format nil blurb-format-supp-p)
				   (full-format nil full-format-supp-p))
			     &body check-forms)
  (let ((criterion-formals (lambda-list-names criterion-args))
	(stream (gensym "stream")) (id (gensym "id"))
	(args (gensym "args")))
    (unless (symbolp forms-formal)
      (error "Expected a symbol but got ~s" forms-formal))
    (unless blurb-format-supp-p
      (setf blurb-format
	`("~s ~@<~{~s~^ ~:_~}~:>" ',name (list ,@criterion-formals))))
    (unless full-format-supp-p
      (setf full-format
	(if stack-transformer
	  (list "~@<~?~_~@<~:{~_  ~s~}~:>~:>"
		(car blurb-format) (cons 'list (cdr blurb-format))
		forms-formal)
	  blurb-format)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       #+allegro (excl:record-source-file ',name :type :nst-criterion)
       (defmethod blurb-context-line (,stream (,id (eql ',name))
				      ,args ,forms-formal)
	 (declare (ignorable ,forms-formal))
	 (destructuring-bind ,criterion-args ,args
	   (declare (ignorable ,@criterion-formals))
	   (format ,stream ,@blurb-format)))
       (defmethod detail-context-line (,stream (,id (eql ',name))
				       ,args ,forms-formal)
	 (declare (ignorable ,forms-formal))
	 (destructuring-bind ,criterion-args ,args
	   (declare (ignorable ,@criterion-formals))
	   (format ,stream ,@full-format)))
       (defmethod stack-transformer ((,id (eql ',name)))
	 ,stack-transformer)
       (defmethod build-check-form ((,id (eql ',name))
				    ,args ,forms-formal)
	 (destructuring-bind ,criterion-args ,args
	   ,@check-forms)))))

#+allegro (excl::define-simple-parser def-check-alias caadr :nst-criterion)
(defmacro def-check-alias ((name &rest args)
			   &body forms
			   &aux
			   (documentation nil)
			   (documentation-supp-p nil)
			   (declaration-form nil)
			   (declaration-form-supp-p nil)
			   (expansion nil))
  "Defines how a criterion should be rewritten as another criterion
or criteria.
     The name is the name of the alias, and the args are the arguments
that appear with it.
     The FORMS argument needs quotation, since the body provides forms
that will be substituted for the \(name &rest args\) where they appear
in a def-check.  Typically the ARGS will be substituted into the forms
when def-check-alias is macroexpanded."
  (when (stringp (car forms))
    (setf documentation (pop forms)
	  documentation-supp-p t))
  (when (eq (caar forms) 'declare)
    (setf declaration-form (pop forms)
	  declaration-form-supp-p t))
  (cond
   ((eql (length forms) 1)
    (setf expansion (pop forms)))
   (t
    (error "Ill-formed (d~@<ef-check-alias (~s~{ ~s~}) ~
                            ~:[~*~;~:@_~s~]~
                            ~:[~*~;~:@_~s~]~
                            ~{~:@_~s~}~:>)"
	   name args 
	   documentation-supp-p documentation
	   declaration-form-supp-p declaration-form
	   forms)))
    
;;;    (unless exp-supp-p (setf expansion documentation
;;;	    documentation nil  documentation-supp-p nil))

  (let* ((forms (gensym "forms")) (stream (gensym "stream"))
	 (id (gensym "id")) (exp (gensym "exp"))
	 (given-args (gensym "given-args")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       #+allegro (excl:record-source-file ',name :type :nst-criterion)
       (defmethod blurb-context-line (,stream (,id (eql ',name))
				      ,given-args ,forms)
	 (destructuring-bind ,args ,given-args
	   ,@(when declaration-form-supp-p `(,declaration-form))
	   (let ((,exp ,expansion))	   
	     (blurb-context-line ,stream
				 (car ,exp) (cdr ,exp) ,forms))))
       (defmethod detail-context-line (,stream (,id (eql ',name))
				       ,given-args ,forms)
	 (destructuring-bind ,args ,given-args
	   ,@(when declaration-form-supp-p `(,declaration-form))
	   (let ((,exp ,expansion))	   
	     (detail-context-line ,stream
				  (car ,exp) (cdr ,exp) ,forms))))
       (defmethod stack-transformer ((,id (eql ',name))) nil)
       (defmethod build-check-form ((,id (eql ',name))
				    ,given-args ,forms)
	 ,@(when documentation-supp-p
	     (when (stringp documentation) `(,documentation)))
	 (destructuring-bind ,args ,given-args
	   ,@(when declaration-form-supp-p `(,declaration-form))
	   (let ((,exp ,expansion))	   
	     (build-check-form (car ,exp) (cdr ,exp) ,forms)))))))

(defmacro def-check (name-or-name-and-args criterion &rest forms)
  (declare (special *the-group*))	; The def-group we're within.

  ;; Decode the name-or-name-and-args, pulling out the individual
  ;; components, and indicating which are given in this test.
  (multiple-value-bind (name setup setup-supp-p cleanup cleanup-supp-p
			fixtures fixtures-supp-p)
      (decode-defcheck-name-and-args name-or-name-and-args)
    (declare (ignorable fixtures-supp-p))

    (let ((*nst-context* nil)
	  (suite-class-name (gensym "suite-class-name"))
	  (standalone-class-name (gensym "standalone-class-name"))
	  (test-config-class-name (gensym "test-config-class-name"))
	  (fixtures-from-group (gensym "fixtures-from-group"))
	  (check-fixture-classes (gensym "check-fixture-classes"))
	  (anon-fixture-forms (gensym "anon-fixture-forms"))
	  (test-in-group-class-name (gensym "test-in-group-class-name"))
	  (core-run-body
	   (cond
	     ((eql 1 (length forms))
	      (continue-check criterion
			      `(common-lisp:multiple-value-list ,(car forms))))
	     (t
	      (continue-check criterion (cons 'list forms))))))
      (declare (special *nst-context*))
      `(block ,name
	 (macrolet ((eval-dbg (form) `(progn (format t "~%~s~%" ,form)
					     (eval ,form)
					     (format t "OK~%"))))
	   (multiple-value-bind (z ,check-fixture-classes ,anon-fixture-forms)
	       (process-fixture-list ',fixtures)
	     (declare (ignorable z))
	     (loop for form in ,anon-fixture-forms do (eval form))
	   
	     (let (;; In this block we make our local binding to the
		   ;; information stored to methods from earlier
		   ;; load-time NST forms.
		   (,suite-class-name (suite-class-name ',*the-group* ',name))
		   (,standalone-class-name
		    (standalone-class-name ',*the-group* ',name))
		   (,test-config-class-name
		    (test-config-class-name ',*the-group* ',name))
		   (,fixtures-from-group (group-fixture-classes ',*the-group*))
		   (,test-in-group-class-name
		    (test-in-group-class-name ',*the-group*)))
	       (unless ,suite-class-name
		 (setf ,suite-class-name
		   (gentemp ,(concatenate 'string
			       (symbol-name *the-group*) "/"
			       (symbol-name name) ".suite.")
			    :sift.nst.test-within-group-class-names))
		 (defmethod suite-class-name ((g (eql ',*the-group*))
					      (c (eql ',name)))
		   ,suite-class-name))
	       (unless ,standalone-class-name
		 (setf ,standalone-class-name
		   (gentemp ,(concatenate 'string
			       (symbol-name *the-group*) "/"
			       (symbol-name name) ".standalone.")
			    :sift.nst.test-within-group-class-names))
		 (defmethod standalone-class-name ((g (eql ',*the-group*))
						   (c (eql ',name)))
		   ,standalone-class-name))
	       (unless ,test-config-class-name
		 (setf ,test-config-class-name
		   (gentemp ,(concatenate 'string
			       (symbol-name *the-group*) "/"
			       (symbol-name name) ".testconfig.")
			    :sift.nst.test-within-group-class-names))
		 (defmethod test-config-class-name ((g (eql ',*the-group*))
						    (c (eql ',name)))
		   ,test-config-class-name))

	       (eval `(defclass ,,test-config-class-name () ()))
	     
	       (eval `(defclass ,,suite-class-name
			      (,,test-in-group-class-name
			       ,,test-config-class-name
			       ,@,check-fixture-classes)
			    ()))

	       (eval `(defclass ,,standalone-class-name
			  (,(group-class-name ',*the-group*)
			   ,@,fixtures-from-group
			   ,@,check-fixture-classes
			   ,(standalone-test-in-group-class-name ',*the-group*)
			   ,,test-in-group-class-name
			   ,,test-config-class-name)
			()))

	       (eval `(defmethod check-name ((obj ,,suite-class-name))
			',',name))
	       (eval `(defmethod check-name ((obj ,,standalone-class-name))
			',',name))
       
	       (eval `(defmethod core-run ((obj ,,standalone-class-name))
			    (core-run-test obj)))

	       (eval `(defmethod core-run-test ((obj ,,suite-class-name))
			,',core-run-body))
	       (eval `(defmethod core-run-test ((obj ,,standalone-class-name))
			,',core-run-body))

	       ,@(when setup-supp-p
		   `((eval `(defmethod core-run-test
				    :before ((obj ,,test-config-class-name))
				  ,',setup))))

	       ,@(when cleanup-supp-p
		   `((eval `(defmethod core-run-test
				    :after ((obj ,,test-config-class-name))
				  ,',cleanup))))

	       ;; Test results are stored under a canonical name.
	       (eval `(defmethod canonical-storage-name ((inst
							  ,,suite-class-name))
			',,suite-class-name))
	       (eval `(defmethod canonical-storage-name
			  ((inst ,,standalone-class-name))
			',,suite-class-name))
	       (eval `(defmethod canonical-storage-name 
			  ((inst (eql ',,suite-class-name)))
			',,suite-class-name))
	       (eval `(defmethod canonical-storage-name
			  ((inst (eql ',,standalone-class-name)))
			',,suite-class-name))
	     
	       ;; Clear any previous stored results, since we've just
	       ;; (re-)defined this check.
	       (when (boundp '+results-record+)
		 (remhash ',suite-class-name (symbol-value '+results-record+)))
	       
	       ;; Provide debugging information about this test.
	       (defmethod trace-test ((gr (eql ',*the-group*))
				      (ts (eql ',name)))
		 (format t "Test ~s (group ~s)~%" gr ts)
		 (format t " - Given name and args: ~s~%"
		   ',name-or-name-and-args)
		 (format t " - Given criterion: ~s~%" ',criterion)
		 (format t " - Given forms: ~@<~{~s~^ ~:_~}~:>~%" ',forms)
	     
		 (format t " - In-suite class name: ~s~%"
		   (suite-class-name ',*the-group* ',name))
		 (format t "              expected: ~s~%" ,suite-class-name)
		 (format t "   Superclasses: ~@<~{~s~^ ~:_~}~:>~%"
		   (loop for super
		       in (class-direct-superclasses
			   (find-class (suite-class-name ',*the-group* ',name)))
		       collect (class-name super)))
		 (when *nst-info-shows-expected*
		   (format t "       expected: ~s~%" ,test-in-group-class-name))
	     
		 (format t " - Standalone class name: ~s~%"
		   (standalone-class-name ',*the-group* ',name))
		 (when *nst-info-shows-expected*
		   (format t "                expected: ~s~%"
		     ,standalone-class-name))
		 (format t "   Superclasses: ~@<~{~s~^ ~:_~}~:>~%"
		   (loop for super
		       in (class-direct-superclasses
			   (find-class (standalone-class-name ',*the-group*
							      ',name)))
		       collect (class-name super)))
		 (when *nst-info-shows-expected*
		   (format t
		       "       expected: ~@<~s ~:_~@<~{~s~^ ~:_~}~:>~:>~%" 
		     (standalone-test-in-group-class-name ',*the-group*)
		     ,fixtures-from-group))
	     
		 ))))))))
