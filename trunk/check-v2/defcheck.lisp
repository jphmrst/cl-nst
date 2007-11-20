;;; File defcheck.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :sift.nst)

;;; Exported macro providing a more expressive test-definition
;;; facility.

;;; Some shorthand we'll use below.

(defmacro def-check (name-or-name-and-args criterion &rest forms)
  "Define a test constructed according to the specified criterion."

  (let (name setup setup-supp-p cleanup cleanup-supp-p
	     fixtures fixtures-supp-p)
    (cond
      ((symbolp name-or-name-and-args)
       (setf name name-or-name-and-args))
      ((listp name-or-name-and-args)
       (destructuring-bind (actual-name
			    &key (actual-setup nil actual-setup-supp-p)
				  (actual-cleanup
				   nil actual-cleanup-supp-p)
				  (actual-fixtures
				   nil actual-fixtures-supp-p))
	   name-or-name-and-args
	 (setf name actual-name
	       setup actual-setup
	       setup-supp-p actual-setup-supp-p
	       cleanup actual-cleanup
	       cleanup-supp-p actual-cleanup-supp-p
	       fixtures actual-fixtures
	       fixtures-supp-p actual-fixtures-supp-p)))
      (t
       (error "~@<Expected symbol or list for def-check argument~_ ~s~:>"
	      name-or-name-and-args)))
    `(def-test ,name
	 ,@(when setup-supp-p    `(:setup ,setup))
	 ,@(when cleanup-supp-p  `(:cleanup ,cleanup))
	 ,@(when fixtures-supp-p `(:fixtures ,fixtures))
	 :form (let ((check-result
		      ,(continue-check criterion
				       (cons 'list forms))))
		 (and (null (check-result-failures check-result))
		      (null (check-result-errors check-result)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric build-check-form (criterion args formals)
    (:documentation "Put together an expression for a test."))
  
  (defun continue-check (criterion forms)
    "criterion is a expression denoting a check to be made.  forms is
an expression evaluating to the stack of values to be tested."
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
	    (*nst-stack* forms))
	(declare (dynamic-extent *nst-context* *nst-stack*)
		 (ignorable *nst-context* *nst-stack*))
	(build-check-form criterion-name criterion-args forms))))

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
		  (car blurb-format) (cdr blurb-format)
		  forms)
	    blurb-format)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (defmethod blurb-context-line (,stream
					(,id (eql ',name)) ,args ,forms)
	   (destructuring-bind ,criterion-args ,args
	     (declare (ignorable ,@criterion-formals))
	     (destructuring-bind ,check-args ,forms
	       (declare (ignorable ,@check-formals))
	       (format ,stream ,@blurb-format))))
	 (defmethod detail-context-line (,stream
					 (,id (eql ',name)) ,args ,forms)
	   (destructuring-bind ,criterion-args ,args
	     (declare (ignorable ,@criterion-formals))
	     (destructuring-bind ,check-args ,forms
	       (declare (ignorable ,@check-formals))
	       (format ,stream ,@full-format))))
	 (defmethod stack-transformer ((,id (eql ',name)))
	   ,stack-transformer)
	 (defmethod build-check-form ((,id (eql ',name)) ,args ,forms)
	   (destructuring-bind ,criterion-args ,args
	     (list 'destructuring-bind ',check-args ,forms
		   ,@expansion))))))

  (defmacro def-control-check ((name criterion-args forms-formal
				 &key (stack-transformer t)
				 (blurb-format nil blurb-format-supp-p)
				 (full-format nil full-format-supp-p))
				&body check-forms)
    (let ((criterion-formals (lambda-list-names criterion-args))
	  (stream (gensym "stream")) (id (gensym "id"))
	  (args (gensym "args")))
      (unless blurb-format-supp-p
	(setf blurb-format
	  `("~s ~@<~{~s~^ ~:_~}~:>" ',name (list ,@criterion-formals))))
      (unless full-format-supp-p
	(setf full-format
	  (if stack-transformer
	    (list "~@<~?~_~@<~:~{@_  ~s~}~:>~:>"
		  (car blurb-format) (cons 'list (cdr blurb-format))
		  forms-formal)
	    blurb-format)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
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

  (defmacro def-check-alias ((name &rest args) documentation
			     &optional (expansion nil exp-supp-p)
			     &aux (documentation-supp-p t))
    (unless exp-supp-p
      (setf expansion documentation
	    documentation nil  documentation-supp-p nil))

    (let* ((forms (gensym "forms")) (stream (gensym "stream"))
	   (id (gensym "id")) (exp (gensym "exp"))
	   (given-args (gensym "given-args")))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (defmethod blurb-context-line (,stream (,id (eql ',name))
					,given-args ,forms)
	   (destructuring-bind ,args ,given-args
	     (let ((,exp ,expansion))	   
	       (blurb-context-line ,stream
				   (car ,exp) (cdr ,exp) ,forms))))
	 (defmethod detail-context-line (,stream (,id (eql ',name))
					 ,given-args ,forms)
	   (destructuring-bind ,args ,given-args
	     (let ((,exp ,expansion))	   
	       (detail-context-line ,stream
				    (car ,exp) (cdr ,exp) ,forms))))
	 (defmethod stack-transformer ((,id (eql ',name))) nil)
	 (defmethod build-check-form ((,id (eql ',name))
				      ,given-args ,forms)
	   ,@(when documentation-supp-p
	       (when (stringp documentation) `(,documentation)))
	   (destructuring-bind ,args ,given-args
	     (let ((,exp ,expansion))	   
	       (build-check-form (car ,exp) (cdr ,exp) ,forms))))))))
