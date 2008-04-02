;;; File defcheck.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006, 2007, 2008 Smart Information Flow Technologies.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with NST.  If not, see <http://www.gnu.org/licenses/>.
(in-package :sift.nst)

;;; Exported macro providing a more expressive test-definition
;;; facility.

;;; Some shorthand we'll use below.

(defmacro def-check (name-or-name-and-args criterion &rest forms)
  "Define a test constructed according to the specified criterion.

name-or-name-and-args - either a symbol, or a list of symbols.  The first symbol
is the name of the test being defined; the remaining symbols are options.

criterion - the criterion for the forms to pass this test.

forms - forms to be evaluated and assessed by the criterion."

  (multiple-value-bind (name setup setup-supp-p cleanup cleanup-supp-p
			fixtures fixtures-supp-p)
      (decode-name-or-name-and-args name-or-name-and-args)
    `(def-test ,name
	 ,@(when setup-supp-p    `(:setup ,setup))
	 ,@(when cleanup-supp-p  `(:cleanup ,cleanup))
	 ,@(when fixtures-supp-p `(:fixtures ,fixtures))
	 :form (let ((check-result
		      ,(continue-check criterion (cons 'list forms))))
		 (values
		  (and (null (check-result-failures check-result))
		       (null (check-result-errors check-result)))
		  check-result)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun decode-name-or-name-and-args (name-or-name-and-args)
    (cond
     ((symbolp name-or-name-and-args)
      (return-from decode-name-or-name-and-args
	(values name-or-name-and-args nil nil nil nil nil nil)))
     ((listp name-or-name-and-args)
      (destructuring-bind (name &key (setup nil setup-supp-p)
				(cleanup nil cleanup-supp-p)
				(fixtures nil fixtures-supp-p))
	  name-or-name-and-args
	(return-from decode-name-or-name-and-args
	  (values name
		  setup setup-supp-p
		  cleanup cleanup-supp-p
		  fixtures fixtures-supp-p))))
     (t
      (error "~@<Expected symbol or list for def-check argument~_ ~s~:>"
	     name-or-name-and-args))))
  
  (defgeneric build-check-form (criterion args formals)
    (:documentation "Put together an expression for a test."))
  
  (defun continue-check (criterion forms)
    "criterion is a expression denoting a check to be made.  forms is
an expression evaluating to the stack of values to be tested."
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

  (defmacro def-check-alias ((name &rest args)
			     &body forms
			     &aux
			     (documentation nil)
			     (documentation-supp-p nil)
			     (declaration-form nil)
			     (declaration-form-supp-p nil)
			     (expansion nil))
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
    
;;;    (unless exp-supp-p
;;;      (setf expansion documentation
;;;	    documentation nil  documentation-supp-p nil))

    (let* ((forms (gensym "forms")) (stream (gensym "stream"))
	   (id (gensym "id")) (exp (gensym "exp"))
	   (given-args (gensym "given-args")))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
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
	       (build-check-form (car ,exp) (cdr ,exp) ,forms))))))))
