;;; File check.lisp
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

(defun continue-check (further)
  (destructuring-bind (method &rest details) further
    (cond
      ((check-form-criterion-p method)
       (let* ((the-form (last details))
	      (submethods (butlast details))
	      (changed (check-form-criterion method submethods))
	      (revised (append changed the-form)))
	 (continue-check revised)))
      
      (t
       (apply #'check-form method details)))))

(defgeneric check-form-criterion-p (method)
  (:method (m)  (declare (ignorable m))
     nil))

(defgeneric check-form-criterion (method details)
  (:method (m d)
     (declare (ignorable d))
     (error "No such check-form criterion ~s" m)))

(defmacro def-check (name &rest commands-and-forms
			  &aux setup cleanup fixtures)
  "Define a test constructed according to the specified method."

  (block process-check-options
    (loop do
      (unless commands-and-forms
	(error "too few arguments in def-check ~s" name))
      (let ((first (car commands-and-forms)))
	(unless (symbolp first)
	  (error "Bad command to def-check: ~s" first))

	(cond
	  ((eq first :setup)
	   (when setup
	     (error "Multiple :setup declaration to def-check ~s" name))
	   (pop commands-and-forms)
	   (let ((form (pop commands-and-forms)))
	     (setf setup `(:setup ,form))))
	    
	  ((eq first :cleanup)
	   (when setup
	     (error "Multiple :cleanup declaration to def-check ~s"
		    name))
	   (pop commands-and-forms)
	   (let ((form (pop commands-and-forms)))
	     (setf cleanup `(:cleanup ,form))))
	    
	  ((eq first :fixtures)
	   (when setup
	     (error "Multiple :fixtures declaration to def-check ~s"
		    name))
	   (pop commands-and-forms)
	   (let ((form (pop commands-and-forms)))
	     (setf fixtures `(:fixtures ,form))))
	    
	  (t (return-from process-check-options))))))
  
  `(def-test ,name
     ,@setup ,@cleanup ,@fixtures
     :form ,(continue-check commands-and-forms)))

(defgeneric check-form (method &rest details)
  (:documentation "Definition of the top-level check forms.")

  (:method (unrecognized &rest whatever)
     "Ill-specified checks are compile-time errors"
     (declare (ignorable whatever))
     (error "Unrecognized def-check form ~s~%" unrecognized)))

(defmacro def-check-form
    (name &optional (documentation nil documentation-supplied-p)
	  &key body
	  (args nil args-supplied-p)
	  (expose-subtests nil)
	  (require-min-bare-subforms 0)
	  (expose-bare-subforms nil expose-bare-subforms-supplied-p)
	  (strip-suffix nil strip-suffix-supplied-p))
    
  (let* ((cmd (gensym "cmd"))
	 (details (gensym "details")))
    
    (when (and (not expose-subtests)
	       (not expose-bare-subforms)
	       (eql require-min-bare-subforms 0))
      (setf body
	    `(progn
	       (unless (null ,details)
		 (warn "~@<Extra arguments to def-check form ~s ~
                           ignored:~_ ~@<~{~s~^ ~_~}~:>~:>"
		       ,cmd ,details))
	       ,body)))

    (when (> require-min-bare-subforms 0)
      (setf body
	    `(progn
	       (unless (<= ,require-min-bare-subforms (length ,details))
		 (error "Too few forms for ~s" ',name))
	       ,body)))
    
    (when expose-bare-subforms-supplied-p
      (setf body
	    `(let ((,expose-bare-subforms ,details))
	       ,body)))
    
    (when expose-subtests
      (setf body
	    `(symbol-macrolet ((subchecks (continue-check ,details)))
	       ,body)))

    (when strip-suffix-supplied-p
      (let ((next-details (gensym "details")))
	(setf body `(let ((,strip-suffix (car (last ,next-details)))
			  (,details (butlast ,next-details)))
		      ,body)
	      details next-details)))
    
    (when args-supplied-p
      (let ((next-details (gensym "details"))
	    (minimum (length args)))
	(setf body `(progn
		      (require-arguments-for-method
		       ,cmd ,next-details ,minimum)
		      (destructuring-bind (,@args &rest ,details)
			  ,next-details
			,body))
	      details next-details)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defmethod check-form ((,cmd (eql ',name)) &rest ,details)
	 ,@(when (and documentation-supplied-p (stringp documentation))
	     (list documentation))
	 ,body))))

(defmacro def-check-form-manip (name doc-string
				     &key (args nil) form manip)
  (let ((methods (gensym))
	(new-form (gensym)))
    `(def-check-form ,name ,doc-string
       :args ,args :strip-suffix ,form :expose-bare-subforms ,methods
       :body (let ((,new-form ,manip))
	       (continue-check (append ,methods (list ,new-form)))))))

(defun add-all-list-calls (xs)
  (cond
    ((eq (car xs) 'quote)
     `',(cadr xs))
			   
    (t
     `(list ,@(loop for x in xs
		    collect (cond
			      ((consp x) (add-all-list-calls x))
			      (t x)))))))

(defmacro def-check-criterion (name &key documentation
				(args nil)
				(rest (gensym) rest-supp-p)
				(expansion nil exp-supp-p))
  (declare (ignorable documentation))
  (unless exp-supp-p
    (error "def-check-criterion ~s given no expansion" name))
  (let* ((details (gensym)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defmethod check-form-criterion-p ((id (eql ',name))) t)
       (defmethod check-form-criterion ((id (eql ',name)) ,details)
	 (destructuring-bind (,@args &rest ,rest) ,details
	   ,(cond
	      (rest-supp-p expansion)
	      (t `(append ,expansion ,rest)))))
       nil)))

(def-check-form :pass
    "This test always passes"
  :body t)

(def-check-form :fail
    "This test always fails"
  :body nil)
  
(def-check-form :bool
    "Check that a value evaluates to non-null"
  :args (form) :body form)

;;; Standard checking forms --- various equality tests.
			       
(def-check-form :symbol
    "Check that the form evaluates to the given atom.  This is
the style of test provided by RT/RRT."
  :args (target form) :body `(eq ,form ',target))
			       
(def-check-form :eq
    "Check that the form evaluates to the given atom.  This is
the style of test provided by RT/RRT."
  :args (target form) :body `(eq ,form ,target))
   
(def-check-form :eql
    "Check that the form is eql to an ideal (which may itself be
another form)."
  :args (target form) :body `(eql ,form ,target))

(def-check-form :forms-eq
    "Check that two forms are eq."
  :args (form1 form2) :body `(eq ,form1 ,form2))
  
(def-check-form :forms-eql
    "Check that two forms are eql."
  :args (form1 form2) :body `(eql ,form1 ,form2))

(def-check-form :round-sig-eql
    "Check that two numbers are eql to the given number of significant\
 digits.  Fails if either form evaluates to a non-number"
  :args (digits form1 form2)
  :body (let ((n1 (gensym))
	      (n2 (gensym)))
	  `(let ((,n1 ,form1) (,n2 ,form2))
	     (and (numberp ,n1)
		  (numberp ,n2)
		  (eql-for-sigdigits ,digits ,n1 ,n2)))))

;;; Standard checking forms --- transforming arguments.
  
(def-check-form :predicate
    "Apply the named boolean predicate to a form, and take the
result as the test result."
  :args (predicate form) :body `(funcall #',predicate ,form))

(def-check-form :apply
    "Apply the named transformation to the value of a form,
and check the resulting value"
  :args (transform)
  :strip-suffix form
  :require-min-bare-subforms 1
  :expose-bare-subforms methods
  :body (let ((application `(funcall #',transform ,form)))
	  (continue-check (append methods (list application)))))

(def-check-form :with
    "Incorporate a list of check forms"
  :require-min-bare-subforms 1
  :expose-bare-subforms methods
  :args (form-list)
  :body (continue-check (append form-list methods)))

(def-check-form :perf
    "Incorporate a list of check forms"
  :strip-suffix form
  :expose-bare-subforms methods
  :args (metrics)
  :body (destructuring-bind (&key (ms nil ms-supp-p)
				  (sec nil sec-supp-p)
				  (min nil min-supp-p)) metrics
	  (warn-if-excess-arguments :perf methods)
	  (let ((core-form form))
	    (when (or sec-supp-p min-supp-p ms-supp-p)
	      (when (or (and sec-supp-p min-supp-p)
			(and ms-supp-p min-supp-p)
			(and ms-supp-p sec-supp-p))
		(error "Multiple time metrics given"))
	      (when sec-supp-p (setf ms (* 1000 sec)))
	      (when min-supp-p (setf ms (* 60000 sec)))
	      (setf core-form
		    `(let* ((start-time (get-internal-real-time))
			    (result ,core-form)
			    (elapsed-ms
			     (* ,(/ 1000
				    internal-time-units-per-second)
				(- (get-internal-real-time)
				   start-time))))
		       (declare (ignorable result))
		       (> ,ms elapsed-ms))))
	    core-form)))

;;; Standard checking forms --- combinations of methods on a single
;;; value form.

(def-check-form :all
    "Require all the given tests to succeed on a value"
  :strip-suffix form
  :require-min-bare-subforms 1
  :expose-bare-subforms methods
  :body (let ((block (gensym)) (val (gensym)))
	  `(block ,block
	     (let ((,val ,form))
	       ,@(loop for method in methods collect
		       `(unless
			    ,(continue-check (append method (list val)))
			  (return-from ,block nil))))
	     t)))

(def-check-form :any
    "Require that at least one of the given tests succeed on a value"
  :strip-suffix form
  :require-min-bare-subforms 1
  :expose-bare-subforms methods
  :body (let ((block (gensym)))
	  `(block ,block
	     ,@(loop for method in methods collect
		     `(when
			  ,(continue-check (append method (list form)))
			(return-from ,block t)))
	     nil)))

(def-check-form :not
    "Require that a check fail."
  :expose-subtests t
  :body `(not ,subchecks))

(def-check-form :multi
    "Provide corresponding tests for a multiple-values form"
  :strip-suffix form
  :require-min-bare-subforms 1
  :expose-bare-subforms methods
  :body (let ((block (gensym "block"))
	      (names (loop for m in methods collect (gensym "multi"))))
	  `(block ,block
	     (multiple-value-bind ,names ,form
	       ,@(loop for method = (pop methods)
		       and name = (pop names)
		       while method collect
		       `(unless ,(continue-check (append method
							(list name)))
			  (return-from ,block nil))))
	     t)))

;;; Standard checking forms --- expecting errors or warnings.

(def-check-form :err
    "The err specifier tells the tester to expect evaluation of the
form to throw an error, and otherwise the test fails."
  :args (form)
  :body (let ((x (gensym "x")))
	  `(block ,x
	     (handler-bind ((error #'(lambda (,x)
				       (declare (ignorable ,x))
				       (return-from ,x t))))
	       ,form)
	     nil)))

;;; Standard checking forms --- operations on lists.

(def-check-form :each
    "The each specifier tells the tester to expect that the form will 
evaluate to a list, and that each element of the list will pass the
check given in the further elements of the check specification."
  :expose-bare-subforms details
  :require-min-bare-subforms 2
  :body (let* ((list-form (car (last details)))
	       (subdetails-rev (cdr (reverse details)))
	       (x (gensym "x"))
	       (subform (continue-check
			 (reverse (cons x subdetails-rev)))))
	  `(block ,x
	     (loop for ,x in ,list-form do
	       (unless ,subform (return-from ,x nil)))
	     t)))
  
(def-check-form :seq
    "The seq specifier takes N further specifier elements of the form\
 plus a form for evaluation.  The check expects the form to evaluate\
 to a list of N elements which match the respective specifier in the\
 further elements."
  :expose-bare-subforms details
  :require-min-bare-subforms 1
  :body (let ((form (car (last details)))
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
			 ,@(when on-last
			     `((declare (ignorable ,last-var))))
			 (unless 
			     ,(continue-check (append method
						     (list first-form)))
			   (return-from ,overall nil))
			 ,result-form))
		    
		    last-var other-forms
		    on-last nil)))
    
	  `(block ,overall
	     (let ((,last-var ,form))
	       ,result-form))))
  
(def-check-form :permute
    "The permute specifier expects that the form will evaluate\
 to a list, some permutation of which will satisfy the further\
 specified check."
  :expose-bare-subforms details
  :require-min-bare-subforms 2
  :body (let ((form (car (last details)))
	      (method (nbutlast details))
	
	      (perms (gensym "perms-"))
	      (x (gensym "x-"))
	      (permute-block (gensym "permute-block-")))

	  `(block ,permute-block
	     (let ((,perms (make-instance 'permuter :src ,form)))
	       (loop while (has-next ,perms) do
		 (let ((,x (next-permutation ,perms)))
		   (when ,(continue-check (append method (list x)))
		     (return-from ,permute-block t))))))))

;;; Standard checking forms --- operations on vectors.

(def-check-form :across
    "The across specifier takes N further specifier elements and a\
 form, and expects the form to evaluate to a vector of N elements\
 which match the respective specifier in the further elements."
  :expose-bare-subforms details
  :require-min-bare-subforms 1
  :body (let ((form (car (last details)))
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
				 (continue-check (append check
							(list ref)))))
			   `(unless ,item-form
			      (return-from ,l nil))))))
	     t)))

;;; Standard checking forms --- operations on records and fields.
  
(def-check-form :slots
  "Apply checks to the individual slots of a class."
  :expose-bare-subforms details
  :require-min-bare-subforms 2
  :body (let* ((form (car (last details)))
	       (tuples (nbutlast details))
	
	       (slots-check (gensym "slots-check-"))
	       (slot-checks
		(loop for tuple in tuples
		      for slot-name = (car tuple)
		      and method = (cadr tuple)
		      collect
		      `(unless
			   ,(continue-check (append method
						   (list slot-name)))
			 (return-from ,slots-check nil))))
	    
	       (slot-names
		(loop for tuple in tuples collect (car tuple))))
       
	  `(block ,slots-check
	     (with-slots ,slot-names ,form ,@slot-checks)
	     t)))
