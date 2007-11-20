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

(def-value-check (:pass () (&rest chk))
  `(declare (ignorable chk))
  `(make-check-result))

(def-value-check (:fail (&rest args) (&rest chk))
  `(declare (ignorable chk))
  `(emit-failure :format ,(car args) :args ,(cdr args)))

(def-value-check (:warn (&rest args) (&rest chk))
  `(declare (ignorable chk))
  `(emit-warning :format ,(car args) :args ,(cdr args)))

(def-value-check (:eq (eq-form) (check-form))
  `(if (eq ,eq-form check-form)
     (make-check-result)
     (emit-failure :format "Not eq to ~s" :args '(,eq-form))))

(def-check-alias (:symbol name) `(:eq ',name))

(def-value-check (:eql (eql-form) (check-form))
  `(if (eql ,eql-form check-form)
     (make-check-result)
     (emit-failure :format "Not eql to ~s" :args '(,eql-form))))

(def-check-alias (:forms-eq)  `(:predicate eq))
(def-check-alias (:forms-eql) `(:predicate eql))

(def-value-check (:predicate (pred) (&rest forms))
  `(if (apply #',pred forms)
     (make-check-result)
     (emit-failure :format "Predicate ~s fails" :args '(,pred))))

(def-control-check (:err () expr-form)
  (let ((x (gensym "x")))
    `(block ,x
       (handler-bind ((error #'(lambda (,x)
				 (declare (ignorable ,x))
				 (return-from ,x (make-check-result)))))
	 ,expr-form)
       (emit-failure :format "~@<No expected error:~{~_ ~s~}~:>"
		     :args '(,(cond
			       ((and (listp expr-form)
				     (eq 'list (car expr-form)))
				(cdr expr-form))
			       (t (list expr-form))))))))

(def-control-check (:perf (&key (ms nil ms-supp-p)
				(sec nil sec-supp-p)
				(min nil min-supp-p)) expr-form)
  (let ((core-form expr-form))
    (cond
     ((or sec-supp-p min-supp-p ms-supp-p)
      (when (or (and sec-supp-p min-supp-p) (and ms-supp-p min-supp-p)
		(and ms-supp-p sec-supp-p))
	(error "Multiple time metrics given"))
      (when sec-supp-p (setf ms (* 1000 sec)))
      (when min-supp-p (setf ms (* 60000 min)))
      (setf core-form
	`(let* ((start-time (get-internal-real-time))
		(result ,core-form)
		(elapsed-ms
		 (* ,(/ 1000
			internal-time-units-per-second)
		    (- (get-internal-real-time)
		       start-time))))
	   (declare (ignorable result))
	   (if (> ,ms elapsed-ms)
	     (make-check-result)
	     (emit-failure
	      :format "Execution time ~dms exceeded allowed time ~dms"
	      :args '(elapsed-ms ,ms))))))
     (t
       (error
	":perf check requires performance criteria specification")))
    core-form))

(def-control-check (:not (subcriterion) expr-form-list)
  (let ((subcheck (gensym)))
    `(let ((,subcheck ,(continue-check subcriterion expr-form-list)))
       (cond
	((check-result-errors ,subcheck)
	 ,subcheck)
	((check-result-failures ,subcheck)
	 (make-check-result :info (check-result-info ,subcheck)))
	(t
	 (emit-failure :format "Expected failure from ~s"
		       :args '(,subcriterion)))))))

(def-control-check (:all (&rest args) expr-list-form)
  (let ((var (gensym "var")) (warnings (gensym "warnings"))
	(failures (gensym "failures")) (errors (gensym "errors"))
	(info (gensym "info")))
    (labels ((test-next (args) 
	       (cond
		((null args)
		 `(make-check-result :warnings ,warnings
				     :failures ,failures
				     :errors ,errors :info ,info))
		(t
		 (destructuring-bind (first-arg &rest other-args) args
		   (let ((subcheck (gensym "sub")))
		     `(let ((,subcheck
			     ,(continue-check first-arg var)))
			(setf ,warnings
			  (nconc ,warnings
				 (check-result-warnings ,subcheck))
			  ,failures
			  (nconc ,failures
				 (check-result-failures ,subcheck))
			  ,errors
			  (nconc ,errors
				 (check-result-errors ,subcheck))
			  ,info
			  (nconc ,errors
				 (check-result-info ,subcheck)))
			(unless (or ,failures ,errors)
			  ,(test-next other-args)))))))))
      `(let ((,var ,expr-list-form) ,warnings ,failures ,errors ,info)
	,(test-next args)))))

(def-control-check (:any (&rest criteria) expr-list-form)
  (let ((new-stack (gensym "expr-list-form"))
	(block (gensym "block"))
	(info (gensym "info")) (i (gensym "i")) (is (gensym "is"))
	(result (gensym "result"))
	(rf (gensym "rf")) (re (gensym "re")) (ri (gensym "ri")))
    `(let ((,new-stack ,expr-list-form)
	   (,info nil))
       (block ,block
	 ,@(loop for criterion in criteria collect
		 `(let* ((,result ,(continue-check criterion new-stack))
			 (,rf (check-result-failures ,result))
			 (,re (check-result-errors ,result))
			 (,ri (check-result-info ,result)))
		    (cond
		      ((check-result-errors ,result)
		       (setf ,info (nconc ,info ,re ,rf ,ri)))
		      (,rf
		       (setf ,info (append ,info ,rf ,ri)))
		      (t
		       (return-from ,block
			 (make-check-result
			  :info (nconc ,info
				       (check-result-info ,result))))))))
	 (emit-failure :format "No disjuncts succeeded:~{~_ ~s~}"
		       :args '(,criteria)
		       :info ,info)))))

(def-control-check (:apply (transform criterion) forms)
  (continue-check criterion
		  `(multiple-value-list (apply #',transform ,forms))))

(def-control-check (:progn (&rest forms-and-criterion) forms)
  (let ((progn-forms (butlast forms-and-criterion))
	(criterion (car (last forms-and-criterion))))
    `(progn
       ,@progn-forms
       ,(continue-check criterion forms))))

(def-control-check (:each (criterion) forms)
  (let ((block (gensym)) (list (gensym "list")) (var (gensym "var"))
	(result (gensym "result"))
	(warnings (gensym "w")) (info (gensym "info")))
    `(block ,block
       (let ((,info nil) (,warnings nil))
	 (destructuring-bind (,list) ,forms
	   (format t "> ~s~%" ,list)
	   (loop for ,var in ,list do
	     (format t ">> ~s~%" ,var)
	      (let ((,result ,(continue-check criterion (list var))))
		(cond
		  ((or (check-result-errors ,result)
		       (check-result-failures ,result))
		   (setf (check-result-info ,result)
			 (append ,info (check-result-info ,result))
			 (check-result-warnings ,result)
			 (append ,warnings
				 (check-result-warnings ,result)))
		   (return-from ,block ,result))
 		 (t
		  (setf ,info (append ,info (check-result-info ,result))
			,warnings
			(append ,warnings
				(check-result-warnings ,result))))))))
	 (make-check-result :info ,info :warnings ,warnings)))))
