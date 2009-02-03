;;; File fixture.lisp
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

#+allegro (excl::define-simple-parser def-fixtures second :nst-fixture-set)
(defmacro def-fixtures (name
			(&key uses assumes outer inner documentation)
			&body bindings)
  "(def-fixtures (FIXTURE-NAME :uses USES :assumes ASSUMES
		    :outer OUTER :inner INNER
		    :documentation DOCUMENTATION)
  (NAME FORM)
  (NAME FORM)
  ...
  (NAME FORM))

Associate names to values.  One or more fixtures may be applied to each test
group, test or another fixture.  None of the keyword options are manditory.

uses - fixtures whose names are assumed to be bound --- although not necessarily
by those same fixtures --- whenever this fixture is used.

assumes - names assumed to be bound at the point of any use of this fixture.

inner - list of declarations to be made inside the let-binding of names of any
use of this fixture.  Do not include the \"declare\" keyword here; NST adds
these declarations to others, including a special declaration of all bound
names.

documentation - a documentation string for the fixture set.

outer - list of declarations to be made outside the let-binding of names of any
use of this fixture.
"

  (declare (ignorable assumes outer inner))

  (let ((group-fixture-class-name (gensym "group-fixture-class-name"))
	(test-fixture-class-name (gensym "test-fixture-class-name"))
	(bound-names (loop for binding in bindings collect (car binding))))
  
    `(eval-when (:compile-toplevel :load-toplevel :execute)

       ;; This eval-when block contains method definitions which will
       ;; be called later in the compile of this same file, so they
       ;; must be evaluated as soon as possible.
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (defmethod bound-names ((f (eql ',name))) ',bound-names))
       
       ;; This eval-when block contains definition which will be used
       ;; only after this file is loaded.
       (eval-when (:load-toplevel :execute)
	 (let* ((,group-fixture-class-name (group-fixture-class-name ',name))
		(,test-fixture-class-name (test-fixture-class-name ',name)))
	   
	   ;; Record the class to be inherited by the classes of
	   ;; groups which apply this fixture.
	   (unless ,group-fixture-class-name
	     (setf ,group-fixture-class-name
	       (gentemp (concatenate 'string (symbol-name ',name) ".class.")
			:nst-fixture))
	     (defmethod group-fixture-class-name ((f (eql ',name)))
	       ,group-fixture-class-name))
	   
	   ;; Record the class to be inherited by the classes of tests
	   ;; which apply this fixture.
	   (unless ,test-fixture-class-name
	     (setf ,test-fixture-class-name
	       (gentemp (concatenate 'string (symbol-name ',name) ".class.")
			:nst-fixture))
	     (defmethod test-fixture-class-name ((f (eql ',name)))
	       ,test-fixture-class-name))
	   
	   ;; Create the group-inherited class, and apply the bindings
	   ;; to included methods.
	   (eval `(defclass ,,group-fixture-class-name () ()
		    ,@(when ,documentation
			`((:documentation ,,documentation)))
		    ))
	   (eval `(defmethod core-run :around ((group
						,,group-fixture-class-name))
		    (let ,',bindings
		      (declare (special ,@',bound-names))
		      (call-next-method))))

	   ;; Create the test-inherited class, and apply the bindings
	   ;; to included methods.
	   (eval `(defclass ,,test-fixture-class-name () ()
		    ,@(when ,documentation
			`((:documentation ,,documentation)))
		    ))
	   (eval `(defmethod core-run-test :around ((test
						     ,,test-fixture-class-name))
		    (let ,',bindings
		      (declare (special ,@',bound-names))
		      (call-next-method))))



	   ;; Function for expanding names into the current namespace.
	   (defmethod open-fixture ((f (eql ',name))
				    &optional (in-package *package*))
	     ,@(when documentation `(,documentation))
	     (declare (special ,@(loop for used-fixture in uses
				       append (bound-names used-fixture))
			       ,@assumes))
	     (unless (packagep in-package)
	       (setf in-package (find-package in-package)))
	     (setf ,@(loop for (var form) in bindings
			 append
			   (cond
			    (name `((symbol-value (intern (symbol-name ',var)
							  in-package))
				    ,form))
			    (t nil))))
	     ',name)
	   
	   ;; WARNING!  This line causes Allegro to crash
	   #-allegro (set-pprint-dispatch ',group-fixture-class-name
		       '#(lambda (stream object)
			  (format stream "Fixture set ~s" ',name)))

	   (defmethod trace-fixture ((f (eql ',name)))
	     (format t "Fixture ~s~% - Bindings:~%" f)
	     ,@(loop for (var form) in bindings
		   collect `(format t "   (~s ~s)~%" ',var ',form))
	     (format t " - Other fixtures: ~@<~{~s~^ ~_~}~:>~%" ',uses)
	     (format t " - Names expected: ~@<~{~s~^ ~_~}~:>~%" ',assumes)
	     (format t " - Outer bindings: ~@<~{~s~^ ~_~}~:>~%" ',outer)
	     (format t " - Inner bindings: ~@<~{~s~^ ~_~}~:>~%" ',inner)
	     (format t " - Documentation string: ~s~%" ,documentation)
	     (format t " - Internal class names:~%")
	     (format t "     For groups - ~s~%" ,group-fixture-class-name)
	     (format t "     For tests  - ~s~%" ,test-fixture-class-name)))
	 ',name))))

(defun process-fixture-list (fixture-list)
  (let ((group-fixture-class-names nil)
	(test-fixture-class-names nil)
	(anonymous-fixture-forms nil))
    (loop for f in fixture-list do
      (cond
       ;; A named fixture
       ((symbolp f)
	(let ((group-class-name (group-fixture-class-name f))
	      (test-class-name (test-fixture-class-name f)))
	  (unless (and group-class-name test-class-name)
	    (error "~f does not correspond to a defined fixture" f))
	  (push group-class-name group-fixture-class-names)
	  (push test-class-name  test-fixture-class-names)))
       ;; Miscellaneous garbage 1
       ((not (listp f))
	(error "Expected a fixture name or anonymous fixture; found ~s" f))
       ;; Anonymous fixture
       ((eq (car f) :fixture)
	(error "Have not yet re-implemented anonymous fixtures."))
       ;; Miscellaneous garbage 2
       (t
	(error "Expected a fixture name or anonymous fixture; found ~s" f))))
    (values (nreverse group-fixture-class-names)
	    (nreverse test-fixture-class-names)
	    (nreverse anonymous-fixture-forms))))

