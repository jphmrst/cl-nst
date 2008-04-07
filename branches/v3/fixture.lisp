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
  (declare (ignorable name uses assumes outer inner documentation bindings))
  
  (let ((fixture-class-name (gensym "fixture-class-name")))
    `(eval-when (:load-toplevel :execute)
       (let* ((,fixture-class-name (fixture-class-name ',name)))
	 (unless ,fixture-class-name
	   (setf ,fixture-class-name
		 (gentemp (concatenate 'string (symbol-name ',name) ".class.")
			  :nst-fixture))
	   (defmethod fixture-class-name ((f (eql ',name)))
	     ,fixture-class-name))
	 (eval `(defclass ,,fixture-class-name () ()))

	 ;; WARNING!  This line causes Allegro to crash
	 #-allegro (set-pprint-dispatch ',fixture-class-name
		     '#(lambda (stream object)
			(format stream "Fixture set ~s" ',name)))
	 
	 (eval `(defmethod run :around ((test ,,fixture-class-name))
		  (let ,',bindings (call-next-method))))
	 (defmethod open-fixture ((f (eql ',name))
				  &optional (in-package *package*))
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
	 (defmethod trace-fixture ((f (eql ',name)))
	   (format t "Fixture ~s~% - Bindings:~%" f)
	   ,@(loop for (var form) in bindings
		   collect `(format t "   (~s ~s)~%" ',var ',form))
	   (format t " - Other fixtures: ~@<~{~s~^ ~_~}~:>~%" ',uses)
	   (format t " - Names expected: ~@<~{~s~^ ~_~}~:>~%" ',assumes)
	   (format t " - Outer bindings: ~@<~{~s~^ ~_~}~:>~%" ',outer)
	   (format t " - Inner bindings: ~@<~{~s~^ ~_~}~:>~%" ',inner)
	   (format t " - Documentation string: ~s~%" ,documentation)
	   (format t " - Internal class name: ~s~%" ,fixture-class-name)))
       ',name)))

(defun process-fixture-list (fixture-list)
  (let ((fixture-class-names nil)
	(anonymous-fixture-forms nil))
    (loop for f in fixture-list do
	  (cond
	   ;; A named fixture
	   ((symbolp f)
	    (let ((class-name (fixture-class-name f)))
	      (unless class-name
		(error "~f does not correspond to a defined fixture" f))
	      (push class-name fixture-class-names)))
	   ;; Miscellaneous garbage 1
	   ((not (listp f))
	    (error "Expected a fixture name or anonymous fixture; found ~s" f))
	   ;; Anonymous fixture
	   ((eq (car f) :fixture)
	    (error "Have not yet re-implemented anonymous fixtures."))
	   ;; Miscellaneous garbage 2
	   (t
	    (error "Expected a fixture name or anonymous fixture; found ~s" f))))
    (values (nreverse fixture-class-names)
	    (nreverse anonymous-fixture-forms))))

