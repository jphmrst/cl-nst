;;; File group.lisp
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


;;;
;;; Helper functions for the macros.
;;;

(defun pull-test-name-list (form)
  (unless (listp form) (return-from pull-test-name-list nil))
  (case (car form)
    ((def-check def-test) (list (symbol-or-car (cadr form))))
    (otherwise nil)))

(defun separate-group-subforms (forms)
  (let ((checks nil)
	(setup nil) (setup-supp-p nil)
	(cleanup nil) (cleanup-supp-p nil)
	(each-setup nil) (each-setup-supp-p nil)
	(each-cleanup nil) (each-cleanup-supp-p nil))
    (loop for form in forms do
      (case (car form)
	(:setup (setf setup (cadr form) setup-supp-p t))
	(:cleanup (setf cleanup (cadr form) cleanup-supp-p t))
	(:each-setup (setf each-setup (cadr form) each-setup-supp-p t))
	(:each-cleanup (setf each-cleanup (cadr form) each-cleanup-supp-p t))
	(otherwise (push form checks))))
    (values (nreverse checks)
	    setup setup-supp-p cleanup cleanup-supp-p
	    each-setup each-setup-supp-p each-cleanup each-cleanup-supp-p)))

;;; This page is intentionally left blank.

#+allegro (excl::define-simple-parser def-test-group second :nst-group)
(defmacro def-test-group (group-name given-fixtures &body forms)
  "Define a group of tests associated with certain fixtures,
initialization and cleanup.

group-name - name of the test group being defined

given-fixtures - list of the names of fixtures and anonymous fixtures to be used
with the tests in this group.

forms - zero or more test forms, given by def-check or def-test."

  ;; Establish a binding of the group name to a special variable for
  ;; use in the expansion of the test-defining forms.
  (let ((*the-group* group-name))
    (declare (special *the-group*))
    
    ;; Separate the test-defining forms from the group and test setup
    ;; definitions.
    (multiple-value-bind (check-forms setup setup-supp-p
				      cleanup cleanup-supp-p
				      each-setup each-setup-supp-p
				      each-cleanup each-cleanup-supp-p)
	(separate-group-subforms forms)
      (let (;; Variable names
	    (group-fixture-classes (gensym "group-fixture-classes"))
	    (anon-fixture-forms (gensym "anon-fixture-forms"))
	    (group-pkg (gensym "class-pkg"))
	    (group-class-name (gensym "group-class-name"))
	    (test-in-group-class-name (gensym "test-in-group-class-name"))
	    (standalone-in-group-class-name (gensym "standalone-class-name"))
	    
	    ;; Get the package where the public group name symbol
	    ;; lives.
	    (group-orig-pkg (symbol-package group-name))
	    
	    ;; Separate access to the names of the tests.
	    (test-names (loop for form in check-forms
			      append (pull-test-name-list form)))
	    
	    ;; Expand the test forms in this environment which include
	    ;; a binding to *the-group*.
	    (expanded-check-forms (mapcar #'macroexpand check-forms)))
	
	;; As with the other NST forms, all execution is at load time
	;; (or less usually, when typed into the REPL manually).
	`(eval-when (:load-toplevel :execute)

	   ;; Fixture processing.
	   (multiple-value-bind (,group-fixture-classes z ,anon-fixture-forms)
	       (process-fixture-list ',given-fixtures)
	     (declare (ignorable z))
	     (loop for form in ,anon-fixture-forms do (eval form))
	     (eval `(defmethod group-fixture-classes ((g (eql ',',*the-group*)))
		      ',,group-fixture-classes))

	     (let ((,group-pkg (groups-package ,group-orig-pkg))
		   (,group-class-name (group-class-name ',group-name))
		   (,test-in-group-class-name 
		    (test-in-group-class-name ',group-name))
		   (,standalone-in-group-class-name
		    (standalone-test-in-group-class-name ',group-name)))
	       (unless ,group-pkg
		 (setf ,group-pkg
		   (make-package (symbol-name
				  (gentemp ,(concatenate 'string
					      (package-name group-orig-pkg)
					      ".testgroups.")))
				 :use nil))
		 (defmethod groups-package ((pkg (eql ,group-orig-pkg)))
		   ,group-pkg))
	       (intern (symbol-name ',group-name) ,group-pkg)
	 
	       (unless ,group-class-name
		 (setf ,group-class-name
		   (gentemp ,(concatenate 'string
			       (symbol-name group-name) ".class.")
			    :sift.nst.group-class-names))
		 (defmethod group-class-name ((g (eql ',group-name)))
		   ,group-class-name))

	       (unless ,test-in-group-class-name
		 (setf ,test-in-group-class-name
		   (gentemp ,(concatenate 'string
			       (symbol-name group-name) ".tests-with-group.")
			    :sift.nst.test-within-group-class-names))
		 (defmethod test-in-group-class-name ((g (eql ',group-name)))
		   ,test-in-group-class-name))
	 
	       (unless ,standalone-in-group-class-name
		 (setf ,standalone-in-group-class-name
		   (gentemp ,(concatenate 'string
			       (symbol-name group-name) ".standalone-tests.")
			    :sift.nst.test-standalone-class-names))
		 (defmethod standalone-test-in-group-class-name
		     ((g (eql ',group-name)))
		   ,standalone-in-group-class-name))

	       (eval `(defclass ,,group-class-name (group-base-class
						    ,@,group-fixture-classes) ()))

	       ;; WARNING!  This hook crashes Allegro Lisp.
	       #-allegro (set-pprint-dispatch ',group-class-name
			   '#(lambda (stream object)
			      (format stream "Group ~s internal NST class"
				',group-name)))
	       
	       ;; Retrieve a group name from its instance
	       (eval `(defmethod group-name ((g ,,group-class-name))
			',',group-name))

	       (eval `(defclass ,,test-in-group-class-name () ()))
	       (eval `(defclass ,,standalone-in-group-class-name
			   ;; (,,group-class-name ,,test-in-group-class-name)
			   () ()))

	       (eval `(defmethod core-run ((obj ,,standalone-in-group-class-name))
			(core-run-test obj)))
		 
	       (when ,setup-supp-p
		 (eval `(defmethod core-run :before ((obj ,,group-class-name))
			  ,',setup)))
	       
	       (when ,cleanup-supp-p
		 (eval `(defmethod core-run :after ((obj ,,group-class-name))
			  ,',cleanup)))

	       (when ,each-setup-supp-p
		 (eval `(defmethod core-run-test
			    :before ((obj ,,test-in-group-class-name))
			  ,',each-setup)))
	       
	       (when ,each-cleanup-supp-p
		 (eval `(defmethod core-run-test
			    :after ((obj ,,test-in-group-class-name))
			  ,',each-cleanup)))

	       (eval `(defmethod test-names ((group ,,group-class-name))
			',',test-names))
	       (eval `(defmethod test-names ((group (eql ',',group-name)))
			',',test-names))

	       (defmethod trace-group ((g (eql ',group-name)))
		 (format t "Group ~s:~%" ',group-name)
		 (format t " - Fixtures: ~@<~{~s~^ ~:_~}~:>~%" ',given-fixtures)
		 (format t " - Defines tests: ~@<~{~s~^ ~:_~}~:>~%"
		   ',test-names)
		 
		 (let* ((group-class-actual (group-class-name g))
			(standalone-class-actual
			 (standalone-test-in-group-class-name g))
			(suite-class-actual (test-in-group-class-name g))
			(class-object (find-class group-class-actual)))
		   (format t
		       " - ~@<Group class name: ~s~
                         ~:_        expected: ~s~
                        ~:@_superclasses: ~@<~{~s~^ ~:_~}~:>~
                        ~:@_    expected: ~@<~s ~:_~{~s~^ ~:_~}~:>~:>~%"
		     group-class-actual ,group-class-name
		     (loop for sup in (class-direct-superclasses class-object)
			 collect (class-name sup))
		     'group-base-class ,group-fixture-classes)
		   (format t
		       " - ~@<Test in suite class name: ~s~
                        ~:@_                expected: ~s~:>~%"
		     suite-class-actual ,test-in-group-class-name)
		   (format t
		       " - ~@<Standalone test class name: ~s~
                        ~:@_                  expected: ~s~
                      ~:@_extends ~@<~s ~:_~s~:>~:>~%"
		     standalone-class-actual ,standalone-in-group-class-name
		     ,test-in-group-class-name ,group-class-name)
		   )))
	     ,@expanded-check-forms
	     ',group-name))))))

