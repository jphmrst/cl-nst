;;; File fixture.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2009 Smart Information Flow Technologies.
;;; Written by John Maraist.
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

  ;; Some arguments can be either a singleton or a list; correct the
  ;; latter into the former so that internally it's all uniform.
  (unless (listp uses) (setf uses (list uses)))
  (unless (listp assumes) (setf assumes (list assumes)))

  (let ((bound-names (loop for binding in bindings collect (car binding)))
        (g-param (gensym))
        (t-param (gensym)))

    `(progn
       #+allegro (excl:record-source-file ',name :type :nst-fixture-set)

       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defclass ,name ()
              ((bound-names :reader bound-names :allocation :class
                            :initform ',bound-names))
           (:metaclass singleton-class)
           ,@(when documentation `((:documentation ,documentation))))

         (finalize-inheritance (find-class ',name))
         #|(let ((proto (class-prototype (find-class ',name))))
             (setf (slot-value proto 'bound-names) ',bound-names))|#)

       (defmethod do-group-fixture-assignment :around ((,g-param ,name)
                                                       ,t-param)
         (declare (ignorable ,t-param)
                  (special ,@(loop for used-fixture in uses
                                 append (bound-names used-fixture))
                           ,@assumes))
         (let* ,bindings
           (declare (special ,@bound-names))
           (call-next-method)))

       (defmethod do-test-fixture-assignment :around ((,t-param ,name))
         (declare (special ,@(loop for used-fixture in uses
                                 append (bound-names used-fixture))
                           ,@assumes))
         (let* ,bindings
           (declare (special ,@bound-names))
           (call-next-method)))

       ;; Function for expanding names into the current namespace.
       (defmethod open-fixture ((f ,name) &optional (in-package *package*))
         ,@(when documentation `(,documentation))
         (declare (special ,@(loop for used-fixture in uses
                                   append (bound-names used-fixture))
                           ,@(loop for var-form in bindings
                                   collect (car var-form))
                           ,@assumes
                           *open-via-repl*))
         (unless (packagep in-package)
           (setf in-package (find-package in-package)))

         ,@(loop for (var form) in bindings
                 collect
                 `(setf ,(cond (var `(symbol-value ',var)) (t (gensym)))
                         ,form))

         (import ',(loop for var-form in bindings
                         if (car var-form) collect (car var-form))
                 in-package)

         ',name)

       (defmethod trace-fixture ((f ,name))
         (format t "Fixture ~s~% - Bindings:~%" ',name)
         ,@(loop for (var form) in bindings
               collect `(format t "   (~s ~s)~%" ',var ',form))
         (format t " - Other fixtures: ~@<~{~s~^ ~_~}~:>~%" ',uses)
         (format t " - Names expected: ~@<~{~s~^ ~_~}~:>~%" ',assumes)
         (format t " - Outer bindings: ~@<~{~s~^ ~_~}~:>~%" ',outer)
         (format t " - Inner bindings: ~@<~{~s~^ ~_~}~:>~%" ',inner)
         (format t " - Documentation string: ~s~%" ,documentation)

       ',name)
    )))

(defun process-fixture-list (fixture-list)
  "Trivial, for now, because anonymous fixtures are offline."
  (let ((fixture-names nil))
    (loop for f in fixture-list do
      (cond
       ;; A named fixture
       ((symbolp f)
        (setf fixture-names (nconc fixture-names (list f))))
       ;; Miscellaneous garbage 1
       ((not (listp f))
        (error "Expected a fixture name or anonymous fixture; found ~s" f))
       ;; Anonymous fixture
       ((eq (car f) :fixture)
        (error "Have not yet re-implemented anonymous fixtures."))
       ;; Miscellaneous garbage 2
       (t
        (error "Expected a fixture name or anonymous fixture; found ~s" f))))

    (values fixture-names nil)))
