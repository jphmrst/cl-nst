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
                        (&key uses assumes outer inner documentation cache
                              export-names
                              (export-bound-names nil
                                                  export-bound-names-supp-p)
                              (export-fixture-name
                               nil export-fixture-name-supp-p))
                        &body bindings)
  "(def-fixtures (FIXTURE-NAME :uses USES :assumes ASSUMES
                    :outer OUTER :inner INNER
                    :documentation DOCUMENTATION
                    :export-names FLAG
                    :export-bound-names FLAG
                    :export-fixture-name FLAG)
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

export-fixture-name - When non-nil, the fixture name will be added to the
list of symbols exported by the current package.

export-bound-names - When non-nil, the names bound by this fixture will be
added to the list of symbols exported by the current package.

export-names - When non-nil, sets the default value to t for the two options
above.

cache - When non-nil, the fixture values are cached at their first use, and
re-applied at subsequent fixture application rather than being recalculated.
"
  (declare (ignorable assumes outer inner))

  ;; Some arguments can be either a singleton or a list; correct the
  ;; latter into the former so that internally it's all uniform.
  (unless (listp uses) (setf uses (list uses)))
  (unless (listp assumes) (setf assumes (list assumes)))

  (when export-names
    (unless export-bound-names-supp-p
      (setf export-bound-names t))
    (unless export-fixture-name-supp-p
      (setf export-fixture-name t)))

  (let ((bound-names (loop for binding in bindings collect (car binding)))
        (bindings-with-tracking
         (loop for (var-name form) in bindings
               collect
               (let ((block (gensym "block")))
                 `(,var-name
                   ,(let ((calc
                           `(block ,block
                              (setf *binding-variable* ',var-name)
                              (with-retry
                                  (,(format nil
                                        "Try binding ~s for fixture ~s again."
                                      var-name name))
                                (return-from ,block ,form)))))
                      (cond
                        (cache
                         `(let ((cache (cached-values (make-instance ',name))))
                            (multiple-value-bind (cached found)
                              (gethash ',var-name cache)
                            (cond
                              (found cached)
                              (t (let ((res ,calc))
                                   (setf (gethash ',var-name cache) res)
                                   res))))))
                        (t calc)))))))
        (g-param (gensym))
        (t-param (gensym)))

    `(progn
       #+allegro (excl:record-source-file ',name :type :nst-fixture-set)
       #+allegro (loop for name in ',bound-names do
         (excl:record-source-file name :type :nst-fixture))

       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defclass ,name ()
              ((bound-names :reader bound-names :allocation :class
                            :initform ',bound-names)
               ,@(when cache
                   `((cached-values :initform (make-hash-table :test 'eq)
                                    :accessor cached-values))))
           (:metaclass singleton-class)
           ,@(when documentation `((:documentation ,documentation))))

         (finalize-inheritance (find-class ',name)))

       (let ((this-name-use (gethash ',name +name-use+)))
         (unless this-name-use
           (setf this-name-use (make-name-use)
                 (gethash ',name +name-use+) this-name-use))
         (setf (name-use-fixture this-name-use) (make-instance ',name)))

       (defmethod do-group-fixture-assignment :around ((,g-param ,name)
                                                       ,t-param)
         (declare (ignorable ,t-param)
                  (special ,@(loop for used-fixture in uses
                                 append (bound-names used-fixture))
                           ,@assumes))
         (let* ,bindings-with-tracking
           (declare (special ,@bound-names))
           (setf *binding-variable* nil)
           (call-next-method)))

       (defmethod do-test-fixture-assignment :around ((,t-param ,name))
         (declare (special ,@(loop for used-fixture in uses
                                 append (bound-names used-fixture))
                           ,@assumes))
         (let* ,bindings-with-tracking
           (declare (special ,@bound-names))
           (setf *binding-variable* nil)
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
                 append
                 `((format-at-verbosity 3
                       ,(format nil " - Calculating ~a~~%" var))
                   (setf ,(cond (var `(symbol-value ',var)) (t (gensym)))
                         ,form)))

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

       ,@(when (or export-bound-names export-fixture-name)
           `((eval-when (:compile-toplevel :load-toplevel :execute)
               ,@(loop for bnd in bindings
                     collect
                       (let ((id (car bnd)))
                         `(export ',id
                                  ,(intern (package-name (symbol-package id))
                                           (find-package :keyword)))))
               ,@(when export-fixture-name
                   `((export ',name
                             ,(intern (package-name (symbol-package name))
                                      (find-package :keyword))))))))

       ',name)))

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
