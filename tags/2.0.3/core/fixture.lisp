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

(defclass standard-fixture () ()
  (:documentation "Common superclass of all fixtures."))

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

  (let ((g-param (gensym)) (t-param (gensym)) (cache-default cache))

    (macrolet ((binding-options-check (binding with-opts without-opts)
                 `(cond
                   ((symbolp (car ,binding)) (funcall #',without-opts ,binding))
                   ((listp (car ,binding)) (funcall #',with-opts ,binding))
                   (t (error "Ill-formed fixture binding ~s" ,binding))))
               (decode-option (options keyword default)
                 `(destructuring-bind (&key (,keyword ,default) &allow-other-keys)
                      ,options
                    ,keyword)))

      ;; Iterate through the bindings, building various lists.
      (loop for binding in bindings

          ;; Pull out the components of this binding.
          for var-name = (let ((given (binding-options-check binding
                                                             second first)))
                           (cond ((null given) (gensym))
                                 (t            given)))

          for form = (binding-options-check binding third second)
          for options = (binding-options-check binding first
                                               (lambda (x)
                                                 (declare (ignore x)) nil))

            ;; Decode options for this binding.
          for cache-this = (decode-option options cache cache-default)

            ;; Full tuples
          collect (list var-name form options) into full-tuples

            ;; First thing to collect is just the bound names
            ;; themselves.  We'll use these in e.g. declarations.
          collect var-name into bound-names

            ;; Next thing is the tuples we can stick into a let-block
            ;; or setf to calculate the actual binding value.
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
                    (cache-this
                     `(let ((the-cache (cached-values (make-instance ',name))))
                        (multiple-value-bind (cached found)
                            (gethash ',var-name the-cache)
                          (cond
                           (found cached)
                           (t (let ((res ,calc))
                                (setf (gethash ',var-name the-cache) res)
                                res))))))
                    (t calc)))))
          into bindings-with-tracking

            ;; Finally check whether any of the individual bindings .
          collect cache-this into cache-any

          finally
            (setf cache-any (block make-cache-any
                              (loop for b in cache-any do
                                (when b
                                  (return-from make-cache-any t)))
                              nil))
            (return-from def-fixtures
              `(progn
                 #+allegro
                 (excl:record-source-file ',name :type :nst-fixture-set)
                 #+allegro
                 (loop for name in ',bound-names do
                   (excl:record-source-file name :type :nst-fixture))

                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (defclass ,name (standard-fixture)
                     ((bound-names :reader bound-names :allocation :class)
                      ,@(when cache-any
                          `((cached-values :initform (make-hash-table :test 'eq)
                                           :accessor cached-values))))
                     (:metaclass singleton-class)
                     ,@(when documentation `((:documentation ,documentation))))

                   (finalize-inheritance (find-class ',name))
                   (setf (slot-value (make-instance ',name) 'bound-names)
                         ',bound-names)

                   ,@(when cache-any
                       `((clrhash (cached-values (make-instance ',name))))))

                 (record-name-use :fixture ',name (make-instance ',name))

                 ,(when cache-any
                    `(defmethod flush-fixture-cache ((f ,name))
                       (clrhash (cached-values f))))

                 (defmethod do-group-fixture-assignment
                     :around ((,g-param ,name) ,t-param)
                   (declare (ignorable ,t-param)
                            (special ,@(loop for used-fixture in uses
                                           append (bound-names used-fixture))
                                     ,@assumes))
                   (let* ,bindings-with-tracking
                     (declare (special ,@bound-names))
                     (setf *binding-variable* nil)
                     (call-next-method)))

                 (defmethod get-fixture-bindings ((f ,name))
                   ',bindings)

                 (defmethod do-test-fixture-assignment
                     :around ((,t-param ,name))
                   (declare (special ,@(loop for used-fixture in uses
                                           append (bound-names used-fixture))
                                     ,@assumes))
                   (let* ,bindings-with-tracking
                     (declare (special ,@bound-names))
                     (setf *binding-variable* nil)
                     (call-next-method)))

                 ;; Function for expanding names into the current namespace.
                 (defmethod open-fixture ((f ,name)
                                          &optional (in-package *package*))
                   ,@(when documentation `(,documentation))
                   (declare (special ,@(loop for used-fixture in uses
                                           append (bound-names used-fixture))
                                     ,@bound-names
                                     ,@assumes
                                     *open-via-repl*))
                   (unless (packagep in-package)
                     (setf in-package (find-package in-package)))

                   ,@(loop for (var form options) in full-tuples
                         append
                           `((format-at-verbosity 3
                                 ,(format nil " - Calculating ~a ~a~~%"
                                    var options))
                             (setf ,(cond
                                     (var `(symbol-value ',var))
                                     (t (gensym)))
                               ,form)))

                   (import ',bound-names in-package)

                   ',name)

                 (defmethod trace-fixture ((f ,name))
                   (format t "Fixture ~s~% - Bindings:~%" ',name)
                   ,@(loop for (var form options) in full-tuples
                           collect
                           (cond
                             (options
                              `(format t "   (~s ~s ~s)~%"
                                 ',options ',var ',form))
                             (t `(format t "   (~s ~s)~%" ',var ',form))))
                   (format t " - Other fixtures: ~@<~{~s~^ ~_~}~:>~%" ',uses)
                   (format t " - Names expected: ~@<~{~s~^ ~_~}~:>~%" ',assumes)
                   (format t " - Outer bindings: ~@<~{~s~^ ~_~}~:>~%" ',outer)
                   (format t " - Inner bindings: ~@<~{~s~^ ~_~}~:>~%" ',inner)
                   (format t " - Documentation string: ~s~%" ,documentation)

                   ',name)

                 ,@(when (or export-bound-names export-fixture-name)
                     `((eval-when (:compile-toplevel :load-toplevel :execute)
                         ,@(loop for tuple in full-tuples
                                 collect
                                 (let ((id tuple))
                                   `(export ',id
                                            ,(intern (package-name (symbol-package id))
                                                     (find-package :keyword)))))
                         ,@(when export-fixture-name
                             `((export ',name
                                       ,(intern (package-name (symbol-package name))
                                                (find-package :keyword))))))))

                 ',name))))))

(defgeneric get-fixture-bindings (fixture)
  (:documentation "Internal function: pull the symbolic fixture bindings")
  (:method ((f symbol)) (get-fixture-bindings (make-instance f))))

(defgeneric flush-fixture-cache (f)
  (:method ((f symbol)) (flush-fixture-cache (make-instance f)))
  (:method ((f standard-fixture)) nil))

(defun process-fixture-list (fixture-set-list)
  "Trivial, for now, because anonymous fixtures are offline."
  (loop for f in fixture-set-list
      for this-fixture-set-name
        = (cond
           ((symbolp f) f)              ; A named fixture
           ((not (listp f))             ; Miscellaneous garbage 1
            (error "Expected a fixture name or anonymous fixture; found ~s"
                   f))
           ((eq (car f) :fixture)       ; Anonymous fixture
            (error "Have not yet re-implemented anonymous fixtures."))
           (t                           ; Miscellaneous garbage 2
            (error "Expected a fixture name or anonymous fixture; found ~s"
                   f)))
      for this-fixture-names
        = (cond
           ((symbolp f) (bound-names f))           ; A named fixture
           ((and (listp f) (eq (car f) :fixture))  ; Anonymous fixture
            (error "Have not yet re-implemented anonymous fixtures.")))
      collect this-fixture-set-name into fixture-set-names
      append this-fixture-names into fixture-names
      finally (return-from process-fixture-list
                (values fixture-set-names nil fixture-names))))

(defmacro with-fixtures ((&rest fixtures) &body forms)
  "Evaluate forms in an environment which provides fixture bindings."
  `(let ,(loop for fixture in fixtures
               append (get-fixture-bindings fixture))
     ,@forms))
