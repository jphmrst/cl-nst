;;; File group.lisp
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
        (fixtures-setup nil) (fixtures-setup-supp-p nil)
        (fixtures-cleanup nil) (fixtures-cleanup-supp-p nil)
        (each-setup nil) (each-setup-supp-p nil)
        (each-cleanup nil) (each-cleanup-supp-p nil)
        (docstring nil) (docstring-supp-p nil))
    (loop for form in forms do
      (case (car form)
        (:setup (setf setup (cdr form) setup-supp-p t))
        (:cleanup (setf cleanup (cdr form) cleanup-supp-p t))
        (:fixtures-setup (setf fixtures-setup (cdr form)
                               fixtures-setup-supp-p t))
        (:fixtures-cleanup (setf fixtures-cleanup (cdr form)
                                 fixtures-cleanup-supp-p t))
        (:each-setup (setf each-setup (cdr form) each-setup-supp-p t))
        (:each-cleanup (setf each-cleanup (cdr form) each-cleanup-supp-p t))
        (:documentation (setf docstring (cadr form) docstring-supp-p t))
        (otherwise (push form checks))))
    (values (nreverse checks)
            setup setup-supp-p cleanup cleanup-supp-p
            fixtures-setup fixtures-setup-supp-p
            fixtures-cleanup fixtures-cleanup-supp-p
            each-setup each-setup-supp-p each-cleanup each-cleanup-supp-p
            docstring docstring-supp-p)))

(defclass nst-group-record-meta (singleton-class)
     ((group-name-src :reader group-name-src :initarg :group-name-src))
  (:documentation "Metaclasses of the group record class."))

(defmethod validate-superclass ((class nst-group-record-meta)
                                (superclass standard-class))
  t)

(defclass nst-group-record ()
     ((%group-name :reader group-name :initarg :group-name))
  (:documentation "Superclass of NST group definitions."))

(defmethod test-names ((group nst-group-record))
  (loop for tt in (test-list group)
      collect (test-name-lookup (make-instance tt))))

(defmethod group-record-p ((obj nst-group-record)) t)

(defmethod trace-group ((g nst-group-record))
  (format t "Group ~s:~%" (group-name g))
  (format t " - Fixtures: ~@<~{~s~^ ~:_~}~:>~%" (group-given-fixtures g))
  (format t " - Defines tests: ~@<~{~s~^ ~:_~}~:>~%" (test-names g)))

(defmethod make-instance :around ((class nst-group-record-meta)
                                  &key &allow-other-keys)
  (let ((result (call-next-method)))
    (setf (slot-value result '%group-name)
          (group-name-src class))
    result))

#+allegro (excl::define-simple-parser def-test-group second :nst-group)
(defmacro def-test-group (group-name given-fixtures &body forms)
  "Define a group of tests associated with certain fixtures,
initialization and cleanup.

group-name - name of the test group being defined

given-fixtures - list of the names of fixtures and anonymous fixtures to be
used with the tests in this group.

forms - zero or more test forms, given by def-check."

  (handler-bind (#+sbcl (style-warning
                         #'(lambda (c)
                             (muffle-warning c))))

  ;; Establish a binding of the group name to a special variable for
  ;; use in the expansion of the test-defining forms.
  (let ((*the-group* group-name)
        ;; (package-finder (intern (symbol-name group-name)
        ;;                        (find-package :nst-name-use-in-packages)))
        )
    (declare (special *the-group*))

    ;; Separate the test-defining forms from the group and test setup
    ;; definitions.
    (multiple-value-bind (check-forms setup setup-supp-p
                                      cleanup cleanup-supp-p
                                      fixtures-setup fixtures-setup-supp-p
                                      fixtures-cleanup fixtures-cleanup-supp-p
                                      each-setup each-setup-supp-p
                                      each-cleanup each-cleanup-supp-p
                                      docstring docstring-supp-p)
        (separate-group-subforms forms)

      (let ((group-orig-pkg (symbol-package group-name))
             ;; Get the package where the public group name symbol
             ;; lives.
            )
        (multiple-value-bind (fixture-class-names anon-fixture-forms
                                                  fixture-names)
            (process-fixture-list given-fixtures)

          ;; Expand the test forms in this environment which include
          ;; a binding to *the-group*.
          (let ((expanded-check-forms
                 (let ((*group-class-name* group-name)
                       (*group-fixture-classes* fixture-class-names))
                   (declare (special *group-class-name*
                                     *group-fixture-classes*))
                   (mapcar #'macroexpand check-forms))))

            ;; As with the other NST forms, all execution is at load
            ;; time (or less usually, when typed into the REPL
            ;; manually).
            `(eval-when (:compile-toplevel :load-toplevel :execute)
               #+allegro
               (excl:record-source-file ',(if (listp group-name)
                                              (first group-name)
                                              group-name)
                                        :type :nst-test-group)
               (let ((*group-class-name* ',group-name)
                     (*group-fixture-classes* ',fixture-class-names))
                 (declare (special *group-class-name* *group-fixture-classes*))

                 (eval-when (:load-toplevel :execute)
                   (let* ((package-hash (gethash ,group-orig-pkg
                                                 +package-groups+)))
                     (unless package-hash
                       (setf package-hash (make-hash-table :test 'eq)
                             (gethash ,group-orig-pkg
                                      +package-groups+) package-hash))
                     (setf (gethash ',group-name package-hash) t)))

                 (defclass ,group-name (,@fixture-class-names nst-group-record)
                      ((anon-fixture-forms :allocation :class
                                           :reader anon-fixture-forms
                                           :initform ',anon-fixture-forms)
                       (test-list :allocation :class
                                  :accessor test-list
                                  :initform nil)
                       (test-name-lookup :allocation :class
                                         :reader test-name-lookup
                                         :initform (make-hash-table :test 'eq))
                       (%given-fixtures :allocation :class
                                        :reader group-given-fixtures
                                        :initform ',given-fixtures)
                       (%fixture-classes :allocation :class
                                         :reader group-fixture-class-names
                                         :initform ',fixture-class-names)
                       (%fixtures-setup-thunk
                        :allocation :class :reader group-fixtures-setup-thunk
                        :initform #'(lambda ()
                                      ,@(when fixtures-setup-supp-p
                                          fixtures-setup)))
                       (%fixtures-cleanup-thunk
                        :allocation :class :reader group-fixtures-cleanup-thunk
                        :initform #'(lambda ()
                                      ,@(when fixtures-cleanup-supp-p
                                          fixtures-cleanup)))
                       (%withfixtures-setup-thunk
                        :allocation :class
                        :reader group-withfixtures-setup-thunk
                        :initform #'(lambda ()
                                      (declare (special ,@fixture-names))
                                      ,@(when setup-supp-p
                                          setup)))
                       (%withfixtures-cleanup-thunk
                        :allocation :class
                        :reader group-withfixtures-cleanup-thunk
                        :initform #'(lambda ()
                                      (declare (special ,@fixture-names))
                                      ,@(when cleanup-supp-p
                                          cleanup)))
                       (%eachtest-setup-thunk
                        :allocation :class
                        :reader group-eachtest-setup-thunk
                        :initform #'(lambda ()
                                      (declare (special ,@fixture-names))
                                      ,@(when each-setup-supp-p
                                          each-setup)))
                       (%eachtest-cleanup-thunk
                        :allocation :class
                        :reader group-eachtest-cleanup-thunk
                        :initform #'(lambda ()
                                      (declare (special ,@fixture-names))
                                      ,@(when each-cleanup-supp-p
                                          each-cleanup))))
                   (:metaclass nst-group-record-meta)
                   (:group-name-src . ,group-name)
                       ,@(when docstring-supp-p `((:documentation ,docstring))))
                 #-sbcl
                 ,@(when docstring-supp-p
                     `((setf (documentation ',group-name :nst-group) ,docstring)
                       (setf (documentation ',group-name :nst-test-group)
                             ,docstring)))

                 (finalize-inheritance (find-class ',group-name))
                 (record-name-use :group
                                  ',group-name (make-instance ',group-name))

                 #|(let ((proto (class-prototype (find-class ',group-name))))
                     (setf (slot-value proto 'anon-fixture-forms)
                           ',anon-fixture-forms))|#

                 ;; Fixture processing.
                 ,@anon-fixture-forms

                 (set-pprint-dispatch ',group-name
                   #'(lambda (stream object)
                       (declare (ignorable object))
                       (format stream
                           ,(format nil "Group ~s" group-name))))

                 ;; Clear the list of tests when redefining the group.
                 (let ((actual (make-instance ',group-name)))
                   (setf (test-list actual) nil)
                   (clrhash (test-name-lookup actual)))

                 ,@expanded-check-forms

                 ;; Store the new artifact against the uses of its
                 ;; name in NST.
                 (note-executable ',group-name (make-instance ',group-name))

                 ',group-name)))))))))
