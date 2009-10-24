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
        (each-setup nil) (each-setup-supp-p nil)
        (each-cleanup nil) (each-cleanup-supp-p nil))
    (loop for form in forms do
      (case (car form)
        (:setup (setf setup (cdr form) setup-supp-p t))
        (:cleanup (setf cleanup (cdr form) cleanup-supp-p t))
        (:each-setup (setf each-setup (cdr form) each-setup-supp-p t))
        (:each-cleanup (setf each-cleanup (cdr form) each-cleanup-supp-p t))
        (otherwise (push form checks))))
    (values (nreverse checks)
            setup setup-supp-p cleanup cleanup-supp-p
            each-setup each-setup-supp-p each-cleanup each-cleanup-supp-p)))

#+allegro (excl::define-simple-parser def-test-group second :nst-group)
(defmacro def-test-group (group-name given-fixtures &body forms)
  "Define a group of tests associated with certain fixtures,
initialization and cleanup.

group-name - name of the test group being defined

given-fixtures - list of the names of fixtures and anonymous fixtures to be
used with the tests in this group.

forms - zero or more test forms, given by def-check."

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

      (let* ((group-orig-pkg (symbol-package group-name))
             ;; Get the package where the public group name symbol
             ;; lives.

             ;; Separate access to the names of the tests.
             (test-names (loop for form in check-forms
                             append (pull-test-name-list form))))

        (multiple-value-bind (fixture-class-names anon-fixture-forms)
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

                 (defclass ,group-name (,@fixture-class-names)
                      ((anon-fixture-forms :allocation :class
                                           :reader anon-fixture-forms
                                           :initform ',anon-fixture-forms)
                       (test-list :allocation :class
                                  :accessor test-list
                                  :initform nil)
                       (test-name-lookup :allocation :class
                                         :reader test-name-lookup
                                         :initform (make-hash-table :test 'eq)))
                   (:metaclass singleton-class))

                 (finalize-inheritance (find-class ',group-name))
                 (eval-when (:load-toplevel :execute)
                   (let ((this-name-use (gethash ',group-name +name-use+)))
                     (unless this-name-use
                       (setf this-name-use (make-name-use)
                             (gethash ',group-name +name-use+) this-name-use))
                     (setf (name-use-group this-name-use)
                           (make-instance ',group-name))))

                 #|(let ((proto (class-prototype (find-class ',group-name))))
                     (setf (slot-value proto 'anon-fixture-forms)
                           ',anon-fixture-forms))|#

                 ;; Retrieve a group name from its instance.  This is
                 ;; increasingly trivial and should probably be
                 ;; dropped.
                 (defmethod group-name ((g ,group-name)) ',group-name)

                 (defmethod group-fixture-class-names ((g ,group-name))
                   ',fixture-class-names)

                 ;; Fixture processing.
                 ,@anon-fixture-forms

                 ,@(when setup-supp-p
                     `((defmethod do-group-postfixture-setup
                           progn ((obj ,group-name))
                         ,@setup)))

                 ,@(when cleanup-supp-p
                     `((defmethod do-group-withfixture-cleanup
                           progn ((obj ,group-name))
                         ,@cleanup)))

                 (when ,each-setup-supp-p
                   (defmethod do-group-each-test-setup progn ((obj ,group-name))
                     ,@each-setup))

                 (when ,each-cleanup-supp-p
                   (defmethod do-group-each-test-cleanup
                       progn ((obj ,group-name))
                     ,@each-cleanup))

                 (defmethod test-names ((group ,group-name))
                   (loop for tt in (test-list group)
                         collect (test-name-lookup (make-instance tt))))

                 ;; Pass the group record predicate.
                 (defmethod group-record-p ((obj ,group-name)) t)

                 (set-pprint-dispatch ',group-name
                   #'(lambda (stream object)
                       (declare (ignorable object))
                       (format stream
                           ,(format nil "Group ~s" group-name))))

                 (defmethod trace-group ((g ,group-name))
                   (format t "Group ~s:~%" ',group-name)
                   (format t " - Fixtures: ~@<~{~s~^ ~:_~}~:>~%"
                     ',given-fixtures)
                   (format t " - Defines tests: ~@<~{~s~^ ~:_~}~:>~%"
                     ',test-names))

                 ;; Clear the list of tests when redefining the group.
                 (let ((actual (make-instance ',group-name)))
                   (setf (test-list actual) nil)
                   (clrhash (test-name-lookup actual)))

                 ,@expanded-check-forms

                 ;; Store the new artifact against the uses of its
                 ;; name in NST.
                 (note-executable ',group-name (make-instance ',group-name))

                 ',group-name))))))))
