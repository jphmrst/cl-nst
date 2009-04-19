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

      (let* ((base-rename (concatenate 'string
                            (package-name (symbol-package group-name))
                            "///" (symbol-name group-name)))
                                        ; The base name that we'll use
                                        ; for parallel names in our
                                        ; internal packages.

             ;; Old variable names
             (group-class-name (intern base-rename :group-class-name-package))
             (test-in-group-class-name
              (intern base-rename :test-in-group-class-name-package))
             (standalone-test-in-group-class-name
              (intern base-rename :standalone-test-in-group-class-name-package))

             ;; Get the package where the public group name symbol
             ;; lives.
             (group-orig-pkg (symbol-package group-name))

             ;; Whether we have a map into a new groups-package.
             (group-pkg-name (concatenate 'string
                               "nst-group-pkg///" base-rename))

             ;; Separate access to the names of the tests.
             (test-names (loop for form in check-forms
                             append (pull-test-name-list form))))

        (multiple-value-bind (group-fixture-classes test-fixture-classes
                                                    anon-fixture-forms)
            (process-fixture-list given-fixtures)

          ;; Expand the test forms in this environment which include
          ;; a binding to *the-group*.
          (let ((expanded-check-forms
                 (let ((*group-fixture-classes* group-fixture-classes)
                       (*group-class-name* group-class-name)
                       (*test-in-group-class* test-in-group-class-name)
                       (*standalone-test-in-group-class*
                        standalone-test-in-group-class-name))
                   (declare (special *group-fixture-classes*
                                     *group-class-name*
                                     *test-in-group-class*
                                     *standalone-test-in-group-class*))
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
               (let ((*the-group* ',group-name))
                 (declare (special *the-group*))

                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (unless (find-package ,group-pkg-name)
                     (make-package ,group-pkg-name)))

                 (eval-when (:load-toplevel :execute)
                   (let* ((package-hash (gethash ,group-orig-pkg
                                                 +package-groups+)))
                     (unless package-hash
                       (setf package-hash (make-hash-table :test 'eq)
                             (gethash ,group-orig-pkg
                                      +package-groups+) package-hash))
                     (setf (gethash ',group-name package-hash) t)))

                 (defclass ,group-name ()
                   ((group-fixture-classes :allocation :class
                                           :reader group-fixture-classes)
                    (test-fixture-classes :allocation :class
                                          :reader test-fixture-classes)
                    (anon-fixture-forms :allocation :class
                                        :reader anon-fixture-forms)
                    (group-class-name :allocation :class
                                      :reader group-class-name)
                    (test-in-group-class-name :allocation :class
                                              :reader test-in-group-class-name)
                    (standalone-test-in-group-class-name
                     :allocation :class
                     :reader standalone-test-in-group-class-name)

                    (suite-test-classes :allocation :class
                                        :initform (make-hash-table :test 'eq)
                                        :reader suite-test-classes)
                    (standalone-test-classes :allocation :class
                                             :initform (make-hash-table
                                                        :test 'eq)
                                             :reader standalone-test-classes)
                    (config-test-classes :allocation :class
                                         :initform (make-hash-table :test 'eq)
                                         :reader config-test-classes)))

                 (finalize-inheritance (find-class ',group-name))
                 (let ((proto (class-prototype (find-class ',group-name))))
                   (setf (slot-value proto 'group-fixture-classes)
                         ',group-fixture-classes
                         (slot-value proto 'test-fixture-classes)
                         ',test-fixture-classes
                         (slot-value proto 'anon-fixture-forms)
                         ',anon-fixture-forms
                         (slot-value proto 'group-class-name)
                         ',group-class-name
                         (slot-value proto 'test-in-group-class-name)
                         ',test-in-group-class-name
                         (slot-value proto 'standalone-test-in-group-class-name)
                         ',standalone-test-in-group-class-name))

                 ;; Record the group name in the package used for
                 ;; recording them.
                 (intern (symbol-name ',group-name)
                         (find-package ,group-pkg-name))

                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (defclass ,group-class-name
                        (group-base-class ,@group-fixture-classes) ())

                   (defclass ,test-in-group-class-name () ())
                   (defclass ,standalone-test-in-group-class-name () ()))

                 ;; Retrieve a group name from its instance
                 (defmethod group-name ((g ,group-class-name))
                   ',group-name)

                 ;; Fixture processing.
                 ,@anon-fixture-forms
                 ;; (loop for form in ',anon-fixture-forms do (eval form))

                 (defmethod core-run
                     ((obj ,standalone-test-in-group-class-name))
                   (core-run-test obj))

                 (when ,setup-supp-p
                   (defmethod core-run :before ((obj ,group-class-name))
                     ,@setup))

                 (when ,cleanup-supp-p
                   (defmethod core-run :after ((obj ,group-class-name))
                     ,@cleanup))

                 (when ,each-setup-supp-p
                   (defmethod core-run-test
                       :before ((obj ,test-in-group-class-name))
                     ,@each-setup))

                 (when ,each-cleanup-supp-p
                   (defmethod core-run-test
                       :after ((obj ,test-in-group-class-name))
                     ,@each-cleanup))

                 (defmethod test-names ((group ,group-class-name)) ',test-names)
                 (defmethod test-names ((group ,group-name)) ',test-names)

                 ;; WARNING!  This hook crashes Allegro Lisp.
                 #-allegro (set-pprint-dispatch ',group-class-name
                             #'(lambda (stream object)
                                 (declare (ignorable object))
                                 (format stream "Group ~s internal NST class"
                                   ',group-name)))

                 (defmethod trace-group ((g ,group-name))
                   (format t "Group ~s:~%" ',group-name)
                   (format t " - Fixtures: ~@<~{~s~^ ~:_~}~:>~%"
                     ',given-fixtures)
                   (format t " - Defines tests: ~@<~{~s~^ ~:_~}~:>~%"
                     ',test-names)

                   (let* ((group-class-actual (group-class-name g))
                          (standalone-class-actual
                           (standalone-test-in-group-class-name g))
                          (suite-class-actual (test-in-group-class-name g))
                          (class-object (find-class group-class-actual)))
                     (format t
                         " - ~@<Group cl~@<ass name: ~s~
                                      ~:[~*~;~:@_expected: ~s~]~:>~
                        ~:@_supe~@<rclasses: ~@<~{~s~^ ~:_~}~:>~
                               ~:[~2*~;~:@_expected: ~
                                         ~@<~s ~:_~{~s~^ ~:_~}~:>~]~:>~:>~%"
                       group-class-actual
                       *nst-info-shows-expected* ',group-class-name
                       (loop for sup in (class-direct-superclasses class-object)
                           collect (class-name sup))
                       *nst-info-shows-expected*
                       'group-base-class ',group-fixture-classes)
                     (format t
                         " - ~@<Test in suite cl~@<ass name: ~s~
                                ~:[~*~;~:@_expected: ~s~]~:>~:>~%"
                       suite-class-actual
                       *nst-info-shows-expected* ',test-in-group-class-name)
                     (format t
                         " - ~@<Standalone test cl~@<ass name: ~s~
                                   ~:[~*~;~:@_expected: ~s~]~:>~
                      ~:@_extends ~@<~s ~:_~s~:>~:>~%"
                       standalone-class-actual
                       *nst-info-shows-expected*
                       ',standalone-test-in-group-class-name
                       ',test-in-group-class-name ',group-class-name)))

                 (eval-when (:load-toplevel :execute)
                   ,@expanded-check-forms)

                 ',group-name))))))))



