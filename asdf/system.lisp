;;; File system.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2009-2010 Smart Information Flow Technologies.
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
(in-package :sift.asdf-nst)


(defclass nst-test-holder (nst-test-runner)
     ((nst-packages :initarg :nst-packages
                    :reader nst-packages
                    :initform nil
                    :documentation
                    "Package whose def-test-groups are to be run.")
      (nst-package :initarg :nst-package
                   :reader nst-package
                   :initform nil
                   :documentation
                   "Packages whose def-test-groups are to be run.")

      (nst-group :initarg :nst-group
                 :reader nst-group
                 :initform nil
                 :documentation
                 "An NST test group, given as a dotted pair of a package
                  name plus the name of a test group in that package.")
      (nst-groups :initarg :nst-groups
                  :reader nst-groups
                  :initform nil
                  :documentation
                  "A list of NST test groups, each given as a dotted pair
                   of a package name plus the name of a test group in that
                   package.")

      (nst-test :initarg :nst-test
                :reader nst-test
                :initform nil
                :documentation
                "A single NST test, given as a three-element list of a
                 package name, the test's group name, and the test name.")
      (nst-tests :initarg :nst-tests
                 :reader nst-tests
                 :initform nil
                 :documentation
                 "A list of NST tests, each given as a three-element list
                  of a package name, the test's group name, and the test
                  name."))

  (:documentation "Class of ASDF systems that use NST for their test-op."))

(defmethod all-nst-testers ((sys nst-test-holder))
  (multiple-value-bind (packages groups tests) (call-next-method)
    (let ((local-packages (if (nst-package sys)
                              (list (nst-package sys))
                              (nst-packages sys)))
           (local-groups (if (nst-group sys)
                             (list (nst-group sys))
                             (nst-groups sys)))
           (local-tests (if (nst-test sys)
                            (list (nst-test sys))
                            (nst-tests sys))))
      (values (append packages local-packages)
              (append groups local-groups)
              (append tests local-tests)))))

(defun report-system (system &optional (stream nil stream-supp-t))
  (multiple-value-bind (packages groups tests) (all-nst-testers system)
    (apply (symbol-function (intern (symbol-name '#:report-multiple)
                                    (find-package '#:nst-control-api)))
           packages groups tests
           (when stream-supp-t `(:stream ,stream)))))

(defclass nst-testable (nst-test-holder) ()
  (:documentation "The once-and-future actual class."))

(defmethod asdf::component-depends-on :around ((op compile-op)
                                               (sys nst-test-holder))
  (list (list 'asdf:load-op :nst)))

(defmethod asdf::component-depends-on :around ((op load-op)
                                               (sys nst-test-holder))
  "When loading this system, also load any systems which NST-testing this
system also tests.  This is not strictly necessary to any test-op, but is
convenient and harmless."
  `((asdf:load-op :nst)
    ,@(call-next-method)
    ,@(loop for sub in (nst-systems sys)
            collect `(asdf:load-op ,sub))))

(defmethod asdf::component-depends-on :around ((op test-op)
                                               (sys nst-test-holder))
  (cons (list 'asdf:load-op :nst) (call-next-method)))

;;; THIS METHOD DOES NOT WORK.  None of the system's slots are filled
;;; in when this method is called; ASDF apparantly plugs these values
;;; in later.
;;;

;;;(defmethod initialize-instance :after ((sys nst-test-holder)
;;;                                       &key &allow-other-keys)
;;;
;;;  (when (and (or (nst-package sys)  (nst-group sys)  (nst-test sys))
;;;             (or (nst-packages sys) (nst-groups sys) (nst-tests sys)))
;;;    (error "Do not mix single-item testing via :nst-package, :nst-group, \
;;;:nst-test with multiple-item testing via :nst-packages, :nst-groups, \
;;;:nst-tests"))
;;;
;;;  (when (< 1 (+ (if (nst-package sys) 1 0)
;;;                (if (nst-group sys) 1 0)
;;;                (if (nst-test sys) 1 0)))
;;;    (error "Do not use more than one of :nst-package, :nst-group, :nst-test \
;;;\(use :nst-packages, :nst-groups, :nst-tests\)"))
;;;
;;;  (when (or (nst-packages sys) (nst-groups sys) (nst-tests sys))
;;;    (error
;;;     "Not currently implemented: :nst-packages, :nst-groups, :nst-tests"))
;;;
;;;  ;; Now push in additional in-order-to's corresponding to
;;;  ;; nst-systems.
;;;  (let ((nst-systems (nst-systems sys))
;;;        (in-order-to (slot-value sys 'asdf::in-order-to)))
;;;    (when nst-systems
;;;      (let* ((the-load-steps `((load-op ,@nst-systems)))
;;;             (the-test-steps `((load-op ,sys) (test-op ,@nst-systems :nst)))
;;;             other-ops)
;;;        (loop for op-steps in in-order-to do
;;;          (destructuring-bind (op . steps) op-steps
;;;            (case op
;;;              (asdf:load-op (setf the-load-steps
;;;                                  (nconc steps the-load-steps)))
;;;              (asdf:test-op (setf the-test-steps
;;;                                  (nconc steps the-test-steps)))
;;;              (otherwise (push op-steps other-ops)))))
;;;        (setf (slot-value sys 'asdf::in-order-to)
;;;              `((asdf:load-op ,@the-load-steps)
;;;                (asdf:test-op ,@the-test-steps)
;;;                ,@other-ops))))))

(defmethod get-test-specs ((s nst-test-holder))
  (with-accessors ((single-package nst-package)
                   (single-group nst-group)
                   (single-test nst-test)

                   (packages-here nst-packages)
                   (groups-here nst-groups)
                   (tests-here nst-tests)

                   (nst-systems nst-systems)) s
    (loop for sys in nst-systems
        for (ps gs ts) = (multiple-value-list (get-test-specs sys))
        append ps into packages
        append gs into groups
        append ts into tests
        finally
          (setf packages (nconc packages packages-here)
                groups (nconc groups groups-here)
                tests (nconc tests tests-here))
          (when single-package (push single-package packages))
          (when single-group (push single-group groups))
          (when single-test (push single-test tests))
          (return-from get-test-specs
            (values packages groups tests)))))

(defmethod all-nst-tested ((nst-test-holder nst-test-holder)
                           &optional
                           (all-packages (make-hash-table :test 'eq))
                           (all-groups (make-hash-table :test 'eq))
                           (all-tests-by-group (make-hash-table :test 'eq)))
  "Given an NST-test defining system, return the packages, groups and tests \(by
group\) that it should execute via NST.  This function is used for reporting on
the system\'s results."

  (with-accessors ((systems nst-systems)
                   (packages nst-packages) (package nst-package)
                   (group nst-group) (groups nst-groups)
                   (test nst-test) (tests nst-tests)) nst-test-holder

    (call-next-method nst-test-holder
                      all-packages all-groups all-tests-by-group)


    ;; Shorthand function for correct access to all-tests-by-group.
    (flet ((note-test-by-group (group test)
             (let ((group-table (gethash group all-tests-by-group)))
               (unless group-table
                 (setf group-table (make-hash-table :test 'eq)
                       (gethash group all-tests-by-group) group-table))
               (setf (gethash test group-table) t))))

      ;; Add local symbols
      (cond
       (test
        (multiple-value-bind (g ts) (test-spec-symbols test)
          (note-test-by-group g ts)))

       (group
        (setf (gethash (group-spec-symbol group) all-groups) t))

       (package (setf (gethash package all-packages) t))

       (t
        (loop for spec in tests do
              (multiple-value-bind (g ts) (test-spec-symbols spec)
                (note-test-by-group g ts)))
        (loop for spec in groups do
              (setf (gethash (group-spec-symbol spec) all-groups) t))
        (loop for p in packages do
              (setf (gethash p all-packages) t)))))

    (values all-packages all-groups all-tests-by-group)))

(defmethod perform ((o asdf:test-op) (c nst-test-holder))
  (macrolet ((nst-fn (fn &rest args)
               `(funcall (symbol-function (intern (symbol-name ',fn) :nst))
                         ,@args)))

    ;; First, run the tests that are local to this system.
    (with-accessors ((single-package nst-package)
                     (single-group nst-group)
                     (single-test nst-test)

                     (packages nst-packages)
                     (group-specs nst-groups)
                     (test-specs nst-tests)

                     (debug-args-expr nst-debug-config)
                     (debug-protect nst-debug-protect)) c
      (let ((debug-args (eval debug-args-expr)))
        ;; (format t "debug-args ~s~%debug-protect ~s~%"
        ;;    debug-args debug-protect)
        (let ((protected-values
               (nst-fn assemble-protected-option-values debug-protect)))
          (nst-fn run-debug-options debug-args)
          (cond

            ;; For running a single package.
            (single-package
             (nst-fn run-package single-package))

            ;; For running a single group.
            (single-group
             (let ((group-actual (intern (symbol-name (cdr single-group))
                                         (find-package (car single-group)))))
               (nst-fn run-group group-actual)))

            ;; For running a single test.
            (single-test
             (let ((group-actual (intern (symbol-name (cadr single-test))
                                         (find-package (car single-test))))
                   (test-actual (intern (symbol-name (caddr single-test))
                                        (find-package (car single-test)))))
               (nst-fn run-test group-actual test-actual)))

            ;; For running possibly several (or none) of each.
            (t
             (loop for pk in packages do (nst-fn run-package pk))
             (loop for spec in group-specs do
               (let ((group (group-spec-symbol spec)))
                 (nst-fn run-group group)))
             (loop for spec in test-specs do
               (multiple-value-bind (group test) (test-spec-symbols spec)
                 (nst-fn run-test group test)))))
          (nst-fn restore-protected-option-values protected-values)))

      ;; Then, do whatever else.
      (call-next-method))))

(defun group-spec-symbol (spec)
  (destructuring-bind (pk . gr) spec
    (intern (symbol-name gr) (find-package pk))))

(defun test-spec-symbols (spec)
  (destructuring-bind (pk gr ts) spec
    (values (intern (symbol-name gr) (find-package pk))
            (intern (symbol-name ts) (find-package pk)))))
