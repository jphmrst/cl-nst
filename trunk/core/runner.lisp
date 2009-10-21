;;; File runner.lisp
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

;;; This file defines the functions which implement the main control
;;; flow of test and group execution.

;;; ----------------------------------------------------------------------
;;; Programmatic starters for a test from Lisp.  Other starters such
;;; as via ASDF and vendor-specific REPL macros call these functions;
;;; from pure Lisp these are the top-level calls.
;;;
(defun run-package (&optional (package-or-name *package*))
  "Run all groups in a package."
  (let* ((user-package (find-package package-or-name))
         (group-names (package-groups user-package)))
    (note-artifact-choice (package-name user-package) user-package)

    ;; Print a message at the appropriate level of verbosity.
    (format-at-verbosity 0 "~@<Running package ~s (groups ~{~s~^ ~:_~})~:>~%"
        (package-name user-package) group-names)

    (cond
      (group-names
       (loop for group-name in group-names do (run-group group-name)))
      (t
       (error 'no-nst-groups-in-package :package package-or-name)))))

(defun run-group (group-class)
  "Run a group by its user-given name."
  ;; Print a message at the appropriate level of verbosity.
  (format-at-verbosity 0 "Running group ~s~%" group-class)

  (unless group-class (error 'no-such-nst-group :group group-class))
  (run-group-inst (make-instance group-class)))

(defun run-group-inst (group-inst)
  (let ((test-lookups (test-name-lookup group-inst)))
    (note-artifact-choice (group-name group-inst) group-inst)
    (run-group-tests group-inst
                     (loop for test-name in (test-names group-inst)
                         collect (gethash test-name test-lookups)))))

(defun run-test-inst (test-inst)
  (let ((group-inst (make-instance (group-name test-inst))))
    (note-artifact-choice (check-user-name test-inst) test-inst)
    (run-group-tests group-inst (list test-inst))))

(defun run-test (group test)
  "Run a test standalone by its user-given name (and its group's name)."
  (let ((group-class (find-class group)))
    (unless group-class (error 'no-such-nst-group :group group))
    (let ((group-inst (make-instance group)))
      (let* ((test-lookups (test-name-lookup group-inst))
             (test-inst (gethash test test-lookups)))
        (unless test-inst (error 'no-such-nst-test :group group :test test))
        (note-artifact-choice (check-user-name test-inst) test-inst)

        ;; Print a message at the appropriate level of verbosity.
        (format-at-verbosity 0 "Running test ~s (group ~s)~%" test group)

        (run-group-tests group-inst (list test-inst))))))

;;; --------------------------------------------------------------

(defmacro with-retry ((continuation-label) &body forms)
  (let ((inner (gensym)) (outer (gensym)) (e (gensym)))
    `(block ,outer
         (loop do
           (block ,inner
             (handler-bind ((error #'(lambda (,e)
                                       (cerror ,continuation-label ,e)
                                       (return-from ,inner))))
               (return-from ,outer
                 (progn ,@forms))))))))

(defun run-group-tests (group-obj test-objs)
  "Programmatic entry point for running all tests in a group."
  (with-retry ("Try performing group setup again.")
    (handler-bind
        ((error #'(lambda (e)
                    (loop for test-obj in test-objs do
                      (setf (gethash (check-group-name test-obj)
                                     +results-record+)
                            (emit-config-error e test-obj
                              "Error in pre-fixture setup")))
                    (when *debug-on-error*
                      (cerror "~@<Exit from attempting tests in this group, ~_~
                             and continue with tests from other groups.~:>" e))
                    (return-from run-group-tests nil))))
      (do-group-prefixture-setup group-obj)))
  (with-retry
      ("Try running this group's tests again, reapplying group fixtures")
    (block group-fixture-assignment
      (handler-bind
          ((error #'(lambda (e)
                      (loop for test-obj in test-objs do
                        (setf (gethash (check-group-name test-obj)
                                       +results-record+)
                          (emit-config-error e test-obj
                            (format nil "Error binding group fixture ~s"
                              *binding-variable*))))
                      (when *debug-on-error*
                        (cerror
                         "~@<Clean up from this group's tests (prefixture)~
                         , ~_and continue with tests from other groups.~:>" e))
                      (return-from group-fixture-assignment nil))))
        (do-group-fixture-assignment group-obj test-objs))))
  (with-retry ("Try performing group cleanup again.")
    (handler-bind
        ((error #'(lambda (e)
                    (loop for test-obj in test-objs do
                      (add-test-config-error test-obj
                        "Error in post-fixtures cleanup: ~s" e))
                    (when *debug-on-error*
                      (cerror "Continue with tests from other groups."
                              e))
                    (return-from run-group-tests nil))))
      (do-group-afterfixture-cleanup group-obj))))

(defgeneric do-group-prefixture-setup (group-obj)
  (:documentation
   "Pre-fixture group application setup specs add a method to this function.")
  (:method-combination progn)
  (:method progn (group-obj) (declare (ignorable group-obj))))

(defgeneric do-group-afterfixture-cleanup (group-obj)
  (:documentation
   "After-group fixture cleanup specs add a method to this function.")
  (:method-combination progn)
  (:method progn (group-obj) (declare (ignorable group-obj))))

(defgeneric do-group-fixture-assignment (group-obj test-objs)
  (:documentation
   "Fixture declarations translate to an :around method making let* bindings
for the group application class.")
  (:method (group-obj test-objs)
     (with-retry ("Try performing postfixture group setup again.")
       (handler-bind
           ((error
             #'(lambda (e)
                 (loop for test-obj in test-objs do
                       (setf (gethash (check-group-name test-obj)
                                      +results-record+)
                         (emit-config-error e test-obj
                           "Error in post-fixture application setup")))
                 (when *debug-on-error*
                   (cerror "~@<Clean up from this group's tests (postfixture), ~_~
                             and continue with tests from other groups.~:>" e))
                 (return-from do-group-fixture-assignment nil))))
         (do-group-postfixture-setup group-obj)))

     (format-at-verbosity 3 "    Starting run loop for ~s~%" group-obj)

     (unwind-protect
         (progn
           (with-retry ("Re-run all tests in this group.")
             (loop for test-inst in test-objs do
               (block this-test
                 (format-at-verbosity 3 "    Instance ~s~%" test-inst)
                 (with-retry ("Try each-test setup for this group again.")
                   (handler-bind
                       ((error #'(lambda (e)
                                   (setf (gethash (check-group-name test-inst)
                                                  +results-record+)
                                     (emit-config-error
                                         e test-inst
                                       "Error in group each-test setup"))
                                   (when *debug-on-error*
                                     (cerror
                                      "~@<Continue with other tests from ~
                                   this group ~_(not likely to succeed).~:>" e))
                                   (return-from this-test nil))))
                     (do-group-each-test-setup group-obj)))
                 (unwind-protect
                     (block test-inner
                       (with-retry ("Try this test's setup again.")
                         (handler-bind
                             ((error
                               #'(lambda (e)
                                   (setf (gethash (check-group-name test-inst)
                                                  +results-record+)
                                     (emit-config-error e test-inst
                                       "Error in test pre-fixture setup"))
                                   (when *debug-on-error*
                                     (cerror
                                      "~@<Perform group each-test ~_~
                                            cleanup, and continue with other ~
                                            tests from this group.~:>" e))
                                   (return-from test-inner nil))))
                           (do-test-prefixture-setup test-inst)))
                       (unwind-protect
                           (with-retry ("Try this test again, reapplying test fixtures.")
                             (block test-fixture-assignment
                               (handler-bind
                                   ((error
                                     #'(lambda (e)
                                         (setf (gethash (check-group-name
                                                         test-inst)
                                                        +results-record+)
                                           (emit-config-error
                                               e test-inst
                                             (format nil
                                                 "Error binding test fixture ~s"
                                               *binding-variable*)))
                                         (when *debug-on-error*
                                           (cerror "Cleanup from this test, ~
                             and continue with other tests from this group." e))
                                         (return-from test-fixture-assignment
                                           nil))))
                                 (do-test-fixture-assignment test-inst))))
                         (with-retry ("Try test cleanup again.")
                           (handler-bind
                               ((error
                                 #'(lambda (e)
                                     (add-test-config-error test-inst
                                       "Error in test postfixture cleanup: ~s"
                                       e)
                                     (when *debug-on-error*
                                       (cerror
                                        "Continue with other tests from this group."
                                        e))
                                     (return-from test-inner))))
                             (do-test-afterfixture-cleanup test-inst)))))
                   (with-retry ("Try each-test cleanup again.")
                     (handler-bind
                         ((error
                           #'(lambda (e)
                               (add-test-config-error test-inst
                                 "Error in group each-test cleanup: ~s" e)
                               (when *debug-on-error*
                                 (cerror "~@<Continue with other tests ~
                                                from this group ~_(this error ~
                                                likely to recur).~:>" e))
                               (return-from this-test))))
                       (do-group-each-test-cleanup group-obj)))
                   (format-at-verbosity 3
                       "      Exiting loop entry ~s~%" test-inst)))))

           (format-at-verbosity 3
               "    Exiting run loop for ~s~%" group-obj))

       (with-retry ("Try performing group with-fixtures cleanup again.")
         (handler-bind
             ((error #'(lambda (e)
                         (loop for test-obj in test-objs do
                               (add-test-config-error test-obj
                                 "Error in group fixtures cleanup: ~s" e))
                         (when *debug-on-error*
                           (cerror
                            "Continue with tests from other groups." e))
                         (return-from do-group-fixture-assignment))))
           (do-group-withfixture-cleanup group-obj))))))

(defgeneric do-group-postfixture-setup (group-obj)
  (:documentation "Fixture setup specs add a method to this function
for the group application class.")
  (:method-combination progn)
  (:method progn (group-obj) (declare (ignorable group-obj))))

(defgeneric do-group-withfixture-cleanup (group-obj)
  (:documentation "With-fixtures cleanup specs add a method to this function
for the group application class.")
  (:method-combination progn)
  (:method progn (group-obj) (declare (ignorable group-obj))))

(defgeneric do-group-each-test-setup (group-obj)
  (:documentation
   "Group each-test setup specs add a method to this function.")
  (:method-combination progn)
  (:method progn (group-obj) (declare (ignorable group-obj))))

(defgeneric do-group-each-test-cleanup (group-obj)
  (:documentation
   "Group each-test cleanup specs add a method to this function.")
  (:method-combination progn)
  (:method progn (group-obj) (declare (ignorable group-obj))))

(defgeneric do-test-prefixture-setup (test-obj)
  (:documentation
   "Pre-fixture test application setup specs add a method to this function.")
  (:method-combination progn)
  (:method progn (test-obj) (declare (ignorable test-obj))))

(defgeneric do-test-afterfixture-cleanup (test-obj)
  (:documentation
   "After-test fixture cleanup specs add a method to this function.")
  (:method-combination progn)
  (:method progn (test-obj) (declare (ignorable test-obj))))

(defgeneric do-test-fixture-assignment (test-obj)
  (:documentation
   "Fixture declarations translate to an :around method making let* bindings
for the test application class.")
  (:method (test-obj)
     (handler-bind
         ((error
           #'(lambda (e)
               (setf (gethash (check-group-name test-obj)
                              +results-record+)
                 (emit-config-error e test-obj
                                    "Error in test post-fixture setup"))
               (when *debug-on-error*
                 (cerror "Continue with other tests in this group" e))
               (return-from do-test-fixture-assignment nil))))
       (do-test-postfixture-setup test-obj))
     (unwind-protect (core-run-test test-obj)
       (handler-bind
           ((error #'(lambda (e)
                       (add-test-config-error test-obj
                          "Error in test fixtures cleanup: ~s" e)
                       (when *debug-on-error*
                         (cerror
                          "Continue with other tests in this group" e))
                       (return-from do-test-fixture-assignment nil))))
         (do-test-withfixture-cleanup test-obj)))))

(defgeneric do-test-postfixture-setup (test-obj)
  (:documentation "With-fixtures cleanup specs add a method to this function
for the test application class.")
  (:method-combination progn)
  (:method progn (test-obj) (declare (ignorable test-obj))))

(defgeneric do-test-withfixture-cleanup (test-obj)
  (:documentation "Fixture setup specs add a method to this function
for the test application class.")
  (:method-combination progn)
  (:method progn (test-obj) (declare (ignorable test-obj))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defgeneric core-run-test (test)
  (:documentation
   "Test fixtures provide name-binding :around methods to this generic function
for individual tests.  Every-test and test-specific setup and cleanup are
encoded as :before and :after methods.")

  (:method :around (test)
    "Capture the result of the test."
    (let ((*nst-group-name* (group-name test))
          (*nst-check-user-name* (check-user-name test))
          (*nst-check-internal-name* (check-group-name test))
          (start-time))
      (declare (special *nst-group-name* *nst-check-user-name*))
      (format-at-verbosity 1 " - Executing test ~s~%" *nst-check-user-name*)
      (setf start-time (get-internal-real-time))
      (let ((result (call-next-method))
            (end-time (get-internal-real-time)))
        (setf (result-stats-elapsed-time result)
              (- end-time start-time)
              (gethash (check-group-name test) +results-record+)
              result)
        (format-at-verbosity 1 "   ~s~%" result)
        result))))

