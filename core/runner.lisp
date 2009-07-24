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
    (when (> *nst-verbosity* 0)
      (format t "~@<Running package ~s (groups ~{~s~^ ~:_~})~:>~%"
        (package-name user-package) group-names))

    (cond
      (group-names
       (loop for group-name in group-names do (run-group group-name)))
      (t
       (error 'no-nst-groups-in-package :package package-or-name)))))

(defun run-group (group-class)
  "Run a group by its user-given name."
  ;; Print a message at the appropriate level of verbosity.
  (cond
   ((> *nst-verbosity* 0)
    (format t "Running group ~s~%" group-class)))

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
        (when (> *nst-verbosity* 0)
          (format t "Running test ~s (group ~s)~%" test group))

        (run-group-tests group-inst (list test-inst))))))

;;; --------------------------------------------------------------

(defun run-group-tests (group-obj test-objs)
  "Programmatic entry point for running all tests in a group."
  (do-group-prefixture-setup group-obj)
  (do-group-fixture-assignment group-obj test-objs)
  (do-group-afterfixture-cleanup group-obj))

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
    (do-group-postfixture-setup group-obj)
    (when (> *nst-verbosity* 3)
      (format t "    Starting run loop for ~s~%" group-obj))
    (loop for test-inst in test-objs do
      (when (> *nst-verbosity* 3) (format t "    Instance ~s~%" test-inst))
      (do-group-each-test-setup group-obj)
      (do-test-prefixture-setup test-inst)
      (do-test-fixture-assignment test-inst)
      (do-test-afterfixture-cleanup test-inst)
      (do-group-each-test-cleanup group-obj)
      (when (> *nst-verbosity* 3)
        (format t "      Exiting loop entry ~s~%" test-inst)))
    (when (> *nst-verbosity* 3)
      (format t "    Exiting run loop for ~s~%" group-obj))
    (do-group-withfixture-cleanup group-obj)))

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
     (do-test-postfixture-setup test-obj)
     (core-run-test test-obj)
     (do-test-withfixture-cleanup test-obj)))

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
      (when (> *nst-verbosity* 1)
        (format t " - Executing test ~s~%" *nst-check-user-name*))
      (setf start-time (get-internal-real-time))
      (let ((result (call-next-method))
            (end-time (get-internal-real-time)))
        (setf (result-stats-elapsed-time result)
              (- end-time start-time)
              (gethash (check-group-name test) +results-record+)
              result)
        (when (> *nst-verbosity* 1)
          (format t "   ~s~%" result))
        result))))

