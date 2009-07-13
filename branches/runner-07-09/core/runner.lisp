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

;;; ----------------------------------------------------------------------
;;; This file defines the functions which implement the main control
;;; flow of test and group execution.


;;;
;;; More generic functions whose methods are defined by the various
;;; macros.
;;;

;; Internal test execution functions.

(defgeneric core-run (group-or-test)
  (:documentation
   "Group fixtures provide name-binding :around methods to this generic
function; group setup and cleanup become :before and :after methods.")
  (:method ((group-inst group-base-class))
     (let ((group-name (group-name group-inst)))
       (when (> *nst-verbosity* 3)
         (format t "    Starting run loop for ~s~%" group-inst))
       (loop for test in (test-names group-inst) do
         (when (> *nst-verbosity* 3)
           (format t "      Starting loop entry ~s~%" test))
         (let ((in-suite-class-name (suite-class-name group-name test)))
           (when (> *nst-verbosity* 3)
             (format t "    Suite class name ~s~%" in-suite-class-name)
             (format t "    Actual class ~s~%"
               (find-class in-suite-class-name)))
           (let ((test-inst (make-instance in-suite-class-name)))
             (when (> *nst-verbosity* 3)
               (format t "    Instance ~s~%" test-inst))
             (core-run-test test-inst)))
         ;; (format t "      Exiting loop entry ~s~%" test)
             )
         ;;(format t "    Exiting run loop for ~s~%" group-inst)
       )
     nil))

(defgeneric core-run-test (test)
  (:documentation
   "Test fixtures provide name-binding :around methods to this generic function
for individual tests.  Every-test and test-specific setup and cleanup are
encoded as :before and :after methods.")

  (:method :around (test)
    "Capture the result of the test."
    (let ((*nst-group-name* (group-name test))
          (*nst-check-name* (check-name test))
          (start-time))
      (when (> *nst-verbosity* 1)
        (format t " - Executing test ~s~%" (check-name test)))
      (setf start-time (get-internal-real-time))
      (let ((result (call-next-method))
            (end-time (get-internal-real-time)))
        (setf (result-stats-elapsed-time result)
              (- end-time start-time)
              (gethash (canonical-storage-name (type-of test))
                       +results-record+)
              result)
        (when (> *nst-verbosity* 1)
          (format t "   ~s~%" result))
        result))))

;;;
;;; Programmatic starters for a test from Lisp.  Other starters such
;;; as via ASDF and vendor-specific REPL macros call these functions;
;;; from pure Lisp these are the top-level calls.
;;;
(defun run-package (&optional (package-or-name *package*))
  "Run all groups in a package."
  (let* ((user-package (find-package package-or-name))
         (group-names (package-groups user-package)))

    ;; Print a message at the appropriate level of verbosity.
    (when (> *nst-verbosity* 0)
      (format t "~@<Running package ~s (groups ~{~s~^ ~:_~})~:>~%"
        (package-name user-package) group-names))

    (cond
      (group-names
       (loop for group-name in group-names do (run-group group-name)))
      (t
       (error 'no-nst-groups-in-package :package package-or-name)))))

;;; --------------------------------------------------------------
;;; This section of function definitions is not immediately in use,
;;; but I'm planning to shift over to them to eliminate side package
;;; creation, and to reduce class generation to one per
;;; fixture/class/test.

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
  (:method progn (group-obj test-objs)
    (do-group-postfixture-setup group-obj)
    (when (> *nst-verbosity* 3)
      (format t "    Starting run loop for ~s~%" group-obj))
    (loop for test-inst in test-objs do
      (when (> *nst-verbosity* 3) (format t "    Instance ~s~%" test-inst))
      (do-test-main test-inst)
          #|(format t "      Exiting loop entry ~s~%" test)|#)
    #|(format t "    Exiting run loop for ~s~%" group-obj)|#
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

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun do-test-main (test-inst)
  "Test execution gateway for both group and standalone execution."
  (do-test-prefixture-setup test-inst)
  (do-test-fixture-assignment test-inst)
  (do-test-afterfixture-cleanup test-inst))

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

(defgeneric do-test-withfixture-cleanup (test-obj)
  (:documentation "Fixture setup specs add a method to this function
for the test application class.")
  (:method-combination progn)
  (:method progn (test-obj) (declare (ignorable test-obj))))

(defgeneric do-test-postfixture-setup (test-obj)
  (:documentation "With-fixtures cleanup specs add a method to this function
for the test application class.")
  (:method-combination progn)
  (:method progn (test-obj) (declare (ignorable test-obj))))

;;; ------------------------------------------------------------

(defun run-group (group)
  "Run a group by its user-given name."
  (let ((group-class (group-class-name group)))

    ;; Print a message at the appropriate level of verbosity.
    (cond
      ((> *nst-verbosity* 3)
       (format t "Running group ~s --> ~s~%" group group-class))
      ((> *nst-verbosity* 0)
       (format t "Running group ~s~%" group)))

    (unless group-class (error 'no-such-nst-group :group group))
    (core-run (make-instance group-class))))

(defun run-test (group test)
  "Run a test standalone by its user-given name (and its group's name)."
  (let ((test-class (standalone-class-name group test)))

    ;; Print a message at the appropriate level of verbosity.
    (when (> *nst-verbosity* 0)
      (format t "Running test ~s (group ~s)~%" test group))

    (unless test-class (error 'no-such-nst-test :group group :test test))
    (core-run (make-instance test-class))))
