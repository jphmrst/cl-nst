;;; File artifacts.lisp
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
;;; This file contains generic functions for which methods are defined
;;; by the macros for user-defined artifacts (fixtures, groups,
;;; tests).

;;;
;;; Internal tables.
;;;
(defvar +package-groups+ (make-hash-table :test 'eq)
  "Map from packages to the test groups declared in each package.")

;; Properties of groups.  Many of these function have methods on
;; symbols (presumably class names) that either relay to class
;; methods, or re-dispatch after instantiating an object of the named
;; class.

(defgeneric group-record-p (obj)
  (:method (obj) (declare (ignorable obj)) nil)
  (:documentation "Return non-nil if an item is a group record."))
(defgeneric test-record-p (obj)
  (:method (obj) (declare (ignorable obj)) nil)
  (:documentation "Return non-nil if an item is a group record."))

(defgeneric group-name (group-instance)
  (:documentation
   "Map from a group or test instance back to its symbolic name."))

(defgeneric group-fixture-class-names (group-instance)
  (:documentation "Map from a group instance to the list of fixture classes associated with it."))

(defgeneric test-names (fixture-or-group)
  (:documentation "The names of tests in a group.  Will be given an eql-method
by the macros which expand tests and groups."))
(add-class-name-instantiator-method test-names)

(defgeneric group-class-name (group-name)
  (:documentation
   "Map from groups to the private name with which NST associates the class of
group-specific activities.")
  (:method (default) (declare (ignorable default)) nil))
(add-class-name-static-method group-class-name)

(defgeneric group-fixture-classes (group-name)
  (:documentation
   "Map from groups to the private names of the group's fixtures."))
(add-class-name-static-method group-fixture-classes)

(defgeneric test-name-lookup (group))

(defgeneric test-fixture-classes (name))
(add-class-name-static-method test-fixture-classes)

(defgeneric package-groups (package-or-symbol))
(defmethod package-groups ((s symbol))
  (package-groups (find-package s)))
(defmethod package-groups ((p package))
  (let ((group-hash (gethash p +package-groups+)))
    (when group-hash
      (loop for g being the hash-keys of group-hash collect g))))

(defgeneric check-user-name (check-instance)
  (:documentation "Map from a check instance back to its user symbolic name.")
  (:method ((s symbol)) s))

(defgeneric check-group-name (check-instance)
  (:documentation "Map from a check instance back to its internal name."))

(defun ensure-group-instance (group)
  (cond
    ((symbolp group) (make-instance group))
    (t group)))

(defun ensure-test-instance (group test)
  (cond
    ((symbolp test)
     (cond
      ((find-class test nil) (make-instance test))
      (t (gethash test (test-name-lookup (ensure-group-instance group))))))
    (t test)))

;; Fixture properties and operations.

(defgeneric bound-names (fixture-or-group)
  (:documentation "The names defined by each fixture.  Will be given
an eql-method by the macros which expand tests and groups."))
(add-class-name-static-method bound-names)

(defgeneric open-fixture (fixture-name &optional package)
  (:documentation
   "Inject the names defined by the named fixture into the given package, by
default the current package."))
(defmethod open-fixture ((s symbol) &optional (in-package *package*))
  (open-fixture (make-instance s) in-package))

;; Diagnostic information display.

(defgeneric blurb-context-line (stream id args forms)
  (:documentation "Give a short description of a context."))

(defgeneric detail-context-line (stream id args forms)
  (:documentation "Give a longer blurb of a context."))

(defgeneric stack-transformer (id)
  (:documentation "Check form-specific stack transformation."))

;; Common superclass of all results holders.  We use the accessors
;; below, so keep this definition here rather than in the status file.

(defstruct result-stats "Statistics common to the different result summaries."
  (tests 0) (passing 0) (erring 0) (failing 0) (warning 0)
  (elapsed-time 0)
  (timestamp (multiple-value-list (get-decoded-time))))


;;;
;;; Tracking what an artifact is by name.
;;;
(defvar +name-use+ (make-hash-table :test 'eq)
  "Known uses of names.")

(defstruct name-use
  "Record for tracking the artifacts which NST associates with a name."
  fixture group (tests (make-hash-table :test 'eq)))

(set-pprint-dispatch 'name-use
  #'(lambda (stream usage)
      (with-accessors ((fixture name-use-fixture)
                       (group name-use-group)
                       (tests name-use-tests)) usage
        (let ((tests-p (> (hash-table-count tests) 0)))
          (cond
            ((and fixture (not group) (not tests-p))
             (format stream
                 "~@<~a (package ~a) is a fixture~
                 ~:@_ - ~@<binds names~{ ~a~^,~:_~}~:>~
                 ~:>"
               (type-of fixture)
               (package-name (symbol-package (type-of fixture)))
               (bound-names fixture)))

            ((and group (not fixture) (not tests-p))
             (format stream
                 "~@<~a (package ~a) is a test group~
                  ~:@_ - ~@<Contains test~p~{ ~a~^,~:_~}~:>~:>"
               (type-of group)
               (package-name (symbol-package (type-of group)))
               (length (test-names group)) (test-names group)))

            ((and tests-p (not fixture) (not group))
             (case (hash-table-count tests)
               (1
                (loop for tst being the hash-values of tests do
                  (format stream "~a (package ~a) is a test in group ~a"
                    (test-name-lookup tst)
                    (package-name (symbol-package (test-name-lookup tst)))
                    (group-name tst))))
               (otherwise
                (loop for tst being the hash-values of tests do
                  (format stream "~a (package ~a) is a test in group ~a~%"
                    (test-name-lookup tst)
                    (package-name (symbol-package (test-name-lookup tst)))
                    (group-name tst))))))

            (t
             (format stream "MULTI")))))))


;;;
;;; Recording of results.  We use a hash table here --- unlike the
;;; method-based recording of test symbols, we're not worried about
;;; straddling the compile/load/run-time borders for result recording.
;;;
(defvar +results-record+ (make-hash-table :test 'eq)
  "Results of test runs.")

;; Extracting information for debugging.

(defgeneric trace-fixture (fx)
  (:documentation "Provide debugging information about a fixture.")
  (:method (fx) (format t "No known fixture ~s~%" fx)))
(add-class-name-instantiator-method trace-fixture)

(defgeneric trace-group (gr)
  (:documentation "Provide debugging information about a group.")
  (:method (gr) (format t "No known group ~s~%" gr)))
(add-class-name-instantiator-method trace-group)

(defgeneric trace-test (gr ts)
  (:documentation "Provide debugging information about a test.")
  (:method (gr ts) (format t "No known test ~s in group ~s~%" ts gr)))

(defun trace-results ()
  "Internal debugging function: dump the results hash."
  (loop for ts being the hash-keys of +results-record+ using (hash-value rs) do
    (format t "~s -> ~s~%" ts rs)))
