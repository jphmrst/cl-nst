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

(defgeneric group-name (group-instance)
  (:documentation "Map from a group instance back to its symbolic name."))

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

(defgeneric test-in-group-class-name (group-name)
  (:documentation
   "Map from groups to the private name with which NST associates a class with
which every test in the group is associated for testing the whole group of
tests.")
  (:method (default) (declare (ignorable default)) nil))
(add-class-name-static-method test-in-group-class-name)

(defgeneric standalone-test-in-group-class-name (group-name)
  (:documentation
   "Map from groups to the private name with which NST associates a class with
which every test in the group is associated for a standalone test.")
  (:method (default) (declare (ignorable default)) nil))
(add-class-name-static-method standalone-test-in-group-class-name)

(defgeneric test-fixture-classes (name))
(add-class-name-static-method test-fixture-classes)

(defgeneric package-groups (package-or-symbol))
(defmethod package-groups ((s symbol))
  (package-groups (find-package s)))
(defmethod package-groups ((p package))
  (let ((group-hash (gethash p +package-groups+)))
    (when group-hash
      (loop for g being the hash-keys of group-hash collect g))))

;; Information by Lisp package.

(defgeneric check-name (check-instance)
  (:documentation "Map from a check instance back to its symbolic name."))

(defgeneric suite-test-classes (group-prototype))
(defgeneric standalone-test-classes (group-prototype))
(defgeneric config-test-classes (group-prototype))

(defun suite-class-name (group-name test-name)
  (gethash test-name
           (suite-test-classes (class-prototype (find-class group-name)))))

(defun standalone-class-name (group-name test-name)
  (gethash test-name
           (standalone-test-classes (class-prototype
                                     (find-class group-name)))))

(defun test-config-class-name (group-name test-name)
  (gethash test-name
           (config-test-classes (class-prototype
                                      (find-class group-name)))))

(defgeneric canonical-storage-name (test-name)
  (:documentation
   "Map from various test names and instances to the private name against which
NST associates test results."))

;; Fixture properties and operations.

(defgeneric bound-names (fixture-or-group)
  (:documentation "The names defined by each fixture.  Will be given
an eql-method by the macros which expand tests and groups."))
(add-class-name-static-method bound-names)

(defgeneric group-fixture-class-name (fixture-name)
  (:documentation
   "Map from fixture names to the private name with which NST associates the
corresponding internal name-binding NST class for adding fixtures to a group.")
  (:method (default) (declare (ignorable default)) nil))
(add-class-name-static-method group-fixture-class-name)

(defgeneric test-fixture-class-name (fixture-name)
  (:documentation
   "Map from fixture names to the private name with which NST associates the
corresponding internal name-binding NST class for adding fixtures to a test.")
  (:method (default) (declare (ignorable default)) nil))
(add-class-name-static-method test-fixture-class-name)

(defgeneric open-fixture (fixture-name &optional package)
  (:documentation
   "Inject the names defined by the named fixture into the given package, by
default the current package."))
(defmethod open-fixture ((s symbol) &optional (in-package *package*))
  (open-fixture (make-instance s) in-package))

(defgeneric anon-fixture-forms (forms))
(add-class-name-static-method anon-fixture-forms)

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
