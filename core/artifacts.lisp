;;; File artifacts.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2011 Smart Information Flow Technologies.
;;; Copyright (c) 2015-2016 John Maraist
;;; Written by John Maraist.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lisp Lesser General Public License,
;;; which consists of the preamble published by Franz Incorporated,
;;; plus the LGPL published by the Free Software Foundation, either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lisp Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the Preamble to the Gnu Lesser
;;; General Public License and the GNU Lesser General Public License
;;; along with NST.  If not, see respectively
;;; <http://opensource.franz.com/preamble.html> and
;;; <http://www.gnu.org/licenses/>.
(in-package :sift.nst)

;;; ----------------------------------------------------------------------
;;; This file contains generic functions for which methods are defined
;;; by the macros for user-defined artifacts (fixtures, groups,
;;; tests).

;;;;;;
;;;;;; Internal tables.
;;;;;;
;;;;; Properties of groups.  Many of these function have methods on
;;;;; symbols (presumably class names) that either relay to class
;;;;; methods, or re-dispatch after instantiating an object of the named
;;;;; class.
;;;
;;;(defgeneric group-name (group-instance)
;;;  (:documentation
;;;   "Map from a group or test instance back to its symbolic name."))
;;;
;;;(defgeneric group-fixture-class-names (group-instance)
;;;  (:documentation "Map from a group instance to the list of fixture classes associated with it."))
;;;
;;;(defgeneric test-names (fixture-or-group)
;;;  (:documentation "The names of tests in a group.  Will be given an eql-method
;;;by the macros which expand tests and groups."))
;;;(add-class-name-instantiator-method test-names)
;;;
;;;(defgeneric group-class-name (group-name)
;;;  (:documentation
;;;   "Map from groups to the private name with which NST associates the class of
;;;group-specific activities.")
;;;  (:method (default) (declare (ignorable default)) nil))
;;;(add-class-name-static-method group-class-name)
;;;
;;;(defgeneric group-fixture-classes (group-name)
;;;  (:documentation
;;;   "Map from groups to the private names of the group's fixtures."))
;;;(add-class-name-static-method group-fixture-classes)
;;;
;;;;;; two declarations that I could not adequately document, because
;;;;;; I don't know their return types.
;;;(defgeneric group-given-fixtures (test-group)
;;;  (:documentation "For documentation purposes --- returns a list
;;;of names of fixtures that are used by TEST-GROUP."))
;;;
;;;(defgeneric test-list (test-group))
;;;
;;;(defgeneric test-name-lookup (group)
;;;  (:method ((s symbol)) s))
;;;
;;;(defgeneric test-fixture-classes (name))
;;;(add-class-name-static-method test-fixture-classes)
;;;
;;;;;;(defgeneric check-user-name (check-instance)
;;;;;;  (:documentation "Map from a check instance back to its user symbolic name.")
;;;;;;  (:method ((s symbol)) s))
;;;
;;;(defgeneric check-group-name (check-instance)
;;;  (:documentation "Map from a check instance back to its internal name."))
;;;
;;;(defgeneric test-forms (test-record)
;;;  (:method (o)
;;;     (declare (ignore o))
;;;     (error "Called ~s on an object which is not a subclass of ~s"
;;;            'test-forms 'test-record))
;;;  (:documentation "The unevaluated forms to be used as input to a test."))
;;;
;;;(defgeneric special-fixture-names (test-record)
;;;  (:method (o)
;;;     (declare (ignore o))
;;;     (error "Called ~s on an object which is not a subclass of ~s"
;;;            'special-fixture-names 'test-record))
;;;  (:documentation "The unevaluated forms to be used as input to a test."))
;;;
;;;(defgeneric test-criterion (test-record)
;;;  (:method (o)
;;;     (declare (ignore o))
;;;     (error "Called ~s on an object which is not a subclass of ~s"
;;;            'test-criterion 'test-record))
;;;  (:documentation "The unevaluated forms to be used as input to a test."))
;;;
;;;(defun ensure-group-instance (group)
;;;  (cond
;;;    ((symbolp group) (make-instance group))
;;;    (t group)))
;;;
;;;(defun ensure-test-instance (group test)
;;;  (cond
;;;    ((symbolp test)
;;;     (cond
;;;      ((find-class test nil) (make-instance test))
;;;      (t (make-instance (gethash test (test-name-lookup (ensure-group-instance group)))))))
;;;    (t test)))
;;;
;;;;; Fixture properties and operations.
;;;
;;;(defgeneric bound-names (fixture-or-group)
;;;  (:documentation "The names defined by each fixture.  Will be given
;;;a methods by the macro which expands fixtures."))
;;;;; (add-class-name-static-method bound-names)
;;;
;;;(defgeneric open-fixture (fixture-name &optional package)
;;;  (:documentation
;;;   "Inject the names defined by the named fixture into the given package, by
;;;default the current package."))
;;;(defmethod open-fixture ((s symbol) &optional (in-package *package*))
;;;  (open-fixture (make-instance s) in-package))
;;;
;;;;; Diagnostic information display.
;;;
;;;(defgeneric blurb-context-line (stream id args forms)
;;;  (:documentation "Give a short description of a context."))
;;;
;;;(defgeneric detail-context-line (stream id args forms)
;;;  (:documentation "Give a longer blurb of a context."))
;;;
;;;(defgeneric stack-transformer (id)
;;;  (:documentation "Check form-specific stack transformation."))

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
  "Known uses of names of a particular package.")
(defvar +name-packages+ (make-hash-table :test 'eq)
  "Known occurances of names in different packages.")


(defun package-lookup (name)
  (intern (symbol-name name) (find-package :nst-name-use-in-packages)))

(defun get-all-named-symbols (name)
  (let ((all-names (gethash (package-lookup name) +name-packages+)))
    (when all-names
      (loop for name being the hash-keys of all-names collect name))))

(defun get-name-use (name)
  (loop for n in (get-all-named-symbols name) collect (gethash n +name-use+)))

(defstruct name-use
  "Record for tracking the artifacts which NST associates with a name."
  fixture group (tests (make-hash-table :test 'eq)))

(defun get-name-use-record (name)
  "Internal function for use within record-name-use --- this is not the function
you want to use to read +name-use+."
  (let ((this-name-use (gethash name +name-use+)))
    (unless this-name-use
      (setf this-name-use (make-name-use)
            (gethash name +name-use+) this-name-use))
    this-name-use))

(defgeneric record-name-use (category name item)
  (:documentation "As of Oct. 2011, all callers pass a symbol for the item.")
  (:method :around (category name item)
     (declare (ignorable category item))
     (eval-when (:load-toplevel :execute)
       (call-next-method)
       (let* ((package-finder (package-lookup name))
              (this-name-package-use (gethash package-finder +name-packages+)))
         (unless this-name-package-use
           (setf this-name-package-use (make-hash-table :test 'eq)
                 (gethash package-finder
                          +name-packages+) this-name-package-use))
         (setf (gethash name this-name-package-use) t))))
  (:method ((category (eql :fixture)) name item)
     (let ((this-name-use (get-name-use-record name)))
       (setf (name-use-fixture this-name-use) item)))
  (:method ((category (eql :group)) name item)
     (let ((this-name-use (get-name-use-record name)))
       (setf (name-use-group this-name-use) item)))
  (:method ((category (eql :test)) name item)
     (let ((this-name-use (get-name-use-record name)))
       (let ((tests-by-group (name-use-tests this-name-use)))
         (setf (gethash (group-record-name (make-instance item)) tests-by-group)
               item)))))

(defvar +last-name-use-sort+ (make-hash-table :test 'eq))
(defvar +last-test-group+ (make-hash-table :test 'eq))
(defun note-name-use-invocation (name sort &optional group)
  (setf (gethash name +last-name-use-sort+) sort)
  (when (and (eq sort :test) group)
    (setf (gethash name +last-test-group+) group)))

(defun lookup-artifact (name)
  (loop for pname in (get-all-named-symbols name)
      append
      (let ((use (gethash pname +name-use+))
            (last-use-sort (gethash pname +last-name-use-sort+)))
          (when use
            (with-slots (fixture group tests) use
              (case last-use-sort
                ((:package)
                 (list (find-package pname)))
                ((:fixture)
                 (when fixture (list (make-instance fixture))))
                ((:group)
                 (when group (list (make-instance group))))
                ((:test)
                 (let ((test-group (gethash pname +last-test-group+)))
                   (cond
                     ((and test-group (gethash test-group tests))
                      (list (make-instance (gethash test-group tests))))
                     (t (loop for tst being the hash-values of tests
                            collect (make-instance tst))))))
                (otherwise `(,@(when fixture (list (make-instance fixture)))
                               ,@(when group (list (make-instance group)))
                               ,@(when tests (loop for tst being the hash-values
                                                 of tests
                                                 collect (make-instance tst)))))))))))

;;;(set-pprint-dispatch 'name-use
;;;  (named-function name-use-pprint-dispatch
;;;    (lambda (stream usage)
;;;      (with-accessors ((fixture name-use-fixture)
;;;                       (group name-use-group)
;;;                       (tests name-use-tests)) usage
;;;        (let ((tests-p (> (hash-table-count tests) 0)))
;;;          (cond
;;;            ((and fixture (not group) (not tests-p))
;;;             (let* ((fixture-inst (make-instance fixture))
;;;                    (the-names (loop for name in (bound-names fixture-inst)
;;;                                  if (symbol-package name)
;;;                                  collect name)))
;;;               (pprint-logical-block (stream '(1 2))
;;;                 (format stream "~a (package ~a) is a fixture"
;;;                   fixture
;;;                   (package-name (symbol-package fixture)))
;;;                 (pprint-newline :mandatory stream)
;;;                 (princ " - " stream)
;;;                 (pprint-logical-block (stream '(1 2))
;;;                   (princ "binds " stream)
;;;                   (cond
;;;                     (the-names (princ "name" stream)
;;;                                (unless (eql 1 the-names)
;;;                                  (princ "s" stream))
;;;                                (loop for (name . others) on the-names do
;;;                                      (princ " " stream)
;;;                                      (format stream "~a" name)
;;;                                      (when others
;;;                                        (princ "," stream)
;;;                                        (pprint-newline :fill stream))))
;;;                     (t (princ-filled-text "no accessible names" stream)))))))
;;;
;;;            ((and group (not fixture) (not tests-p))
;;;             (pprint-logical-block (stream '(1 2))
;;;               (format stream "~a (package ~a) is a test group"
;;;                 group
;;;                 (package-name (symbol-package group)))
;;;               (pprint-newline :mandatory stream)
;;;               (princ " - " stream)
;;;               (let ((test-names (test-names (make-instance group))))
;;;                 (pprint-logical-block (stream test-names)
;;;                   (princ "Contains test" stream)
;;;                   (unless (eql 1 (length test-names))
;;;                     (princ "s" stream))
;;;                   (loop for name = (pprint-pop) while name do
;;;                         (format stream " ~a" name)
;;;                         (pprint-exit-if-list-exhausted)
;;;                         (princ "," stream)
;;;                         (pprint-newline :fill stream))))))
;;;
;;;            ((and tests-p (not fixture) (not group))
;;;             (case (hash-table-count tests)
;;;               (1
;;;                (loop for tst being the hash-values of tests do
;;;                      (format stream "~a (package ~a) is a test in group ~a"
;;;                        tst
;;;                        (package-name (symbol-package tst))
;;;                        (group-record-name (make-instance tst)))))
;;;               (otherwise
;;;                (loop for tst being the hash-values of tests do
;;;                      (format stream "~a (package ~a) is a test in group ~a~%"
;;;                        tst
;;;                        (package-name (symbol-package tst))
;;;                        (group-record-name (make-instance tst)))))))
;;;
;;;            (t
;;;             (format stream "MULTI"))))))))

;;;
;;; Recording of results.  We use a hash table here --- unlike the
;;; method-based recording of test symbols, we're not worried about
;;; straddling the compile/load/run-time borders for result recording.
;;;
(defvar +results-record+ (make-hash-table :test 'eq)
  "Results of test runs.")

;;;;; Extracting information for debugging.
;;;
;;;(defgeneric trace-fixture (fx)
;;;  (:documentation "Provide debugging information about a fixture.")
;;;  (:method (fx) (format t "No known fixture ~s~%" fx)))
;;;(add-class-name-instantiator-method trace-fixture)
;;;
;;;(defgeneric trace-group (gr)
;;;  (:documentation "Provide debugging information about a group.")
;;;  (:method (gr) (format t "No known group ~s~%" gr)))
;;;(add-class-name-instantiator-method trace-group)
;;;
;;;(defgeneric trace-test (gr ts)
;;;  (:documentation "Provide debugging information about a test.")
;;;  (:method (gr ts) (format t "No known test ~s in group ~s~%" ts gr)))

(defun trace-results ()
  "Internal debugging function: dump the results hash."
  (loop for ts being the hash-keys of +results-record+ using (hash-value rs) do
    (format t "~s -> ~s~%" ts rs)))

